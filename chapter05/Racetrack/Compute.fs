module Compute

open System.Collections.Generic
open MathNet.Numerics.Distributions
open Track

//-------------------------------------------------------------------------------------------------

let track = racetrack1
let maxVelocity = (5, 5)
let rewardTrack = -1
let rewardCrash = -5
let epsilon = 0.1

//-------------------------------------------------------------------------------------------------

type State =
    (int * int * int * int)

type Action =
    (int * int)

type Policy =
    Action[,,,]

type Values =
    double[,][,,,]

type Counts =
    int[,][,,,]

//-------------------------------------------------------------------------------------------------

let private availableActions =

    let actions =
        [ (-1, -1); ( 0, -1); (+1, -1)
          (-1,  0); ( 0,  0); (+1,  0)
          (-1, +1); ( 0, +1); (+1, +1) ]

    let vxMax = fst maxVelocity
    let vyMax = snd maxVelocity

    let initialize vx vy =
        actions
        |> List.filter (fun (ax, ay) -> ax + vx <> 0 || ay + vy <> 0)
        |> List.filter (fun (ax, ay) -> ax + vx >= 0 && ay + vy >= 0)
        |> List.filter (fun (ax, ay) -> ax + vx <= vxMax && ay + vy <= vyMax)

    initialize |> Array2D.init (vxMax + 1) (vyMax + 1)

//-------------------------------------------------------------------------------------------------

let private selectAction random explore (policy : Policy) (px, py, vx, vy) =
    
    let explore = explore && Sample.continuousUniform 0.0 1.0 random < epsilon
    if (explore) then
        let actions = availableActions.[vx, vy]
        let index = Sample.discreteUniform 0 (actions.Length - 1) random
        actions.[index]
    else
        policy.[px, py, vx, vy]

let private computeDisplacement random =
    match Sample.discreteUniform 0 3 random with
    | 0 -> (0, 1)
    | 1 -> (1, 0)
    | _ -> (0, 0)

let private computeOffsetTarget random vx' vy' =
    let d = computeDisplacement random
    let x = vx' + fst d
    let y = vy' + snd d
    (x, y)

let private computeOffsetActual tx ty px py =
    let crashes = function
        | px, py when px > Array2D.length1 track - 1 -> true
        | px, py when py > Array2D.length2 track - 1 -> true
        | px, py -> track.[px, py] = Crash
    let rec computeX tx = if crashes (px + tx, py) then computeX (tx - 1) else tx
    let rec computeY ty = if crashes (px, py + ty) then computeY (ty - 1) else ty
    let x = computeX tx
    let y = computeY ty
    (x, y)

let private computeReward tx ty ox oy =
    if ox = tx && oy = ty then
        rewardTrack
    else
        rewardCrash

let private advanceOffsetActual ox oy vx' vy' =
    let x = if ox = 0 && oy = 0 && vx' = 0 then 1 else ox
    let y = if ox = 0 && oy = 0 && vy' = 0 then 1 else oy
    (x, y)

let rec private driveTheTrack random explore policy state traces =

    let action = selectAction random explore policy state

    let px, py, vx, vy = state
    let ax, ay = action

    let vx' = vx + ax
    let vy' = vy + ay

    let tx, ty = computeOffsetTarget random vx' vy'
    let ox, oy = computeOffsetActual tx ty px py
    let reward = computeReward tx ty ox oy
    let ox, oy = advanceOffsetActual ox oy vx' vy'

    let px' = px + ox
    let py' = py + oy

    let traces = (state, action, reward) :: traces
    let state' = (px', py', vx', vy')

    match track.[px', py'] with
    | Final
        -> (state', (0, 0), 0) :: traces
    | _ -> driveTheTrack random explore policy state' traces

//-------------------------------------------------------------------------------------------------

let private improveValues traces (values : Values) (counts : Counts) =

    let outcome = traces |> List.map (fun (s, a, r) -> r) |> List.sum

    let updateValues (state, action, _) =
        let px, py, vx, vy = state
        let ax, ay = action
        let v = values.[px, py, vx, vy].[ax + 1, ay + 1]
        let n = counts.[px, py, vx, vy].[ax + 1, ay + 1]
        let v' = v + ((double outcome - v) / double (n + 1))
        let n' = n + 1
        values.[px, py, vx, vy].[ax + 1, ay + 1] <- v'
        counts.[px, py, vx, vy].[ax + 1, ay + 1] <- n'

    traces |> List.iter updateValues

let private improvePolicy traces (values : Values) (policy : Policy) =

    let updatePolicy (state, action, _) =
        let px, py, vx, vy = state
        let actions = availableActions.[vx, vy]
        let action = actions |> Seq.maxBy (fun (ax, ay) -> values.[px, py, vx, vy].[ax + 1, ay + 1])
        policy.[px, py, vx, vy] <- action

    traces |> List.iter updatePolicy

let private executeOneEpisode random startPositions (policy, values, counts) =
    
    let count = startPositions |> Array.length
    let start = startPositions.[Sample.discreteUniform 0 (count - 1) random]

    let explore = true
    let px, py = start
    let vx, vy = (0, 0)
    let state = (px, py, vx, vy)

    let traces = driveTheTrack random explore policy state []

    improveValues traces values counts
    improvePolicy traces values policy

    (policy, values, counts)

//-------------------------------------------------------------------------------------------------

let computePolicy random =

    let positions =
        [| for px = 0 to Array2D.length1 track - 1 do
           for py = 0 to Array2D.length2 track - 1 do
               if track.[px, py] <> Crash then
                   yield px, py |]

    let states =
        [| for px, py in positions do
           for vx = 0 to fst maxVelocity do
           for vy = 0 to snd maxVelocity do
               yield px, py, vx, vy |]

    let stateActions =
        [| for px, py, vx, vy in states do
               let actions = availableActions.[vx, vy]
               yield (px, py, vx, vy), actions |]
        |> Map.ofArray

    let startPositions =
        positions
        |> Array.filter (fun (px, py) -> track.[px, py] = Start)

    let pxLength = Array2D.length1 track
    let pyLength = Array2D.length2 track
    let vxLength = fst maxVelocity + 1
    let vyLength = snd maxVelocity + 1

    let policy = Array4D.init pxLength pyLength vxLength vyLength (fun _ _ _ _ -> (0, 0))
    let values = Array4D.init pxLength pyLength vxLength vyLength (fun _ _ _ _ -> Array2D.create 3 3 0.0)
    let counts = Array4D.init pxLength pyLength vxLength vyLength (fun _ _ _ _ -> Array2D.create 3 3 0)

    for (px, py, vx, vy) in states do
        let actions = availableActions.[vx, vy]
        let action = actions.[Sample.discreteUniform 0 (actions.Length - 1) random]
        policy.[px, py, vx, vy] <- action

    let pairResult x = Some (x, x)
    let generate f x =
        seq { yield x; yield! x |> Seq.unfold (f >> pairResult) }

    generate (executeOneEpisode random startPositions) (policy, values, counts)
    |> Seq.map (fun (policy, _, _) -> policy)

let executePolicy random policy start =

    let explore = false
    let px, py = start
    let vx, vy = (0, 0)
    let state = (px, py, vx, vy)

    driveTheTrack random explore policy state []
    |> List.rev
