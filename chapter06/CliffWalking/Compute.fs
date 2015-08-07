module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

type LearningMethod =
    | QLearning
    | Sarsa

type Action =
    | North
    | South
    | East
    | West

type Cell =
    | Walk
    | Goal
    | Fall

type Values =
    double[,,]

type State =
    (int * int)

//-------------------------------------------------------------------------------------------------

let private cellInitialization =

    let pivotCells cells =
        let rows = cells |> Array2D.length1
        let cols = cells |> Array2D.length2
        Array2D.init cols rows (fun m n -> cells.[n, m])

    Seq.rev >> array2D >> pivotCells

let cells =
    cellInitialization
        [ [ Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk ]
          [ Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk ]
          [ Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk; Walk ]
          [ Walk; Fall; Fall; Fall; Fall; Fall; Fall; Fall; Fall; Fall; Fall; Goal ] ]

let start = (0, 0)
let gamma = 1.0
let alpha = 0.5
let epsilon = 0.1
let rewardWalk = -1.0
let rewardFall = -100.0

//-------------------------------------------------------------------------------------------------

let actions = [| North; South; East; West |]

let private selectAction random explore (values : Values) (x, y) =

    let explore = explore && Sample.continuousUniform 0.0 1.0 random < epsilon
    if (explore) then
        let index = Sample.discreteUniform 0 (actions.Length - 1) random
        actions.[index]
    else
        actions
        |> Array.mapi (fun i action -> action, values.[x, y, i])
        |> Array.maxBy (fun (action, value) -> value)
        |> fst

//-------------------------------------------------------------------------------------------------

let private getValue (values : Values) state action =
    let x, y = state
    let z = actions |> Array.findIndex (fun a -> a = action)
    values.[x, y, z]

let private setValue (values : Values) state action value =
    let x, y = state
    let z = actions |> Array.findIndex (fun a -> a = action)
    values.[x, y, z] <- value

let private copyValues values =
    let length1 = values |> Array3D.length1
    let length2 = values |> Array3D.length2
    let length3 = values |> Array3D.length3
    Array3D.init length1 length2 length3 (fun x y z -> values.[x, y, z])

//-------------------------------------------------------------------------------------------------

let private moveOffset = function
    | North     -> ( 0, +1)
    | South     -> ( 0, -1)
    | East      -> (+1,  0)
    | West      -> (-1,  0)

let private nextState (x, y) action =

    let move = moveOffset action
    let x = x + fst move
    let y = y + snd move
    let x = min (max x 0) (Array2D.length1 cells - 1)
    let y = min (max y 0) (Array2D.length2 cells - 1)

    match cells.[x, y] with
    | Walk
    | Goal -> rewardWalk, (x, y)
    | Fall -> rewardFall, start

//-------------------------------------------------------------------------------------------------

module private QLearning =

    let executeOneStep random explore (values, state, action, _) =

        let values' = copyValues values
        let reward, state' = nextState state action

        let qNext = actions |> Array.map (getValue values state') |> Array.max
        let q = getValue values state action
        let q = q + alpha * (reward + (gamma * qNext) - q)

        q |> setValue values' state action

        let action' = selectAction random explore values' state'

        values', state', action', reward

    let executeOneEpisode random explore values start =

        let state = start
        let action = selectAction random false values state

        let pairResult x = Some (x, x)

        let generator (values, state, action, reward) =
            let x, y = state
            match cells.[x, y] with
            | Goal -> None
            | Walk
            | Fall -> executeOneStep random explore (values, state, action, reward) |> pairResult

        (values, state, action, 0.0) |> List.unfold generator

//-------------------------------------------------------------------------------------------------

module private Sarsa =

    let executeOneStep random explore (values, state, action, _) =

        let values' = copyValues values
        let reward, state' = nextState state action
        let action' = selectAction random explore values state'

        let qNext = getValue values state' action'
        let q = getValue values state action
        let q = q + alpha * (reward + (gamma * qNext) - q)

        q |> setValue values' state action

        values', state', action', reward

    let executeOneEpisode random explore values start =

        let state = start
        let action = selectAction random explore values state

        let pairResult x = Some (x, x)

        let generator (values, state, action, reward) =
            let x, y = state
            match cells.[x, y] with
            | Goal -> None
            | Walk
            | Fall -> executeOneStep random explore (values, state, action, reward) |> pairResult

        (values, state, action, 0.0) |> List.unfold generator

//-------------------------------------------------------------------------------------------------

let private executeOneEpisode = function
    | QLearning -> QLearning.executeOneEpisode
    | Sarsa -> Sarsa.executeOneEpisode

let generateResults random learningMethod =

    let explore = true

    let length1 = cells |> Array2D.length1
    let length2 = cells |> Array2D.length2
    let length3 = actions |> Array.length

    let values = Array3D.create length1 length2 length3 0.0
    let totals = 0.0

    let generator (values, _) =
        let output = executeOneEpisode learningMethod random explore values start
        let values = output |> List.map (fun (v, s, a, r) -> v) |> List.last
        let totals = output |> List.map (fun (v, s, a, r) -> r) |> List.sum
        (values, totals)

    let pairResult x = Some (x, x)

    Seq.unfold (generator >> pairResult) (values, totals)

let computeTheRoute random learningMethod values =

    let explore = false
    let states = executeOneEpisode learningMethod random explore values start |> List.map (fun (v, s, a, r) -> s)
    start :: states
