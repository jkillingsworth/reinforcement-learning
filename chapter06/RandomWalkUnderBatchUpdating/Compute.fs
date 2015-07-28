module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let initialValue = 0.0

let trueValues =
    [| 1.0 / 6.0
       2.0 / 6.0
       3.0 / 6.0
       4.0 / 6.0
       5.0 / 6.0 |]

let plays = 100
let tasks = 100

let theta = 0.0001

//-------------------------------------------------------------------------------------------------

type private State =
    | A
    | B
    | C
    | D
    | E
    | Terminal

type private Action =
    | L
    | R

//-------------------------------------------------------------------------------------------------

let private index = function
    | A -> 0
    | B -> 1
    | C -> 2
    | D -> 3
    | E -> 4
    | Terminal -> failwith "Unexpected state."

let private nextStateL = function
    | A -> 0, Terminal
    | B -> 0, A
    | C -> 0, B
    | D -> 0, C
    | E -> 0, D
    | Terminal -> 0, Terminal

let private nextStateR = function
    | E -> 1, Terminal
    | D -> 0, E
    | C -> 0, D
    | B -> 0, C
    | A -> 0, B
    | Terminal -> 0, Terminal

let private nextState = function
    | L -> nextStateL
    | R -> nextStateR

let private nextAction random =
    match Sample.discreteUniform 0 1 random with
    | 0 -> L
    | _ -> R

let private executeOneStep random = function
    | Terminal -> None
    | state ->
        let action = nextAction random
        let reward', state' = nextState action state
        Some ((state, reward', state'), state')

//-------------------------------------------------------------------------------------------------

let private improveValuesMC alpha (values : double[]) steps =

    let outcome = Array.sumBy (fun (s, r', s') -> r') steps

    for (state, reward', state') in steps do
        let i = index state
        let v = values.[i]
        values.[i] <- v + alpha * (double outcome - v)

    values

let private improveValuesTD alpha (values : double[]) steps =

    for (state, reward', state') in steps do
        let i = index state
        let v = values.[i]
        let vNext = if state' = Terminal then 0.0 else values.[index state']
        values.[i] <- v + alpha * (double reward' + vNext - v)

    values

let private executeOneEpisode random improveValues (values, episodes) =

    let initialState = C
    let steps = Array.unfold (executeOneStep random) initialState
    let values = Array.copy values
    let episodes = steps :: episodes

    let computeDelta previous values =
        values
        |> Array.map2 (fun x y -> abs (x - y)) previous
        |> Array.max

    let batchUpdate = function
        | values, delta when delta < theta -> None
        | values, delta
            ->
            let previous = values |> Array.copy
            let values = episodes |> List.fold improveValues values
            let delta = computeDelta previous values
            Some (values, (values, delta))

    let values = Seq.unfold batchUpdate (values, theta) |> Seq.last

    values, episodes

//-------------------------------------------------------------------------------------------------

let private computeValues random improveValues =

    let (values, episodes) = Array.create 5 initialValue, []

    let pairResult x = Some (x,x)
    let generate f x =
        seq { yield x; yield! x |> Seq.unfold (f >> pairResult) }

    (values, episodes) |> generate (executeOneEpisode random improveValues)

let private executeOneTask random improveValues _ =

    computeValues random improveValues
    |> Seq.map fst
    |> Seq.take plays
    |> Seq.toArray

let private computeRootMeanSquareError (tasks : double[][][]) i =
    tasks
    |> Seq.map (fun plays -> plays.[i])
    |> Seq.map (Array.mapi (fun i x -> trueValues.[i] - x))
    |> Seq.map (Array.mapi (fun i x -> x ** 2.0))
    |> Seq.map (Array.average)
    |> Seq.map (sqrt)
    |> Seq.average

let private computeErrors random improveValues =

    executeOneTask random improveValues
    |> Array.init tasks
    |> computeRootMeanSquareError
    |> Array.init plays

let computeErrorsMC random alpha = computeErrors random (improveValuesMC alpha)
let computeErrorsTD random alpha = computeErrors random (improveValuesTD alpha)
