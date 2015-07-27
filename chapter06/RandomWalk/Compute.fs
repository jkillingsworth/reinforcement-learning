module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let initialValue = 0.5

let trueValues =
    [| 1.0 / 6.0
       2.0 / 6.0
       3.0 / 6.0
       4.0 / 6.0
       5.0 / 6.0 |]

let plays = 100
let tasks = 100

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

let private executeOneEpisode random =
    let initialState = C
    List.unfold (executeOneStep random) initialState

//-------------------------------------------------------------------------------------------------

let private executeOneEpisodeMC random alpha values =

    let steps = executeOneEpisode random
    let values = Array.copy values
    let outcome = steps |> List.sumBy (fun (s, r', s') -> r')

    for (state, reward', state') in steps do
        let i = index state
        let v = values.[i]
        values.[i] <- v + alpha * (double outcome - v)

    values

let private executeOneEpisodeTD random alpha values =

    let steps = executeOneEpisode random
    let values = Array.copy values

    for (state, reward', state') in steps do
        let i = index state
        let v = values.[i]
        let vNext = if state' = Terminal then 0.0 else values.[index state']
        values.[i] <- v + alpha * (double reward' + vNext - v)

    values

//-------------------------------------------------------------------------------------------------

let private computeValues random alpha executeOneEpisode =

    let values = Array.create 5 initialValue

    let pairResult x = Some (x,x)
    let generate f x =
        seq { yield x; yield! x |> Seq.unfold (f >> pairResult) }

    values |> generate (executeOneEpisode random alpha)

let computeValuesMC random alpha = computeValues random alpha executeOneEpisodeMC
let computeValuesTD random alpha = computeValues random alpha executeOneEpisodeTD

//-------------------------------------------------------------------------------------------------

let private executeOneTask random alpha executeOneEpisode _ =

    computeValues random alpha executeOneEpisode
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

let private computeErrors random alpha executeOneEpisode =

    executeOneTask random alpha executeOneEpisode
    |> Array.init tasks
    |> computeRootMeanSquareError
    |> Array.init plays

let computeErrorsMC random alpha = computeErrors random alpha executeOneEpisodeMC
let computeErrorsTD random alpha = computeErrors random alpha executeOneEpisodeTD
