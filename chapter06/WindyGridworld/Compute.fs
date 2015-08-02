module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

type ActionSet =
    | StandardMoves
    | KingsMoves
    | KingsMovesWithDrift

type Action =
    | North
    | South
    | East
    | West
    | NorthEast
    | NorthWest
    | SouthEast
    | SouthWest
    | Drift

type Cell =
    | WS of int
    | Goal

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
        [ [ WS 0; WS 0; WS 0; WS 1; WS 1; WS 1; WS 2; WS 2; WS 1; WS 0 ]
          [ WS 0; WS 0; WS 0; WS 1; WS 1; WS 1; WS 2; WS 2; WS 1; WS 0 ]
          [ WS 0; WS 0; WS 0; WS 1; WS 1; WS 1; WS 2; WS 2; WS 1; WS 0 ]
          [ WS 0; WS 0; WS 0; WS 1; WS 1; WS 1; WS 2; Goal; WS 1; WS 0 ]
          [ WS 0; WS 0; WS 0; WS 1; WS 1; WS 1; WS 2; WS 2; WS 1; WS 0 ]
          [ WS 0; WS 0; WS 0; WS 1; WS 1; WS 1; WS 2; WS 2; WS 1; WS 0 ]
          [ WS 0; WS 0; WS 0; WS 1; WS 1; WS 1; WS 2; WS 2; WS 1; WS 0 ] ]

let start = (0, 3)
let gamma = 1.0
let alpha = 0.5
let epsilon = 0.1
let reward = -1.0

let actionSet = StandardMoves

//-------------------------------------------------------------------------------------------------

let private actions =
    match actionSet with
    | StandardMoves       -> [| North; South; East; West |]
    | KingsMoves          -> [| North; South; East; West; NorthEast; NorthWest; SouthEast; SouthWest |]
    | KingsMovesWithDrift -> [| North; South; East; West; NorthEast; NorthWest; SouthEast; SouthWest; Drift |]

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
    | NorthEast -> (+1, +1)
    | NorthWest -> (-1, +1)
    | SouthEast -> (+1, -1)
    | SouthWest -> (-1, -1)
    | Drift     -> ( 0,  0)

let private windOffset (x, y) =
    match cells.[x, y] with
    | WS w -> (0, w)
    | Goal -> (0, 0)

let private nextState (x, y) action =

    let move = moveOffset action
    let wind = windOffset (x, y)
    let x = x + fst move + fst wind
    let y = y + snd move + snd wind
    let x = min (max x 0) (Array2D.length1 cells - 1)
    let y = min (max y 0) (Array2D.length2 cells - 1)
    let state' = (x, y)

    state'

let private executeOneStep random learn (values, state, action) =

    let values' = copyValues values
    let state' = nextState state action
    let action' = selectAction random learn values state'

    if learn then
        let qNext = getValue values state' action'
        let q = getValue values state action
        let q = q + alpha * (reward + (gamma * qNext) - q)
        q |> setValue values' state action

    values', state', action'

let private executeOneEpisode random learn values start =

    let state = start
    let action = selectAction random learn values state

    let pairResult x = Some (x,x)

    let generator (values, state, action) =
        let x, y = state
        match cells.[x, y] with
        | Goal -> None
        | WS w -> executeOneStep random learn (values, state, action) |> pairResult

    (values, state, action) |> List.unfold generator

//-------------------------------------------------------------------------------------------------

let generateResults random =

    let learn = true

    let length1 = cells |> Array2D.length1
    let length2 = cells |> Array2D.length2
    let length3 = actions |> Array.length

    let values = Array3D.create length1 length2 length3 0.0
    let count = 0

    let generator (values, count) =
        let results = executeOneEpisode random learn values start |> Seq.map (fun (v, s, a) -> v, count)
        let values = results |> Seq.last |> fst
        let count = count + 1
        Some (results, (values, count))

    (values, count)
    |> Seq.unfold generator
    |> Seq.concat

let calculateTraces random values =

    let learn = false
    let states = executeOneEpisode random learn values start |> List.map (fun (v, s, a) -> s)
    start :: states
