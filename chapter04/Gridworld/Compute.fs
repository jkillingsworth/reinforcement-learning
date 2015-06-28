module Compute

//-------------------------------------------------------------------------------------------------

type Action =
    | North
    | South
    | East
    | West

type State =
    | S of int
    | Term
    | Null

//-------------------------------------------------------------------------------------------------

let cells =
    array2D
        [ [ Term; S 01; S 02; S 03 ]
          [ S 04; S 05; S 06; S 07 ]
          [ S 08; S 09; S 10; S 11 ]
          [ S 12; S 13; S 14; Term ]
          [ Null; S 15; Null; Null ] ]

let reward = -1.0
let gamma = 1.0

//-------------------------------------------------------------------------------------------------

let private actions = [ North; South; East; West ]

let private cellCoordinates s =
    let rows = cells |> Array2D.length1
    let cols = cells |> Array2D.length2
    seq { for m = 0 to rows - 1 do
          for n = 0 to cols - 1 do
          if cells.[m, n] = s then yield m, n }

let private cellCoordinate =
    cellCoordinates >> Seq.head

let private offset = function
    | North -> (-1, 0)
    | South -> (+1, 0)
    | East  -> (0, +1)
    | West  -> (0, -1)

let private move s a =
    let m, n = cellCoordinate s
    let i, j = offset a
    cells.[m + i, n + j]

let private actionGoesOffGrid s a =
    let m, n = cellCoordinate s
    let i, j = offset a
    match m + i, n + j with
    | m, n when m < 0 -> true
    | m, n when n < 0 -> true
    | m, n when m >= Array2D.length1 cells -> true
    | m, n when n >= Array2D.length2 cells -> true
    | m, n -> cells.[m, n] = Null

let private evaluateAction = function
    | S 15, North -> S 13
    | S 15, South -> S 15
    | S 15, East  -> S 14
    | S 15, West  -> S 12
    | S 13, South -> S 15
    | S sn, a when actionGoesOffGrid (S sn) a -> S sn
    | S sn, a -> move (S sn) a
    | _ -> failwith "Cannot evaluate action from null or terminal state."

let private pi values = function
    | North -> 0.25
    | South -> 0.25
    | East  -> 0.25
    | West  -> 0.25

//-------------------------------------------------------------------------------------------------

let private executeOneStep (values : double[,]) =

    let calculate s action =
        let s' = evaluateAction (s, action)
        let m', n' = s' |> cellCoordinate
        (pi values action) * (reward + gamma * values.[m', n'])

    let calculate m n action =
        match cells.[m, n] with
        | Null -> nan
        | Term -> 0.0
        | s -> calculate s action

    let calculate m n _ = actions |> Seq.sumBy (calculate m n)

    values |> Array2D.mapi calculate

let generateResults initialValue =

    let rows = cells |> Array2D.length1
    let cols = cells |> Array2D.length2

    let initializer m n =
        match cells.[m, n] with
        | Null -> nan
        | Term -> 0.0
        | S sn -> initialValue

    let values = Array2D.init rows cols initializer

    let pairResult x = Some (x,x)
    let generate f x =
        seq { yield x; yield! x |> Seq.unfold (f >> pairResult) }

    generate executeOneStep values
