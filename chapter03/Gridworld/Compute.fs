module Compute

//-------------------------------------------------------------------------------------------------

type Action =
    | North
    | South
    | East
    | West

type Cell =
    | Special of (int * int) * double
    | Regular

type Policy =
    | Equiprobable
    | Optimal

//-------------------------------------------------------------------------------------------------

let private initializeCells m n =
    match m, n with
    | 0, 1 -> Special ((4, 1), +10.0)
    | 0, 3 -> Special ((2, 3), +5.0)
    | _, _ -> Regular

let cells = Array2D.init 5 5 initializeCells
let rewardRegular =  0.0
let rewardOffGrid = -1.0
let policy = Equiprobable
let gamma = 0.9

//-------------------------------------------------------------------------------------------------

let private actions = [ North; South; East; West ]

let private offset = function
    | North -> (-1, 0)
    | South -> (+1, 0)
    | East  -> (0, +1)
    | West  -> (0, -1)

let private move action (m, n) =
    let i, j = offset action
    m + i, n + j

let private actionGoesOffGrid action (m, n) =
    match move action (m, n) with
    | m, n when m < 0 -> true
    | m, n when n < 0 -> true
    | m, n when m >= Array2D.length1 cells -> true
    | m, n when n >= Array2D.length2 cells -> true
    | _ -> false

let private evaluateAction m n action =
    let cell = cells.[m, n]
    match cell with
    | Special ((m', n'), reward) -> reward, (m', n')
    | Regular when actionGoesOffGrid action (m, n) -> rewardOffGrid, (m, n)
    | Regular -> rewardRegular, move action (m, n)

let private piEquiprobable values m n = function
    | North -> 0.25
    | South -> 0.25
    | East  -> 0.25
    | West  -> 0.25

let private piOptimal (values : double[,]) m n action =
    let mapping a =
        let _, (m', n') = evaluateAction m n a
        values.[m', n'], a
    actions
    |> List.map mapping
    |> List.maxBy fst
    |> snd
    |> (fun a -> if a = action then 1.0 else 0.0)

let private pi = match policy with Equiprobable -> piEquiprobable | Optimal -> piOptimal

//-------------------------------------------------------------------------------------------------

let private executeOneStep (values : double[,]) =

    let calculate m n action =
        let reward, (m', n') = evaluateAction m n action
        (pi values m n action) * (reward + gamma * values.[m', n'])

    let calculate m n _ = actions |> Seq.sumBy (calculate m n)

    values
    |> Array2D.mapi calculate
    |> (fun x -> x, x)

let generateValues initialValue =

    let rows = cells |> Array2D.length1
    let cols = cells |> Array2D.length2

    initialValue
    |> Array2D.create rows cols
    |> Seq.unfold (executeOneStep >> Some)
