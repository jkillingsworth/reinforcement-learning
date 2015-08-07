module Compute

//-------------------------------------------------------------------------------------------------

let goal = 100
let pHeads = 0.55
let pTails = 1.0 - pHeads
let epsilon = 0.000000001

//-------------------------------------------------------------------------------------------------

let private actions s = seq { for a = 0 to min s (goal - s) do yield a }

let private computeActionValue (values : double[]) s a =

    let vHeads = pHeads * values.[s + a]
    let vTails = pTails * values.[s - a]

    vHeads + vTails

let private executeOneStep values =

    let compute s _ =
        actions s
        |> Seq.map (computeActionValue values s)
        |> Seq.max

    values |> Array.mapi compute

let computeValues sweeps =

    let initialize s = if s = goal then 1.0 else 0.0
    let values = Array.init (goal + 1) initialize

    let pairResult x = Some (x, x)
    let generate f x =
        seq { yield x; yield! x |> Seq.unfold (f >> pairResult) }

    generate executeOneStep values
    |> Seq.item sweeps

let computePolicy values =

    let aboutEqual x y = abs (x - y) < epsilon

    let compute s _ =

        let valuesByAction =
            actions s
            |> Seq.map (fun a -> a, computeActionValue values s a)
            |> Seq.toArray

        let valueMax =
            valuesByAction
            |> Seq.map snd
            |> Seq.max

        valuesByAction
        |> Seq.where (fun (a, value) -> aboutEqual value valueMax)
        |> Seq.map fst
        |> Seq.toArray

    values |> Array.mapi compute
