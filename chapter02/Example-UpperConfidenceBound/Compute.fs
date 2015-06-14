module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let tasks = 2000
let steps = 1000
let n = 10

//-------------------------------------------------------------------------------------------------

type State =
    { ActionValuesActual : double[]
      ActionValuesApprox : double[]
      CountsOfEachAction : int[] }

type Value =
    { Reward : double
      OptimalActionTaken : bool }

type Result =
    { AverageReward : double
      OptimalAction : double }

//-------------------------------------------------------------------------------------------------

let selectOptimalAction actionValues =
    actionValues
    |> Array.mapi (fun i x -> i, x)
    |> Array.maxBy snd
    |> fst

let selectAction c random state =
    let evaluate action value =
        let na = double (state.CountsOfEachAction.[action])
        let t' = double (state.CountsOfEachAction |> Array.sum) + 1.0
        action, value + (c * (sqrt ((log t') / na)))
    state.ActionValuesApprox
    |> Array.mapi evaluate
    |> Array.maxBy snd
    |> fst

let executeOneStep c random state =

    let actionOptimal = selectOptimalAction state.ActionValuesActual
    let action = selectAction c random state
    let reward = state.ActionValuesActual.[action] + (Sample.normal 0.0 1.0 random)
    let approx = state.ActionValuesApprox.[action]
    let k' = 1 + state.CountsOfEachAction.[action]

    state.ActionValuesApprox.[action] <- approx + ((reward - approx) / double k')
    state.CountsOfEachAction.[action] <- k'

    { Reward = reward; OptimalActionTaken = (action = actionOptimal) }, state

let executeOneTask c random _ =

    let state =
        { ActionValuesActual = Array.init n (fun i -> Sample.normal 0.0 1.0 random)
          ActionValuesApprox = Array.create n 0.0
          CountsOfEachAction = Array.create n 0 }

    state
    |> Seq.unfold (executeOneStep c random >> Some)
    |> Seq.take steps
    |> Seq.toArray

let computeAverageResults (valuesOfTasks : Value[][]) =

    let average step selection =
        valuesOfTasks |> Seq.averageBy (fun values -> selection values.[step])

    let averageResult step =
        { AverageReward = average step (fun x -> x.Reward)
          OptimalAction = average step (fun x -> if x.OptimalActionTaken then 1.0 else 0.0) }

    Array.init steps averageResult

let computeResults c random =
    executeOneTask c random
    |> Array.init tasks
    |> computeAverageResults
