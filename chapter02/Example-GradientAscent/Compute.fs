module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let tasks = 2000
let steps = 1000
let n = 10

//-------------------------------------------------------------------------------------------------

type State =
    { ActionValuesActual : double[]
      PreferencesForEach : double[]
      AccumulatedRewards : double[]
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

let selectAction random state =
    let distribution = List.init n (fun action -> exp state.PreferencesForEach.[action])
    Sample.discreteDistribution distribution random

let executeOneStep taskdef random state =

    let actionOptimal = selectOptimalAction state.ActionValuesActual
    let action = selectAction random state
    let reward = state.ActionValuesActual.[action] + (Sample.normal 0.0 1.0 random)
    let k' = 1 + state.CountsOfEachAction.[action]

    state.AccumulatedRewards.[action] <- reward + state.AccumulatedRewards.[action]
    state.CountsOfEachAction.[action] <- k'

    let alpha, useBaseline = taskdef
    let divisor = state.PreferencesForEach |> Array.map exp |> Array.sum
    let rewardBaseline = (Array.sum state.AccumulatedRewards) / double (Array.sum state.CountsOfEachAction)
    let rewardBaseline = if useBaseline then rewardBaseline else 0.0

    let perferenceUpdate preference pi = function
        | a when a = action
            -> preference + alpha * (reward - rewardBaseline) * (1.0 - pi)
        | _ -> preference - alpha * (reward - rewardBaseline) * pi

    for a = 0 to (n - 1) do
        let pi = exp state.PreferencesForEach.[a] / double divisor
        let preference = state.PreferencesForEach.[a]
        state.PreferencesForEach.[a] <- perferenceUpdate preference pi a

    { Reward = reward; OptimalActionTaken = (action = actionOptimal) }, state

let executeOneTask epsilon random _ =

    let state =
        { ActionValuesActual = Array.init n (fun i -> Sample.normal 4.0 1.0 random)
          PreferencesForEach = Array.create n 0.0
          AccumulatedRewards = Array.create n 0.0
          CountsOfEachAction = Array.create n 0 }

    state
    |> Seq.unfold (executeOneStep epsilon random >> Some)
    |> Seq.take steps
    |> Seq.toArray

let computeAverageResults (valuesOfTasks : Value[][]) =

    let average step selection =
        valuesOfTasks |> Seq.averageBy (fun values -> selection values.[step])

    let averageResult step =
        { AverageReward = average step (fun x -> x.Reward)
          OptimalAction = average step (fun x -> if x.OptimalActionTaken then 1.0 else 0.0) }

    Array.init steps averageResult

let computeResults taskdef random =
    executeOneTask taskdef random
    |> Array.init tasks
    |> computeAverageResults
