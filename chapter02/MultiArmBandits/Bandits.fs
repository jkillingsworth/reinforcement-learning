module Bandits

open MathNet.Numerics.Distributions
open Sample

//-------------------------------------------------------------------------------------------------

let tasks = 2000
let steps = 1000
let n = 10
let meanActionValue = 0.0

//-------------------------------------------------------------------------------------------------

type private State =
    { ActualActionValues : double[]
      EstimationCriteria : double[]
      CountsOfEachAction : int[]
      AccumulatedRewards : double }

type private Value =
    { Reward : double
      OptimalActionTaken : bool }

type Result =
    { AverageReward : double
      OptimalAction : double }

type EpsilonGreedyAverage = { Q1 : double; Epsilon : double }
type UpperConfidenceBound = { Q1 : double; Confidence : double }
type GradientAscentBandit = { H1 : double; Alpha : double; Baseline : bool }

type Taskdef =
    | EpsilonGreedyAverage of EpsilonGreedyAverage
    | UpperConfidenceBound of UpperConfidenceBound
    | GradientAscentBandit of GradientAscentBandit

//-------------------------------------------------------------------------------------------------

let private selectActionOptimal actionValues =
    actionValues
    |> Array.mapi (fun action value -> action, value)
    |> Array.maxBy snd
    |> fst

let private selectActionEpsilonGreedyAverage taskdef random state =
    match (Sample.continuousUniform 0.0 1.0 random) with
    | x when x < taskdef.Epsilon
        -> Sample.discreteUniform 0 (n - 1) random
    | _ -> selectActionOptimal state.EstimationCriteria

let private selectActionUpperConfidenceBound taskdef random state =
    let evaluate action value =
        let na = double (state.CountsOfEachAction.[action])
        let t' = double (state.CountsOfEachAction |> Array.sum) + 1.0
        action, value + (taskdef.Confidence * (sqrt ((log t') / na)))
    state.EstimationCriteria
    |> Array.mapi evaluate
    |> Array.maxBy snd
    |> fst

let private selectActionGradientAscentBandit taskdef random state =
    let distribution = List.init n (fun action -> exp state.EstimationCriteria.[action])
    Sample.discreteDistribution distribution random

let private selectAction = function
    | EpsilonGreedyAverage taskdef -> selectActionEpsilonGreedyAverage taskdef
    | UpperConfidenceBound taskdef -> selectActionUpperConfidenceBound taskdef
    | GradientAscentBandit taskdef -> selectActionGradientAscentBandit taskdef

//-------------------------------------------------------------------------------------------------

let private recomputeEstimationEpsilonGreedyAverage taskdef state action reward =

    let approx = state.EstimationCriteria.[action]
    let k' = 1 + state.CountsOfEachAction.[action]
    state.EstimationCriteria.[action] <- approx + ((reward - approx) / double k')
    state

let private recomputeEstimationUpperConfidenceBound taskdef state action reward =

    let approx = state.EstimationCriteria.[action]
    let k' = 1 + state.CountsOfEachAction.[action]
    state.EstimationCriteria.[action] <- approx + ((reward - approx) / double k')
    state

let private recomputeEstimationGradientAscentBandit taskdef state action reward =

    let divisor = state.EstimationCriteria |> Array.map exp |> Array.sum
    let rewardBaseline = (reward + state.AccumulatedRewards) / double (1 + Array.sum state.CountsOfEachAction)
    let rewardBaseline = if taskdef.Baseline then rewardBaseline else 0.0

    let perferenceUpdate preference pi = function
        | a when a = action
            -> preference + taskdef.Alpha * (reward - rewardBaseline) * (1.0 - pi)
        | _ -> preference - taskdef.Alpha * (reward - rewardBaseline) * pi

    for a = 0 to (n - 1) do
        let pi = exp state.EstimationCriteria.[a] / double divisor
        let preference = state.EstimationCriteria.[a]
        state.EstimationCriteria.[a] <- perferenceUpdate preference pi a

    state

let private recomputeEstimation = function
    | EpsilonGreedyAverage taskdef -> recomputeEstimationEpsilonGreedyAverage taskdef
    | UpperConfidenceBound taskdef -> recomputeEstimationUpperConfidenceBound taskdef
    | GradientAscentBandit taskdef -> recomputeEstimationGradientAscentBandit taskdef

//-------------------------------------------------------------------------------------------------

let private executeOneStep taskdef random state =

    let actionOptimal = selectActionOptimal state.ActualActionValues
    let action = selectAction taskdef random state
    let reward = state.ActualActionValues.[action] + (Sample.normal 0.0 1.0 random)
    let state = recomputeEstimation taskdef state action reward

    state.CountsOfEachAction.[action] <- 1 + state.CountsOfEachAction.[action]

    let state = { state with AccumulatedRewards = reward + state.AccumulatedRewards }
    let value = { Reward = reward; OptimalActionTaken = (action = actionOptimal) }
    value, state

let private executeOneTask taskdef random _ =

    let initialEstimation =
        match taskdef with
        | EpsilonGreedyAverage taskdef -> taskdef.Q1
        | UpperConfidenceBound taskdef -> taskdef.Q1
        | GradientAscentBandit taskdef -> taskdef.H1

    let state =
        { ActualActionValues = Array.init n (fun i -> Sample.normal meanActionValue 1.0 random)
          EstimationCriteria = Array.create n initialEstimation
          CountsOfEachAction = Array.create n 0
          AccumulatedRewards = 0.0 }

    state
    |> Seq.unfold (executeOneStep taskdef random >> Some)
    |> Seq.take steps
    |> Seq.toArray

let private computeAverageResults (valuesOfTasks : Value[][]) =

    let average step selection =
        valuesOfTasks |> Seq.averageBy (fun values -> selection values.[step])

    let averageResult step =
        { AverageReward = average step (fun x -> x.Reward)
          OptimalAction = average step (fun x -> if x.OptimalActionTaken then 1.0 else 0.0) }

    Array.init steps averageResult

let computeResults tasdkdef random =
    executeOneTask tasdkdef random
    |> Array.init tasks
    |> computeAverageResults
