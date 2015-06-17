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

type Alpha =
    | Constant of double
    | OneOverK

type EpsilonGreedyProcess = { Q1 : double; Alpha : Alpha; Epsilon : double }
type UpperConfidenceBound = { Q1 : double; Alpha : Alpha; Confidence : double }
type GradientAscentBandit = { H1 : double; Alpha : Alpha; Baseline : bool }

type Taskdef =
    | EpsilonGreedyProcess of EpsilonGreedyProcess
    | UpperConfidenceBound of UpperConfidenceBound
    | GradientAscentBandit of GradientAscentBandit

//-------------------------------------------------------------------------------------------------

let private selectActionOptimal actionValues =
    actionValues
    |> Array.mapi (fun action value -> action, value)
    |> Array.maxBy snd
    |> fst

let private selectActionEpsilonGreedyProcess taskdef random state =
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
    | EpsilonGreedyProcess taskdef -> selectActionEpsilonGreedyProcess taskdef
    | UpperConfidenceBound taskdef -> selectActionUpperConfidenceBound taskdef
    | GradientAscentBandit taskdef -> selectActionGradientAscentBandit taskdef

//-------------------------------------------------------------------------------------------------

module private Array =
    let mapUpdate k update = Array.mapi (fun i x -> if i = k then update x else x)

let private recomputeAlpha state action = function
    | Constant alpha -> alpha
    | OneOverK -> 1.0 / double (1 + state.CountsOfEachAction.[action])

let private recomputeActionValue alpha state action reward =

    let alpha = recomputeAlpha state action alpha
    let calculate value = value + alpha * (reward - value)
    state.EstimationCriteria |> Array.mapUpdate action calculate

let private recomputePreferences taskdef state action reward =

    let alpha = recomputeAlpha state action taskdef.Alpha
    let baseline = (reward + state.AccumulatedRewards) / double (1 + Array.sum state.CountsOfEachAction)
    let baseline = if taskdef.Baseline then baseline else 0.0
    let divisor = state.EstimationCriteria |> Array.map exp |> Array.sum

    let calculate preference pi = function
        | a' when a' = action
            -> preference + alpha * (reward - baseline) * (1.0 - pi)
        | _ -> preference - alpha * (reward - baseline) * pi

    let calculate a' preference =
        let pi = exp preference / divisor
        calculate preference pi a'

    state.EstimationCriteria |> Array.mapi calculate

let private recomputeEstimation = function
    | EpsilonGreedyProcess taskdef -> recomputeActionValue taskdef.Alpha
    | UpperConfidenceBound taskdef -> recomputeActionValue taskdef.Alpha
    | GradientAscentBandit taskdef -> recomputePreferences taskdef

let private recomputeCountsOfEachAction state action =
    state.CountsOfEachAction |> Array.mapUpdate action (fun count -> count + 1)

let private recomputeAccumulatedRewards state reward =
    state.AccumulatedRewards + reward

//-------------------------------------------------------------------------------------------------

let private executeOneStep taskdef random state =

    let actionOptimal = selectActionOptimal state.ActualActionValues
    let action = selectAction taskdef random state
    let reward = state.ActualActionValues.[action] + (Sample.normal 0.0 1.0 random)

    { Reward = reward
      OptimalActionTaken = (action = actionOptimal) },

    { ActualActionValues = state.ActualActionValues
      EstimationCriteria = recomputeEstimation taskdef state action reward
      CountsOfEachAction = recomputeCountsOfEachAction state action
      AccumulatedRewards = recomputeAccumulatedRewards state reward }

let private executeOneTask taskdef random _ =

    let initialEstimation =
        match taskdef with
        | EpsilonGreedyProcess taskdef -> taskdef.Q1
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
