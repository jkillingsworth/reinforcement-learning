module Program

open System
open Bandits

//-------------------------------------------------------------------------------------------------

let taskdefs =
    [ EpsilonGreedyAverage { Q1 = 0.0; Epsilon = 0.00 }
      EpsilonGreedyAverage { Q1 = 0.0; Epsilon = 0.01 }
      EpsilonGreedyAverage { Q1 = 0.0; Epsilon = 0.10 } ]

let randomng = Random()
let outcomes =
    taskdefs |> List.map (fun (taskdef) -> taskdef, Bandits.computeResults taskdef randomng)

let map selection =
    outcomes |> List.map (fun (taskdef, results) -> taskdef, results |> Array.map selection)

Chart.renderAverageReward @"..\..\..\AverageReward.png" (map (fun x -> x.AverageReward))
Chart.renderOptimalAction @"..\..\..\OptimalAction.png" (map (fun x -> x.OptimalAction))
