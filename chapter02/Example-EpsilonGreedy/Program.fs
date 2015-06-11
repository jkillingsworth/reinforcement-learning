module Program

open System

//-------------------------------------------------------------------------------------------------

let epsilons = [ 0.00; 0.01; 0.10 ]

let randomng = Random()
let outcomes =
    epsilons |> List.map (fun (epsilon) -> epsilon, Compute.computeResults epsilon randomng)

let map selection =
    outcomes |> List.map (fun (epsilon, results) -> epsilon, results |> Array.map selection)

Chart.renderAverageReward @"..\..\..\AverageReward.png" (map (fun x -> x.AverageReward))
Chart.renderOptimalAction @"..\..\..\OptimalAction.png" (map (fun x -> x.OptimalAction))
