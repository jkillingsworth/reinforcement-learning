module Program

open System

//-------------------------------------------------------------------------------------------------

let cFactors = [ 2.0; 3.0; 4.0 ]

let randomng = Random()
let outcomes =
    cFactors |> List.map (fun (cFactor) -> cFactor, Compute.computeResults cFactor randomng)

let map selection =
    outcomes |> List.map (fun (cFactor, results) -> cFactor, results |> Array.map selection)

Chart.renderAverageReward @"..\..\..\AverageReward.png" (map (fun x -> x.AverageReward))
Chart.renderOptimalAction @"..\..\..\OptimalAction.png" (map (fun x -> x.OptimalAction))
