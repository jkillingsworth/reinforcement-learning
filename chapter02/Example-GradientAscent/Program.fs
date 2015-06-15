module Program

open System

//-------------------------------------------------------------------------------------------------

let taskdefs = [ (0.10, true); (0.40, true); (0.10, false); (0.40, false) ]

let randomng = Random()
let outcomes =
    taskdefs |> List.map (fun (taskdef) -> taskdef, Compute.computeResults taskdef randomng)

let map selection =
    outcomes |> List.map (fun (taskdef, results) -> taskdef, results |> Array.map selection)

Chart.renderAverageReward @"..\..\..\AverageReward.png" (map (fun x -> x.AverageReward))
Chart.renderOptimalAction @"..\..\..\OptimalAction.png" (map (fun x -> x.OptimalAction))
