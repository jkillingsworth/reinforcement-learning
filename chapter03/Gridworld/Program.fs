module Program

open System

//-------------------------------------------------------------------------------------------------

let values = Compute.generateValues 0.0 |> Seq.nth 100

Chart.renderGrid @"..\..\..\Gridworld.png" values
