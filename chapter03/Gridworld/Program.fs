module Program

open System

//-------------------------------------------------------------------------------------------------

let values = Compute.generateResults 0.0 |> Seq.item 1000

Chart.renderGrid @"..\..\..\Gridworld.png" values
