module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()

let result = Compute.generateResults random
let counts = result |> Seq.take 8000 |> Seq.map snd |> Seq.toArray
let values = result |> Seq.item 1000000 |> fst
let traces = Compute.calculateTraces random values

Chart.renderStats @"..\..\..\WindyGridworld-Stats.png" counts
Chart.renderRoute @"..\..\..\WindyGridworld-Route.png" traces
