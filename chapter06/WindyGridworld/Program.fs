module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()

let result = Compute.generateResults random
let counts = result |> Seq.take 8000 |> Seq.map snd |> Seq.toArray
let values = result |> Seq.item 1500000 |> fst
let states = Compute.calculateTraces random values

Chart.renderStats @"..\..\..\WindyGridworld-Stats.png" counts
Chart.renderRoute @"..\..\..\WindyGridworld-Route.png" states
