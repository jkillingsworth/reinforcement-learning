module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()

let output = Compute.generateResults random
let counts = output |> Seq.take 8000 |> Seq.map snd |> Seq.toArray
let values = output |> Seq.item 1500000 |> fst
let states = Compute.computeTheRoute random values

Chart.renderStats @"..\..\..\WindyGridworld-Stats.png" counts
Chart.renderRoute @"..\..\..\WindyGridworld-Route.png" states
