module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let values = Compute.computeResults random

let ordinary = "Ordinary", fst values
let weighted = "Weighted", snd values

Chart.renderChart @"..\..\..\OffPolicyEstimationOfBlackjackStateValue.png" [ ordinary; weighted ]
