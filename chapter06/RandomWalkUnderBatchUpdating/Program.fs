module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()

let alpha = 0.01

let errors =
    [ "MC", Compute.computeErrorsMC random alpha
      "TD", Compute.computeErrorsTD random alpha ]

Chart.renderErrors @"..\..\..\RandomWalkUnderBatchUpdating.png" errors "Batch Training"
