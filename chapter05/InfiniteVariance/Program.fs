module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let values = Compute.computeResults random

Chart.renderChart @"..\..\..\InfiniteVariance.png" values
