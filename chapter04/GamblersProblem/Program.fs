module Program

open System

//-------------------------------------------------------------------------------------------------

let values  = Compute.computeValues 1000
let policy = Compute.computePolicy values

Chart.renderValues @"..\..\..\GamblersProblem-Values.png" values
Chart.renderPolicy @"..\..\..\GamblersProblem-Policy.png" policy
