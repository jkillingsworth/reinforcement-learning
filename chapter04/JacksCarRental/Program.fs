module Program

open System

//-------------------------------------------------------------------------------------------------

let policy, values = Compute.generateResults () |> Seq.nth 4

Chart.renderPolicy @"..\..\..\JacksCarRental-Policy.png" policy
Chart.renderValues @"..\..\..\JacksCarRental-Values.png" values
