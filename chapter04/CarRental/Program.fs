module Program

open System

//-------------------------------------------------------------------------------------------------

let policy, values = Compute.generatePolicies () |> Seq.nth 4

Chart.renderPolicy @"..\..\..\CarRental-Policy.png" policy
Chart.renderValues @"..\..\..\CarRental-Values.png" values
