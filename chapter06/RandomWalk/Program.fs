module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()

let alpha = 0.1

let countsMC = [ 0; 1; 10; 100 ]
let valuesMC = countsMC |> List.map (fun count -> count, Compute.computeValuesMC random alpha |> Seq.item count)

let countsTD = [ 0; 1; 10; 100 ]
let valuesTD = countsTD |> List.map (fun count -> count, Compute.computeValuesTD random alpha |> Seq.item count)

let alphasMC = [ 0.01; 0.02; 0.03; 0.04 ]
let errorsMC = alphasMC |> List.map (fun alpha -> alpha, Compute.computeErrorsMC random alpha)

let alphasTD = [ 0.05; 0.10; 0.15 ]
let errorsTD = alphasTD |> List.map (fun alpha -> alpha, Compute.computeErrorsTD random alpha)

Chart.renderValues @"..\..\..\RandomWalk-Values-MC.png" valuesMC "MC"
Chart.renderValues @"..\..\..\RandomWalk-Values-TD.png" valuesTD "TD"
Chart.renderErrors @"..\..\..\RandomWalk-Errors-MC.png" errorsMC "MC"
Chart.renderErrors @"..\..\..\RandomWalk-Errors-TD.png" errorsTD "TD"
