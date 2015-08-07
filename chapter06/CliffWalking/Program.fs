module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()

let output = Compute.generateResults random >> Seq.take 10000
let toTotals = Seq.map snd >> Seq.toArray
let toValues = Seq.map fst >> Seq.last

let totals =
    [ "Q-learning", (output >> toTotals) Compute.QLearning
      "Sarsa",      (output >> toTotals) Compute.Sarsa ]

Chart.renderStats @"..\..\..\CliffWalking-Stats.png" totals

let renderRoute learningMethod =
    let values = (output >> toValues) learningMethod
    let states = Compute.computeTheRoute random learningMethod values
    let path = sprintf @"..\..\..\CliffWalking-Route-%A.png" learningMethod
    Chart.renderRoute path states

renderRoute Compute.QLearning
renderRoute Compute.Sarsa
