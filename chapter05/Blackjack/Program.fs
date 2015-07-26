module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let values = Compute.generateResults random |> Seq.item 500000

Chart.renderChart @"..\..\..\Blackjack-HardHand.png" values.HardHand "Hard Hand"
Chart.renderChart @"..\..\..\Blackjack-SoftHand.png" values.SoftHand "Soft Hand"
