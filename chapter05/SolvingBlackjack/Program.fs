module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let values, policy = Compute.generateResults random |> Seq.item 10000000

Chart.renderValues @"..\..\..\SolvingBlackjack-HardHand-Values.png" values.HardHand "Hard Hand"
Chart.renderValues @"..\..\..\SolvingBlackjack-SoftHand-Values.png" values.SoftHand "Soft Hand"
Chart.renderPolicy @"..\..\..\SolvingBlackjack-HardHand-Policy.png" policy.HardHand "Hard Hand"
Chart.renderPolicy @"..\..\..\SolvingBlackjack-SoftHand-Policy.png" policy.SoftHand "Soft Hand"
