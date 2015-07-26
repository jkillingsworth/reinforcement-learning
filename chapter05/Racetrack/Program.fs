module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()

let policy = Compute.computePolicy random |> Seq.item 10000
let traces = Compute.executePolicy random policy (6, 1)

Chart.renderChart @"..\..\..\Racetrack.png" Compute.track traces

printfn " position | velocity | action   | reward"
for (state, action, reward) in traces do
    let px, py, vx, vy = state
    let ax, ay = action
    printfn " (%2i, %2i) | (%2i, %2i) | (%2i, %2i) | %2i" px py vx vy ax ay reward
