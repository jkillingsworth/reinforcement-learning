module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let maxCarsToHold1 = 20
let lambdaRequest1 = 3.0
let lambdaDropoff1 = 3.0

let maxCarsToHold2 = 20
let lambdaRequest2 = 4.0
let lambdaDropoff2 = 2.0

let maxCarsCanMove = 5

let rewardRequest = 10.0
let rewardMoveCar = -2.0

let gamma = 0.9
let theta = 0.0000001

//-------------------------------------------------------------------------------------------------

let private actions =
    seq { yield 0
          for a = 1 to maxCarsCanMove do yield +a
          for a = 1 to maxCarsCanMove do yield -a }

let rec private probabilities lambda n =
    seq { let p = Poisson.PMF(lambda, n)
          if (p > theta) then
              yield (p, n)
              yield! probabilities lambda (n + 1) }

let private generateLookupValues maxCarsToHold lambdaRequest lambdaDropoff =
    let maxCarsInTheMorning = maxCarsToHold + maxCarsCanMove
    seq { for pRequest, nRequest in probabilities lambdaRequest 0 do
          for pDropoff, nDropoff in probabilities lambdaDropoff 0 do
          for n = 0 to maxCarsInTheMorning do
              let nRequest = (min nRequest n)
              let n' = n - nRequest + nDropoff
              let n' = min n' maxCarsToHold
              let p = pRequest * pDropoff
              let r = p * rewardRequest * double (min nRequest n)
              yield n, n', p, r }

let private createLookups maxCarsToHold lambdaRequest lambdaDropoff =
    let maxCarsInTheMorning = maxCarsToHold + maxCarsCanMove
    let lookupP = Array2D.create (maxCarsInTheMorning + 1) (maxCarsToHold + 1) 0.0
    let lookupR = Array.create (maxCarsInTheMorning + 1) 0.0
    for (n, n', p, r) in generateLookupValues maxCarsToHold lambdaRequest lambdaDropoff do
        lookupP.[n, n'] <- lookupP.[n, n'] + p
        lookupR.[n] <- lookupR.[n] + r
    lookupP, lookupR

let private lookupP1, lookupR1 = createLookups maxCarsToHold1 lambdaRequest1 lambdaDropoff1
let private lookupP2, lookupR2 = createLookups maxCarsToHold2 lambdaRequest2 lambdaDropoff2

//-------------------------------------------------------------------------------------------------

let private computeActionValue (values : double[,]) (n1, n2) action =
    
    let action = min action +n1
    let action = max action -n2
    let n1 = n1 - action
    let n2 = n2 + action
    let rAction = rewardMoveCar * double (abs action)

    seq { for n1' = 0 to maxCarsToHold1 do
          for n2' = 0 to maxCarsToHold2 do
          let p1 = lookupP1.[n1, n1']
          let p2 = lookupP2.[n2, n2']
          let r1 = lookupR1.[n1]
          let r2 = lookupR2.[n2]
          let r = r1 + r2
          let p = p1 * p2
          yield p * (r + gamma * values.[n1', n2']) }
    |> Seq.sum
    |> (+) rAction

let rec private improveValues (policy : int[,]) (values : double[,]) =

    let mapping n1 n2 _ = computeActionValue values (n1, n2) policy.[n1, n2]
    let values' = values |> Array2D.mapi mapping

    let deltas =
        seq { for n1 = 0 to maxCarsToHold1 do
              for n2 = 0 to maxCarsToHold2 do
              yield abs (values.[n1, n2] - values'.[n1, n2]) }

    let converged = Seq.exists (fun delta -> delta > theta) >> not
    if (converged deltas) then values' else improveValues policy values'

let private improvePolicy (policy : int[,]) (values : double[,]) =
    
    let mapping n1 n2 _ =
        actions
        |> Seq.where (fun a -> a <= +n1)
        |> Seq.where (fun a -> a >= -n2)
        |> Seq.maxBy (computeActionValue values (n1, n2))
    
    policy |> Array2D.mapi mapping

//-------------------------------------------------------------------------------------------------

let private executeOneStep (policy, values) =

    let values' = improveValues policy values
    let policy' = improvePolicy policy values'

    (policy', values')

let generateResults () =

    let rows = maxCarsToHold1 + 1
    let cols = maxCarsToHold2 + 1

    let values = Array2D.create rows cols 0.0
    let policy = Array2D.create rows cols 0

    let pairResult x = Some (x,x)
    let generate f x =
        seq { yield x; yield! x |> Seq.unfold (f >> pairResult) }

    generate executeOneStep (policy, values)
