module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let steps = 100000
let tasks = 6

//-------------------------------------------------------------------------------------------------

type private Action =
    | Back
    | End

type private State =
    | Play
    | Term

//-------------------------------------------------------------------------------------------------

let rec private play random acc =

    let action =
        match Sample.discreteUniform 0 1 random with
        | 0 -> Back
        | _ -> End

    match action with
    | Back
        ->
        match Sample.discreteUniform 0 9 random with
        | 0 -> (Term, action, 1.0) :: acc
        | _ -> (Play, action, 0.0) :: acc |> play random
    | End
        -> (Term, action, 0.0) :: acc

//-------------------------------------------------------------------------------------------------

let private policyPi = function
    | Back -> 1.0
    | End  -> 0.0

let private policyMu = function
    | Back -> 0.5
    | End  -> 0.5

let private mapRatioReward states =
    
    let folder (rho, reward) (s, a, r) =
        let pi = policyPi a
        let mu = policyMu a
        let rho = rho * (pi / mu)
        let reward = reward + r
        rho, reward

    states |> Seq.fold folder (1.0, 0.0)

let private computeSums (numerator, denominator) (rho, reward) =
    let numerator = numerator + (rho * reward)
    let denominator = denominator + 1.0
    numerator, denominator

let private mapStateValues (numerator, denominator) =
    numerator / denominator

//-------------------------------------------------------------------------------------------------

let private executeOneTask random _ =
    seq { while true do yield play random [] }
    |> Seq.map mapRatioReward
    |> Seq.scan computeSums (0.0, 0.0)
    |> Seq.map mapStateValues
    |> Seq.skip 1
    |> Seq.take steps
    |> Seq.toArray

let computeResults random =

    executeOneTask random |> Array.init tasks
