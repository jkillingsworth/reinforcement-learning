module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let dealerStandsHard = 17
let dealerStandsSoft = 17
let playerStandsHard = 20
let playerStandsSoft = 20

let stateDealerFacing = [ 2 ]
let statePlayerFacing = [ 2; 1 ]
let stateValue = -0.27726

let steps = 1000
let tasks = 100

//-------------------------------------------------------------------------------------------------

type private Action =
    | Stand
    | Hit

type private Outcome =
    | Win
    | Lose
    | Draw

type private Hand =
    | Hard
    | Soft

type private State =
    { DealerFacingCount : int
      PlayerFacingCount : int
      Hand : Hand }

type private Table =
    { DealerHidden : int
      DealerFacing : int list
      PlayerFacing : int list }

type private Turn =
    | Deal
    | PlayerTurn of Table * (State * Action) list
    | DealerTurn of Table * (State * Action) list
    | Conclusion of Table * (State * Action) list

//-------------------------------------------------------------------------------------------------

let private takeCardFromDeck random =
    match Sample.discreteUniform 1 13 random with
    | card when card > 10 -> 10
    | card -> card

let private countHand cards =
    let sum = cards |> List.sum
    let ace = cards |> List.exists (fun x -> x = 1)
    if (ace && sum < 12) then (sum + 10, Soft) else (sum, Hard)

let private countHandForDealer table =
    table.DealerHidden :: table.DealerFacing
    |> countHand
    |> fst

let private countHandForPlayer table =
    table.PlayerFacing
    |> countHand
    |> fst

let private computeState table =
    let dealerFacingCount = table.DealerFacing |> Seq.exactlyOne
    let playerFacingCount, hand = countHand table.PlayerFacing
    { DealerFacingCount = dealerFacingCount
      PlayerFacingCount = playerFacingCount
      Hand = hand }

let private policyDealer table =
    let cards = table.DealerHidden :: table.DealerFacing
    match countHand cards with
    | count, Hard -> if (count >= dealerStandsHard) then Stand else Hit
    | count, Soft -> if (count >= dealerStandsSoft) then Stand else Hit

let private policyPlayer table random =
    let cards = table.PlayerFacing
    let count, _ = countHand cards
    if (count > 21) then
        Stand
    else
        match Sample.discreteUniform 0 1 random with
        | 0 -> Stand
        | _ -> Hit

//-------------------------------------------------------------------------------------------------

let rec private playerTakesTurn table random states =
    let action = policyPlayer table random
    let states = (computeState table, action) :: states
    match action with
    | Stand -> table, states
    | Hit
        ->
        let card = takeCardFromDeck random
        let table = { table with PlayerFacing = card :: table.PlayerFacing }
        playerTakesTurn table random states

let rec private dealerTakesTurn table random =
    match policyDealer table with
    | Stand -> table
    | Hit
        ->
        let card = takeCardFromDeck random
        let table = { table with DealerFacing = card :: table.DealerFacing }
        dealerTakesTurn table random

let rec private play random = function
    | Deal
        ->
        let table =
            { DealerHidden = takeCardFromDeck random
              DealerFacing = stateDealerFacing
              PlayerFacing = statePlayerFacing }

        let states = []
        let playerCount = countHandForPlayer table
        let dealerCount = countHandForDealer table

        if (playerCount = 21) then
            states, if (dealerCount = 21) then Draw else Win
        else
            play random <| PlayerTurn (table, states)

    | PlayerTurn (table, states)
        ->
        let table, states = playerTakesTurn table random states
        let count = countHandForPlayer table
        if (count > 21) then
            states, Lose
        else
            play random <| DealerTurn (table, states)

    | DealerTurn (table, states)
        ->
        let table = dealerTakesTurn table random
        let count = countHandForDealer table
        if (count > 21) then
            states, Win
        else
            play random <| Conclusion (table, states)

    | Conclusion (table, states)
        ->
        let playerCount = countHandForPlayer table
        let dealerCount = countHandForDealer table
        match countHandForPlayer table, countHandForDealer table with
        | playerCount, dealerCount when playerCount > dealerCount -> states, Win
        | playerCount, dealerCount when playerCount < dealerCount -> states, Lose
        | _ -> states, Draw

//-------------------------------------------------------------------------------------------------

let private policyPlayerPi = function
    | state, Stand when state.PlayerFacingCount < 12 -> 0.0
    | state, Hit   when state.PlayerFacingCount < 12 -> 1.0
    | state, Stand when state.PlayerFacingCount > 21 -> 1.0
    | state, Hit   when state.PlayerFacingCount > 21 -> 0.0
    | { PlayerFacingCount = pc; Hand = Hard }, Stand -> if pc >= playerStandsHard then 1.0 else 0.0
    | { PlayerFacingCount = pc; Hand = Hard }, Hit   -> if pc >= playerStandsHard then 0.0 else 1.0
    | { PlayerFacingCount = pc; Hand = Soft }, Stand -> if pc >= playerStandsSoft then 1.0 else 0.0
    | { PlayerFacingCount = pc; Hand = Soft }, Hit   -> if pc >= playerStandsSoft then 0.0 else 1.0

let private policyPlayerMu = function
    | state, Stand when state.PlayerFacingCount < 12 -> 0.0
    | state, Hit   when state.PlayerFacingCount < 12 -> 1.0
    | state, Stand when state.PlayerFacingCount > 21 -> 1.0
    | state, Hit   when state.PlayerFacingCount > 21 -> 0.0
    | state, Stand -> 0.5
    | state, Hit   -> 0.5

let private mapRatioReward (states, outcome) =

    let reward =
        match outcome with
        | Win  -> +1.0
        | Lose -> -1.0
        | Draw ->  0.0

    let folder acc (s, a) =
        let pi = policyPlayerPi (s, a)
        let mu = policyPlayerMu (s, a)
        acc * (pi / mu)

    let rho = states |> Seq.fold folder 1.0

    rho, reward

let private computeSums (numerator, denominatorOrdinary, denominatorWeighted) (rho, reward) =

    let numerator = numerator + (rho * reward)
    let denominatorOrdinary = denominatorOrdinary + 1.0
    let denominatorWeighted = denominatorWeighted + rho

    numerator, denominatorOrdinary, denominatorWeighted

let private mapStateValues (numerator, denominatorOrdinary, denominatorWeighted) =

    let compute = function
        | 0.0 -> 0.0
        | denominator -> numerator / denominator

    let ordinary = compute denominatorOrdinary
    let weighted = compute denominatorWeighted

    ordinary, weighted

//-------------------------------------------------------------------------------------------------

let private executeOneTask random _ =
    seq { while true do yield play random <| Deal }
    |> Seq.map mapRatioReward
    |> Seq.scan computeSums (0.0, 0.0, 0.0)
    |> Seq.map mapStateValues
    |> Seq.skip 1
    |> Seq.take steps
    |> Array.ofSeq
    |> Array.unzip

let private computeMeanSquareError (tasks : double[][]) i =
    tasks
    |> Seq.map (fun values -> (stateValue - values.[i]) ** 2.0)
    |> Seq.average

let computeResults random =

    let ordinarys, weighteds =
        executeOneTask random
        |> Array.init tasks
        |> Array.unzip

    let ordinary = Array.init steps (computeMeanSquareError ordinarys)
    let weighted = Array.init steps (computeMeanSquareError weighteds)

    ordinary, weighted
