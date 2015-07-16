module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let dealerStandsHard = 17
let dealerStandsSoft = 17
let playerStandsHard = 20
let playerStandsSoft = 20

//-------------------------------------------------------------------------------------------------

type Values<'T> =
    { HardHand : 'T[,]
      SoftHand : 'T[,] }

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
    | PlayerTurn of Table * State list
    | DealerTurn of Table * State list
    | Conclusion of Table * State list

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

let private policyPlayer table =
    let cards = table.PlayerFacing
    match countHand cards with
    | count, Hard -> if (count >= playerStandsHard) then Stand else Hit
    | count, Soft -> if (count >= playerStandsSoft) then Stand else Hit

//-------------------------------------------------------------------------------------------------

let rec private playerTakesTurn table random states =
    let states = computeState table :: states
    match policyPlayer table with
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
        let playerCard1 = takeCardFromDeck random
        let dealerCard1 = takeCardFromDeck random
        let playerCard2 = takeCardFromDeck random
        let dealerCard2 = takeCardFromDeck random

        let table =
            { DealerHidden = dealerCard2
              DealerFacing = [ dealerCard1 ]
              PlayerFacing = [ playerCard1; playerCard2 ] }

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

let private improveValues states outcome values counts =

    let reward =
        match outcome with
        | Win  -> +1.0
        | Lose -> -1.0
        | Draw ->  0.0

    let values =
        { HardHand = Array2D.copy values.HardHand
          SoftHand = Array2D.copy values.SoftHand }

    let counts =
        { HardHand = Array2D.copy counts.HardHand
          SoftHand = Array2D.copy counts.SoftHand }

    let update dc pc (values : double[,]) (counts : int[,]) =
        let v = values.[dc, pc]
        let n = counts.[dc, pc]
        let v' = v + ((reward - v) / double (n + 1))
        let n' = n + 1
        values.[dc, pc] <- v'
        counts.[dc, pc] <- n'

    let states = states |> List.filter (fun s -> s.PlayerFacingCount >= 12)
    let states = states |> List.filter (fun s -> s.PlayerFacingCount <= 21)
    for state in states do
        let dc = state.DealerFacingCount
        let pc = state.PlayerFacingCount
        match state.Hand with
        | Hard -> update dc pc values.HardHand counts.HardHand
        | Soft -> update dc pc values.SoftHand counts.SoftHand

    values, counts

//-------------------------------------------------------------------------------------------------

let private executeOneEpisode random (values, counts) =

    let states, outcome = play random <| Deal
    let values, counts = improveValues states outcome values counts

    values, (values, counts)

let generateResults random =
    
    let minDealerCount = 01
    let maxDealerCount = 10
    let minPlayerCount = 12
    let maxPlayerCount = 21

    let base1 = minDealerCount
    let base2 = minPlayerCount
    let length1 = maxDealerCount - minDealerCount + 1
    let length2 = maxPlayerCount - minPlayerCount + 1

    let createArray initial = Array2D.createBased base1 base2 length1 length2 initial

    let values = 
        { HardHand = createArray 0.0
          SoftHand = createArray 0.0 }

    let counts =
        { HardHand = createArray 0
          SoftHand = createArray 0 }

    let generate f x =
        seq { yield fst x; yield! x |> Seq.unfold (f >> Some) }

    generate (executeOneEpisode random) (values, counts)
