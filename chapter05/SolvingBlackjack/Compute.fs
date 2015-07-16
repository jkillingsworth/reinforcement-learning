module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

type Exploration = ExploringStarts | EpsilonGreedy of double

let exploration = ExploringStarts
let dealerStandsHard = 17
let dealerStandsSoft = 17

//-------------------------------------------------------------------------------------------------

type Values<'T> =
    { HardHand : 'T[,]
      SoftHand : 'T[,] }

type Action =
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
    | Deal of Values<Action>
    | PlayerTurn of Table * (State * Action) list * Values<Action>
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

let private policyPlayer table random states policy =
    let chooseRandomAction =
        match exploration with
        | ExploringStarts -> List.length states = 0
        | EpsilonGreedy epsilon -> Sample.continuousUniform 0.0 1.0 random < epsilon
    if chooseRandomAction then
        match Sample.discreteUniform 0 1 random with
        | 0 -> Stand
        | _ -> Hit
    else
        match computeState table with
        | { PlayerFacingCount = pc } when pc >= 21 -> Stand
        | { PlayerFacingCount = pc } when pc < 12 -> Hit
        | { DealerFacingCount = dc; PlayerFacingCount = pc; Hand = Hard } -> policy.HardHand.[dc, pc]
        | { DealerFacingCount = dc; PlayerFacingCount = pc; Hand = Soft } -> policy.SoftHand.[dc, pc]

//-------------------------------------------------------------------------------------------------

let rec private playerTakesTurn table random states policy =
    let action = policyPlayer table random states policy
    let states = (computeState table, action) :: states
    match action with
    | Stand -> table, states
    | Hit
        ->
        let card = takeCardFromDeck random
        let table = { table with PlayerFacing = card :: table.PlayerFacing }
        playerTakesTurn table random states policy

let rec private dealerTakesTurn table random =
    match policyDealer table with
    | Stand -> table
    | Hit
        ->
        let card = takeCardFromDeck random
        let table = { table with DealerFacing = card :: table.DealerFacing }
        dealerTakesTurn table random

let rec private play random = function
    | Deal policy
        ->
        if exploration = ExploringStarts then
            let dealerFacingCount = Sample.discreteUniform 01 10 random
            let playerFacingCount = Sample.discreteUniform 12 21 random
            let playerHasSoftHand = Sample.discreteUniform 0 1 random = 1

            let dealerCards = [ dealerFacingCount ]
            let playerCards =
                match if playerHasSoftHand then Soft else Hard with
                | Hard when playerFacingCount = 21 -> [ 10; 05; 06 ]
                | Hard -> [ playerFacingCount - 10; 10 ]
                | Soft -> [ playerFacingCount - 11; 01 ]

            let table =
                { DealerHidden = takeCardFromDeck random
                  DealerFacing = dealerCards
                  PlayerFacing = playerCards }

            let states = []

            play random <| PlayerTurn (table, states, policy)
        else
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
                play random <| PlayerTurn (table, states, policy)

    | PlayerTurn (table, states, policy)
        ->
        let table, states = playerTakesTurn table random states policy
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

    let update dc pc action (values : Map<Action, double>[,]) (counts : Map<Action, int>[,]) =
        let v = values.[dc, pc].[action]
        let n = counts.[dc, pc].[action]
        let v' = v + ((reward - v) / double (n + 1))
        let n' = n + 1
        values.[dc, pc] <- values.[dc, pc] |> Map.add action v'
        counts.[dc, pc] <- counts.[dc, pc] |> Map.add action n'

    let states = states |> List.filter (fun (s, a) -> s.PlayerFacingCount >= 12)
    let states = states |> List.filter (fun (s, a) -> s.PlayerFacingCount <= 21)
    for state, action in states do
        let dc = state.DealerFacingCount
        let pc = state.PlayerFacingCount
        match state.Hand with
        | Hard -> update dc pc action values.HardHand counts.HardHand
        | Soft -> update dc pc action values.SoftHand counts.SoftHand

    values, counts

let private improvePolicy states outcome values policy =

    let policy =
        { HardHand = Array2D.copy policy.HardHand
          SoftHand = Array2D.copy policy.SoftHand }

    let update dc pc (policy : Action[,]) (values : Map<Action, double>[,]) =
        let actions = [ Stand; Hit ]
        policy.[dc, pc] <- actions |> Seq.maxBy (fun a -> values.[dc, pc].[a])

    let states = states |> List.filter (fun (s, a) -> s.PlayerFacingCount >= 12)
    let states = states |> List.filter (fun (s, a) -> s.PlayerFacingCount <= 21)
    for state, _ in states do
        let dc = state.DealerFacingCount
        let pc = state.PlayerFacingCount
        match state.Hand with
        | Hard -> update dc pc policy.HardHand values.HardHand
        | Soft -> update dc pc policy.SoftHand values.SoftHand

    policy

//-------------------------------------------------------------------------------------------------

let private executeOneEpisode random ((values, policy), counts) =

    let states, outcome = play random <| Deal policy
    let values, counts = improveValues states outcome values counts
    let policy = improvePolicy states outcome values policy

    (values, policy), ((values, policy), counts)

let generateResults random =

    let minDealerCount = 01
    let maxDealerCount = 10
    let minPlayerCount = 12
    let maxPlayerCount = 21

    let base1 = minDealerCount
    let base2 = minPlayerCount
    let length1 = maxDealerCount - minDealerCount + 1
    let length2 = maxPlayerCount - minPlayerCount + 1

    let createArray initialize = Array2D.initBased base1 base2 length1 length2 initialize

    let policy =
        { HardHand = createArray (fun dc pc -> if pc >= 20 then Stand else Hit)
          SoftHand = createArray (fun dc pc -> if pc >= 20 then Stand else Hit) }

    let values =
        { HardHand = createArray (fun dc pc -> Map [ (Stand, 0.0); (Hit, 0.0) ])
          SoftHand = createArray (fun dc pc -> Map [ (Stand, 0.0); (Hit, 0.0) ]) }

    let counts =
        { HardHand = createArray (fun dc pc -> Map [ (Stand, 0); (Hit, 0) ])
          SoftHand = createArray (fun dc pc -> Map [ (Stand, 0); (Hit, 0) ]) }

    let computeStateValues (values, policy) =
        let mapping (m : Map<Action, double>) = max m.[Stand] m.[Hit]
        let convert values =
            { HardHand = values.HardHand |> Array2D.map mapping
              SoftHand = values.SoftHand |> Array2D.map mapping }
        convert values, policy

    let generate f x =
        seq { yield fst x; yield! x |> Seq.unfold (f >> Some) }

    generate (executeOneEpisode random) ((values, policy), counts)
    |> Seq.map computeStateValues
