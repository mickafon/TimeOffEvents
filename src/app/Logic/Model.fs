﻿namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    | RefuseCancellationRequest of UserId * Guid
    | BalanceRequest of UserId
    | HistoryRequest of UserId
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | RefuseCancellationRequest (userId, _) -> userId
        | BalanceRequest (userId) -> userId
        | HistoryRequest (userId) -> userId

type RequestState =
    | NotCreated
    | PendingValidation of TimeOffRequest
    | Validated of TimeOffRequest
    | Refused of TimeOffRequest
    | PendingCancellation of TimeOffRequest
    | CancellationRefused of TimeOffRequest
    | CanceledByEmployee of TimeOffRequest
    | CanceledByManager of TimeOffRequest with
    member this.Request =
        match this with
        | NotCreated -> invalidOp "Not created"
        | PendingValidation request
        | Validated request -> request
        | PendingCancellation request
        | CanceledByEmployee request -> request
        | CanceledByManager request -> request
        | CancellationRefused request -> request
        | Refused request -> request
    member this.IsActive =
        match this with
        | NotCreated -> false
        | PendingValidation _
        | Validated _ -> true
        | PendingCancellation _ -> true
        | CanceledByEmployee _ -> false
        | CanceledByManager _ -> false
        | CancellationRefused _ -> true
        | Refused _ -> false
   
type UserHistory = List<RequestState>

type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCanceledByEmployee of TimeOffRequest
    | RequestCanceledByManager of TimeOffRequest
    | RequestPendingCancellation of TimeOffRequest
    | RequestCancellationRefused of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestBalance of TimeOffBalance
    | RequestHistory of UserHistory
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCanceledByEmployee request -> request
        | RequestCanceledByManager request -> request
        | RequestPendingCancellation request -> request
        | RequestRefused request -> request
        | RequestCancellationRefused request -> request
        | RequestBalance _ -> invalidOp "balance" 
        | RequestHistory _ -> invalidOp "history"  
    member this.Balance =
        match this with
        | RequestBalance balance -> balance
        | _ -> invalidOp "request"
    member this.History =
        match this with
        | RequestHistory history -> history
        | _ -> invalidOp "request"

type UserRequestsState = Map<Guid, RequestState>


// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCanceledByEmployee request -> CanceledByEmployee request
        | RequestCanceledByManager request -> CanceledByManager request
        | RequestPendingCancellation request -> PendingCancellation request
        | RequestBalance _ -> invalidOp "balance"
        | RequestHistory _ -> invalidOp "history"

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let getAllUserRequests (userRequests: UserHistory) (event: RequestEvent) =
        let requestState = evolveRequest NotCreated event
        let newUserRequests = requestState::userRequests
        newUserRequests

    let overlapsWith request1 request2 =
        if request1.End.Date < request2.Start.Date || request2.End.Date < request1.Start.Date then
            false
        elif request1.End.Date = request2.Start.Date && request1.End.HalfDay = AM && request2.Start.HalfDay = PM then
            false
        else
            true //TODO: write a function that checks if 2 requests overlap

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        Seq.exists (fun element -> overlapsWith element request) otherRequests
        //TODO: write this function using overlapsWith

    let createRequest today activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ ->
            Error "Request cannot be refused"

    let refuseCancellation requestState =
        match requestState with
        | PendingCancellation request ->
            Ok [RequestCancellationRefused request]
        | _ ->
            Error "Request cannot be refused"

    let cancelRequestByEmployee requestState today =
        match requestState with
        | PendingValidation request | Validated request ->
            if request.Start.Date > today then
                Ok [RequestCanceledByEmployee requestState.Request]
            else
                Ok [RequestPendingCancellation requestState.Request]
        | _ ->
            Error "Request cannot be canceled by employee"

    let cancelRequestByManager requestState =
        match requestState with
        | NotCreated ->
            Error "Request cannot be canceled by manager"
        | _ ->
            match requestState.IsActive with
            | true ->
                Ok [RequestCanceledByManager requestState.Request]
            | _ ->
                Error "Request cannot be canceled by manager"

    let computeTimeOff userRequests =
        let time = userRequests.End.Date - userRequests.Start.Date
        let mutable result = time.TotalDays + 1.0
        if userRequests.End.HalfDay.Equals AM then do
            result <- result - 0.5
        if userRequests.Start.HalfDay.Equals PM then do
            result <- result - 0.5
        result

    let monthPresence (userEnteredDate: DateTime) (year: int) =        
        let nbMonth = if userEnteredDate.Year.Equals(year) then 12 - (userEnteredDate.Month-1) else 12                           
        nbMonth

    let getBalance (today: DateTime) activeUserRequests userId (userEnteredDate: DateTime)  =
        
        let earnedThisYear = (25.0 / 12.0) * float (today.Month - 1) 

        let enteredDate = userEnteredDate
        let mutable counter = enteredDate.Year
        let mutable report = 0.
        
        while counter < (today.Year) do
            let oldBalance = 
                Seq.sumBy computeTimeOff (activeUserRequests
                |> Seq.where (fun (request) -> request.Start.Date.Year.Equals(counter)))
            let oldEarned = (25.0 / 12.0) * float (monthPresence userEnteredDate counter)
            report <- report + (oldEarned - oldBalance)
            counter <- counter + 1

        let taken = 
            Seq.sumBy computeTimeOff (activeUserRequests
            |> Seq.where (fun (request) -> request.Start.Date.Year.Equals(today.Year) && request.Start.Date <= today))

        let planned = 
            Seq.sumBy computeTimeOff (activeUserRequests
            |> Seq.where (fun (request) -> request.Start.Date.Year.Equals(today.Year) && request.Start.Date > today))

        {
            UserId = userId
            EarnedThisYear = earnedThisYear
            Report = report
            Taken = taken
            Planned = planned
            Balance = earnedThisYear + report - (taken + planned)
        }
                
    let getHistory (today: DateTime) (userHistory: UserHistory) = 
        let history : UserHistory = 
            userHistory
            |> List.map (fun (state) -> state)
            |> List.where (fun state -> state.Request.Start.Date.Year = today.Year)
            |> List.map (fun state -> state)
        history


    let decide (today: DateTime) (userRequests: UserRequestsState) (userHistory: UserHistory) (user: User) (command: Command) =
        let relatedUserId = command.UserId                   
        match user with
        | Employee userInfo when userInfo.UserId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest today activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState

             | RefuseRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseRequest requestState

            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                match user with
                | Manager ->
                    cancelRequestByManager requestState
                | _ ->
                    cancelRequestByEmployee requestState today

            | RefuseCancellationRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseCancellation requestState

            | BalanceRequest (userId) ->
                match user with 
                | Employee userInfo ->
                    let activeUserRequests =
                        userRequests
                        |> Map.toSeq
                        |> Seq.map (fun (_, state) -> state)
                        |> Seq.where (fun state -> state.IsActive)
                        |> Seq.map (fun state -> state.Request)

                    let balance = getBalance today activeUserRequests userInfo.UserId userInfo.EnteredDate
                    Ok [RequestBalance balance]
                | _ ->
                    Error "Unauthorized"

            | HistoryRequest (userId) ->
                let history = getHistory today userHistory                    
                Ok [RequestHistory history]
          
  