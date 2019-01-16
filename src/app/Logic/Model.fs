namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | SummaryRequests of UserId * Guid
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    | RefuseCancellationRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | SummaryRequests (userId, _) -> userId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | RefuseCancellationRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCanceledByEmployee of TimeOffRequest
    | RequestCanceledByManager of TimeOffRequest
    | RequestPendingCancellation of TimeOffRequest
    | RequestCancellationRefused of TimeOffRequest
    | RequestRefused of TimeOffRequest
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

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingCancellation of TimeOffRequest
        | CanceledByEmployee of TimeOffRequest
        | CanceledByManager of TimeOffRequest
        | CancellationRefused of TimeOffRequest
        | Refused of TimeOffRequest
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

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCanceledByEmployee request -> CanceledByEmployee request
        | RequestCanceledByManager request -> CanceledByManager request
        | RequestPendingCancellation request -> PendingCancellation request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        if request1.End.Date < request2.Start.Date || request2.End.Date < request1.Start.Date then
            false
        elif request1.End.Date = request2.Start.Date && request1.End.HalfDay = AM && request2.Start.HalfDay = PM then
            false
        else
            true //TODO: write a function that checks if 2 requests overlap

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        if Seq.isEmpty otherRequests then
            false
        else
            let rec check requests =
                if overlapsWith (requests |> Seq.head) request then
                    true
                elif Seq.isEmpty (requests |> Seq.tail) then
                    false
                else
                    check (requests |> Seq.tail)
            if check otherRequests then
                true
            else
                false //TODO: write this function using overlapsWith

    let createRequest today activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    //let historyRequests userId =
         // match userId with
    //    UserId

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
            | PendingValidation request ->
                if request.Start.Date > today then
                    Ok [RequestCanceledByEmployee requestState.Request]
                else
                    Ok [RequestPendingCancellation requestState.Request]
            | Validated request ->
                if request.Start.Date > today then
                    Ok [RequestCanceledByEmployee requestState.Request]
                else
                    Ok [RequestPendingCancellation requestState.Request]
            | _ ->
                Error "Request cannot be canceled"

    let cancelRequestByManager requestState =
        match requestState with
            | NotCreated ->
                Error "Request cannot be canceled"
            | _ ->
                Ok [RequestCanceledByManager requestState.Request]

    let decide (today: DateTime) (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
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

            //| HistoryRequests (_, requestId) ->
                //summaryRequests requestId
