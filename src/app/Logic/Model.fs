namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    | CancellationRequest of UserId * Guid
    | RefuseCancellationRequest of UserId * Guid
    | CancelByEmployeeRequest of UserId * Guid
    | CancelByManagerRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | CancellationRequest (userId, _) -> userId
        | RefuseCancellationRequest (userId, _) -> userId
        | CancelByEmployeeRequest (userId, _) -> userId
        | CancelByManagerRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestForCancellationCreated of TimeOffRequest
    | RequestForCancellationRefused of TimeOffRequest
    | RequestCanceledByEmployee of TimeOffRequest
    | RequestCanceledByManager of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestRefused request -> request
        | RequestForCancellationCreated request -> request
        | RequestForCancellationRefused request -> request
        | RequestCanceledByEmployee request -> request
        | RequestCanceledByManager request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

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
            | Refused request -> request
            | PendingCancellation request -> request
            | CancellationRefused request -> request
            | CanceledByEmployee request -> request
            | CanceledByManager request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true
            | Refused _ -> false
            | PendingCancellation _ -> true
            | CancellationRefused _ -> true
            | CanceledByEmployee _ -> false
            | CanceledByManager _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestRefused request -> Refused request
        | RequestForCancellationCreated request -> PendingCancellation request
        | RequestForCancellationRefused request -> CancellationRefused request
        | RequestCanceledByEmployee request -> CanceledByEmployee request
        | RequestCanceledByManager request -> CanceledByManager request


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

    let cancellationRequest today requestState =
        match requestState with
            | Validated request ->
                if request.Start.Date <= today then
                    Ok [RequestForCancellationCreated request]
                else 
                    Error "Request for cancellation cannot be created"
            | _ ->
                Error "Request for cancellation cannot be created"

    let refuseCancellationRequest requestState =
        match requestState with
            | PendingCancellation request ->
                Ok [RequestForCancellationRefused request]
            | _ ->
                Error "Request for cancellation cannot be refused"

    let cancelByEmployeeRequest requestState =
        match requestState with
            | PendingValidation request ->
                Ok [RequestCanceledByEmployee request]
            | _ ->
                Error "Request cannot be canceled (by employee)"

    let cancelByManagerRequest requestState = 
        match requestState with
            | PendingValidation request ->
                Ok [RequestCanceledByManager request]
            | Validated request ->
                Ok [RequestCanceledByManager request]
            | PendingCancellation request ->
                Ok [RequestCanceledByManager request]
            | _ ->
                Error "Request cannot be canceled (by manager)"

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
            | CancellationRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                cancellationRequest today requestState
            | RefuseCancellationRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                refuseCancellationRequest requestState
            | CancelByEmployeeRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                cancelByEmployeeRequest requestState
            | CancelByManagerRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelByManagerRequest requestState
