namespace TimeOff

open System

// First, we define our domain
type UserId = int

type UserInfo = {
    UserId: UserId
    EnteredDate: DateTime
}

type User =
    | Employee of UserInfo
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

type TimeOffBalance = {
    UserId: UserId
    EarnedThisYear: float
    Report: float
    Taken: float
    Planned: float
    Balance: float
}

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
