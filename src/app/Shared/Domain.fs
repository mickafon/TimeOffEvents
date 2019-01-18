namespace TimeOff

open System

// First, we define our domain
type UserId = string

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

<<<<<<< HEAD
type TimeOffBalance = {
    UserId: UserId
    EarnedThisYear: float
    Report: float
    Taken: float
    Planned: float
    Balance: float
}
=======
[<CLIMutable>]
type UserVacationBalance = {
  UserName : UserId
  BalanceYear: int
  CarriedOver: float
  PortionAccruedToDate: float
  TakenToDate: float
  CurrentBalance: float
}
>>>>>>> upstream/add-gui
