module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let AndDateIs (year, month, day) (events: RequestEvent list, user: User) = events, user, DateTime(year, month, day)
let When (command: Command) (events: RequestEvent list, user: User, today: DateTime) = events, user, today, command
let WhenHistory (command: Command) (events: RequestEvent list, user: User, today: DateTime) = events, user, today, command
let Then expected message (events: RequestEvent list, user: User, today: DateTime, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)
    
    let historyGlobal (userHistory: UserHistory) (event: RequestEvent) =
        let requestState = Logic.evolveRequest NotCreated event
        let newUserRequests = requestState::userHistory
        newUserRequests

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let globalHistory = List.fold historyGlobal List.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide today userRequestsState globalHistory user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests =
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Request don't overlap with list" {
        let request = {
           UserId = 1
           RequestId = Guid.NewGuid()
           Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
           End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
        }

        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        }
        let requests = [request1;request2]

        Expect.isFalse(Logic.overlapsWithAnyRequest requests request) "The request don't overlap with a request in the list"
    }

    test "Request overlap with list" {
        let request = {
           UserId = 1
           RequestId = Guid.NewGuid()
           Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
           End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
        }

        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        }

        let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
        }
        let requests = [request1;request2;request3]

        Expect.isTrue(Logic.overlapsWithAnyRequest requests request) "The request overlap with a request in the list"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }
        
      let userInfo = {
        UserId = 1 
        EnteredDate = DateTime(2016, 06, 15)
      }

      Given [ ]
      |> ConnectedAs (Employee userInfo)
      |> AndDateIs (2018, 12, 3)
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "A request in the past cannot be created" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 11, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 11, 28); HalfDay = PM } }

      let userInfo = {
        UserId = 1 
        EnteredDate = DateTime(2016, 06, 15)
      }

      Given [ ]
      |> ConnectedAs (Employee userInfo)
      |> AndDateIs (2018, 12, 3)
      |> When (RequestTimeOff request)
      |> Then (Error "The request starts in the past") "The request should not have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs (2018, 12, 3)
      |> When (ValidateRequest (1, request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let refuseTests =
  testList "Refuse tests" [
    test "A request is refused" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs (2018, 12, 3)
      |> When (ValidateRequest (1, request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been refused"
    }

    test "A pending cancellation request is refused" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestPendingCancellation request ]
      |> ConnectedAs Manager
      |> AndDateIs (2018, 12, 3)
      |> When (RefuseCancellationRequest (1, request.RequestId))
      |> Then (Ok [RequestCancellationRefused request]) "The pending cancellation request should have been refused"
    }
  ]

[<Tests>]
let cancellationTests =
  testList "Cancel tests" [
    test "A request is canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs (2019, 01, 7)
      |> When (CancelRequest(1, request.RequestId))
      |> Then (Ok [RequestCanceledByManager request]) "The request should have been canceled"
    }

    test "A request is canceled by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } }
      
      let userInfo = {
        UserId = 1 
        EnteredDate = DateTime(2016, 06, 15)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee userInfo)
      |> AndDateIs (2019, 01, 7)
      |> When (CancelRequest(1, request.RequestId))
      |> Then (Ok [RequestCanceledByEmployee request]) "The request should have been canceled"
    }

    test "Cancel request with a passed date" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      let userInfo = {
        UserId = 1 
        EnteredDate = DateTime(2016, 06, 15)
      }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee userInfo)
      |> AndDateIs (2019, 01, 7)
      |> When (CancelRequest(1, request.RequestId))
      |> Then (Ok [RequestPendingCancellation request]) "The request should have been pending canceled"
    }
  ]

[<Tests>]
let balanceTests =
  testList "Balance tests" [
    test "One future validated request and one past taken" {
      let userInfo : UserInfo = {
        UserId = 1
        EnteredDate = DateTime(2018, 01, 01)
      }

      let pastRequest = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } 
      }

      let futurRequest = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }

      let balance: TimeOffBalance = {
        UserId = 1
        EarnedThisYear = 0.
        Report = 24.
        Taken = 0.
        Planned = 1.
        Balance = 23.
      }

      Given [ 
        RequestValidated pastRequest 
        RequestValidated futurRequest 
      ]
      |> ConnectedAs (Employee userInfo)
      |> AndDateIs (2019, 01, 7)
      |> When (BalanceRequest (1))
      |> Then (Ok [RequestBalance balance]) "The request balance should be equals"
    }
  ]


[<Tests>]
let historyTests =
  testList "History tests" [
    test "get history" {
      let userInfo : UserInfo = {
        UserId = 1
        EnteredDate = DateTime(2018, 01, 01)
      }

      let pastRequest1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2017, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 28); HalfDay = PM } 
      }

      let futurRequest = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }
      
      let history : UserHistory = [
        CanceledByManager futurRequest      
        PendingCancellation futurRequest        
        Validated futurRequest
        PendingValidation futurRequest        
      ]

      Given [ 
        RequestCreated pastRequest1
        RequestValidated pastRequest1        
        RequestCreated futurRequest
        RequestValidated futurRequest 
        RequestPendingCancellation futurRequest 
        RequestCanceledByManager futurRequest 
      ]
      |> ConnectedAs (Employee userInfo)
      |> AndDateIs (2019, 01, 7)
      |> WhenHistory (HistoryRequest (1))
      |> Then (Ok [RequestHistory history]) "The request history should contains 4 stuff"
    }
  ]