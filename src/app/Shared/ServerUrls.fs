namespace TimeOff

/// API urls shared between client and server.
module ServerUrls =

    [<Literal>]
    let Login = "/api/users/login"

    [<Literal>]
    let Request = "/api/timeoff/request"

    [<Literal>]
    let UserBalance = "/api/timeoff/balance"

    [<Literal>]
    let UserHistory = "/api/timeoff/history"