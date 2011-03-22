getUser <- function(user, ...) {
    if (inherits(user, "user"))
        user <- screenName(user)
    url <- paste("http://api.twitter.com/1/users/show/",
                 user, '.json', sep="")
    buildUser(doAPICall(url, ...))
}

