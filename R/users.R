getUser <- function(user, session=getCurlHandle(), ...) {
    if (inherits(user, "user"))
        user <- screenName(user)

    ## For god knows what reason, my normal methods don't work here,
    ## build URL by hand
    url <- paste("http://api.twitter.com/1/users/show/",
                 URLencode(user), '.json', sep="")
    out <- getURL(url, curl=session, ...)
    ## Need some error checking
    buildUser(twFromJSON(out))
}

