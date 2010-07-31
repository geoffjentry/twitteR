authMsg <- paste('Twitter has changed their authentication scheme',
                 'as of 6/2010 and will now require OAuth.',
                 'Until there is an R-OAuth solution, these functions',
                 'will not work.')

friendsTimeline <- function(session=getCurlHandle(), ...) {
    ## AUTH:  Requires authentication, won't work until OAuth
    .Defunct(msg=authMsg)
    out <- getURL("http://twitter.com/statuses/friends_timeline.json",
                  curl=session, ...)
    jsonList <- twFromJSON(out)
    sapply(jsonList, buildStatus)
}

mentions <- function(session=getCurlHandle(), ...) {
        .Defunct(msg=authMsg)

    ## AUTH: Requires authentication
    out <- getURL("http://twitter.com/statuses/mentions.json",
                  curl=session, ...)
    jsonList <- twFromJSON(out)
    sapply(jsonList, buildStatus)
}

updateStatus <- function(text, session=getCurlHandle(), ...) {
    ## AUTH
        .Defunct(msg=authMsg)

    out <- postForm("http://twitter.com/statuses/update.json",
                    ..., status=text, curl=session)
    ## out is in byte code, need to pass that through a raw conversion
    ## to get the string.  Not sure why sometimes it's byte arrays and
    ## sometimes it is strings
    buildStatus(twFromJSON(rawToChar(out)))
}

tweet <- function(text, session=getCurlHandle(), ...) {
    ## AUTH
    .Defunct(msg=authMsg)
    ## Just a wrapper around updateStatus
    updateStatus(text, session, ...)
}

deleteStatus <- function(status, session=getCurlHandle(), ...) {
    ## AUTH:  Requires authentication, needs OAuth
        .Defunct(msg=authMsg)

    url <- paste("http://twitter.com/statuses/destroy/",
                 status@id, ".json", sep="")
    ## I don't know how to simply POST or send a DELETE via RCurl w/o
    ## postForm, but this isn't a form so it throws a warning.
    ## Suppress these warnings
    out <- suppressWarnings(postForm(url, curl=session), ...)
    TRUE
}

dmGet <- function(session=getCurlHandle(), num=20, ...) {
    ## No parameters for now
        .Defunct(msg=authMsg)

    out <- getURL(paste("http://twitter.com/direct_messages.json?count=",
                        num, sep=""),
                  curl=session, ...)
    jsonList <- twFromJSON(out)
    sapply(jsonList, buildDM)
}

dmSent <- function(session=getCurlHandle(), ...) {
    ## No parameters for now
        .Defunct(msg=authMsg)

    out <- getURL("http://twitter.com/direct_messages/sent.json",
                  curl=session, ...)
    jsonList <- twFromJSON(out)
    sapply(jsonList, buildDM)
}

dmDestroy <- function(dm, session=getCurlHandle(), ...) {
        .Defunct(msg=authMsg)

    url <- paste("http://twitter.com/direct_messages/destroy/",
                 dm@id, ".json", sep="")
    ## I don't know how to simply POST or send a DELETE via RCurl w/o
    ## postForm, but this isn't a form so it throws a warning.
    ## Suppress these warnings
    out <- suppressWarnings(postForm(url, ..., curl=session))
    TRUE
}

dmSend <- function(text, user, session=getCurlHandle(), ...) {
        .Defunct(msg=authMsg)

    if (inherits(user, "user"))
        user <- screenName(user)
    ## I don't know how to simply POST or send a DELETE via RCurl w/o
    ## postForm, but this isn't a form so it throws a warning.
    ## Suppress these warnings
    out <- suppressWarnings(postForm("http://twitter.com/direct_messages/new.json",
                                     ..., text=text, user=user, curl=session))
    buildDM(twFromJSON(rawToChar(out)))
}

taskStatus <- function(expr, to, msg="",
                       session=getCurlHandle()) {
        .Defunct(msg=authMsg)

    ##
    status <- try(expr, silent=TRUE)
    if (inherits(status, "try-error")) {
        out <- paste(paste("Your task failed with error message",
                           status), msg, ":")
        dmSend(out, to, session)
    }
    else {
        out <- paste("Your task has completed successfully",
                     msg, sep=":")
        dmSend(out, to, session)
    }
    status
}

