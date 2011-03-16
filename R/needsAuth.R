buildDM <- function(json) {
  sender <- buildUser(json[['sender']])
  recip <- buildUser(json[['recipient']])
  if (is.null(json$text))
    json$text <- character()
  if (is.null(json$"recipient_id"))
    json[['recipient_id']] <- numeric()
  if (is.null(json[['sender_id']]))
    json[['sender_id']] <- numeric()
  if (is.null(json[['sender_screen_name']]))
    json[['sender_screen_name']] <- character()
  if (is.null(json[['created_at']]))
    json[['created_at']] <- character()
  if (is.null(json[['recipient_screen_name']]))
    json[['recipient_screen_name']] <- character()
  if (is.null(json[['id']]))
    json[['id']] <- numeric()
  new("directMessage",
      text=json[['text']],
      recipientSN=json[['recipient_screen_name']],
      created=json[['created_at']],
      recipientID=json[['recipient_id']],
      sender=sender,
      recipient=recip,
      senderID=json[['sender_id']],
      id=json[['id']],
      senderSN=json[['sender_screen_name']]
      )
}

## FIXME:  Needs a lot of work in terms of 'n', & other args

friendsTimeline <- function(...) {
  if (!hasOAuth())
    stop("friendsTimeline requires OAuth authentication")
  out <- getURL("http://api.twitter.com/1/statuses/friends_timeline.json",
                ...)
  jsonList <- doAPICall(out)
  sapply(jsonList, buildStatus)
}

mentions <- function(...) {
  if (!hasOAuth())
    stop("mentions requires OAuth authentication")
  out <- getURL("http://twitter.com/statuses/mentions.json",
                  curl=session, ...)
    jsonList <- doAPICall(out)
    sapply(jsonList, buildStatus)
}

updateStatus <- function(text, ...) {
  if (!hasOAuth())
    stop("updateStatus requires OAuth authentication")
  url <- paste("http://twitter.com/statuses/update.json?status=",
               text, sep='')
  out <- doAPICall(url, method="POST")
  ## out is in byte code, need to pass that through a raw conversion
  ## to get the string.  Not sure why sometimes it's byte arrays and
  ## sometimes it is strings
  buildStatus(doAPICall(rawToChar(out)))
}

tweet <- function(text, ...) {
    ## Just a wrapper around updateStatus
    updateStatus(text, session, ...)
}

deleteStatus <- function(status, ...) {
  if (!hasOAuth())
    stop("deleteStatus requires OAuth authentication")
  url <- paste("http://twitter.com/statuses/destroy/",
                 status@id, ".json", sep="")
    ## I don't know how to simply POST or send a DELETE via RCurl w/o
    ## postForm, but this isn't a form so it throws a warning.
    ## Suppress these warnings
    out <- suppressWarnings(postForm(url, curl=session), ...)
    TRUE
}

dmGet <- function(num=20, ...) {
  if (!hasOAuth())
    stop("dmGet requires OAuth authentication")
  out <- getURL(paste("http://api.twitter.com/1/direct_messages.json?count=",
                        num, sep=""),
                  curl=session, ...)
    jsonList <- doAPICall(out)
    sapply(jsonList, buildDM)
}

dmSent <- function(...) {
  if (!hasOAuth())
    stop("dmSent requires OAuth authentication")
  out <- getURL("http://twitter.com/direct_messages/sent.json",
                  curl=session, ...)
    jsonList <- doAPICall(out)
    sapply(jsonList, buildDM)
}

dmDestroy <- function(dm, ...) {
  if (!hasOAuth())
    stop("dmDestroy requires OAuth authentication")
  url <- paste("http://twitter.com/direct_messages/destroy/",
                 dm@id, ".json", sep="")
    ## I don't know how to simply POST or send a DELETE via RCurl w/o
    ## postForm, but this isn't a form so it throws a warning.
    ## Suppress these warnings
    out <- suppressWarnings(postForm(url, ..., curl=session))
    TRUE
}

dmSend <- function(text, user, ...) {
  if (!hasOAuth())
    stop("dmSend requires OAuth authentication")
  if (inherits(user, "user"))
        user <- screenName(user)
    ## I don't know how to simply POST or send a DELETE via RCurl w/o
    ## postForm, but this isn't a form so it throws a warning.
    ## Suppress these warnings
    out <- suppressWarnings(postForm("http://twitter.com/direct_messages/new.json",
                                     ..., text=text, user=user, curl=session))
    buildDM(doAPICall(rawToChar(out)))
}

taskStatus <- function(expr, to, msg="",
                       session=getCurlHandle()) {
  if (!hasOAuth())
    stop("taskStatus requires OAuth authentication")
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

