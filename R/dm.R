setRefClass('dmList',
            contains='twitterObjList'
            )

setValidity('dmList', function(object) {
  listClassValidity(object, 'directMessage')
})

setRefClass("directMessage",
            contains='twitterObj',
            fields = list(
              text = "character",
              recipientSN = "character",
              created = "POSIXct",
              recipientID = "character",
              sender = "user",
              recipient = "user",
              senderID = "character",
              id = "character",
              senderSN = "character"),
            methods = list(
              initialize = function(json, ...) {
                if (!missing(json)) {
                  if (!is.null(json[['sender']]))
                    sender <<- buildUser(json[['sender']])
                  if (!is.null(json[['recipient']]))
                    recipient <<- buildUser(json[['recipient']])
                  if (!is.null(json[['text']]))
                    text <<- json[['text']]
                if (!is.null(json[['recipient_screen_name']]))
                  recipientSN <<- json[['recipient_screen_name']]
                  if (!is.null(json[['created']]))
                    created <<- twitterDateToPOSIX(json[['created']])
                  if (!is.null(json[['recipient_id']]))
                    recipientID <<- json[['recipient_id']]
                  if (!is.null(json[['sender_id']]))
                    senderID <<- json[['sender_id']]
                  if (!is.null(json[['sender_screen_name']]))
                    senderSN <<- json[['sender_screen_name']]
                  if (!is.null(json[['id']]))
                    id <<- json[['id']]
                }
                callSuper(...)
              },
              destroy = function() {
                dmDestroy(.self)
              }
              )
            )

setMethod("show", signature="directMessage", function(object) {
    print(paste(screenName(object$sender), "->",
                screenName(object$recipient),  ": ",
                object$text, sep=""))
})
setMethod('as.data.frame', signature='directMessage',
          function(x, row.names=NULL, optional=FALSE, ...)
          x$toDataFrame(row.names, optional))

dmFactory <- getRefClass("directMessage")
dmFactory$accessors(names(dmFactory$fields()))


dmGet <- function(n=25, sinceID=NULL, maxID=NULL, ...) {
  dmGETBase(n, sinceID, maxID)
}

dmSent <- function(n=25, sinceID=NULL, maxID=NULL, ...) {
  dmGETBase(n, sinceID, maxID, "/sent")
}

dmGETBase <- function(n, sinceID, maxID, type='', ...) {
  if (!hasOAuth())
    stop("dmGet requires OAuth authentication")

  if (n <= 0)
    stop("n must be positive")
  else
    n <- as.integer(n)
  params <- buildCommonArgs(since_id=sinceID, max_id=maxID)
  jsonList <- doPagedAPICall(paste('direct_messages', type, sep=''),
                             num=n, params=params, ...)
  sapply(jsonList, function(x) dmFactory$new(x))
}

dmDestroy <- function(dm, ...) {
  if (!hasOAuth())
    stop("dmDestroy requires OAuth authentication")
  if (!inherits(dm, "directMessage"))
    stop("dm must be of class directMessage")
  twInterfaceObj$doAPICall(paste('direct_messages/destroy',
                                 dm$getId(), sep='/'),
                           method='POST', ...)
  TRUE
}

dmSend <- function(text, user, ...) {
  if (!hasOAuth())
    stop("dmSend requires OAuth authentication")
  if (inherits(user, "user"))
        user <- screenName(user)
  if (nchar(text) > 140)
    stop("Maximum of 140 chars may be sent via a direct message")
  params[['text']] <- text
  res <- twInterfaceObj$doAPICall('direct_messages/new',
                                  params=params, method='POST', ...)
  dmFactory$new(res)
}
