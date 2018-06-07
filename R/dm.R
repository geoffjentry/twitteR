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
                  if (!is.null(json[['recipient']][['id_str']]))
                    recipientID <<- json[['recipient']][['id_str']]
                  if (!is.null(json[['sender']][['id_str']]))
                    senderID <<- json[['sender']][['id_str']]
                  if (!is.null(json[['sender_screen_name']]))
                    senderSN <<- json[['sender_screen_name']]
                  if (!is.null(json[['id_str']])) {
                    id <<- json[['id_str']]
                  }
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

dmFactory <- getRefClass("directMessage")
dmFactory$accessors(names(dmFactory$fields()))


dmGet <- function(n=25, sinceID=NULL, maxID=NULL, ...) {
  dmGETBase(n, sinceID, maxID)
}

dmSent <- function(n=25, sinceID=NULL, maxID=NULL, ...) {
  dmGETBase(n, sinceID, maxID, "/sent")
}

dmGETBase <- function(n, sinceID, maxID, type='', ...) {
  if (!has_oauth_token())
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
  if (!has_oauth_token())
    stop("dmDestroy requires OAuth authentication")
  if (!inherits(dm, "directMessage"))
    stop("dm must be of class directMessage")
  json <- twInterfaceObj$doAPICall('direct_messages/destroy',
                                   params=list(id=dm$getId()),
                                   method='POST', ...)
  if (is.null(json$errors)) {
    TRUE
  } else {
    for (error in json$errors) {
      cat(error$message, error$code, fill = TRUE)
    }
    FALSE
  }
}

dmSend <- function(text, user, ...) {
  if (!has_oauth_token()) {
    stop("dmSend requires OAuth authentication")
  }
  if (inherits(user, "user")) {
    u.params <- c(list(...)[["uParams"]], list(screen_name = screenName(user)))
  } else {
    uParams <- parseUsers(user)
  }
  # if (nchar(text) > 140) {
  #   stop("Maximum of 140 chars may be sent via a direct message")
  # }
  params <- c(list(...)[["params"]], list(text=text))
  params[["user_id"]] <- uParams[["user_id"]]
  params[["screen_name"]] <- uParams[["screen_name"]]
  res <- twInterfaceObj$doAPICall('direct_messages/new',
                                  params=params, method='POST', ...)
  dmFactory$new(res)
}
