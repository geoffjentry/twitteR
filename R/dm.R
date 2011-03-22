## FIXME:  Need a 'show' method of some sort

setRefClass("directMessage",
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
                sender <<- buildUser(json[['sender']])
                recipient <<- buildUser(json[['recipient']])
                if (!is.null(json$text))
                  text <<- json$text
                if (!is.null(json$recipient_screen_name))
                  recipientSN <<- json$recipient_screen_name
                if (!is.null(json$created))
                  created <<- twitterDateToPOSIX(json$created)
                if (!is.null(json$recipient_id))
                  recipientID <<- json$recipient_id
                if (!is.null(json$sender_id))
                  senderID <<- json$sender_id
                if (!is.null(json$sender_screen_name))
                  senderSN <<- json$sender_screen_name
                if (!is.null(json$id))
                  id <<- json$id
                callSuper(...)
              },
              show = function() {
                print(paste(screenName(sender),
                            '->', screenName(recipient),
                            ':', text, sep=''))
              }
              )
            )

dmFactory <- getRefClass("directMessage")
dmFactory$accessors(names(dmFactory$fields()))
              
#setMethod("show", signature="directMessage", function(object) {
#    print(paste(screenName(object@sender), "->",
#                screenName(object@recipient),  ":",
#                object@text, sep=""))
#})


dmGet <- function(n=25, sinceID=NULL, maxID=NULL) {
  dmGETBase(n, sinceID, maxID, "direct_messages")
}

dmSent <- function(n=25, sinceID=NULL, maxID=NULL) {
  dmGETBase(n, sinceID, maxID, "direct_messages/sent")
}

dmGETBase <- function(n, sinceID, maxID, type) {
  if (!hasOAuth())
    stop("dmGet requires OAuth authentication")

  if (n <= 0)
    stop("n must be positive")
  else
    n <- as.integer(n)

  if (! is.null(sinceID))
    baseURL <- paste(baseURL, "&since_id=", sinceID, sep="")
  if (! is.null(maxID))
    baseURL <- paste(baseURL, "&max_id=", maxID, sep="")
  page <- 1
  total <- n
  count <- ifelse(n < 200, n, 200)
  jsonList <- list()
  while (total > 0) {
    url <- paste("http://api.twitter.com/1/", type,
                 ".json?count=",
                 count, '&page=', page, sep='')
    jsonList <- c(jsonList, doAPICall(url))
    total <- total - count
    page <- page + 1
  }
  if ((length(jsonList) > 0) && (length(jsonList) > n))
    jsonList <- jsonList[1:n]
  sapply(jsonList, function(x) dmFactory$new(x))
}

dmDestroy <- function(dm) {
  if (!hasOAuth())
    stop("dmDestroy requires OAuth authentication")
  if (!inherits(dm, "directMessage"))
    stop("dm must be of class directMessage")
  url <- paste("http://api.twitter.com/1/direct_messages/destroy/",
                 dm$getId(), ".json", sep="")
  doAPICall(url, method="POST")
  TRUE
}

dmSend <- function(text, user, ...) {
  if (!hasOAuth())
    stop("dmSend requires OAuth authentication")
  if (inherits(user, "user"))
        user <- screenName(user)
  if (nchar(text) > 140)
    stop("Maximum of 140 chars may be sent via a direct message")

  url <- URLencode(paste("http://api.twitter.com/1/direct_messages/new.json",
                         "?text=", text, "&user=", user, sep=''))
  dmFactory$new(doAPICall(url, method="POST"))
}
