setRefClass('dmList',
            contains='twitterObjList'
            )

setValidity('dmList', function(object) {
  listClassValidity(object, 'directMessage')
})

#' Class "directMessage": A class to represent Twitter Direct Messages
#' 
#' Provides a model representing direct messages (DMs) from Twitter
#' 
#' The \code{directMessage} class is implemented as a reference class.  As
#' there should be no backwards compatibility issues, there are no S4 methods
#' provided as with the \code{user} and \code{status} classes.  An instance of
#' a generator for this class is provided as a convenience to the user as it is
#' configured to handle most standard cases.  To access this generator, use the
#' object \code{dmFactory}.  Accessor set & get methods are provided for every
#' field using reference class \code{$accessors()} methodology (see
#' \code{\link{setRefClass}} for more details).  As an example, the
#' \code{sender} field could be accessed using \code{object$getSender()} and
#' \code{object$setSender()}.
#' 
#' The constructor of this object assumes that the user is passing in a JSON
#' encoded Twitter Direct Message.  It is also possible to directly pass in the
#' arguments.
#' 
#' @name directMessage-class
#' @aliases directMessage-class directMessage dmFactory
#' show,directMessage-method
#' @docType class
#' @section Fields: \describe{ \item{list("text")}{Text of the DM}\item{:}{Text
#' of the DM} \item{list("recipient")}{A \code{user} object representing the
#' recipient of the message}\item{:}{A \code{user} object representing the
#' recipient of the message} \item{list("recipientSN")}{Screen name of the
#' recipient}\item{:}{Screen name of the recipient}
#' \item{list("recipientID")}{ID number of the recipient}\item{:}{ID number of
#' the recipient} \item{list("sender")}{A \code{user} object representing the
#' sender of the message}\item{:}{A \code{user} object representing the sender
#' of the message} \item{list("senderSN")}{Screen name of the
#' sender}\item{:}{Screen name of the sender} \item{list("senderID")}{ID number
#' of the sender}\item{:}{ID number of the sender} \item{list("created")}{When
#' the messages was created}\item{:}{When the messages was created} }
#' @author Jeff Gentry
#' @seealso \code{\link{dmGet}}, \code{\link{dmSend}}, \code{\link{dmDestroy}},
#' \code{\link{setRefClass}}
#' @keywords classes
#' @examples
#' 
#'   \dontrun{
#'     dm <- dmFactory$new(text='foo', recipientSN='blah')
#'     dm$getText()
#'   
#'     ## assume 'json' is the return from a Twitter call
#'     dm <- dmFactory$new(json)
#'     dm$getSenderID()
#'   }
#'
directMessage = methods:::setRefClass("directMessage",
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




#' Functions to manipulate Twitter direct messages
#' 
#' These functions allow you to interact with, send, and delete direct messages
#' (DMs) in Twitter.
#' 
#' 
#' @aliases dmGet dmSent dmDestroy dmSend
#' @param text The text of a message to send
#' @param user The user to send a message to, either \code{character} or an
#' \code{\link{user}} object.
#' @param dm The message to delete, an object of class
#' \code{\link{directMessage}}
#' @param n The maximum number of direct messages to return
#' @param sinceID If not \code{NULL}, an ID representing the earliest boundary
#' @param maxID If not \code{NULL}, an ID representing the newest ID you wish
#' to retrieve
#' @param ... Further arguments to pass along the communication chain
#' @return These functions will not work without \code{OAuth} authentication
#' 
#' The \code{dmGet} and \code{dmSent} functions will return a list of
#' \code{\link{directMessage}} objects.  The former will retrieve DMs sent to
#' the user while the latter retrieves messages sent from the user.
#' 
#' The \code{dmDestroy} function takes a \code{\link{directMessage}} object
#' (perhaps from either \code{dmGet} or \code{dmSent}) and will delete it from
#' the Twitter server.
#' 
#' The \code{dmSend} function will send a message to another Twitter user.
#' @author Jeff Gentry
#' @seealso \code{\link{directMessage}}, \code{\link{registerTwitterOAuth}}
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'            dms <- dmGet()
#'            dms
#'            ## delete the first one
#'            dms[[1]]$destroy()
#'            dmDestroy(dms[[2]])
#'            ## send a DM
#'            dmSend('Testing out twitteR!', 'twitter')
#'    }
#' 
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
  if (nchar(text) > 140) {
    stop("Maximum of 140 chars may be sent via a direct message")
  }
  params <- c(list(...)[["params"]], list(text=text))
  params[["user_id"]] <- uParams[["user_id"]]
  params[["screen_name"]] <- uParams[["screen_name"]]
  res <- twInterfaceObj$doAPICall('direct_messages/new',
                                  params=params, method='POST', ...)
  dmFactory$new(res)
}
