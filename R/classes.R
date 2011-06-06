setRefClass("status",
            fields = list(
              text="character",
              favorited="logical",
              replyToSN="character",
              created="POSIXct",
              truncated="logical",
              replyToSID="character",
              id="character",
              replyToUID="character",
              statusSource="character",
              screenName="character"
              ),
            methods=list(
              initialize = function(json, ...) {
                if (!missing(json)) {
                  if ('user' %in% names(json)) {
                    userObj <- userFactory$new(json[['user']])
                    screenName <<- userObj$getScreenName()
                  } else if ('from_user' %in% names(json)) {
                    screenName <<- json[['from_user']]
                  } else {
                    screenName <<- "Unknown"
                  }
                  if (!is.null(json[['text']]))
                    text <<- json[['text']]
                  if (is.null(json[['favorited']]))
                    favorited <<- json[['favorited']]
                  else
                    favorited <<- TRUE
                  if (is.null(json[['truncated']]))
                    truncated <<- FALSE
                  else
                    truncated <<- TRUE
                  if (!is.null(json[['source']]))
                    statusSource <<- json[['source']]
                  if (is.null(json[['created_at']]))
                    created <<- Sys.time()
                  else
                    created <<- twitterDateToPOSIX(json[['created_at']])
                  if ((!is.null(json[['in_reply_to_screen_name']])) &&
                      (!is.na(json[['in_reply_to_screen_name']])))
                    replyToSN <<- json[['in_reply_to_screen_name']]
                  if ((!is.null(json[['in_reply_to_status_id']])) &&
                      (!is.na(json[['in_reply_to_status_id']])))
                    replyToSID <<- json[['in_reply_to_status_id']]
                  if ((!is.null(json[['in_reply_to_user_id']])) &&
                      (!is.na(json[['in_reply_to_user_id']])))
                    replyToUID <<- json[['in_reply_to_user_id']]
                  if (!is.null(json[['id']]))
                    id <<- json[['id']]
                }
                callSuper(...)
              }
              )
            )

statusFactory <- getRefClass("status")
statusFactory$accessors(names(statusFactory$fields()))

buildStatus <- function(json)
  statusFactory$new(json)

setMethod("show", signature="status", function(object) {
    print(paste(screenName(object), object$text, sep=": "))
})



setRefClass("user",
            fields = list(
              description="character",
              statusesCount="numeric",
              followersCount="numeric",
              favoritesCount="numeric",
              friendsCount="numeric",
              url="character",
              name="character",
              created="POSIXct",
              protected="logical",
              verified="logical",
              screenName="character",
              location="character",
              id="character",
              lastStatus="status"
              ),
            methods = list(
              initialize = function(json, ...) {
                if (!missing(json)) {
                  if (!is.null(json[['status']]))
                    lastStatus <<- buildStatus(json[['status']])
                  if (is.character(json[['description']]))
                    description <<- json[['description']]
                  if (!is.null(json[['statuses_count']]))
                    statusesCount <<- json[['statuses_count']]
                  if (!is.null(json[['followers_count']]))
                    followersCount <<- json[['followers_count']]
                  if (!is.null(json[['friends_count']]))
                    friendsCount <<- json[['friends_count']]
                  if ((!is.null(json[['url']]))&&(!is.na(json[['url']])))
                    url <<- json[['url']]
                  if (is.character(json[['name']]))
                    name <<- json[['name']]
                  if (is.null(json[['created_at']])) {
                    created <<- Sys.time()
                  }
                  else {
                    created <<- twitterDateToPOSIX(json[['created_at']])
                  }
                  if ((is.null(json[['protected']])) || (json[['protected']] == FALSE))
                    protected <<- FALSE
                  else
                    protected <<- TRUE

                  if ((is.null(json[['verified']])) || (json[['verified']] == FALSE))
                    verified <<- FALSE
                  else
                    verified <<- TRUE
                  if (is.character(json[['screen_name']]))
                    screenName <<- json[['screen_name']]
                  if (!is.null(json[['id']]))
                    id <<- json[['id']]
                }
                callSuper(...)
              },
              getFollowerIDs = function(n=NULL, ...) {
                followers(.self$id, n, ...)
              },
              getFollowers = function(n=NULL, ...) {
                fol <- .self$followerIDs(n, ...)
                lookupUsers(fol, ...)
              },
              getFriendsIDs = function(n=NULL, ...) {
                friends(.self$id, n, ...)
              },
              getFriends = function(n=NULL, ...) {
                fri <- .self$friendIDs(n, ...)
                lookupUsers(fri, ...)
              }
              )
            )

userFactory <- getRefClass("user")
userFactory$accessors(names(userFactory$fields()))

buildUser <- function(json)
  userFactory$new(json)


setMethod("show", signature="user", function(object) {
    print(screenName(object))
})


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

dmFactory <- getRefClass("directMessage")
dmFactory$accessors(names(dmFactory$fields()))
