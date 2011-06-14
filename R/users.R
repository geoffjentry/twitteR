setRefClass('userList',
            contains='twitterObjList'
            )

setValidity('userList', function(object) {
  listClassValidity(object, 'user')
})

setRefClass("user",
            contains='twitterObj',
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
                  if ((is.null(json[['protected']])) ||
                      (json[['protected']] == FALSE))
                    protected <<- FALSE
                  else
                    protected <<- TRUE

                  if ((is.null(json[['verified']])) ||
                      (json[['verified']] == FALSE))
                    verified <<- FALSE
                  else
                    verified <<- TRUE
                  if (is.character(json[['screen_name']]))
                    screenName <<- json[['screen_name']]
                  if (!is.null(json[['id']]))
                    id <<- json[['id']]
                  if (!is.null(json[['location']]))
                    location <<- json[['location']]
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
              },
              toDataFrame = function(row.names=NULL, optional=FALSE) {
                ## FIXME:
                ## There is such little difference between this version
                ## and the standard that there has to be a way to take
                ## advantage of inheritance here
                fields <- setdiff(names(.self$getRefClass()$fields()),
                                  'lastStatus')
                fieldList <- lapply(fields, function(x) {
                  val <- .self$field(x)
                  if (length(val) == 0)
                    NA
                  else
                    val
                })
                names(fieldList) <- fields
                as.data.frame(fieldList, row.names=row.names,
                              optional=optional)
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

getUser <- function(user, ...) {
  params <- parseUsers(user)
  buildUser(twInterfaceObj$doAPICall(paste('users', 'show', sep='/'),
                                     params=params, ...))
}

lookupUsers <- function(users, ...) {
  batches <- split(users, ceiling(seq_along(users) / 100))
  results <- lapply(batches, function(batch) {
    params <- parseUsers(users)
    twInterfaceObj$doAPICall(paste('users', 'lookup', sep='/'),
                             params=params, ...)
  })
  sapply(do.call(c, results), buildUser)
}
