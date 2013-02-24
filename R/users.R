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
              lastStatus="status",
              listedCount="numeric",
              followRequestSent="logical",
              profileImageUrl="character"
              ),
            methods = list(
              initialize = function(json, ...) {
                if (!missing(json)) {
                  if (!is.null(json[['status']]))
                    lastStatus <<- buildStatus(json[['status']])
                  if (is.character(json[['description']]))
                    description <<- json[['description']]
                  if (!is.null(json[['statuses_count']]))
                    statusesCount <<- as.numeric(json[['statuses_count']])
                  if (!is.null(json[['followers_count']]))
                    followersCount <<- as.numeric(json[['followers_count']])
                  if (!is.null(json[['friends_count']]))
                    friendsCount <<- as.numeric(json[['friends_count']])
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
                  if (!is.null(json[["id_str"]])) {
                    id <<- as.character(json[["id_str"]])
                  }
                  if (!is.null(json[['location']])) {
                    location <<- json[['location']]
                  }
                  if (!is.null(json[["listed_count"]])) {
                    listedCount <<- json[["listed_count"]]
                  }
                  if ((is.null(json[["followRequestSent"]])) ||
                      (json[["followRequestSent"]] == FALSE)) {
                    followRequestSent <<- FALSE
                  } else {
                    followRequestSent <<- TRUE
                  }
                  if (!is.null(json[["profile_image_url"]])) {
                    profileImageUrl <<- json[["profile_image_url"]]
                  }
                }
                callSuper(...)
              },
              getFollowerIDs = function(n=NULL, ...) {
                return(unlist(followers(.self$id, n, ...)))
              },
              getFollowers = function(n=NULL, ...) {
                fol <- .self$getFollowerIDs(n, ...)
                lookupUsers(fol, ...)
              },
              getFriendIDs = function(n=NULL, ...) {
                return(unlist(friends(.self$id, n, ...)))
              }, 
              getFriends = function(n=NULL, ...) {
                fri <- .self$getFriendIDs(n, ...)
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

lookupUsers <- function(users, includeNA=FALSE, ...) {
  MatchLookedUpUsers <- function(vals) {
    order <- match(tolower(users), tolower(vals))
    na.eles <- which(is.na(order))
    
    if (length(na.eles) > 0) {
      if (!includeNA) {
        order <- order[-na.eles]
        users <- users[-na.eles]
      }
    }
    out <- out[order]
    names(out) <- users
    
    return(out)
  }
  
  batches <- split(users, ceiling(seq_along(users) / 100))
  results <- lapply(batches, function(batch) {
    params <- parseUsers(batch)
    twInterfaceObj$doAPICall(paste('users', 'lookup', sep='/'),
                             params=params, ...)
  })
  out <- sapply(do.call(c, results), buildUser)

  ## Order these to match the users vector - if !includeNA,
  ## drop out the elements of the return list which weren't
  ## found
  sn.lookups <- MatchLookedUpUsers(sapply(out,
                                          function(x) x$getScreenName()))
  id.lookups <- MatchLookedUpUsers(sapply(out, function(x) x$getId()))

  ## The problem with doing it in the two batch way above is that
  ## anything that was SN will be NULL for ID and vice versa.
  ## If includeNA is TRUE, we can't just remove all empty
  ## entries. As a hack, only retain the NULL values that are shared
  ## between both lists
  if (includeNA) {
    sn.nulls <- sapply(sn.lookups, is.null)
    id.nulls <- sapply(id.lookups, is.null)
    false.nulls <- xor(sn.nulls, id.nulls)
    sn.lookups <- sn.lookups[!(sn.nulls & false.nulls)]
    id.lookups <- id.lookups[!(id.nulls & false.nulls)]
  } else {
    ## Otherwise, just strip out the names that have been
    ## taken out
    users <- intersect(users, union(names(sn.lookups), names(id.lookups)))
  }
  
  out <- c(sn.lookups, id.lookups)
  return(out[users])
}

 
