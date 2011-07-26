rateLimitInfoFactory <- setRefClass("rateLimitInfo", 
                                    contains='twitterObj',
                                    fields=list(
                                      remainingHits = "numeric",
                                      resetTimeInSeconds = "numeric",
                                      hourlyLimit = "numeric",
                                      resetTime = "POSIXct"),
                                    methods=list(
                                      initialize = function(json, ...) {
                                        if (!missing(json)) {
                                          if (!is.null(json[["remaining_hits"]]))
                                            remainingHits <<- json[["remaining_hits"]]
                                          if (!is.null(json[["reset_time_in_seconds"]]))
                                            resetTimeInSeconds <<- json[["reset_time_in_seconds"]]
                                          if (!is.null(json[["hourly_limit"]]))
                                            hourlyLimit <<- json[["hourly_limit"]]
                                          if (!is.null(json[["reset_time"]]))
                                            resetTime <<- twitterDateToPOSIX(json[["reset_time"]])
                                        }
                                        callSuper(...)
                                      }
                                      )
                                    )

rateLimitInfoFactory$accessors(names(rateLimitInfoFactory$fields()))

buildRateLimitInfo <- function(json)
  rateLimitInfoFactory$new(json)

setMethod("show", signature="rateLimitInfo", function(object) {
  print(object$getRemainingHits())
})

getCurRateLimitInfo <- function(...) {
  json <- twInterfaceObj$doAPICall("account/rate_limit_status", check=FALSE, ...)
  buildRateLimitInfo(json)
}


          
