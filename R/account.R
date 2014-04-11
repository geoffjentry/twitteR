resource_families = c("account", "application", "blocks", "direct_message", "favorites", "followers",
             "friends", "friendships", "geo", "help", "lists", "saved_searches", "search",
             "statuses", "trends", "users")



#' A function to retrieve current rate limit information
#' 
#' Will retrieve the current rate limit information for the authenticated user,
#' displayed as a data.frame displaying specifc information for every Twitter
#' resource
#' 
#' By default, all known resource families will be polled. These families are
#' contained in the object \code{resource_families}. If you would like to
#' filter this down you may tweak the \code{resources} argument.
#' 
#' The full list of allowed values in \code{resources} is as follows:
#' \code{lists}, \code{application}, \code{friendships}, \code{blocks},
#' \code{geo}, \code{users}, \code{followers}, \code{statuses}, \code{help},
#' \code{friends}, \code{direct_messages}, \code{account}, \code{favorites},
#' \code{saved_searches}, \code{search}, \code{trends}.
#' 
#' @aliases getCurRateLimitInfo resource_families
#' @param resources A character vector of specific resources to get information
#' for
#' @param \dots Optional arguments to pass to cURL
#' @return A four column data.frame with columns \code{resource}, \code{limit},
#' \code{remaining} and \code{reset}. These detail the specific resource name,
#' the rate limit for that block, the number of calls remaining and the time
#' the rate limit will be reset in UTC time.
#' @author Jeff Gentry
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'     zz <- getCurRateLimitInfo(c("lists", "users"))
#'   }
#' 
getCurRateLimitInfo <- function(resources=resource_families, ...) {
  if ((length(resources) == 0) || (!all(resources %in% resource_families))){
    stop("Must provide at least one valid resource family: ", 
         paste(resource_families, collapse=", "))
  }
  params = list()
  if (length(resources) > 0) {
    params[["resources"]] = paste(resources, collapse=",")
  }
  json <- twInterfaceObj$doAPICall("application/rate_limit_status", params=params, ...)
  if (! "resources" %in% names(json)) {
    stop("Invalid response from server - no 'resources' field in JSON")
  }
  resources = unlist(json[["resources"]], recursive=FALSE)
  resource_names = sapply(strsplit(names(resources), "\\."), function(x) x[2])
  resource_rows = lapply(resources, function(x) {
    return(c(limit=x$limit, remaining=x$remaining, reset=twitterDateToPOSIX(x$reset)))
  })
  resource_matrix = cbind(resource=resource_names, do.call(rbind, resource_rows))
  rownames(resource_matrix) = NULL
  resource_df = as.data.frame(resource_matrix, stringsAsFactors=FALSE)
  resource_df[, "reset"] = as.POSIXct(as.numeric(resource_df[, "reset"]), tz="UTC", origin="1970-01-01")
  return(resource_df)
 }


          
