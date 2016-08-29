resource_families = c("account", "application", "blocks", "direct_message", "favorites", "followers",
             "friends", "friendships", "geo", "help", "lists", "saved_searches", "search",
             "statuses", "trends", "users")

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

get_authenticated_user = function(...) {
  buildUser(twInterfaceObj$doAPICall("account/verify_credentials", ...))
}

can_access_other_account = function(other_id) {
  # The idea here is to first get our own username, and then make sure that we're 
  # following screenName if they're protected. If they're protected and we're not
  # following, we can't see their details
  authenticated_user_id = get_authenticated_user()$getId()
  if (authenticated_user_id != other_id) {
    other_user = getUser(other_id)
    if (other_user$getProtected()) {
      relationship = friendships(user_ids = other_id)
      return(as.logical(relationship[1, "following"]))
    }
  }
  
  TRUE
}
          
