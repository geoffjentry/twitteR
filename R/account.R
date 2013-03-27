getCurRateLimitInfo <- function(resources=character(), ...) {
  params <- list()
  if (length(resources) > 0) {
    params[["resources"]] <- paste(resources, collapse=",")
  }
  json <- twInterfaceObj$doAPICall("application/rate_limit_status", params=params, ...)
  if (! "resources" %in% names(json)) {
    stop("Invalid response from server - no 'resources' field in JSON")
  }
  resources <- unlist(json[["resources"]], recursive=FALSE)
  resource_names <- sapply(strsplit(names(resources), "\\."), function(x) x[2])
  resource_rows <- lapply(resources, function(x) {
    return(c(limit=x$limit, remaining=x$remaining, reset=twitterDateToPOSIX(x$reset)))
  })
  resource_matrix <- cbind(resource=resource_names, do.call(rbind, resource_rows))
  rownames(resource_matrix) <- NULL
  resource_df <- as.data.frame(resource_matrix, stringsAsFactors=FALSE)
  resource_df[, "reset"] <- as.POSIXct(as.numeric(resource_df[, "reset"]), tz="UTC", origin="1970-01-01")
  return(resource_df)
 }


          
