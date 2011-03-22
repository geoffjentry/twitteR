taskStatus <- function(expr, to, msg="") {
  if (!hasOAuth())
    stop("taskStatus requires OAuth authentication")
  
  status <- try(expr, silent=TRUE)
    if (inherits(status, "try-error")) {
        out <- paste(paste("Your task failed with error message",
                           status), msg, ":")
        dmSend(out, to)
    }
    else {
        out <- paste("Your task has completed successfully",
                     msg, sep=":")
        dmSend(out, to)
    }
    status
}

