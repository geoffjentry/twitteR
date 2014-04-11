#' A function to send a Twitter DM after completion of a task
#' 
#' This function will run an R expression and send a direct message to a
#' specified user on success or failure.
#' 
#' This function will run \code{expr}, and send a Direct Message (DM) upon
#' completion which will report the expression's success or failure.
#' 
#' @param expr An R expression that will be run
#' @param to The user to send a message to, either \code{character} or an
#' \code{\link{user}} object.
#' @param msg An extra message to append to the standard DM
#' @return Either the value of the expression or an object of class
#' \code{try-error}.
#' @author Jeff Gentry
#' @seealso \code{\link{dmSend}}
#' @keywords interface
#' @examples
#' 
#'    \dontrun{
#'        taskStatus(z<-5, "username", session=sess)
#'    }
#' 
taskStatus <- function(expr, to, msg="") {
  if (!has_oauth_token())
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

