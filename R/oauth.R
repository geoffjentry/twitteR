getTwitterOAuth = function(consumer_key, consumer_secret) {
  stop("ROAuth is no longer used in favor of httr, please see ?setup_twitter_oauth")
}



#' Register OAuth credentials to twitter R session
#' 
#' These functions are deprecated
#' 
#' These functions are deprecated, see \code{\link{setup_twitter_oauth}}
#' 
#' @aliases registerTwitterOAuth getTwitterOAuth
#' @param consumer_key The consumer key supplied by Twitter
#' @param consumer_secret The consumer secret supplied by Twitter
#' @param oauth An object of class \code{OAuth}
#' @return \code{TRUE} on success, otherwise an error will be thrown
#' @author Jeff Gentry
#' @seealso \code{setup_twitter_oauth}
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'      fakeExample = 5
#'   }
#' 
registerTwitterOAuth <- function(oauth) {
  stop("ROAuth is no longer used in favor of httr, please see ?setup_twitter_oauth")
}

check_twitter_oauth = function() {
  req = try(stop_for_status(GET("https://api.twitter.com/1.1/account/settings.json", 
                                config(token=get_oauth_sig()))), silent=TRUE)
  
  if (inherits(req, "try-error")) {
    stop("OAuth authentication error:\nThis most likely means that you have incorrectly called setup_twitter_oauth()'")
  }  
}

get_twitter_token_via_sign = function(app, access_token, access_secret) {
  print("Using direct authentication")
  sig = sign_oauth1.0(app, token=access_token, token_secret=access_secret)
  if (! (is.list(sig)) && ("token" %in% names(sig))) {
    stop("Invalid response from sig_oauth1.0")
  }

  sig[["token"]]
}

get_twitter_token_via_browser = function(app, ...) {
  print("Using browser based authentication") 
  oauth1.0_token(oauth_endpoints('twitter'), app)
}



#' Sets up the OAuth credentials for a twitteR session
#' 
#' This function wraps the OAuth authentication handshake functions from the
#' httr package for a twitteR session
#' 
#' The \code{httr} package can cache authentication. See \code{\link{Token}}
#' for details
#' 
#' If both \code{access_token} and \code{access_secret} are set (i.e. not
#' \code{NULL}), these will be supplied directly to the OAuth authentication
#' instead of the browser based authentication dance one would normally
#' experience. This requires you to already know the access tokens for your
#' Twitter app. The usefuleness of this feature is primarily in a headless
#' environment where a web browser is not available.
#' 
#' @param consumer_key The consumer key supplied by Twitter
#' @param consumer_secret The consumer secret supplied by Twitter
#' @param access_token The access token supplied by Twitter
#' @param access_secret The access secret supplied by Twitter
#' @return This is called for its side effect
#' @author Jeff Gentry
#' @seealso \code{\link{Token}}, \code{\link{GET}}, \code{\link{POST}}
#' @keywords interface
#' @examples
#' 
#'  \dontrun{
#'     setup_twitter_oauth("CONSUMER_KEY", "CONSUMER_SECRET")
#'  }
#' 
setup_twitter_oauth = function(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL) {
  app <- oauth_app("twitter", key=consumer_key, secret=consumer_secret)
  
  if (is.null(access_token) || is.null(access_secret)) {
    token_func = get_twitter_token_via_browser
  } else {
    token_func = get_twitter_token_via_sign
  }
  
  twitter_token = token_func(app, access_token, access_secret)
  assign("oauth_token", twitter_token, envir=oauth_cache)  
  
  check_twitter_oauth()
}

has_oauth_token = function() {
  exists("oauth_token", envir=oauth_cache)
}

get_oauth_sig = function() {
  if (!has_oauth_token()) {
    stop("OAuth has not been registered for this session")
  }  
  
  return(get("oauth_token", envir=oauth_cache))
}
