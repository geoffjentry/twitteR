getTwitterOAuth = function(consumer_key, consumer_secret) {
  stop("ROAuth is no longer used in favor of httr, please see ?setup_twitter_oauth")
}

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

use_oauth_token = function(twitter_token) {
  assign("oauth_token", twitter_token, envir=oauth_cache)
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