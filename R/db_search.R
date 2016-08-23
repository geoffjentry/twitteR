search_twitter_and_store = function(searchString, table_name="tweets", n = 5000, lang=NULL, 
                                    locale=NULL, geocode=NULL, retryOnRateLimit=120, ...) {
  if (table_exists(table_name)) {
    since_id = get_latest_tweet_id(table_name)
  } else {
    since_id = NULL
  }
  
  new_tweets = suppressWarnings(searchTwitter(searchString, n=n, sinceID=since_id, lang=lang, 
                                              locale=locale, retryOnRateLimit=retryOnRateLimit, ...))
  if (length(new_tweets) > 0) {
    store_tweets_db(new_tweets, table_name)
  }
  
  length(new_tweets)
}
