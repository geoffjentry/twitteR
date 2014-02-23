tweet_columns = c("text", "favorited", "favoriteCount", "replyToSN", "created", 
                  "truncated", "replyToSID", "id", "replyToUID", "statusSource", 
                  "screenName", "retweetCount", "isRetweet", "retweeted", "longitude", 
                  "latitude")

user_columns = c("description", "statusesCount", "followersCount", "favoritesCount",
                 "friendsCount", "url", "name", "created", "protected", "verified",
                 "screenName", "location", "id", "listedCount", "followRequestSent",
                 "profileImageUrl")

camel_case_to_twitter_names = function(name) {
  tolower(gsub("([A-Z])", "_\\1", name, perl=TRUE))
}