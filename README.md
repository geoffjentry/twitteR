## twitteR
twitteR is an R package which provides access to the Twitter API. Most functionality of the API is supported, with a bias towards API calls that are more useful in data analysis as opposed to daily interaction. 

## Getting Started

- Please read the [user vignette](http://geoffjentry.hexdump.org/twitteR.pdf), which admittedly can get a bit out of date
- I generally recommend that users track this GitHub version as opposed to the CRAN version as releases only happen every few months (if not more) and many fixes can show up in that time. Currently there's also a dependence on the github version of [httr](https://github.com/hadley/httr). To do this:
  - Create a Twitter application at http://dev.twitter.com. Make sure to give the app read, write and direct message authority.
  - Take note of the following values from the Twitter app page: "API key", "API secret", "Access token", and "Access token secret". 
  - `install.packages(c("devtools", "rjson", "bit64"))`
  - `install_github("httr")`
  - `install_github("twitteR", username="geoffjentry")`
  - Make sure to restart your R session at this point
  - `library(twitteR)`
  - `setup_twitter_oauth("API key", "API secret", "Access token", "Access token secret", credentials_file="filename")`
    - All values besides `filename` are from the Twitter app page above. The `credentials_file` argument is optional and allows you to restore your authentication later on (using `load_twitter_oauth("filename")`)
  - You should be ready to go!
- If you have any questions or issues, check out the [mailing list](http://lists.hexdump.org/listinfo.cgi/twitter-users-hexdump.org)
