oauthCache <- new.env(hash=TRUE)

## Detect if ROAuth is available, if so load it.  
suppressWarnings(require("ROAuth", quietly=TRUE))

