initSession <- function(username, password) {
    handle <- getCurlHandle(userpwd=paste(username, password, sep=":"),
                            .opts=curlOptions(httpHeader=c(Expect="")))
    testConn <- try(dmGet(handle, num=1), silent=TRUE)
    if (inherits(testConn, "try-error"))
        stop("Could not authenticate you")
    else
        handle
}
