setRefClass('trendList',
            contains='twitterObjList'
            )

setValidity('trendList', function(object) {
  listClassValidity(object, 'trend')
})

setRefClass('trend',
            contains='twitterObj',
            fields=list(
              name='character',
              events='character',
              promoted_content='logical',
              woeid='character',
              country='character',
              countryCode='character',
              when='POSIXct'),
            methods = list(
              initialize = function(json, date, ...) {
                if (!missing(json)) {
                  for (term in c('name', 'events',
                                 'woeid', 'country',
                                 'countryCode')) {
                    if (term %in% names(json))
                      assign(term, json[[term]],
                             inherits=TRUE)
                  }
                  if (is.null(json[['promoted_content']]))
                    promoted_content <<- FALSE
                  else
                    promoted_content <<- TRUE
                  when <<- date
                  }
                callSuper(...)
              }
              )
            )

trendFactory <- getRefClass('trend')
trendFactory$accessors(names(trendFactory$fields()))

buildTrend <- function(json, date) {
  trendFactory$new(json, date)
}

setMethod('show', signature='trend', function(object) {
  print(object$getName())
})

currentTrends <- function(exclude=NULL) {
  if (is.null(exclude))
    params <- NULL
  else
    params <- list(exclude=exclude)

  jsonList <- twInterfaceObj$doAPICall('trends/current', params=params)

}
