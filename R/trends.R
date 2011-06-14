trendListFactory <- setRefClass('trendList',
                                contains='twitterObjList'
                                )
trendListFactory$accessors(names(trendListFactory$fields()))

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
              date='POSIXct'),
            methods = list(
              initialize = function(json, date, ...) {
                if (!missing(json)) {
                  ## FIXME: one of these days I'll figure out how
                  ## to do this programatically - tried various
                  ## permutations of do.call(), assign(), $field(),
                  ## etc
                  if ('name' %in% names(json))
                    name <<- json[['name']]
                  if ('events' %in% names(json))
                    events <<- json[['events']]
                  if ('woeid' %in% names(json))
                    woeid <<- json[['woeid']]
                  if ('country' %in% names(json))
                    country <<- json[['country']]
                  if ('countryCode' %in% names(json))
                    countryCode <<- json[['countryCode']]
                  if (is.null(json[['promoted_content']]))
                    promoted_content <<- FALSE
                  else
                    promoted_content <<- TRUE
                  date <<- date
                  }
                callSuper(...)
              }
              )
            )

trendFactory <- getRefClass('trend')
trendFactory$accessors(names(trendFactory$fields()))

buildTrend <- function(json, date) {
  ## we don't need to do the fancy twitter date mapping, this one is
  ## already ok
  trendFactory$new(json, as.POSIXct(date))
}

setMethod('show', signature='trend', function(object) {
  print(object$getName())
})


getTrends <- function(period=c('current', 'daily', 'weekly'),
                      exclude=NULL, date=NULL) {
  period <- match.arg(period)
  params <- buildCommonArgs(exclude=exclude, date=date)
  jsonList <- twInterfaceObj$doAPICall(paste('trends', period, sep='/'),
                                       params=params)
  trends <- jsonList[['trends']]
  trendObjs <- do.call('c', lapply(names(trends), function(x, trends) {
    lapply(trends[[x]], buildTrend, x)
  }, trends))
  trendObjs
###  trendListFactory$new(objectList=trendObjs)
}
