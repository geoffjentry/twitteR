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
                    promoted_content <- FALSE
                  else
                    promoted_content <- TRUE
                  when <<- date
                  }
                callSuper(...)
              }
              )
            )
