setRefClass('twitterObj',
            contains='VIRTUAL',
            methods = list(
              toDataFrame = function(row.names=NULL, optional=FALSE) {
                fields <- names(.self$getRefClass()$fields())
                fieldList <- lapply(fields, function(x) {
                  val <- .self$field(x)
                  if (length(val) == 0)
                    NA
                  else
                    val
                })
                names(fieldList) <- fields
                as.data.frame(fieldList, row.names=row.names,
                              optional=optional)
              }
              )
            )

setMethod('as.data.frame', signature='twitterObj',
          function(x, row.names=NULL, optional=FALSE, ...)
          x$toDataFrame(row.names, optional))

setRefClass('twitterObjList',
            contains = 'VIRTUAL',
            fields = list(
              objectList = 'list'
              ),
            methods = list()
            )

setMethod('show', signature='twitterObjList', function(object) {
  print(object$objectList)
})

setMethod('[[', signature='twitterObjList', function(x, i) {
  x$objectList[[i]]
})

listClassValidity <- function(object, objClass) {
  all(sapply(object$objectList, is, objClass))
}
