\name{trend-class}
\Rdversion{1.1}
\docType{class}
\alias{trend-class}
\alias{trend}
\alias{buildTrend}
\alias{trendFactory}
\alias{show,trend-method}
\title{Class "trend":  A class to represent Twitter trends }
\description{ Provides a model representing trends from Twitter}
\section{Fields}{
  \describe{
    \item{\code{name}:}{Name of the trend}
    \item{\code{date}:}{\code{POSIXct} representation of the date
      associated with this trend}
    \item{\code{promoted_content}:}{Logical, if this was promoted by
      Twitter}
    \item{\code{events}:}{Appears to be unused by Twitter}
    \item{\code{woeid}:}{Yahoo based location code, currently
      unimplemented}
    \item{\code{country}:}{Country associated with the trend, currently
      unimplemented}
    \item{\code{countryCode}:}{Country code associated with the trend,
      currently unimplemented}
  }
}
\section{Methods}{
  \describe{
     \item{\code{toDataFrame}:}{Converts this into a one row
       \code{\link{data.frame}}, with each field representing a column.
       This can also be accomplished by the S4 style
       \code{as.data.frame(objectName)}.}
  }
}
\details{
  The \code{trend} class is implemented as a reference class.
  As there should be no backwards compatibility issues, there are no S4
  methods provided as with the \code{user} and \code{status} classes.
  An instance of a generator for this class is provided as a convenience
  to the user as it is configured to handle most standard cases.  To
  access this generator, use the object \code{trendFactory}.  Accessor set
  & get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}} for
  more details).  As an example, the \code{date} field could be
  accessed using \code{object$getDate()} and \code{object$setDate()}.
}
\author{ Jeff Gentry }
\seealso{\code{\link{getTrends}}, \code{\link{setRefClass}}}
\examples{
  ## It is suggested that one does not call the constructor directly,
  ## but if desired this is the appropriate structure
  zz <- trendFactory$new(name='foo', date=Sys.Date())
  zz$getName()

  ## Instead, use getTrends:
  xx <- getTrends()
  xx[[1]]$getName()
}
\keyword{classes}
