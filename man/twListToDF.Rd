\name{twListToDF}
\alias{twListToDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twListToDF(twList)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twList}{
%%     ~~Describe \code{twList} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(twList) {
  ## iff all elements of twList are from a class defined in this
  ## package, and all of the same class, will collapse these into
  ## a data.frame and return
  classes <- unique(sapply(twList, class))
  if (length(classes) != 1)
    stop("Not all elements of twList are of the same class")
  if (! classes \%in\% c('status', 'user', 'directMessage', 'trend'))
    stop("Elements of twList are not of an appropriate class")
  do.call('rbind', lapply(twList, as.data.frame))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
