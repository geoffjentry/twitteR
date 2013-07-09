\name{import_statuses}
\alias{import_statuses}
\alias{import_obj}
\alias{import_trends}
\alias{import_users}
\alias{json_to_statuses}
\alias{json_to_trends}
\alias{json_to_users}
\title{
Functions to import twitteR objects from various sources
}
\description{
Functions designed to import data into twitteR objects from a variety of data
sources. Currently only JSON is supported, and this entire branch of functionality
should be considered experimental & under development.
}
\usage{
import_statuses(raw_data, conversion_func = json_to_statuses)
import_trends(raw_data, conversion_func = json_to_trends)
import_users(raw_data, conversion_func = json_to_users)
import_obj(raw_data, conversion_func, ...)
json_to_users(raw_data)
json_to_statuses(raw_data)
json_to_trends(raw_data)
}
\arguments{
  \item{raw_data}{Data to be be parsed via the prescribed function}
  \item{conversion_func}{The function to convert \code{raw_data} into the specified twitteR object}
  \item{...}{Arguments to pass along to \code{conversion_func}}
}
\value{
A list of twitteR objects of the appropriate type, e.g. \code{\link{status}}, \code{\link{user}}, etc
}
\author{Jeff Gentry}
\seealso{\code{\link{status}}, \code{\link{user}}}
\examples{
   \dontrun{
      status_list = import_statuses(list_of_status_json)   
   }
}
\keyword{interface}