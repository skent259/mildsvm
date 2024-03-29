% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mi.R
\name{mi}
\alias{mi}
\title{Create an \code{mi} object}
\usage{
mi(bag_label, bag_name)
}
\arguments{
\item{bag_label}{The bag label or response, recorded as 0 = negative, 1 =
positive.}

\item{bag_name}{A unique bag identifier for each instance.}
}
\value{
An object of class \code{mi}.  Currently, no methods are implemented for
this.
}
\description{
Create an \code{mi} object, usually used as a response variable in a model
formula.
}
\examples{
mil_data <- generate_mild_df(positive_degree = 3, nbag = 10)
with(mil_data, head(mi(bag_label, bag_name)))
df <- get_all_vars(mi(bag_label, bag_name) ~ X1 + X2, data = mil_data)
head(df)

}
\seealso{
Other multiple instance formula helper functions: 
\code{\link{mild}()}
}
\author{
Sean Kent
}
\concept{multiple instance formula helper functions}
