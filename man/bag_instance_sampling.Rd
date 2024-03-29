% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{bag_instance_sampling}
\alias{bag_instance_sampling}
\title{Sample \code{mild_df} object by bags and instances}
\usage{
bag_instance_sampling(data, size)
}
\arguments{
\item{data}{A \code{mild_df} object containing the data.}

\item{size}{A non-negative integer giving the number of rows to choose from
\code{data}.}
}
\value{
A numeric vector of length \code{size} indicating which rows were sampled.
}
\description{
From a \code{mild_df} object, return a sample that evenly pulls from the unique
bags and unique instances from each bag as much as possible.  This is a form
of stratified sampling to avoid randomly sampling many rows from a few bags.
}
\examples{
mil_data <- generate_mild_df(positive_dist = "mvnormal",
                             nbag = 2,
                             ninst = 2,
                             nsample = 2)

rows <- bag_instance_sampling(mil_data, 6)
table(mil_data$bag_name[rows])
table(mil_data$instance_name[rows])

rows <- bag_instance_sampling(mil_data, 4)
table(mil_data$bag_name[rows])
table(mil_data$instance_name[rows])

}
\author{
Sean Kent
}
