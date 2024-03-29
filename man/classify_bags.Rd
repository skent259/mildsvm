% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{classify_bags}
\alias{classify_bags}
\title{Classify y from bags}
\usage{
classify_bags(y, bags, condense = TRUE)
}
\arguments{
\item{y}{A numeric, character, or factor vector of bag labels for each
instance.  Must satisfy \code{length(y) == nrow(x)}. Suggest that one of the
levels is 1, '1', or TRUE, which becomes the positive class; otherwise, a
positive class is chosen and a message will be supplied.}

\item{bags}{A vector specifying which instance belongs to each bag.  Can be a
string, numeric, of factor.}

\item{condense}{A logical (default \code{TRUE}) for whether to return
classification at the level of unique bags or not.}
}
\value{
a named vector of length \code{length(unique(b))} which gives the
classification for each bag.  Names come from \code{bags}.
}
\description{
Formally, this function applies \code{max()} on \code{y} for each level of \code{bags}.
}
\examples{
y <- c(1, 0, 0, 1, 1, 1, 0, 0, 0)
bags <- rep(1:3, each = 3)

classify_bags(y, bags)
classify_bags(y, bags, condense = FALSE)

# works with regular vector too
scores <- 1:9
classify_bags(scores, bags)

}
\author{
Sean Kent
}
