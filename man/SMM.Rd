% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smm.R
\name{smm}
\alias{smm}
\alias{smm.default}
\alias{smm.formula}
\alias{smm.mild_df}
\title{Fit SMM model to the data}
\usage{
\method{smm}{default}(
  x,
  y,
  instances,
  cost = 1,
  weights = TRUE,
  control = list(kernel = "radial", sigma = if (is.vector(x)) 1 else 1/ncol(x), scale =
    TRUE),
  ...
)

\method{smm}{formula}(formula, data, instances = "instance_name", ...)

\method{smm}{mild_df}(x, ...)
}
\arguments{
\item{x}{A data.frame, matrix, or similar object of covariates, where each
row represents a sample. If a \code{mild_df} object is passed, \verb{y, instances}
are automatically extracted, \code{bags} is ignored, and all other columns will
be used as predictors.}

\item{y}{A numeric, character, or factor vector of bag labels for each
instance.  Must satisfy \code{length(y) == nrow(x)}. Suggest that one of the
levels is 1, '1', or TRUE, which becomes the positive class; otherwise, a
positive class is chosen and a message will be supplied.}

\item{instances}{A vector specifying which samples belong to each instance.
Can be a string, numeric, of factor.}

\item{cost}{The cost parameter in SVM, fed to the \code{C} argument in
\code{kernlab::ksvm()}.}

\item{weights}{named vector, or \code{TRUE}, to control the weight of the cost
parameter for each possible y value.  Weights multiply against the cost
vector. If \code{TRUE}, weights are calculated based on inverse counts of
instances with given label, where we only count one positive instance per
bag. Otherwise, names must match the levels of \code{y}.}

\item{control}{A list of additional parameters passed to the method that
control computation with the following components:
\itemize{
\item \code{kernel} either a character the describes the kernel ('linear' or
'radial') or a kernel matrix at the instance level.
\item \code{sigma} argument needed for radial basis kernel.
\item \code{scale} argument used for all methods. A logical for whether to rescale
the input before fitting.
}}

\item{...}{Arguments passed to or from other methods.}

\item{formula}{A formula with specification \code{y ~ x}. This argument is an
alternative to the \code{x}, \code{y} arguments, but requires the \code{data} and
\code{instances} argument. See examples.}

\item{data}{If \code{formula} is provided, a data.frame or similar from which
formula elements will be extracted.}
}
\value{
An object of class \code{smm}  The object contains at least the
following components:
\itemize{
\item \code{ksvm_fit}: A fit of class \code{ksvm} from the kernlab package.
\item \code{call_type}: A character indicating which method \code{smm()} was called with.
\item \code{x}: The training data needed for computing the kernel matrix in
prediction.
\item \code{features}: The names of features used in training.
\item \code{levels}: The levels of \code{y} that are recorded for future prediction.
\item \code{cost}: The cost parameter from function inputs.
\item \code{sigma}: The radial basis function kernel parameter.
\item \code{weights}: The calculated weights on the \code{cost} parameter, if applicable.
\item \code{x_scale}: If \code{scale = TRUE}, the scaling parameters for new predictions.
}
}
\description{
Function to carry out support measure machines algorithm which is appropriate
for multiple instance learning. The algorithm calculates the kernel matrix of
different empirical measures using kernel mean embedding. The data set should
be passed in with rows corresponding to samples from a set of instances.  SMM
will compute a kernel on the instances and pass that to \code{kernlab::ksvm()} to
train the appropriate SVM model.
}
\section{Methods (by class)}{
\itemize{
\item \code{default}: Method for data.frame-like objects

\item \code{formula}: Method for passing formula

\item \code{mild_df}: Method for \code{mild_df} objects. Use the \code{bag_label} as \code{y} at
the instance level, then perform \code{smm()} ignoring the MIL structure.
}}

\examples{
set.seed(8)
n_instances <- 10
n_samples <- 20
y <- rep(c(1, -1), each = n_samples * n_instances / 2)
instances <- as.character(rep(1:n_instances, each = n_samples))
x <- data.frame(x1 = rnorm(length(y), mean = 1*(y==1)),
                x2 = rnorm(length(y), mean = 2*(y==1)),
                x3 = rnorm(length(y), mean = 3*(y==1)))

df <- data.frame(instance_name = instances, y = y, x)

mdl <- smm(x, y, instances)
mdl2 <- smm(y ~ ., data = df)

# instance level predictions
suppressWarnings(library(dplyr))
df \%>\%
  dplyr::bind_cols(predict(mdl, type = "raw", new_data = x, new_instances = instances)) \%>\%
  dplyr::bind_cols(predict(mdl, type = "class", new_data = x, new_instances = instances)) \%>\%
  dplyr::distinct(instance_name, y, .pred, .pred_class)

}
\references{
Muandet, K., Fukumizu, K., Dinuzzo, F., & Schölkopf, B. (2012).
Learning from distributions via support measure machines. \emph{Advances in
neural information processing systems}, \emph{25}.
}
\seealso{
\code{\link[=predict.smm]{predict.smm()}} for prediction on new data.
}
\author{
Sean Kent, Yifei Liu
}
