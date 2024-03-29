% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mildsvm-package.R
\docType{package}
\name{mildsvm-package}
\alias{mildsvm}
\alias{mildsvm-package}
\title{mildsvm: Multiple-Instance Learning with Support Vector Machines}
\description{
Weakly supervised (WS), multiple instance (MI) data lives in numerous interesting applications such as drug discovery, object detection, and tumor prediction on whole slide images. The 'mildsvm' package provides an easy way to learn from this data by training Support Vector Machine (SVM)-based classifiers. It also contains helpful functions for building and printing multiple instance data frames. The core methods from 'mildsvm' come from the following references: Kent and Yu (2022) <arXiv:2206.14704>; Xiao, Liu, and Hao (2018) <doi:10.1109/TNNLS.2017.2766164>; Muandet et al. (2012) <https://proceedings.neurips.cc/paper/2012/file/9bf31c7ff062936a96d3c8bd1f8f2ff3-Paper.pdf>; Chu and Keerthi (2007) <doi:10.1162/neco.2007.19.3.792>; and Andrews et al. (2003) <https://papers.nips.cc/paper/2232-support-vector-machines-for-multiple-instance-learning.pdf>. Many functions use the 'Gurobi' optimization back-end to improve the optimization problem speed; the 'gurobi' R package and associated software can be downloaded from <https://www.gurobi.com> after obtaining a license.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/skent259/mildsvm}
  \item Report bugs at \url{https://github.com/skent259/mildsvm/issues}
}

}
\author{
\strong{Maintainer}: Sean Kent \email{skent259@gmail.com} (\href{https://orcid.org/0000-0001-8697-9069}{ORCID})

Authors:
\itemize{
  \item Yifei Liou
}

}
\keyword{internal}
