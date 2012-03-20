\name{data_sim}
\alias{data_sim}
\docType{data}
\title{
dataset for CPMCGLM package.
}
\description{
This simulated dataset contains 100 subjects and 8 variables. 
}
\usage{data(data_sim)}
\format{
  A data frame with 100 observations on the following 8 variables.
  \describe{
    \item{\code{Height}}{a numeric vector}
    \item{\code{Weight}}{a numeric vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Smoke}}{a binary vector}
    \item{\code{Sport}}{a numeric vector}
    \item{\code{Desease}}{a numeric vector}
    \item{\code{Parameter}}{a binary vector}
    \item{\code{Stroke}}{a categorical vector}

  }
}

\examples{
data(data_sim)
head(data_sim)
}
\keyword{datasets}
