\name{print.CPMCGLM}
\alias{print.CPMCGLM}

\title{
Output of a \code{CPMCGLM} object
}
\description{
The function provides the output of a \code{CPMCGLM} correction of pvalue.
}
\usage{
\method{print}{CPMCGLM}(x,\dots)
}

\arguments{
  \item{x}{
an object inheriting from classes \code{CPMCGLM}.   
}
  \item{\dots}{
other parameters.   
}
}
\author{
J.Riou, A.Diakite, and B.Liquet
}
\seealso{
\code{\link{CPMCGLM}},\code{\link{summary.CPMCGLM}}
}
\examples{
# load data
\dontrun{
data(data_sim)

#Linear Gaussian Model
fit1 <- CPMCGLM(formula= Weight~Age+as.factor(Sport)+Desease+Height,
family="gaussian",link="identity",data=data_sim,varcod="Age",N=1000,
boxcox=c(0,1,2,3))
### print fit1
fit1
}
}
\keyword{print}

