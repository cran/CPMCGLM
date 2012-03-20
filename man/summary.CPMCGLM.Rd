\name{summary.CPMCGLM}
\alias{summary.CPMCGLM}

\title{
Short summary of a \code{CPMCGLM} object
}
\description{
The function provides the summary of a \code{CPMCGLM} correction of pvalue.
}
\usage{
\method{summary}{CPMCGLM}(object,\dots)
}

\arguments{
  \item{object}{
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
\code{\link{CPMCGLM}},\code{\link{print.CPMCGLM}}
}
\examples{
\dontrun{
# load data
data(data_sim)

#Linear Gaussian Model
fit1 <- CPMCGLM(formula= Weight~Age+as.factor(Sport)+Desease+Height,
family="gaussian",link="identity",data=data_sim,varcod="Age",N=1000,
boxcox=c(0,1,2,3))
### summary fit1
summary(fit1)
}
}
\keyword{summary}

