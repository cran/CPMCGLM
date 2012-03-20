\name{CPMCGLM}
\alias{CPMCGLM}
\title{
Correction of the significance level after multiple coding of an explanatory variable in generalized linear model.
}
\description{
We propose to determine the correction of the significance level after multiple
coding of an explanatory variable in Generalized Linear Model. The possible codings
are: Box-Cox, dichotomous and categorical transformations. The different methods of
correction of the p-value are the Single step Bonferroni procedure, and resampling
based methods(permutation and the parametric bootstrap procedure). If only some 
continuous, and dichotomous transformations are performed this package offers an 
exact correction of the p-value developped by B.Liquet & D.Commenges in 2005.
The naive method with no correction is also available.
}

\usage{
CPMCGLM(formula, family, link, data, varcod, dicho, nb.dicho, categ, 
nb.categ, boxcox, nboxcox, N=1000, cutpoint)
}

\arguments{
  \item{formula}{
   an object of class "formula" : a symbolic description of the model to be fitted. The details of model specification are given under 'Details'.
}
  \item{family}{
a description of the error distribution to be used in the model. This should be a character string naming a family function. The possible family functions are: "binomial", "gaussian", and "poisson".
}
  \item{link}{
a description of the link function to be used in the model. This needs to be a character string naming a link function. For the "gaussian" family, you must use the "identity" link.
For the "binomial" family function, "logit" and "probit" link are available. And for "Poisson" family function, you must use the "log" link function.
}
  \item{data}{
a data frame containing the variables of the model.
}
  \item{varcod}{
a continuous variable that you want to transform.
}
  \item{dicho}{
a vector with the order of the quantile which are used for computing the cutpoint of each dichotomous transformation. The length of the vector corresponds to the number of transformation. If you specify this argument, "nb.dicho" must not be present.
}
  \item{nb.dicho}{
if you do not enter the "dicho" argument, you can enter the number of dichotomous transformations that you want. The strategy of coding is presented in "Details" section.
}
  \item{categ}{
a matrix with the order of quantile which are used for computing the categorical cutpoints of each transformation. The details of the "categ" specification are given under "Details". If you specify this argument, "nb.categ" must not be present.
}
  \item{nb.categ}{
if you do not enter the "categ" argument, you can enter the number of categorical transformations that you want. The strategy of coding is presented in "Details" section.
}
  \item{boxcox}{
a vector of \eqn{\lambda} parameters corresponding to each BoxCox transformation. The BoxCox transformation is explained in "Details" section.
}
  \item{nboxcox}{
if you do not enter the "boxcox" argument, you can enter the number of boxcox transformations that you want. The maximum number of transformations that you can enter is 5. For the strategy of coding, it seems natural to try the crude variable (\eqn{\lambda}1=1) and  the log transformation. When two transformations are condidered we
propose \eqn{\lambda}1 =1 and  \eqn{\lambda}2 =0. Further interesting transformations 
include the square \eqn{\lambda}3 =2 and the square root \eqn{\lambda}4 =0.5. Finally, the power \eqn{\lambda}5 =3/2 may be tried.
}
  \item{N}{
the number of resampling that you want to do.
}
  \item{cutpoint}{
a matrix with the different numeric values for the cutpoints. The details of the cutpoint specification are given under 'Details'.
}
}

\details{
- formula: A typical predictor has the form "response ~ terms" where "response" is the numeric response (possibly binary "0","1") vector and "terms" is a serie of terms which specifies a linear predictor for response.

- nb.dicho: Dichotomous transformations include only the categorical transformations in two classes. The most natural method is to use a transformation based on the quantile. For one transformation, the median is used as a cutpoint for the dichotomous coding. 
For two transformations, the first tercile is used for the first dichotomous transformation, and the second tercile for the second one, and so on. 

- categ: The categ argument needs to be a matrix. You need to have one line per transformation. Therefore, the dimension of the matrix is  nbq \eqn{\times} maxq, where nbq is the number of transformations tried with the categ
transformations, and maxq is the maximum of number of quantiles that is used in one quantile transformation. 
For example: 
\tabular{rllll}{
         [1,] \tab 0.33 \tab 0.66 \tab NA \tab NA\cr
         [2,] \tab 0.25 \tab 0.5 \tab 0.75\tab NA\cr
	       [3,] \tab 0.2 \tab 0.4 \tab 0.6 \tab 0.8 \cr   
       }       
In this example, three transformations are performed, so nbq=3. And maxq=4, because the maximum of number of quantiles that we used for the quintiles is 4. The first transformation leads to 
a categorical transformation in three classes, with cutpoints at the first and the second tercile. The second transformation allows to obtain a categorical variable in four classes with cutpoints
at the quartile. And the third one allows to obtain a variable in five classes with the cutpoints at the quintiles.


- nb.categ: This concerns categorical transformations in more than two classes. Considering one of these transformations, the most intuitive method is to use a transformation in three classes at the tercile. For two of such transformations, we added the previous coding and a categorical transformation in four classes based on the quartile, and so on.


- boxcox: The BoxCox transformation \eqn{X(k)} of \eqn{X} is defined as follows:

\eqn{ X(k)= \{
   \begin{array} {ll}
   \lambda_{k}^{-1}(X^{\lambda_{k}}-1),\quad $if$ \ \lambda_{k} > 0;\\
    \log{X},\quad $if$ \ \lambda_{k} =0.
   \end{array}   
}

- cutpoint: The cutpoint argument needs to be a matrix. The form of this matrix is similar as one of the quantile matrix. The number of rows corresponds to the number of tranformations (nbc)
tried with this method, and the number of columns corresponds to the maximum of cutpoints (maxc) that is used in one transformation. 
For example:  
\tabular{rllll}{
         [1,] \tab 8 \tab 16 \tab NA \tab NA\cr
         [2,] \tab 6 \tab 12 \tab 18\tab NA\cr
	       [3,] \tab 5 \tab 10 \tab 15 \tab 20 \cr
       }
In this example, one wants to perform three transformations, hence the three rows. The first transformation leads to a categorical variable in three classes, with two cutpoints for the value "8",
and the value "16". The second transformation allows to obtain a categorical transformation in four classes, with cutpoints for values: "6","12" and "18". The last transformation tried allows to obtain
a categorical transformation in five classes with cutpoint for values: "5","10","15", and "20". 
Therefore, we used four columns because four is the maximum of cutpoints used, in the third transformation.
 }
\value{
\item{call}{the code used for the model.}
\item{n}{the number of subjects in the dataset.}
\item{N}{the number of resampling.}
\item{family}{the family function used.}
\item{link}{the link function used.}
\item{nbt}{the number of score tests realised.}
\item{nbb}{the number of score tests realised with BoxCox transformation.}
\item{nbq}{the number of score tests realised with Quantile transformation.}
\item{nbc}{the number of score tests realised with Cutpoint transformation.}
\item{vq}{the vector quantiles' values for the best coding.}
\item{adj}{the number of adjustment variables.}
\item{trans}{the method of transformation for each coding.}
\item{BC}{the method of the best transformation: "Dichotomous", "Categorical", "Boxcox", "Continuous","Cutpoint".}
\item{bestcod}{the corresponding value of the transformation parameter for the best transformation.}
\item{naive.pvalue}{the Pvalue of the best association without correction.}
\item{exact.pvalue}{the adjusted Pvalue of the best association with an exact correction.}
\item{bonferroni.adjusted.pvalue}{the adjusted Pvalue of the best association with the Bonferroni correction.}
\item{parametric.bootstrap.adjusted.pvalue}{the adjusted Pvalue of the best association with the parametric bootstrap correction.}
\item{permutation.adjusted.pvalue}{the adjusted Pvalue of the best association with the permutation correction.}
}
\references{

Liquet, B. and Riou, J. (2012). Correction of significance level after multiple coding in the Generalized 
Linear Model. [Submitted].

Liquet, B. and Commenges, D. (2005). Computation of the p-value of the minimum of score tests in the
generalized linear model, application to multiple coding. \emph{Statistics & Probability Letters}, 71:33-38.

Liquet, B. and Commenges, D. (2001). Correction of the p-value after multiple coding of anexplanatory
variable in logistic regression. \emph{Statistics in Medicine}, 20:2815-2826.

Westfall, P. H. and Young, S. (1992). Resampling-based multiple testing: examples and methods for pvalue
adjustment. Wiley Series in Probability and Mathematical Statistics. \emph{Applied Probability and
Statistics}. New York, NY: Wiley. xvii, 340 p.

Yu, K., Liang, F., Ciampa, J., and Chatterjee, N. (2011). Efficient p-value evaluation for resampling-based
tests. \emph{Biostatistics}, 12(3):582-593.
}
\author{
J.Riou, A.Diakite, and B.Liquet
}

\seealso{
\code{\link{print.CPMCGLM}}, \code{\link{summary.CPMCGLM}}
}
\examples{
\dontrun{
# load data
data(data_sim)
#
#Example of quantile matrix definition
	
#Linear Gaussian Model

fit1 <- CPMCGLM(formula= Weight~Age+as.factor(Sport)+Desease+Height,
family="gaussian",link="identity",data=data_sim,varcod="Age",N=1000,
boxcox=c(0,1,2,3),nb.dicho=3,nb.categ=4)
### print fit1
fit1
### summary fit1
summary(fit1)

#Loglinear Poisson Model
fit2 <- CPMCGLM(formula= Stroke~Age+as.factor(Sport)+Height+Weight,
family="poisson",link="log",data=data_sim,varcod="Age",N=1000,
boxcox=c(0,1,2,3))

### print fit2
fit2 
### summary fit2
summary(fit2)

#Logit Model
fit3 <- CPMCGLM(formula= Parameter~Age+as.factor(Sport)+Height+Weight,
family="binomial",link="logit",data=data_sim,varcod="Age",N=1000,
boxcox=c(0,1,2,3),nb.dicho=3)
### print fit3
fit3 
### summary fit3
summary(fit3)

#Probit Model
fit4 <- CPMCGLM(formula= Parameter~Age+as.factor(Sport)+Height+Weight,
family="binomial",link="probit",data=data_sim,varcod="Age",N=1000,
nboxcox=2,nb.categ=4)
### print fit4
fit4 
### summary fit4
summary(fit4)
}
}
