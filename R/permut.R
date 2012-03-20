permut<-function(n)
{
	ind<-1:n
	a<-sample(ind,n,replace="F")
	asort <- sort(a)
	b <- sample(asort,n,replace="F")
	ind <- b
 	return(ind)
}
