PF <- function(FP,varcod){
  coup5 <- NULL
  X5 <- NULL
  if (min(varcod)<0){
    varcod <- varcod - floor(min(varcod))
  }
  if (missing(FP)){
    XFP <- NULL
    coup5 <- NULL
  } else {
    XFP <-array(dim=c(length(varcod),ncol(FP),nrow(FP)))
    for (k in 1:nrow(FP)){
        coup5 <- c(coup5,length(na.omit(FP[k,])))
        for(j in 1:length(na.omit(FP[k,]))){
            for(i in 1:length(varcod)){
                if(j==1){
                    XFP[i,j,k]<-varcod[i]^(FP[k,j])
                } else {
                    if(FP[k,j]!=FP[k,j-1]){
                        XFP[i,j,k]<-varcod[i]^(FP[k,j])
                    } else {
                        XFP[i,j,k] <- XFP[i,j-1,k]*log(varcod[i])
                    }
                }
            }
        }
    }
  }
  res <- list(X5=XFP, coup5=coup5)
  
}



