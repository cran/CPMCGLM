bestcod<-function(quantile,continuous,boxcox,cutpoint,PF,pval.naive){
posminp<-which.min(pval.naive)
if(missing(PF)){
  if (missing(cutpoint)){
  	if (class(quantile)=="NULL"){
  				if (missing(boxcox)){
  						if (continuous=="TRUE"){
  						  transf<-"original continuous variable"
  						}else{
  						  transf<-"Coding definition is missing"}
  				}else{
  						if (continuous=="TRUE"){ 
  						  if(posminp==1){
  						    transf<-"original continuous variable"}
  						  if(posminp>1){
  						    transf<-boxcox[posminp-1] }
  						}else{
  						  transf<-boxcox[posminp]
  						}
  					}
  	}else{
  					if (missing(boxcox)){
  						if (continuous=="TRUE"){
  						  if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
  					  	if(posminp>nrow(quantile)){transf<-"original continuous variable"}
  						}else{
  						transf<-quantile[posminp,]}
  					}else{ 
  					  if (continuous=="TRUE") {
  						  if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
  						  if(posminp==nrow(quantile)+1){transf<-"original continuous variable"}
  						  if(posminp>nrow(quantile)+1){transf<-boxcox[posminp-nrow(quantile)+1]}
  					  }else{
  						if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
  						if(posminp>nrow(quantile)){transf<-boxcox[posminp-nrow(quantile)]}
  					  }
  					}
        }
  }else{
  	if (class(quantile)=="NULL"){
  					if (missing(boxcox)){
  						if (continuous=="TRUE"){
  						  if(posminp<nrow(cutpoint)+1){transf<-cutpoint[posminp,]}
  						  if(posminp>nrow(cutpoint)){transf<-"original continuous variable"}
              }else{
  						transf<-cutpoint[posminp,]
  						}
  					}else{
  						if (continuous=="TRUE"){ 
  						  if(posminp<nrow(cutpoint)+1){transf<-cutpoint[posminp,]}
  						  if(posminp==nrow(cutpoint)+1){transf<-"original continuous variable"}
  						  if(posminp>nrow(cutpoint)+1){transf<-boxcox[posminp-nrow(cutpoint)+1]}
  						}else{
  						  if(posminp<nrow(cutpoint)+1){transf<-cutpoint[posminp,]}
  						  if(posminp>nrow(cutpoint)){transf<-boxcox[posminp-nrow(quantile)]}
              }
  					}
  	}else{
  					if (missing(boxcox)){
  						if(continuous=="TRUE"){
  						  if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
  						  if( (posminp>nrow(quantile)) & (posminp<nrow(quantile)+nrow(cutpoint+1)) ){transf<-cutpoint[posminp-nrow(quantile),]}
  						  if(posminp==(nrow(quantile)+nrow(cutpoint)+1)){transf<-"original continuous variable"}
  						}else{
  						  if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
  						  if((posminp>nrow(quantile)) & (posminp<nrow(quantile)+nrow(cutpoint+1)) ){transf<-cutpoint[posminp-nrow(quantile),]}
  						}
  					}else{
  						if (continuous=="TRUE"){
  						  if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
  						  if( (posminp>nrow(quantile)) & (posminp<nrow(quantile)+nrow(cutpoint+1)) ){transf<-cutpoint[posminp-nrow(quantile),]}
  						  if(posminp==(nrow(quantile)+nrow(cutpoint)+1)){transf<-"original continuous variable"}
  						  if(posminp>(nrow(quantile)+nrow(cutpoint)+1)){transf<-boxcox[posminp-(nrow(quantile)+nrow(cutpoint)+1)]}
  						}else{
  						  if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
  						  if( (posminp>nrow(quantile)) & (posminp<nrow(quantile)+nrow(cutpoint+1)) ){transf<-cutpoint[posminp-nrow(quantile),]}
  						  if(posminp>(nrow(quantile)+nrow(cutpoint))){transf<-boxcox[posminp-(nrow(quantile)+nrow(cutpoint))]}
  						}
  					}
  	}
  }
}else{
  if (missing(cutpoint)){
    if (class(quantile)=="NULL"){
      if (missing(boxcox)){
        if (continuous=="TRUE"){
          if(posminp==1){transf<-"original continuous variable"}
          if(posminp>1){transf <- PF[posminp-1,]}
        }else{
          transf<-transf <- PF[posminp,]}
      }else{
        if (continuous=="TRUE"){ 
          if(posminp==1){transf<-"original continuous variable"}
          if((posminp>1)&(posminp<length(boxcox)+2)){transf<-boxcox[posminp-1]}
          if(posminp>length(boxcox)+1){transf<-PF[posminp-(length(boxcox)+1),]}
        }else{
          if(posminp<length(boxcox)+1){transf<-boxcox[posminp]}
          if(posminp>length(boxcox)){transf<-PF[posminp-(length(boxcox)),]}
        }
      }
    }else{
      if (missing(boxcox)){
        if (continuous=="TRUE"){
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if(posminp==nrow(quantile)+1){transf<-"original continuous variable"}
          if(posminp>nrow(quantile)+1){transf<-PF[posminp-nrow(quantile)+1,]}
        }else{
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if(posminp>nrow(quantile)){transf<-PF[posminp-nrow(quantile),]}}
      }else{ 
        if (continuous=="TRUE") {
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if(posminp==nrow(quantile)+1){transf<-"original continuous variable"}
          if((posminp>nrow(quantile)+1)&(posminp<(nrow(quantile)+length(boxcox)+2))){transf<-boxcox[posminp-(nrow(quantile)+1)]}
          if(posminp>(nrow(quantile)+length(boxcox)+1)){transf<-PF[posminp-(nrow(quantile)+length(boxcox)+1),]}
        }else{
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if((posminp>nrow(quantile))&(posminp<(nrow(quantile)+length(boxcox)+1))){transf<-boxcox[posminp-(nrow(quantile))]}
             if(posminp>(nrow(quantile)+length(boxcox))){transf<-PF[posminp-(nrow(quantile)+length(boxcox)),]}
        }
      }
    }
  }else{ 
    if (class(quantile)=="NULL"){
      if (missing(boxcox)){
        if (continuous=="TRUE"){
          if(posminp<nrow(cutpoint)+1){transf<-cutpoint[posminp,]}
          if(posminp==nrow(cutpoint)+1){transf<-"original continuous variable"}
          if(posminp>nrow(cutpoint)+1){transf<-PF[posminp-nrow(cutpoint)+1,]}
        }else{
          if(posminp<nrow(cutpoint)+1){transf<-cutpoint[posminp,]}
          if(posminp>nrow(cutpoint)){transf<-PF[posminp-nrow(cutpoint),]}}
      }else{ 
        if (continuous=="TRUE"){ 
          if(posminp<nrow(cutpoint)+1){transf<-cutpoint[posminp,]}
          if(posminp==nrow(cutpoint)+1){transf<-"original continuous variable"}
          if((posminp>nrow(cutpoint)+1)&(posminp<nrow(cutpoint)+1+length(boxcox)+1)){transf <- boxcox[posminp-(nrow(cutpoint)+1)]}
          if(posminp>(nrow(cutpoint)+1+length(boxcox))){transf<-PF[posminp-(nrow(cutpoint)+1+length(boxcox)),]} 
        }else{
          if(posminp<nrow(cutpoint)+1){transf<-cutpoint[posminp,]}
          if((posminp>nrow(cutpoint))&(posminp<nrow(cutpoint)+length(boxcox)+1)){transf <- boxcox[posminp-(nrow(cutpoint))]}
          if(posminp>nrow(cutpoint)+length(boxcox)){transf<-PF[posminp-(nrow(cutpoint)+length(boxcox)),]} 
        }
      }
    }else{ 
      if (missing(boxcox)){
        if (continuous=="TRUE"){ 
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if((posminp>nrow(quantile))&(posminp<nrow(cutpoint)+nrow(quantile)+1)){transf<-cutpoint[posminp-nrow(quantile),]}
          if(posminp==nrow(cutpoint)+nrow(quantile)+1){transf<-"original continuous variable"}
          if(posminp>nrow(cutpoint)+nrow(quantile)+1){transf<-PF[posminp-(nrow(quantile)+nrow(cutpoint)+1),]}
        }else{
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if((posminp>nrow(quantile))&(posminp<nrow(cutpoint)+nrow(quantile)+1)){transf<-cutpoint[posminp-nrow(quantile),]}
          if(posminp>nrow(cutpoint)+nrow(quantile)){transf<-PF[posminp-(nrow(quantile)+nrow(cutpoint)),]}
        }
      }else{ 
        if (continuous=="TRUE"){
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if( (posminp>nrow(quantile)) & (posminp<nrow(quantile)+nrow(cutpoint+1)) ){transf<-cutpoint[posminp-nrow(quantile),]}
          if(posminp==(nrow(quantile)+nrow(cutpoint)+1)){transf<-"original continuous variable"}
          if((posminp>(nrow(quantile)+nrow(cutpoint)+1))&(posminp<(nrow(quantile)+nrow(cutpoint)+length(boxcox)+2))){transf<-boxcox[posminp-(nrow(quantile)+nrow(cutpoint)+1)]}
          if(posminp>(nrow(quantile)+nrow(cutpoint)+length(boxcox)+1)){transf<-PF[posminp-(nrow(quantile)+nrow(cutpoint)+1+length(boxcox)),]}
        }else{
          if(posminp<nrow(quantile)+1){transf<-quantile[posminp,]}
          if( (posminp>nrow(quantile)) & (posminp<nrow(quantile)+nrow(cutpoint+1)) ){transf<-cutpoint[posminp-nrow(quantile),]}
          if((posminp>(nrow(quantile)+nrow(cutpoint)))&(posminp<(nrow(quantile)+nrow(cutpoint)+length(boxcox)+1))){transf<-boxcox[posminp-(nrow(quantile)+nrow(cutpoint))]}
          if(posminp>(nrow(quantile)+nrow(cutpoint)+length(boxcox))){transf<-PF[posminp-(nrow(quantile)+nrow(cutpoint)+length(boxcox)),]}
        }
      }
    }
  }
  
}
return(transf)
}
