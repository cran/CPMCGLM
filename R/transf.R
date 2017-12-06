transf<-function(quantile,continuous,boxcox,cutpoint,FP)
{
#Vecteur des transformations
if(missing(FP)){
  if (missing(cutpoint))
  {
  	if (class(quantile)=="NULL")
  	{
  		if (missing(boxcox))
  		{
  			if (continuous=="TRUE")
  			{
  			trans<-c("Original")
  			}
  			else{
  			trans<-"Coding definition is missing"
  			}
  		}
  		else{
  			if (continuous=="TRUE")
  			{
  			trans<-c("Original",paste("Boxcox",1:length(boxcox),sep=""))
  									}
  			else{
  			trans<-c(paste("Boxcox",1:length(boxcox),sep=""))
  			}
  		}
  	}
  
  	else{
  		if (missing(boxcox))
  		{
  			if (continuous=="TRUE")
  			{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""),"Original")
  			}
  			else{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""))
  			}
  		}
  		else{
  			if (continuous=="TRUE")
  			{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""),"Original",paste("Boxcox",1:length(boxcox),sep=""))
  
  			}
  			else{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Boxcox",1:length(boxcox),sep=""))
  			}
  	
  		}
  
  
  	}
  
  }
  else{
  	if (class(quantile)=="NULL")
  	{
  		if (missing(boxcox))
  		{
  			if (continuous=="TRUE")
  		
  			{
  			trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original")
  			}
  			else{
  			trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""))
  			}
  		}
  		else{
  			if (continuous=="TRUE")
  			{
  			trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original",paste("Boxcox",1:length(boxcox),sep=""))
  			}
  			else{
  			trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""),paste("Boxcox",1:length(boxcox),sep=""))
  			}
  		}
  	}
  
  	else{
  		if (missing(boxcox))
  		{
  			if (continuous=="TRUE")
  			{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original")
  			}
  			else{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""))
  			}
  		}
  		else{
  			if (continuous=="TRUE")
  			{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original",paste("Boxcox",1:length(boxcox),sep=""))
  
  			}
  			else{
  			trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""),paste("Boxcox",1:length(boxcox),sep=""))
  			}
  	
  		}
  
  
  	}
  }
}else{
  if (missing(cutpoint))
  {
    if (class(quantile)=="NULL")
    {
      if (missing(boxcox))
      {
        if (continuous=="TRUE")
        {
          trans<-c("Original",paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        else{
          trans<-c(paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
      }
      else{
        if (continuous=="TRUE")
        {
          trans<-c("Original",paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        else{
          trans<-c(paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
      }
    }
    
    else{
      if (missing(boxcox))
      {
        if (continuous=="TRUE")
        {
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),"Original",paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        else{
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
      }
      else{
        if (continuous=="TRUE")
        {
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),"Original",paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
          
        }
        else{
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        
      }
      
      
    }
    
  }
  else{
    if (class(quantile)=="NULL")
    {
      if (missing(boxcox))
      {
        if (continuous=="TRUE")
          
        {
          trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original",paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        else{
          trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
      }
      else{
        if (continuous=="TRUE")
        {
          trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original",paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        else{
          trans<-c(paste("Cutpoint",1:nrow(cutpoint),sep=""),paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
      }
    }
    
    else{
      if (missing(boxcox))
      {
        if (continuous=="TRUE")
        {
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original",paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        else{
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
      }
      else{
        if (continuous=="TRUE")
        {
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""),"Original",paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
          
        }
        else{
          trans<-c(paste("Quantile",1:nrow(quantile),sep=""),paste("Cutpoint",1:nrow(cutpoint),sep=""),paste("Boxcox",1:length(boxcox),sep=""),paste("Fractionnal Polynomial",1:nrow(FP),sep=""))
        }
        
      }
      
      
    }
    
    
  }
  
}
return(trans)
}
