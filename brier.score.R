## Brier Score for multiclass classification evaluation
## 
## ARGUMENTS: 
## df    a data frame
## 
## Each column of the df has the probability associated to each class.
## The last column contains the observed variable


brier.score <- function(df){
  s <- ncol(df)
  BS <- c()
  for(j in 1:nrow(df)){
    
    Obs <- rep(0,s-1)
    
    for(i in 1:(s-1)){
      if(colnames(df[i])==df[j,s]){
        Obs[i]=1
      }
    }
    
    BS[j] = sum((df[j,1:(s-1)] - Obs)^2)
  }
  return(mean(BS))
}
