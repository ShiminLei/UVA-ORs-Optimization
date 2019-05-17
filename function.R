# get the overlap value of interval (low1,up1) and (low2,up2)
inter <- function(low1,up1,low2,up2,x){
  for(i in 1:length(low1)){
    if((low1[i]>up2[i])|(low2[i]>up1[i]))
      x[i]=0
    else if(up1[i]>=up2[i])
    {
      if(low1[i]>low2[i])
        x[i]=(up2[i]-low1[i])
      else
        x[i]=(up2[i]-low2[i])
    }
    else if(up2[i]>up1[i])
    {
      if(low2[i]>low1[i])
        x[i]=(up1[i]-low2[i])
      else
        x[i]=(up1[i]-low1[i])
    }
  }
  return (x)
}


# change the time into numeric (which can be calculated)
change <- function(x){
  for(i in 1:length(x)){
    if(nchar(x[i])==5)
      (x[i]=as.numeric(substr(x[i],1,2))+(as.numeric(substr(x[i],4,5)))/60) 
    
    else if(nchar(x[i])==4)
      ( x[i]=as.numeric(substr(x[i],1,1))+(as.numeric(substr(x[i],3,4)))/60)
  }
  return (as.numeric(x))
}