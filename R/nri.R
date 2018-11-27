nri=function(y,m1,m2,method="multinom",k=3,...){
  if (method!="prob"&method!="label"){
    ccp1=ccp(y=y,d=m1,method=method,k=k,...)
    ccp2=ccp(y=y,d=cbind(m1,m2),method=method,k=k,...)
  }else{
    ccp1=ccp(y=y,d=m1,method=method,k=k,...)
    ccp2=ccp(y=y,d=m2,method=method,k=k,...)
  }
  return(ccp2-ccp1)
  
}

