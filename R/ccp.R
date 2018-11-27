ccp=function(y,d,method="multinom",k=3,...){
  num=k
  option=method
  if(num==3){
    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    #d is the continuous marker, turn out to be the probability matrix when option="prob"
    
    y=as.numeric(y)
    d=data.matrix(d)
    
    n1=sum(y==1)
    n2=sum(y==2)
    n3=sum(y==3)
    nn=n1+n2+n3
    ro1=n1/nn
    ro2=n2/nn
    ro3=n3/nn
    
    #define the id
    if(option=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~d,...)
      pv=predict(fit,type='class')
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      pv <- predict(fit,type="class")
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...)
      pv <- predict(fit)
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.fit <- predict(fit)
      pv <- predict.test.fit$class
      
      
    }else if(option=="label"){
      temp <- sum(d==1|d==2|d==3)
      if (temp!=length(d)){
        cat("ERROR: The input value \"d\" should be a tri-nomial label.")
        return(NULL)
      }
      pv=d
    }else if(option=="prob"){
      l=max.col(d)
      for(i in 1:nrow(d)){
        if (length(which(max(d[i,])==d[i,]))>1){
          cat("WARNING: there exists two same max probability in one sample.\n")
          break
        }
      }
      pv=l
    }
    
    ccp=(sum(pv==1 & y==1)+ sum(pv==2 & y==2)+sum(pv==3 & y==3))/nn
    return(ccp)
  }else if(num==4){
    #y is the quandr-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    #d is the continuous marker, turn out to be the probability matrix when option="prob"
    
    y=as.numeric(y)
    d=data.matrix(d)
    
    n1=sum(y==1)
    n2=sum(y==2)
    n3=sum(y==3)
    n4=sum(y==4)
    nn=n1+n2+n3+n4
    ro1=n1/nn
    ro2=n2/nn
    ro3=n3/nn
    ro4=n4/nn
    
    #define the id
    if(option=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~d,...)
      pv=predict(fit,type='class')
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      pv <- predict(fit,type="class")
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...)
      pv <- predict(fit)
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.fit <- predict(fit)
      pv <- predict.test.fit$class
      
      
    }else if(option=="label"){
      temp <- sum(d==1|d==2|d==3|d==4)
      if (temp!=length(d)){
        cat("ERROR: The input value \"d\" should be a quandr-nomial label.")
        return(NULL)
      }
      pv=d
    }else if(option=="prob"){
      l=max.col(d)
      for(i in 1:nrow(d)){
        if (length(which(max(d[i,])==d[i,]))>1){
          cat("WARNING: there exists two same max probability in one sample.\n")
          break
        }
      }
      pv=l
    }
    
    ccp=(sum(pv==1 & y==1)+ sum(pv==2 & y==2)+sum(pv==3 & y==3)+sum(pv==4 & y==4))/nn
    return(ccp)
  }else if(num==2){
    
    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    #d is the continuous marker, turn out to be the probability matrix when option="prob"
    
    y=as.numeric(y)
    d=data.matrix(d)
    
    n1=sum(y==1)
    n2=sum(y==2)
    
    nn=n1+n2
    ro1=n1/nn
    ro2=n2/nn
    
    
    #define the id
    if(option=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~d,...)
      pv=predict(fit,type='class')
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      pv <- predict(fit,type="class")
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...)
      pv <- predict(fit)
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.fit <- predict(fit)
      pv <- predict.test.fit$class
      
      
    }else if(option=="label"){
      temp <- sum(d==1|d==2)
      if (temp!=length(d)){
        cat("ERROR: The input value \"d\" should be a bi-nomial label.")
        return(NULL)
      }
      pv=d
    }else if(option=="prob"){
      l=max.col(d)
      for(i in 1:nrow(d)){
        if (length(which(max(d[i,])==d[i,]))>1){
          cat("WARNING: there exists two same max probability in one sample.\n")
          break
        }
      }
      pv=l
    }
    
    ccp=(sum(pv==1 & y==1)+ sum(pv==2 & y==2))/nn
    return(ccp)
  }
}
