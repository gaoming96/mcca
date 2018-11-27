rsq=function(y,d,method="multinom",k=3,...){
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
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...,probability = T)
      predict.test <- predict(fit,d,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df[c("X1","X2","X3")]
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df
      
    }else if(option=="prob"){
      pp_sum <- apply(d,1,sum)
      a <- pp_sum<0.999 | pp_sum>1.001
      b <- sum(a)
      if (b!=0){
        cat("ERROR: The input value \"d\" should be a probability matrix.")
        return(NULL)
      }
      pp=d
    }
    
    pv=pp
    rsq=(
      stats::var(pv[,1])/(ro1*(1-ro1))+
        stats::var(pv[,2])/(ro2*(1-ro2))+
        stats::var(pv[,3])/(ro3*(1-ro3))
    )/3/nn*(nn-1)
    return(rsq)
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
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...,probability = T)
      predict.test <- predict(fit,d,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df[c("X1","X2","X3","X4")]
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df
      
    }else if(option=="prob"){
      pp_sum <- apply(d,1,sum)
      a <- pp_sum<0.999 | pp_sum>1.001
      b <- sum(a)
      if (b!=0){
        cat("ERROR: The input value \"d\" should be a probability matrix.")
        return(NULL)
      }
      pp=d
    }
    
    pv=pp
    rsq=(stats::var(pv[,1])/(ro1*(1-ro1))+stats::var(pv[,2])/(ro2*(1-ro2))+stats::var(pv[,3])/(ro3*(1-ro3))+stats::var(pv[,4])/(ro4*(1-ro4)))/4/nn*(nn-1)
    return(rsq)
  }else if(num==2){
    
    #y is the quandr-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
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
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
      pp <- data.frame(1-pp,pp)
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,...)
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,...,probability = T)
      predict.test <- predict(fit,d,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df[c("X1","X2")]
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d,...)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df
      
    }else if(option=="prob"){
      pp_sum <- apply(d,1,sum)
      a <- pp_sum<0.999 | pp_sum>1.001
      b <- sum(a)
      if (b!=0){
        cat("ERROR: The input value \"d\" should be a probability matrix.")
        return(NULL)
      }
      pp=d
    }
    
    pv=pp
    rsq=(stats::var(pv[,1])/(ro1*(1-ro1))+stats::var(pv[,2])/(ro2*(1-ro2)))/2/nn*(nn-1)
    return(rsq)
  }
}
