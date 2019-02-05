pm=function(y,d,method="multinom",k=3,...){
  num=k
  option=method
  if(num==3){
    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    #d is the continuous marker, turn out to be the probability matrix when option="prob"
    
    #x1 is position of observations from the 1st category
    #x2 is position of observations from the 2nd category
    #x3 is position of observations from the 3rd category
    y=as.numeric(y)
    d=data.matrix(d)
    x1=which(y==1) #return the label
    x2=which(y==2)
    x3=which(y==3)
    n=length(y)
    
    #n is the sample size
    a=matrix(0,n,3);
    one1=a;
    one1[,1]=1;
    one2=a;
    one2[,2]=1;
    one3=a;
    one3[,3]=1;
    
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
    
    return(pp)
    
  }else if(num==4){
    #y is the four-category multinomial response, must be of dimension n by 4, d is the continuous marker
    #eg. try generating y=rmultinom(1,size=1,prob=cbind(1,exp(d%*%1),exp(d%*%2),exp(d%*%3))) need MASS
    
    y=as.numeric(y)
    #d=scale(d)
    d=data.matrix(d)
    n=length(y)
    #n is the sample size
    a=matrix(0,n,4);
    one1=a;
    one1[,1]=1;
    one2=a;
    one2[,2]=1;
    one3=a;
    one3[,3]=1;
    one4=a;
    one4[,4]=1;
    
    x1=which(y==1);
    x2=which(y==2);
    x3=which(y==3);
    x4=which(y==4);
    n1=sum(y==1)
    n2=sum(y==2)
    n3=sum(y==3)
    n4=sum(y==4)
    
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
    return(pp)
  }else if(num==2){
    #y is the tri-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical
    #d is the continuous marker, turn out to be the probability matrix when option="prob"
    
    #x1 is position of observations from the 1st category
    #x2 is position of observations from the 2nd category
    #x3 is position of observations from the 3rd category
    y=as.numeric(y)
    d=data.matrix(d)
    x1=which(y==1) #return the label
    x2=which(y==2)
    n=length(y)
    
    #n is the sample size
    a=matrix(0,n,2);
    one1=a;
    one1[,1]=1;
    one2=a;
    one2[,2]=1;
    
    
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
        #   return(NULL)
      }
      pp=d
    }
    
    
    
    return(pp)
  }

}
