rsq=function(y,d,method="multinom",k=3){
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
    fit <- nnet::multinom(y~d,maxit = 1000,MaxNWts = 2000,trace=F)
    predict.test.probs <- predict(fit,type='probs')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pp=predict.test.df
  }else if(option=="tree"){
    #require(rpart)
    y <- as.factor(y)
    fit <- rpart::rpart(y~d)
    predict.test.probs <- predict(fit,type='prob')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pp=predict.test.df
  }else if(option=="svm"){
    #require(e1071)
    y <- as.factor(y)
    fit <- e1071::svm(y~d,type="C",kernel="radial",cost=1,scale=T,probability = T)
    predict.test <- predict(fit,d,probability = T)
    predict.test <- attr(predict.test,"probabilities")
    predict.test.df <- data.frame(predict.test)
    #extract the probablity assessment vector
    pp=predict.test.df[c("X1","X2","X3")]
  }else if(option=="lda"){
    #require(MASS)
    fit <- MASS::lda(y~d)
    predict.test.probs <- predict(fit,type='probs')
    predict.test.fit <- predict(fit)
    predict.test <- predict.test.fit$posterior
    predict.test.df <- data.frame(predict.test)
    #extract the probablity assessment vector
    pp=predict.test.df
  }else if(option=="mlp"){
    xxx <- require(mxnet)
    if(!xxx){
      print("installing mxnet because you don't have it...")
      cran <- getOption("repos")
      cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
      options(repos = cran)
      install.packages("mxnet")
      require(mxnet)
    }
    y <- as.numeric(y)-1
    #model
    data <- mx.symbol.Variable("data")
    fc1 <- mx.symbol.FullyConnected(data, num_hidden=500)
    act1 <- mx.symbol.Activation(fc1,  act_type="tanh")
    drop1 <- mx.symbol.Dropout(act1,p=0.01)
    fc2 <- mx.symbol.FullyConnected(drop1,  num_hidden=100)
    act2 <- mx.symbol.Activation(fc2,  act_type="tanh")
    drop2 <- mx.symbol.Dropout(act2,p=0.01)
    fc3 <- mx.symbol.FullyConnected(drop2,  num_hidden=3)
    softmax <- mx.symbol.SoftmaxOutput(fc3)

    devices <- mx.cpu()
    mx.set.seed(0)
    model <- mx.model.FeedForward.create(softmax, X=d, y=y,
                                         ctx=devices, num.round=41, array.batch.size=40,
                                         learning.rate=0.008, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                         initializer=mx.init.uniform(0.07),
                                         epoch.end.callback=mx.callback.log.train.metric(100))
    preds = predict(model, d)
    predict.test.df <- data.frame(t(preds))
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
    var(pv[,1])/(ro1*(1-ro1))+
      var(pv[,2])/(ro2*(1-ro2))+
      var(pv[,3])/(ro3*(1-ro3))
  )/3
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
      fit <- nnet::multinom(y~d,maxit = 1000,MaxNWts = 2000,trace=F)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d)
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,type="C",kernel="linear",cost=4.3,scale=T,probability = T)
      predict.test <- predict(fit,d,probability = T)
      predict.test <- attr(predict.test,"probabilities")
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df[c("X1","X2","X3","X4")]
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.fit <- predict(fit)
      predict.test <- predict.test.fit$posterior
      predict.test.df <- data.frame(predict.test)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="mlp"){
      xxx <- require(mxnet)
      if(!xxx){
        print("installing mxnet because you don't have it...")
        cran <- getOption("repos")
        cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
        options(repos = cran)
        install.packages("mxnet")
        require(mxnet)
      }
      y <- as.numeric(y)-1
      #model
      data <- mx.symbol.Variable("data")
      fc1 <- mx.symbol.FullyConnected(data, num_hidden=500)
      act1 <- mx.symbol.Activation(fc1,  act_type="tanh")
      drop1 <- mx.symbol.Dropout(act1,p=0.0)
      fc2 <- mx.symbol.FullyConnected(drop1,  num_hidden=100)
      act2 <- mx.symbol.Activation(fc2,  act_type="tanh")
      drop2 <- mx.symbol.Dropout(act2,p=0.0)
      fc3 <- mx.symbol.FullyConnected(drop2,  num_hidden=4)
      softmax <- mx.symbol.SoftmaxOutput(fc3)

      devices <- mx.cpu()
      mx.set.seed(0)
      model <- mx.model.FeedForward.create(softmax, X=d, y=y,
                                           ctx=devices, num.round=81, array.batch.size=40,
                                           learning.rate=0.05, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                           initializer=mx.init.uniform(0.07),
                                           epoch.end.callback=mx.callback.log.train.metric(100))
      preds = predict(model, d)
      predict.test.df <- data.frame(t(preds))
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
    rsq=(var(pv[,1])/(ro1*(1-ro1))+var(pv[,2])/(ro2*(1-ro2))+var(pv[,3])/(ro3*(1-ro3))+var(pv[,4])/(ro4*(1-ro4)))/4
    return(rsq)
  }
}
