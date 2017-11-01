ccp=function(y,d,method="multinom",k=3){
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
    pv=predict(fit,type='class')
  }else if(option=="tree"){
    #require(rpart)
    y <- as.factor(y)
    fit <- rpart::rpart(y~d)
    pv <- predict(fit,type="class")
  }else if(option=="svm"){
    #require(e1071)
    y <- as.factor(y)
    fit <- e1071::svm(y~d,type="C",kernel="radial",cost=1,scale=T)
    pv <- predict(fit,data=train)
  }else if(option=="lda"){
    #require(MASS)
    fit <- MASS::lda(y~d)
    predict.test.fit <- predict(fit)
    pv <- predict.test.fit$class
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
                                         ctx=devices, num.round=81, array.batch.size=40,
                                         learning.rate=0.008, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                         initializer=mx.init.uniform(0.07),
                                         epoch.end.callback=mx.callback.log.train.metric(100))
    preds = predict(model, d)
    pv <- max.col(t(preds))-1
    pv=pv+1; y=y+1;

  }else if(option=="label"){
    temp <- sum(d==1|d==2|d==3)
    if (temp!=length(d)){
      cat("ERROR: The input value \"d\" should be a tri-nomial label.")
      return(NULL)
    }
    pv=d
  }

  ccp=(sum(pv==1 & y==1)/n1+ sum(pv==2 & y==2)/n2+sum(pv==3 & y==3)/n3)/3
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
      fit <- nnet::multinom(y~d,maxit = 1000,MaxNWts = 2000,trace=F)
      pv=predict(fit,type='class')
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d)
      pv <- predict(fit,type="class")
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,type="C",kernel="radial",cost=1,scale=T)
      pv <- predict(fit,data=train)
    }else if(option=="lda"){
      #require(MASS)
      fit <- MASS::lda(y~d)
      predict.test.fit <- predict(fit)
      pv <- predict.test.fit$class
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
      fc3 <- mx.symbol.FullyConnected(drop2,  num_hidden=4)
      softmax <- mx.symbol.SoftmaxOutput(fc3)

      devices <- mx.cpu()
      mx.set.seed(0)
      model <- mx.model.FeedForward.create(softmax, X=d, y=y,
                                           ctx=devices, num.round=81, array.batch.size=40,
                                           learning.rate=0.008, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                           initializer=mx.init.uniform(0.07),
                                           epoch.end.callback=mx.callback.log.train.metric(100))
      preds = predict(model, d)
      pv <- max.col(t(preds))-1
      pv=pv+1; y=y+1;

    }else if(option=="label"){
      temp <- sum(d==1|d==2|d==3|d==4)
      if (temp!=length(d)){
        cat("ERROR: The input value \"d\" should be a quandr-nomial label.")
        return(NULL)
      }
      pv=d
    }

    ccp=(sum(pv==1 & y==1)/n1+ sum(pv==2 & y==2)/n2+sum(pv==3 & y==3)/n3+sum(pv==4 & y==4)/n4)/4
    return(ccp)
  }
}
