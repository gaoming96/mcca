pdi=function(y,d,method="multinom",k=3){
  num=k
  option=method

  if(num==3){
  y=as.numeric(y)
  d=data.matrix(d)
  n1=which(y==1) #return the label
  n2=which(y==2)
  n3=which(y==3)

  #define the id
  if(option=="multinom"){
    #require(nnet)
    fit <- nnet::multinom(y~d,maxit = 1000,MaxNWts = 2000)
    predict.test.probs <- predict(fit,type='probs')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pp=predict.test.df
  }else if(option=="tree"){
    #require(rpart)
    y <- as.factor(y)
    fit <- rpart::rpart(y~d,control = rpart::rpart.control(minsplit = 4))
    predict.test.probs <- predict(fit,type='prob')
    predict.test.df <- data.frame(predict.test.probs)
    #extract the probablity assessment vector
    pp=predict.test.df
    ###  #adjustment
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
    drop1 <- mx.symbol.Dropout(act1,p=0)
    fc2 <- mx.symbol.FullyConnected(drop1,  num_hidden=100)
    act2 <- mx.symbol.Activation(fc2,  act_type="tanh")
    drop2 <- mx.symbol.Dropout(act2,p=0)
    fc3 <- mx.symbol.FullyConnected(drop2,  num_hidden=3)
    softmax <- mx.symbol.SoftmaxOutput(fc3)

    devices <- mx.cpu()
    mx.set.seed(0)
    model <- mx.model.FeedForward.create(softmax, X=d, y=y,
                                         ctx=devices, num.round=80, array.batch.size=40,
                                         learning.rate=0.01, momentum=0.9,  eval.metric=mx.metric.accuracy,
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
  pv1=pv[n1,]
  pv2=pv[n2,]
  pv3=pv[n3,]
  pdi1<-0
  pdi2<-0
  pdi3<-0
  for(i in 1:length(n1)){
    pdi1=pdi1+sum(pv1[i,1]>pv2[,1])*sum(pv1[i,1]>pv3[,1])
  }
  for(i in 1:length(n2)){
    pdi2=pdi2+sum(pv2[i,2]>pv1[,2])*sum(pv2[i,2]>pv3[,2])
  }
  for(i in 1:length(n3)){
    pdi3=pdi3+sum(pv3[i,3]>pv1[,3])*sum(pv3[i,3]>pv2[,3])
  }
  pdi<-(pdi1+pdi2+pdi3)/(3*length(n1)*length(n2)*length(n3))
  return(pdi)
  }else if(num==4){
    y=as.numeric(y)
    d=data.matrix(d)

    n1=which(y==1) #return the label
    n2=which(y==2)
    n3=which(y==3)
    n4=which(y==4)

    #define the id
    if(option=="multinom"){
      #require(nnet)
      fit <- nnet::multinom(y~d,maxit = 1000,MaxNWts = 2000)
      predict.test.probs <- predict(fit,type='probs')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="tree"){
      #require(rpart)
      y <- as.factor(y)
      fit <- rpart::rpart(y~d,control = rpart::rpart.control(minsplit = 5))
      predict.test.probs <- predict(fit,type='prob')
      predict.test.df <- data.frame(predict.test.probs)
      #extract the probablity assessment vector
      pp=predict.test.df
    }else if(option=="svm"){
      #require(e1071)
      y <- as.factor(y)
      fit <- e1071::svm(y~d,type="C",kernel="linear",cost=0.7,scale=T,probability = T)
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
      act1 <- mx.symbol.Activation(fc1,  act_type="relu")
      drop1 <- mx.symbol.Dropout(act1,p=0.01)
      fc2 <- mx.symbol.FullyConnected(drop1,  num_hidden=100)
      act2 <- mx.symbol.Activation(fc2,  act_type="relu")
      drop2 <- mx.symbol.Dropout(act2,p=0.01)
      fc3 <- mx.symbol.FullyConnected(drop2,  num_hidden=4)
      softmax <- mx.symbol.SoftmaxOutput(fc3)

      devices <- mx.cpu()
      mx.set.seed(0)
      model <- mx.model.FeedForward.create(softmax, X=d, y=y,
                                           ctx=devices, num.round=41, array.batch.size=40,
                                           learning.rate=0.1, momentum=0.9,  eval.metric=mx.metric.accuracy,
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

    pv1=pv[n1,]
    pv2=pv[n2,]
    pv3=pv[n3,]
    pv4=pv[n4,]
    pdi1<-0
    pdi2<-0
    pdi3<-0
    pdi4<-0
    for(i in 1:length(n1)){
      pdi1=pdi1+sum(pv1[i,1]>pv2[,1])*sum(pv1[i,1]>pv3[,1])*sum(pv1[i,1]>pv4[,1])
    }
    for(i in 1:length(n2)){
      pdi2=pdi2+sum(pv2[i,2]>pv1[,2])*sum(pv2[i,2]>pv3[,2])*sum(pv2[i,2]>pv4[,2])
    }
    for(i in 1:length(n3)){
      pdi3=pdi3+sum(pv3[i,3]>pv1[,3])*sum(pv3[i,3]>pv2[,3])*sum(pv3[i,3]>pv4[,3])
    }
    for(i in 1:length(n4)){
      pdi4=pdi4+sum(pv4[i,4]>pv1[,4])*sum(pv4[i,4]>pv2[,4])*sum(pv4[i,4]>pv3[,4])
    }
    pdi<-(pdi1+pdi2+pdi3+pdi4)/(4*length(n1)*length(n2)*length(n3)*length(n4))
    return(pdi)
  }
}

