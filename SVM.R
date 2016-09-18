   library(class)	#k-nearest	neighbors
   library(kknn)	#weighted	k-nearest	neighbors
   library(e1071)	#SVM 
   library(caret)	#select	tuning	parameters 
   library(MASS)	#	contains	the	data 
   library(reshape2)	#assist	in	creating	boxplots 
   library(ggplot2)	#create	boxplots 
   library(kernlab)	#assist	with	SVM	feature	selection 
   library(pROC) 
   library(psych)
   
   rm(list=ls())
   
   ### split train/ test set
   tt<-function(dataset){
      set.seed(666)
      ind=sample(2,nrow(dataset),replace = TRUE,prob=c(0.7,0.3))
      train<- dataset[ind==1,]
      test<- dataset[ind==2,]
      result=list("train"=train, "test"=test)
      return(result)
   }
   
   
   ### data preparation - scale integer and factorize y
   scaled<-function(dataset,y){
      dataset[,y]<-as.factor(dataset[,y])
      a=match(y,colnames(dataset))
      df_scale=as.data.frame(scale(dataset[,-a]))
      df_scale[,y]=dataset[,y]
      names(df_scale)[names(df_scale)==y]<-"class"
      return (df_scale)
   }
   
   
   ### visualize  
   plots<-function(dataset,y){
      # scatterplot
      n=ncol(dataset)-1 
      for (i in 1:n){
         for (j in i:n){
            if (i!=j){
               windows()
               print (ggplot(data=dataset,aes(x=dataset[,i], y=dataset[,j], col=dataset[,y]))+
                         geom_point() +
                         xlab(colnames(dataset)[i])+
                         ylab(colnames(dataset)[j]))
            }
         }
      }
      df_scale_melt=melt(dataset,id.var=y)
      ggplot(data=df_scale_melt, aes_string(x=y, y="value")) + 
         geom_boxplot()+
         facet_wrap(~variable,	ncol=2)
   }
   
   
   # svm - linear
   tune_svm<-function(train, test,cost=c(0.001,	0.01,	0.1,	1,5,10)){
      set.seed(123)
      best.linear<-best.svm(class~.,data=train,kernel="linear",cost=cost)
      tune.test	=	predict(best.linear,	newdata=test)
      print(table(tune.test,	test$class)	)
      mean=mean(tune.test==test$class)
      cat("\nMean of predicted values: ", mean)
   }
   
   
   # svm - polynomial
   poly_svm<-function(train, test,coefficients=c(0.1,0.5,1,2,3,4),dgr=c(3,4,5)){
      set.seed(123)
      best.poly<-best.svm(class~.,data=train,kernel="polynomial",coef0 = coefficients,degree=dgr)
      poly.test	=	predict(best.poly,	newdata=test)
      print(table(poly.test,	test$class)	)
      mean=mean(poly.test==test$class)
      cat("\nMean of predicted values: ", mean)
   }
   
   
   # svm - gaussian (RBF)
   rad_svm<-function(train, test,gam=c(0.1,0.5,1,2,3,4)){
      set.seed(123)
      best.rad<-best.svm(class~.,data=train,kernel="radial",gamma=gam) #gamma parameter,not function
      rad.test	=	predict(best.rad,	newdata=test)
      print(table(rad.test,	test$class)	)
      mean=mean(rad.test==test$class)
      cat("\nMean of predicted values: ", mean)
   }
   
   
   # svm - sigmoid function.
      # SVM model using a sigmoid kernel function is equivalent to a two-layer, perceptron neural network.
   sigmoid_svm<-function(train, test,gam=c(0.1,0.5,1,2,3,4),coef=c(0.1,0.5,1,2,3,4)){
      set.seed(123)
      best.sigm<-best.svm(class~.,data=train,kernel="sigmoid",gamma=gam,coef0=coef) 
      sigm.test	=	predict(best.sigm,	newdata=test)
      print(table(sigm.test,	test$class)	)
      mean=mean(sigm.test==test$class)
      cat("\nMean of predicted values: ", mean)
   }
   
   #################################################################################################################33
   
   #############################
   # data preparation
   #############################
   
   ### data
   link<-"https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt"
   df<-read.table(link,header = FALSE, sep = ",",dec = ".",col.names=c("variance","skewness","curtosis","entropy","y"))

   ### scale data, factorize y; rename y as "class"
   df_scale<-scaled(df,"y")
   
   ### plot data
   plots(df_scale,"class")
   
   ### train/ test set
   t<-tt(df_scale)
   
   
   #############################
   # SVM performance
   #############################
   
   # linear function
   tune_svm(t$train,t$test)
   
   #polynomial function.
   poly_svm(t$train,t$test)
   
   # radial function
   rad_svm(t$train,t$test)
   
   # sigmoid function
   sigmoid_svm(t$train,t$test)

 