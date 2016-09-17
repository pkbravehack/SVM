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
   
   
   #################################################################################################################33
   
   #############################
   # data preparation
   #############################
   
   ### data
   link<-"https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt"
   df<-read.table(link,header = FALSE, sep = ",",dec = ".",col.names=c("variance","skewness","curtosis","entropy","class"))
   
   ### scale data, factorize y
   df_scale<-scaled(df,"class")
   
   ### plot data
   plots(df_scale,"class")
   
   ### train/ test set
   t<-tt(df_scale)
   
   
   #############################
   # SVM performance
   #############################
   
  