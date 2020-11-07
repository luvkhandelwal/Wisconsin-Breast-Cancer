dev.off()
rm(list = ls())
data=read.csv("C://Users//luv//Desktop//Online Courses//Kaggle//Breast Cancer Wisconsin//data.csv")

#Exploratory

for (i in ncol(data))
{
if (any(is.na(data[,i])))
  data=data[,-i]
}

#Data Partition
num_data=data[,3:33]
num_data$result=as.numeric(data$diagnosis=='M')

data1=sample.split(data,SplitRatio = 0.8)

train=subset(data,data1==TRUE)
test=subset(data,data1==FALSE)

train$diagnosis=as.numeric(train$diagnosis=='M')
test$diagnosis=as.numeric(test$diagnosis=='M')

#Correlation Analysis
corr=cor(test[,-1])
corrplot(corr,tl.cex = 0.5)

#Logistic Regression


g<-glm(diagnosis~.,family = binomial(link = 'logit'),data = train)
summary(g)
 
#Fitness 

gof<-hoslem.test(g$y,fitted(g))  
gof
?hoslem.test


#Testing
t<-predict.glm(object=g,newdata=test,type = "response")

t=ifelse(test=t>0.5,yes = 1, no=0 )
table(test$diagnosis,t)
cat("The accuracy is",116/124*100)

#Plotting Logistic Data



ggplot(data = pred_data,aes(x=rank,y=pmalignant))+
  geom_point(aes(color=malignant),alpha=1,shape=4,stroke=2)+
  xlab("Index")+
  ylab("predicted")

#Classification Trees
library(tree)
tr<-tree(diagnosis~.,data = train)
summary(tr)
plot(tr)

t2<-predict(object=tr,newdata=test,type = "vector")

t2=ifelse(test=t2>0.5,yes = 1, no=0 )
table(test$diagnosis,t2)
cat("The accuracy is",115/124*100)

#K-Nearest Neighbour
ktrain<-train[,1:2]
ktrain[,3:32]<-scale(train[,3:32])

ktest<-test[,1:2]
ktest[,3:32]<-scale(test[,3:32])
library(class)
kt<-knn(train = ktrain,test = ktest,cl=ktrain$diagnosis,k=5)

table(test$diagnosis,kt)

# K-Means

library(ClusterR) 
library(cluster) 
krtrain<-train[,c(-1,-2)]

kr<-kmeans(krtrain,centers = 2,nstart = 10)
cm <- table(train$diagnosis, kr$cluster)
cm
