dev.off()
data=read.csv("C://Users//luv//Desktop//Online Courses//Kaggle//Breast Cancer Wisconsin//data.csv")

#Exploratory
num_data=data[,3:33]
num_data$result=as.numeric(data$diagnosis=='M')

corr=cor(num_data)
corrplot(corr,tl.cex = 0.5)

num_data<-num_data[,-31]

g<-glm(result~.,family = binomial(link = 'logit'),data = num_data)
summary(g)
pred_data<-data.frame(pmalignant=g$fitted.values,malignant=num_data$result)
pred_data<-pred_data[order(pred_data$pmalignant,decreasing = FALSE),]
pred_data$rank<-1:nrow(pred_data)


ggplot(data = pred_data,aes(x=rank,y=pmalignant))+
  geom_point(aes(color=malignant),alpha=1,shape=4,stroke=2)+
  xlab("Index")+
  ylab("predicted")

