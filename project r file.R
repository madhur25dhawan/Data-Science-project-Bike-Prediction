library(rpart)

rm(list=ls())
setwd("E:/data scientist/project edwiser/project 2")
getwd()

day_data=read.csv("day.csv")
View(day_data)

str(day_data)

#ploting histogram to view data in proper shape

hist(day_data$season)
hist(day_data$yr)
hist(day_data$mnth)
hist(day_data$holiday)
hist(day_data$weekday)
hist(day_data$weathersit)
hist(day_data$temp)
hist(day_data$atemp)
hist(day_data$hum)
hist(day_data$windspeed)
hist(day_data$cnt)



#convert varable to proper type

day_data$season=as.factor(day_data$season)
day_data$weathersit=as.factor(day_data$weathersit)
day_data$holiday=as.factor(day_data$holiday)
day_data$workingday=as.factor(day_data$workingday)

day_data$yr=as.factor(day_data$yr)
day_data$mnth=as.factor(day_data$mnth)
day_data$weekday=as.factor(day_data$weekday)



# Checking missing value 
sum(is.na(day_data))



# Checking for outlier



numeric_index=sapply(day_data,is.numeric)
numeric_data=day_data[,numeric_index]

num_col_names=colnames(numeric_data[-16])


for(i in 1:length(num_col_names)){
  assign(paste("graph",i),ggplot(aes_string(y= (num_col_names[i]), x= day_data$cnt),data = subset(day_data))+ stat_boxplot(geom = "errorbar",width=.5)+geom_boxplot(outlier.color = "red",fill="grey",outlier.shape = 18,outlier.size = 1,notch = FALSE)+
           labs(y=num_col_names[i],x="Count")+ggtitle(paste("boxplot of",num_col_names[i]) ) )
}

gridExtra::grid.arrange(`graph 1`,`graph 2`,`graph 3`,ncol = 3)
gridExtra::grid.arrange(`graph 4`,`graph 5`,`graph 6`,ncol = 3)
gridExtra::grid.arrange(`graph 7`,`graph 8`,ncol = 2)

for (i in num_col_names) {
  print(i)  
  maxi=quantile(day_data[,i],c(.75))+(1.5*IQR(day_data[,i]))
  mini=quantile(day_data[,i],c(.75))-(1.5*IQR(day_data[,i]))
  
  #val=mydata[,i][fulldata[,i] %in% boxplot.stats(mydata[,i])$out]    
  
  #mydata[,i][mydata[,i] %in% val]=last
  day_data[,i][day_data[,i] > maxi]=maxi
  day_data[,i][day_data[,i] <mini]=mini
}



#Feature Selection
corrgram(day_data[,sapply(day_data, is.numeric)])
cor(day_data$temp,day_data$atemp)
var(day_data$temp,day_data$atemp)
cor(day_data$cnt,day_data$windspeed)
day_data=subset(day_data,select = -c(atemp,dteday,casual,registered,instant))
vif(day_data$temp,day_data$cnt)

catvar=sapply(day_data, is.factor)

cdata=day_data[,catvar]

for (i in length(cdata)) {
  print(names(cdata)[i])
  print(chisq.test(table(cdata  )))
  
}



# Sampling techniques... divide data in train and test
data=sample(1:nrow(day_data),0.8*nrow(day_data))
train=day_data[data,]
test=day_data[-data,]


# model Building
#         ***************Decision tree*****************

dmodel=rpart(cnt ~ . ,data = train, method = "anova")
dmodel
summary(dmodel)
pred=predict(dmodel,test[,-11])

mape= function(y,yhat){
  mean(abs((y-yhat)/y)) * 100
}
mape(test[,11],pred)

regr.eval(test[,11],pred,stats = c("mae","mse","rmse"))

# mape =  29.44   Accuracy = 70.56
# mae  =  772.3556
# mse  =  1119662.8006
# rmse  = 1058.1412

#   ***************Random Forest*****************

ran=randomForest(cnt ~.,data = train,importance= TRUE,ntree=200)
treelist=RF2List(ran)
exec=extractRules(treelist,train[,-11])
exec[1:2,]
readable=presentRules(exec,colnames(train))
readable[1:2,]
targ=getRuleMetric(exec,train[,-11],train$cnt)
targ[1:2,]

pred=predict(ran,test[,-11])
ran
mape(test[,11],pred)

regr.eval(test[,11],pred,stats = c("mae","mse","rmse"))

# mape =  21.54   Accuracy = 78.46
# mae  =  553.1357
# mse  =  553968.2754
# rmse =  744.2905


#   ***************Lenear Regression*****************
library(sp,raster)
library(usdm)
vif(as.numeric(df))
vif()

lm_model=lm(cnt ~.,data = train)


summary(lm_model)

pr=predict(lm_model,test[,-11])

mape(test[,11],pr)

regr.eval(test[,11],pr,stats = c("mae","mse","rmse"))

# mape =  17.34   Accuracy = 82.66
# mae  =  548.6573
# mse  =  583055.1388
# rmse =  763.5805
