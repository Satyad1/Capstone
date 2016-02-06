setwd("~/Work/R/SlideRule/Capstone")
churn<-read.csv("churn.csv")

names(churn)
str(churn)
summary(churn)

#CLEANING DATA
#removing dots from names
names(churn)<-gsub("\\.","",names(churn))

#Renaming True/False and Yes/no to binary digits
churn$VMailPlan<-ifelse(churn$VMailPlan=="no",0,1)
churn$IntlPlan<-ifelse(churn$IntlPlan=="no",0,1)
churn$Churn<-as.integer(churn$Churn)
churn$Churn[churn$Churn=="1"]<-0
churn$Churn[churn$Churn=="2"]<-1

#Changing classes 
churn$Churn<-as.factor(churn$Churn)
churn$IntlPlan<-as.factor(churn$IntlPlan)
churn$VMailPlan<-as.factor(churn$VMailPlan)

#Adding columns
churn$totalcalls<-churn$DayCalls+churn$EveCalls+churn$NightCalls
churn$totalmins<-churn$DayMins+churn$EveMins+churn$NightMins
churn$totalcharge<-churn$DayCharge+churn$EveCharge+churn$NightCharge+churn$IntlCharge
churn$AccountBucket<-cut(churn$AccountLength,c(0,50,100,150,200,250))

#EXPLORATORY DATA ANALYSIS

library(dplyr)
library(ggplot2)
summary(churn)
table(churn$Churn)

#Looking at the number of accounts by state
churn %>% count(State,sort=TRUE)

#Looking at churn by State
churn %>% group_by(State) %>% select(Churn) %>% table()

library(ggplot2)
qplot(x=State, data=churn)

#Looking at Churn by Account Length
qplot(x=AccountLength,data=subset(churn,!is.na(Churn)),binwidth=10)+scale_x_continuous(lim=c(0,250), breaks = seq(0,250,25))+facet_wrap(~Churn)
by(churn$AccountLength,churn$Churn, summary)

#Churn By VMail Messages
by(churn$VMailMessage,churn$Churn, summary)

ggplot(aes(x = VMailMessage, y = ..count../sum(..count..)), data = subset(churn, !is.na(Churn)))+geom_freqpoly(aes(color = Churn), binwidth=10) +scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 1)) +xlab('VMail Message') +ylab('Percentage of People with/without Voice Mails')

#Using boxplots to see Churn By Day Minutes+
ggplot(aes(Churn,DayMins),data=subset(churn,!is.na(Churn)))+geom_boxplot()
#Zooming in by limiting Y axis
ggplot(aes(Churn,DayMins),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(120,280))
#Churners have a higher median.
by(churn$DayMins,churn$Churn, summary)

#Using boxplots to see Churn By Day Calls
  ggplot(aes(Churn,DayCalls),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(75,125))
by(churn$DayCalls,churn$Churn, summary)

#Using boxplots to see Churn By Day Charge+
by(churn$DayCharge,churn$Churn, summary)
ggplot(aes(Churn,DayCharge),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(20,50))
#The median for Churners is almost at the 3rd Quartile for non churners

#Using boxplots to see Churn by Evening Minutes
ggplot(aes(Churn,EveMins),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(150,250))
by(churn$EveMins,churn$Churn,summary)

#Using boxplots to see Churn by Evening Calls
by(churn$EveCalls,churn$Churn,summary)

#Using boxplots to see Churn by Evening Charges
by(churn$EveCharge,churn$Churn,summary)
ggplot(aes(Churn,EveCharge),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(12,21))

#Using boxplots to see Churn by Night Minutes
by(churn$NightMins,churn$Churn,summary)

#Using boxplots to see Churn by Night Calls
by(churn$NightCalls,churn$Churn,summary)

#Using boxplots to see Churn by Night Charges
by(churn$NightCharge,churn$Churn,summary)

#Using boxplots to see Churn by Intl Minutes
by(churn$IntlMins,churn$Churn,summary)

#Using boxplots to see Churn by Intl Calls
by(churn$IntlCalls,churn$Churn,summary)

#Using boxplots to see Churn by Intl Charges
by(churn$IntlCharge,churn$Churn,summary)

#Using boxplots to see Churn by CustServCalls+
by(churn$CustServCalls,churn$Churn,summary)
ggplot(aes(Churn,CustServCalls),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(1,4))
#75% of non churners made only 3 or less calls of which 50% made 1 or less calls to Customer Service.
#However, a majority of churners made between 1 to 4 calls to Customer Service. Plotting the mean :
ggplot(aes(Churn,CustServCalls),data=subset(churn,!is.na(Churn)))+geom_boxplot()+stat_summary(fun.y=mean,geom="point",shape=4)+coord_cartesian(ylim = c(1,4))
#Mean values are higher than the medians in both cases. 

#Using boxplots to see Churn by Total Calls
by(churn$totalcalls,churn$Churn,summary)
ggplot(aes(Churn,totalcalls),data=subset(churn,!is.na(Churn)))+geom_boxplot()

#Using boxplots to see Churn by Total Minutes+
by(churn$totalmins,churn$Churn,summary)
ggplot(aes(Churn,totalmins),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(500,720))
#The majority of Churners appear to be heavy users. Almost 50% of churners used over 600 minutes 

#Using boxplots to see Churn by Total Charges+
by(churn$totalcharge,churn$Churn,summary)
ggplot(aes(Churn,totalcharge),data=subset(churn,!is.na(Churn)))+geom_boxplot()+coord_cartesian(ylim=c(52,78))
#Again appear to be heavy users with 50% of users over the 3rd quartile for non Churners. 

#LOGISTIC REGRESSION MODELLING
table(churn$Churn)
1-((483)/(2850+483))

#Creating two sets of data - test and train
library(caTools)
split=sample.split(churn$Churn,SplitRatio = 0.60)
train<-subset(churn, split==TRUE)
test<-subset(churn, split==FALSE)

#Regression Model
churnLog<-glm(Churn~AccountLength+IntlPlan+VMailPlan+VMailMessage+DayMins+DayCalls+DayCharge+EveMins+EveCalls+EveCharge+NightMins+NightCalls+NightCharge+IntlMins+IntlCalls+IntlCharge+CustServCalls+totalcalls+totalmins+totalcharge, data= train, family = binomial)
summary(churnLog)
#AIC is 1335.2

#Cleaning the Model
churnLog1<-glm(Churn~IntlPlan+VMailPlan+VMailMessage+IntlCalls+CustServCalls+totalcharge, data= train, family = binomial)
summary(churnLog1)
#AIC is slightly better at 1324.2

#Checking for Model fit on Test Data
predictTest<-predict(churnLog1, type="response",newdata=test)
table(test$Churn,predictTest>0.5)

(1105+39)/(1105+35+39+154)

#Predictions on Test Data
library(ROCR)
ROCRtestpred<-prediction(predictTest,test$Churn)
ROCRtestperf<-performance(ROCRtestpred,"tpr","fpr")
plot(ROCRtestperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRtestpred, "auc")@y.values)

#Trees
library(rpart)
library(rpart.plot)

ChurnTree<- rpart(Churn~IntlPlan+VMailPlan+VMailMessage+IntlCalls+CustServCalls+totalcharge,data=train,method="class",minbucket=30,parms = list(prior=c(0.5,0.5)))
prp(ChurnTree)

predictCART<-predict(ChurnTree,newdata=test,type="class")
table(test$Churn,predictCART)

(1066+165)/(1066+74+28+165)

#ROC 
predictROC<-predict(ChurnTree,newdata=test)
pred<-prediction(predictROC[,2],test$Churn)
perf<-performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

#AUC value is 0.9241


