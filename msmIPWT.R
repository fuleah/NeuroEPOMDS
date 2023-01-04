#Author Fuleah A Razzaq
library(tidyverse)#load libraries
library(readr)
library(dplyr)

rm(list = ls(all.names = TRUE)) #clear memory
load("") #load data
colnames(subTB)[7]="side"
subgr=subTimeTB
subgr$group=NA
subgr$group[subgr$time==2 & subgr$Dose==0]="Placebo"
subgr$group[subgr$time==2 & subgr$Dose==5]="NeuroEPO"
subgr<-na.omit(subgr)%>%dplyr::select(ID,group)
subTB<-subgr %>%  
  inner_join(subTB, by = ('ID'))


data<-cognitionTB %>%  
  inner_join(motorTB, by = c('ID','time'))
data=data[data$time %in% c(1,2,3),]

Y=as.matrix(subset(data,select=(speech:dyskinesia)))

######FA ######

library(psych)
my_fa <- fa(r = Y, nfactors = 3)
resY=my_fa$scores

####### combine data #####
resCCA=data.frame(ID=data$ID,time=data$time,resY)
####covariates
resCCA<-subTB %>%  
  inner_join(resCCA, by = ('ID'))
resCCA<-subTimeTB %>%  
  inner_join(resCCA, by = c('ID','time'))




resCCA$Doseind=0
resCCA$Doseind[resCCA$Dose>0]=1
resCCA$Doseind=as.integer(resCCA$Doseind)
resCCA$severity=as.integer(resCCA$severity)
resCCA$handness=as.factor(resCCA$handness)
resCCA$side=as.factor(resCCA$side)
resCCA$days=resCCA$age-resCCA$initage
resCCA$prevX=0
resCCA$prevY=resCCA$prevY2=resCCA$prevY3=NA
for (id in 1:26 )
{
  resCCA$prevX[resCCA$ID==id & resCCA$time %in% c(3,4)]=resCCA$Doseind[resCCA$ID==id & resCCA$time==2]
  resCCA$prevY[resCCA$ID==id & resCCA$time %in% c(1,2,3,4)]=resCCA$MR1[resCCA$ID==id & resCCA$time==1]
  resCCA$prevY2[resCCA$ID==id & resCCA$time %in% c(1,2,3,4)]=resCCA$MR2[resCCA$ID==id & resCCA$time==1]
  resCCA$prevY3[resCCA$ID==id & resCCA$time %in% c(1,2,3,4)]=resCCA$MR3[resCCA$ID==id & resCCA$time==1]
 
  
}

dataCausal=data.frame( resCCA)


library(geepack)
library(ipw)
library(EValue)
siptw <- ipwtm(exposure = Doseind,timevar=time,
               family = "binomial",  link="logit",
               numerator= ~1 + prevX,
               denominator =  ~  prevX +prevY+progression+age+severity+handness+side#+education#
               ,id = ID,  type = "all",  data = dataCausal, corstr=AR1)

quantile(siptw$ipw.weights) #check distribution of weights

dataCausal$siptw <- siptw$ipw.weights
msm=nlme::lme(MR1~Doseind, data=dataCausal, 
              weights =~siptw,
              random=~1|ID)
summary(msm)
s=summary(msm)

msm <- geeglm(MR1~Doseind,
              data = dataCausal , weights =dataCausal$siptw , family = gaussian,
              id = dataCausal$ID, waves=dataCausal$time, corstr = "ar1")
summary(msm)
beta = coef(summary(msm))[,1]
SE = coef(summary(msm))[, 2]
lcl = beta - qnorm(0.975) * SE
ucl = beta + qnorm(0.975) * SE


e=evalues.OLS(est = beta[2],
              se = SE[2],
              sd = s$sigma,
              delta =1)




