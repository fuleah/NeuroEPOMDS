#Author Fuleah A Razzaq
library(tidyverse)#load libraries
library(readr)
library(dplyr)
library(whitening)
library(mediation)
rm(list = ls(all.names = TRUE)) #clear memory
load("") #load data subset based on LME######

data<-cognitionTB %>%  
  inner_join(motorTB, by = c('ID','time'))

data<-data %>%  
  inner_join(eegTB, by = c('ID','time'))
data=na.omit(data)
colnames(subTB)[7]="side"

Y=as.matrix(subset(data,select=(speech:actionTremorRH)))
 
X=as.matrix(subset(data,select=(EEG.X1:EEG.X158956)))
Y=as.matrix(subset(data,select=(Initiation:Inhibition_errors)))
cca.out = scca(X, Y, scale=T)

CCAX = tcrossprod(scale(X,center=T,scale=F), cca.out$WX )
CCAY = tcrossprod(scale(Y,center=T,scale=F), cca.out$WY )

resCCA=data.frame(ID=data$ID,time=data$time,
                  CCAX=CCAX,CCAY=CCAY)
resCCA<-subTB %>%  
  inner_join(resCCA, by = ('ID'))
resCCA<-subTimeTB %>%  
  inner_join(resCCA, by = c('ID','time'))

model.m.lmer=as.formula(paste0("CCAX.1~1+Dose+progression+handness+side+severity+age+(1|ID)"))
model.o.lmer=as.formula(paste0("CCAY.1~1+CCAX.1+Dose+progression+handness+side+severity+age+(1|ID)"))

fit.m = lme4::lmer(model.m.lmer, data = resCCA)
fit.o = lme4::lmer(model.o.lmer, data = resCCA)

results = mediation::mediate(fit.m, fit.o, treat='Dose',
                             mediator="CCAX.1" , sims=1000,na.action="na.omit"
                             )



  