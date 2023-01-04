#Author Fuleah A Razzaq
library(tidyverse)#load libraries
library(readr)
library(dplyr)
library(clubSandwich)
library(afex)
library(ordinal)
library(parameters)
rm(list = ls(all.names = TRUE)) #clear memory
load("") #load data
cov<-subTimeTB %>%  
  inner_join(subTB, by = ('ID'))
data<-as.data.frame(cov %>%  
  inner_join(motorTB, by = c('ID','time')))

colnames(data)[11]="side.onset"
data$side.onset=as.factor(data$side.onset)
data$handness=as.factor(data$handness)
cnames=colnames(motorTB)[3:ncol(motorTB)]
lmmsum=NULL

for(i in cnames)
{

  y=i
  t=mp=H1=opt=NULL
  
  try({
    data[,i]=as.factor(data[,i])
    H1=as.formula(paste0(i,"~ Dose+age+progression+severity+
                handness+side.onset+(1|ID)"))
    opt=clmm(H1,data=data,Hess = TRUE)
  })
  try({
    t=data.frame(coef(summary(opt)))
    t=rownames_to_column(t,"contrast")
    t$variable=y
    
    mp=model_parameters(opt,pretty_names = T,
                     bootstrap = T,
                     iterations = 1000,
                     ci_random = FALSE,
                     effects = "fixed",
                     vcov = "CR0",
                     ci_method = NULL,
                     control=(msMaxIter=100)
    )
    t$PB=mp$p
    t$lci=mp$CI_low
    t$uci=mp$CI_high
    t$est=mp$Coefficient
    
    lmmsum=rbind(lmmsum,t)
    })
  }

write.csv(lmmsum,"motorLongitudinal.csv",row.names = F)
