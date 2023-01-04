#Author Fuleah A Razzaq
library(tidyverse)#load libraries
library(readr)
library(dplyr)
library(clubSandwich)
library(afex)
library(ordinal)
library(parameters)
library(emmeans)
rm(list = ls(all.names = TRUE)) #clear memory
load("") #load data
subgr=subTimeTB
subgr$group=NA
subgr$group[subgr$time==2 & subgr$Dose==0]="Placebo"
subgr$group[subgr$time==2 & subgr$Dose==5]="NeuroEPO"
subgr<-na.omit(subgr)%>%dplyr::select(ID,group)
subTB<-subgr %>%  
  inner_join(subTB, by = ('ID'))
cov<-subTimeTB %>%  
  inner_join(subTB, by = ('ID'))
data<-as.data.frame(cov %>%  
  inner_join(motorTB, by = c('ID','time')))

colnames(data)[12]="side.onset"
data<-data %>% filter(time %in% c(1,2,3))
data$side.onset=as.factor(data$side.onset)
data$handness=as.factor(data$handness)
data$group=as.factor(data$group)
data <- within(data, group <- relevel(group, ref = "Placebo"))
data$time=as.factor(data$time)
data <- within(data, time <- relevel(time, ref = 1))
data$severity=as.factor(data$severity)
data$side.onset=as.factor(data$side.onset)
data$handness=as.factor(data$handness)
cnames=colnames(motorTB)[3:ncol(motorTB)]
lmmsum=GTsum=TGsum=NULL
for(i in cnames)
{
  y=i
  t=mp=H1=opt=tGT=tTG=cc=PB=screenlmer=NULL
  
  try({
    
    H1=(paste0(i,"~ group*time+age+progression+severity+
                handness+side.onset+(1|ID)"))
    
    data[,i]=as.factor(data[,i])
    opt=clmm(H1,data=data,Hess = TRUE)
   
  })
  try({
    t=data.frame(coef(summary(opt)))
    t=rownames_to_column(t,"contrast")
    t$variable=y

    lmmsum=rbind(lmmsum,t)
    })
  
  try({
    est=mp=cc=NA
  est <- emmeans(opt, revpairwise ~ time|group,data=data,adjust="sidak")
  mp=model_parameters(est$contrasts,
                    ci = 0.95,p_adjust = "none",
                    bootstrap = T,
                    iterations = 1000,
                    vcov = "CR0",
                    control=(msMaxIter=1000),
                    parameters_cimethod = TRUE
  )
  tTG=mp
  tTG$variable=y
  
  TGsum=rbind(TGsum,tTG)
  
  })
  try({est=mp=cc=NA
  est <- emmeans(opt, revpairwise ~ group |time,data=data,adjust="sidak")
                 
  mp=model_parameters(est$contrasts,
                      ci = 0.95,p_adjust = "none",
                      vcov = "CR0",
                      bootstrap = T, iterations = 1000,
                      control=(msMaxIter=1000),
                      parameters_cimethod = TRUE
  )
  tGT=mp
  tGT$variable=y
  GTsum=rbind(GTsum,tGT)
  })
}

write.csv(lmmsum,"motorBlind.csv",row.names = F)
write.csv(GTsum,"motorBlindT_groupTime.csv",row.names = F)
write.csv(TGsum,"motorBlind_timeGroup.csv",row.names = F)
