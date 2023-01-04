#Author Fuleah A Razzaq
library(tidyverse)#load libraries
library(readr)
library(dplyr)
library(clubSandwich)
library(afex)
library(ordinal)
library(parameters)
library(multcomp)
library(emmeans)
library(nlme)
library(boot)
library(boot.pval)
library(coda)
rm(list = ls(all.names = TRUE)) #clear memory
load("")#load preprocessed data
subgr=subTimeTB
subgr$group=NA
subgr$group[subgr$time==2 & subgr$Dose==0]="Placebo"
subgr$group[subgr$time==2 & subgr$Dose==5]="NeuroEPO"
subgr<-na.omit(subgr)%>%dplyr::select(ID,group)
subTB<-subgr %>%  
  inner_join(subTB, by = ('ID'))
cov<-subTimeTB %>%  
  inner_join(subTB, by = ('ID'))

data<-cov %>%  
  inner_join(cognitionTB
    , by = c('ID','time'))
data<-data %>% filter(time %in% c(1,2,3))
cnames=colnames(cognitionTB)[3:34]
data$time=as.factor(data$time)
data$group=as.factor(data$group)
data$age=scale(data$age,center = T,scale=F)
data$education=scale(data$education,center = T,scale=F)
data$progression=scale(data$progression,center = T,scale=F)
data <- within(data, group <- relevel(group, ref = "Placebo"))
data <- within(data, time <- relevel(time, ref = 1))
data$severity=as.factor(data$severity)

lmmsum=GTsum=TGsum=Gsum=NULL

for(i in cnames)
{
  y=i
  t=mdG=tG=mdGT=mdTG=tGT=tTG=lmerfmodel=screenlmer=b=est=mp=NULL
  
 
  try({
    lmerfmodel=
      as.formula(paste0(y,"~group*time+age+severity+education+progression+(1|ID)"))
      screenlmer=lmer(lmerfmodel,data=data)
     
      
      b=mixed(lmerfmodel,data = data,check_contrasts = F,
                          method = "PB", args_test = list(nsim = 1000))

      
      PBsum=summary(b)
      PBsum=PBsum[["coefficients"]]
      t$PB=PBsum[,5]
      conf=conf_int(screenlmer, vcov = "CR0",level=0.95,
                    test = "z",)
      t$lci=conf$CI_L
      t$uci=conf$CI_U
      t$estR=conf$beta
      t$SER=conf$SE
  
    })      
  try({est=mp=cc=NA
       est <- emmeans(b, revpairwise ~ time|group,data=data, adjust="sidak")
       mp=print(est$contrasts,side=">")
       tTG=mp
       tTG$variable=y
       cc=confint(est$contrasts)
       tTG$lcl=cc$lower.CL
       tTG$ucl=cc$upper.CL
     })
     try({est=mp=cc=NA
       est <- emmeans(b, revpairwise ~ group |time,data=data, adjust="sidak")
       mp=print(est$contrasts,side=">")
       tGT=mp
       tGT$variable=y
       cc=confint(est$contrasts)
       tGT$lcl=cc$lower.CL
       tGT$ucl=cc$upper.CL
     })
    
    
    try({lmmsum=rbind(lmmsum,t)
    GTsum=rbind(GTsum,tGT)
    TGsum=rbind(TGsum,tTG)})
    
  
}

 write.csv(lmmsum,"cogLME_main.csv",row.names = F)
 write.csv(GTsum,"cogLME_groupTime.csv",row.names = F)
 write.csv(TGsum,"cogLME_timeGroup.csv",row.names = F)