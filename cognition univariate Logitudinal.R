#Author Fuleah A Razzaq
library(tidyverse)#load libraries
library(readr)
library(dplyr)
library(clubSandwich)
library(afex)
library(isni)
rm(list = ls(all.names = TRUE)) #clear memory
load("")#data
cov<-subTimeTB %>%  
  inner_join(subTB, by = ('ID'))
data<-cov %>%  
  inner_join(cognitionTB, by = c('ID','time'))

cnames=colnames(cognitionTB)[3:34]
lmmsum=isnisum=NULL
for(i in cnames)
{
  
  y=i
  t=tisni=lmerfmodel=screenlmer=REML.afex.PB=PBsum=conf=result=ymodel=qnew=ckflag=dLongmissck=NULL
  ckflag=sum(is.na(data[data$time==4,i]))
  if(ckflag==26)
  {dLongmissck=as.data.frame(data%>%filter(!(time==4)))
  }else {dLongmissck=as.data.frame(data)}
  
  try({
    lmerfmodel=as.formula(paste0(y,"~Dose+age+severity+education+progression+(1|ID)"))
    screenlmer = lme4::lmer(lmerfmodel,
                            data = data)
    
    qnew = definemissingstatus(dLongmissck, id=ID, time=time, y=DRS)
    ymodel=as.formula(paste0(i,"|g_+gp_~ Dose+age+severity+progression+education"))
    result=isnilmm(as.formula(ymodel), random=~1, id=ID, data=qnew)
    tisni=as.data.frame(summary(result))
    tisni$variable=i
    tisni=rownames_to_column(tisni,"contrast")
    isnisum=rbind(isnisum,tisni)
    
    t=data.frame(coef(summary(screenlmer)))
    t=rownames_to_column(t,"contrast")
    t$variable=y
    t$c=tisni$c[1:6]
    t$isni=tisni$ISNI[1:6]
    # # 
    try({REML.afex.PB=mixed(lmerfmodel,data = data,
                            method = "PB", args_test = list(nsim = 1000))#LRT,#PB,
    })
    summary(REML.afex.PB)
    PBsum=summary(REML.afex.PB)
    PBsum=PBsum[["coefficients"]]
    t$PB=PBsum[,5]
    conf=conf_int(screenlmer, vcov = "CR0")
    t$lci=conf$CI_L
    t$uci=conf$CI_U
    
    
    lmmsum=rbind(lmmsum,t)})
}

write.csv(lmmsum,"cogLongitudinal.csv",row.names = F)
