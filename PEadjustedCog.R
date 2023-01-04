#Author Fuleah A Razzaq
library(tidyverse)#load libraries
library(sjlabelled)
library(purrr)
library(readr)
library(dplyr)
library(vtable)
library(nlme)
library(data.table)
library(EMAtools)
library(effectsize)
library(effsize)
library(gsubfn)
rm(list = ls(all.names = TRUE)) #clear memory
setwd('')#set working directory
load("./outReverse/Data/tidyEPOdataRev.Rdata")
cnames=colnames(cognitionTB)[3:34]
#####prepare data wide format #####
cognitionTBW<- subTimeTB  %>%
  inner_join(cognitionTB, by = c('ID','time'))

cognitionTBW<-cognitionTBW %>% #change format from long to wide
  pivot_wider(names_from = 'time',
              values_from = c(Dose:B_A))


#combine CognitionTB with subject attributes
cognitionTBW<- subTB  %>%
  inner_join(cognitionTBW, by = 'ID')
####### Compute practice effects and update data #####
cohenval=tibble(variable=NA,d2=NA,d3=NA)
for(i in cnames)
{
  #initialize loop variables
  res=m2=m3=p2=p3=d2=d3=pdlm2=pdlm3=y.1=y.2=y.3=y.4=test=yhat2=yhat3=yhat4=NULL
  SG2=SG3=SG4=MSD2=MSD3=MSD4=MSDdf=NULL
  y.1= paste0(i,"_1")
  y.2= paste0(i,"_2")
  y.3= paste0(i,"_3")
  y.4= paste0(i,"_4")
  #lm models
  m2=as.formula(paste0(y.2,"~",y.1,
                       "+age_2+education+as.factor(severity)+progression_2"))
  
  m3=as.formula(paste0(y.3,"~",y.1,
                       "+age_3+education+as.factor(severity)+progression_3"))
  #Run regressions
  pdlm2=lm(m2,data=cognitionTBW[cognitionTBW$Dose_2==0,])
  pdlm3=lm(m3,data=cognitionTBW[cognitionTBW$Dose_2==0,])
  #compute cohen d for y.1 in both regressions
  d2=d3=6
  try({
    p2=parameters::model_parameters(pdlm2)
    d2=t_to_d(p2$t[2], df_error = p2$df_error[2])[[1]]
    p3=parameters::model_parameters(pdlm3)
    d3=t_to_d(p3$t[2], df_error = p3$df_error[2])[[1]]
    })
  #compute yhat
  test=data.frame(cognitionTBW[,y.1],age_3=cognitionTBW$age_4,
                   education=cognitionTBW$education,
                   severity=as.factor(cognitionTBW$severity),
                   progression_3=cognitionTBW$progression_4)
  
  yhat2=predict.lm(pdlm2,cognitionTBW)
  yhat3=predict.lm(pdlm3,cognitionTBW)
  yhat4=predict.lm(pdlm3,test)
  
  #score gain without effects
  SG2=cognitionTBW[,y.2]-yhat2#-y.1
  SG3=cognitionTBW[,y.3]-yhat3#-y.1
  SG4=cognitionTBW[,y.4]-yhat4#-y.1
  #actual score without PE
  MSD2=cognitionTBW[,y.1]+SG2
  MSD3=cognitionTBW[,y.1]+SG3
  MSD4=cognitionTBW[,y.1]+SG4
  ####dont correct for PE if cohen'd is less than 0.5
  if(abs(d2<0.5)){  MSD2=cognitionTBW[,y.2] }
  if(abs(d3<0.5)){  MSD3=cognitionTBW[,y.3]
  MSD4=cognitionTBW[,y.4]}
  #assign in tibble
  MSDdf=data.frame(ID=cognitionTBW$ID,t1=cognitionTBW[,y.1]*1.0,
               t2=MSD2*1.0, t3=MSD3*1.0,t4=MSD4*1.0)
  colnames(MSDdf)=c("ID","t1","t2","t3","t4")
  MSDdf<-MSDdf %>%
    pivot_longer(cols = 2:5, # tidyselect functions help us pick columns based on name patterns
                 names_to = 'time',
                 names_prefix = 't', # Remove the "t" at the start of the column names
                 values_to ="v")
  
  cognitionTB[cognitionTB$ID==MSDdf$ID & cognitionTB$time==MSDdf$time,i]=
    MSDdf$v
  cohenval<-bind_rows(cohenval,c(variable=i,d2=d2,d3=d3))
}

cohenval<-cohenval[-1,]
  
 
###### Save data and description #######

cohenval<- cohenval  %>% 
  var_labels(
    variable = "Key column:Name of cognitive variable",
    d2 = "Cohen's d effect size for PE of T1 on T2",
    d3 = "Cohen's d effect size for PE of T1 on T3"
  )

cognitionTB<- cognitionTB  %>% 
  var_labels(
    ID = "Key column:Numeric ID extracted from raw Subject Code",
    time = "Key column: 1=baseline(T1),2= Week7(T2),3=Week30(T3),4=Week79(T4)",
    MMSE = "MMSE total Score(PE adjusted)",
    Atention = "DRS sub-score(PE adjusted)",
    Initiation = "DRS sub-score(PE adjusted)",
    Construction = "DRS sub-score(PE adjusted)",
    Conceptualization = "DRS sub-score(PE adjusted)",
    DRS_Memory = "DRS sub-score(PE adjusted)",
    DRS = "DRS total-score(PE adjusted)",              
    Arithmetic = "Working Memory WAIS III sub-score(PE adjusted)",
    digitspan = "Working Memory WAIS III sub-score(PE adjusted)",
    sequency = "Working Memory WAIS III sub-score(PE adjusted)",         
    Trial1 = "RAVLT sub-score(PE adjusted)",
    Sum = "RAVLT sub-score (Sum Trial1 to Trial5)(PE adjusted)",          
    ListB = "RAVLT sub-score(PE adjusted)",     
    recall = "RAVLT sub-score(PE adjusted)",
    Recognition = "RAVLT sub-score(PE adjusted)",
    Copy = "Rey Complex Figure sub-score(PE adjusted)",              
    Memory =  "Rey Complex Figure sub-score(PE adjusted)",
    FAB = "Frontal Assessment Battery total score(PE adjusted)",
    Phonemic = "Verbal fluency sub-score(PE adjusted)",         
    fluency = "Verbal fluency sub-score(PE adjusted)",
    Switch = "Verbal fluency sub-score(PE adjusted)",
    colour = "Stroop sub-score(Reverse coded & PE adjusted)",        
    colours_errors = "Stroop sub-score(Reverse coded & PE adjusted)",
    word = "Stroop sub-score(Reverse coded & PE adjusted)",
    word_errors = "Stroop sub-score(Reverse coded & PE adjusted)",      
    Inhibition = "Stroop sub-score(Reverse coded & PE adjusted)",
    Inhibition_errors = "Stroop sub-score(Reverse coded & PE adjusted)",
    inhibition_switch = "Stroop sub-score(Reverse coded & PE adjusted)",
    total_errors = "Stroop sub-score(Reverse coded & PE adjusted)",
    total_time = "Trail Making Test sub-score(Reverse coded & PE adjusted)",        
    B_time = "Trail Making Test sub-score(Reverse coded & PE adjusted)",           
    B_A = "Trail Making Test sub-score(Reverse coded & PE adjusted)"  
  ) 

#check directories
dir <- file.path("outPE") 
if (!dir.exists(dir)) dir.create(dir)
dir <- file.path("outPE","Description") 
if (!dir.exists(dir)) dir.create(dir)
dir <- file.path("outPE","Data") 
if (!dir.exists(dir)) dir.create(dir)

vt(cognitionTB,lush=T,file="./outPE/Description/cognition_Desc")
vt(cognitionTB,lush=T,out="csv",file="./outPE/Description/cognition_Desc.csv")

save(cognitionTB,file="./outPE/Data/tidyEPOCogPE.Rdata")
write.table(cognitionTB , file = "./outPE/Data/cognitionTB.csv", sep=",", row.names=FALSE)
write.table(cohenval , file = "./outPE/Data/cohenvalues.csv", sep=",", row.names=FALSE)

