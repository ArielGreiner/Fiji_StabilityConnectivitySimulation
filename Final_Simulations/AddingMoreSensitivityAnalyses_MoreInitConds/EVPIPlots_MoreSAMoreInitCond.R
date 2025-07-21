library(fields)
library(mapdata)
library(sp)
library(rgdal)
library(dplyr)
library(fields)
library(mapdata)
library(sp)
library(rgdal)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)
library(deSolve)
library(geometry)
library(maps)
library(ggplot2)
library(viridis)
library(plyr)
#library(Hmisc) #won't work
library(ggbeeswarm)
#devtools::install_github("eclarke/ggbeeswarm")

###What effect did the management scenarios have on the system?
#only looking at the dvar scenarios
#tabu qoli qoli, tabu 5km, 10% alt sed, 25% alt sed, qoli qoli + 10%, qoli qoli + 25%

setwd("~/Dropbox/Github/PhDProjects/Fiji_StabilityConnectivitySimulation/")

a_vals <- c(0.05,0.07,0.15,0.2) #only need the new values
d_meds <- c(0.12,0.16,0.36,0.48) #only need the new values
initconds <- c(0.1,0.8,0.1,0.8)
initconds_more <- c(0.01,0.98)
varying_a <- data.frame(a = rep(a_vals, each = 3), d = 0.24, g = rep(c(1,2,3),length(a_vals)), C_init = NA, M_init = NA, vary = NA)
varying_d <- data.frame(a = 0.1, d = rep(d_meds, each = 3), g = rep(c(1,2,3),length(d_meds)), C_init = NA, M_init = NA, vary = NA)
varying_initcond <- data.frame(a = 0.1, d = 0.24, g = rep(c(1,2,3),length(initconds)), C_init = rep(initconds, each = 3), M_init = rep(c(0.8,0.1,0.8,0.1), each = 3), vary = rep(c("n","y"),each = 3*length(initconds)))
varying_initcond_more <- data.frame(a = 0.1, d = 0.24, g = rep(c(1,2,3),length(initconds_more)), C_init = rep(initconds_more, each = 3), M_init = rep(c(0.98,0.01), each = 3), vary = "n")
paramsies <- rbind(varying_a, varying_d, varying_initcond,varying_initcond_more)
#3 instead of 'length(scalingfactors)


numreefs <- 75
r <- 6500 #rate coral recruits onto/overgrows turf algae
gamma <- 1900 #rate macroalgae recruits onto/overgrows turf algae
buffsize <- c("original", "qoliqoli", "5km") 
altsed <- c("baseline", "10_alt", "25_alt")
zerocutoff <- 0.01

scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")
over30cov <- data.frame(sensitivs = rep(seq(1,((dim(paramsies)[1])/3),1), each = length(scens)*3), graz = rep(rep(c(1,2,3),each=length(scens)), ((dim(paramsies)[1])/3)), scenario = rep(scens, ((dim(paramsies)[1])/3)), numover30=NA)
#^ /3 because don't need every grazing scenario
sensitv <- c(rep(seq(1,18,1), each = 3)) #changed from 16 -> 18 to reflect the addition of two more initial conditions
#sensitvs <- data.frame(sens = seq(1,max(sensitv),1) ,a = c(0.05,0.07,0.15,0.2,rep(0.1,12)), d = c(rep(0.24,4),0.12,0.16,0.36,0.48, rep(0.24,8)), C_init = c(rep(NA,8),rep(c(0.1,0.8),4)), M_init = c(rep(NA,8),rep(c(0.8,0.1),4)), vary = c(rep(NA,8), rep("n",4), rep("y",4)))


#saveRDS(datalist, file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_4.9.2025.rds")) 

for(j in 1:dim(paramsies)[1]){
  for(k in 1:length(scens)){
    
    if(j < 25){
      inity <- "default"
      date <- "2.20.2025"}
    if(j > 24 & j < 37){
      inity <- paramsies$C_init[j]
      date <- "2.20.2025"
    }
    if(j > 36 & j < 49){
      inity <- paste0("vary",paramsies$C_init[j])
      date <- "4.9.2025"
    }
    if(j > 48){
      inity <- paramsies$C_init[j]
      date <- "5.15.2025"
    }
    
    if(j< 49){
    #load in data
    #base_dvar
    if(k == 1){ 
      data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds"))
    }
    
    #10altsed
    if(k == 2){
      data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds"))
    }
    
    #25altsed
    if(k == 3){
      data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
    }
    
    #tabu_qoliqoli_dvar
    if(k == 4){
      data <- readRDS(file=paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
    }
    
    #tabu_0.05_dvar
    if(k == 5){
      data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabu5km_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
    }
    
    #10% + qoli qoli
    if(k == 6){
      data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
    }
    
    #25% + qoli qoli
    if(k == 7){
      data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
    }
    }
    
    if(j > 48){
      sf <- j-48
      #load in data
      #base_dvar
      if(k == 1){ 
        data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds"))
      }
      
      #10altsed
      if(k == 2){
        data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds"))
      }
      
      #25altsed
      if(k == 3){
        data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
      }
      
      #tabu_qoliqoli_dvar
      if(k == 4){
        data <- readRDS(file=paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
      }
      
      #tabu_0.05_dvar
      if(k == 5){
        data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabu5km_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
      }
      
      #10% + qoli qoli
      if(k == 6){
        data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
      }
      
      #25% + qoli qoli
      if(k == 7){
        data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
      }
    }
    
    benthic_traj <- data[[3]]
    over30cov$numover30[over30cov$graz == paramsies$g[j] & over30cov$scenario == scens[k] & over30cov$sensitivs == sensitv[j]] <- length(benthic_traj$final_coralcover[benthic_traj$final_coralcover > 0.3])
     
    #just in case
    benthic_traj <- NULL
    data <- NULL
    date <- NULL
    inity <- NULL 
  }
}
