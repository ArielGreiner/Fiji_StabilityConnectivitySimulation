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

setwd("~/Dropbox/Github/PhDProjects/Fiji_StabilityConnectivitySimulation/")


###How much better could our decision (re: management intervention) be if we knew the grazing rate?
#just want the final average coral cover across a variety of scenarios

numreefs <- 75
r <- 6500 #rate coral recruits onto/overgrows turf algae
gamma <- 1900 #rate macroalgae recruits onto/overgrows turf algae
connectivs <- c("small", "original", "largest")
buffsize <- c("original", "qoliqoli", "5km") #buffsize <- c(0.02,"qoliqoli") 
altsed <- c("baseline", "10_alt", "25_alt")

#LOAD IN THE FINAL VALUES
#conn = 1 = smallest, = 2 = original, = 3 = largest
finalavgcov <- data.frame(graz = rep(seq(1,3,1), each = 3), conn = seq(1,3,1), altsedten = NA, altsedtwentyfive = NA, tabu_qoliqoli_dvar = NA,  tabu_0.05_dvar = NA, altsed10_qoliqolitabu = NA, altsed25_qoliqolitabu = NA)
over30cov <- data.frame(graz = rep(seq(1,3,1), each = 3), conn = seq(1,3,1), altsedten = NA, altsedtwentyfive = NA, tabu_qoliqoli_dvar = NA,  tabu_0.05_dvar = NA, altsed10_qoliqolitabu = NA, altsed25_qoliqolitabu = NA)
for(j in 1:3){ #look across the 3 grazing rates, only looking at the dvar ones
  for(i in 1:3){
  if(i == 2){
  #tabu qoli qoli
  tabu_qoliqoli_dvar_data <- readRDS(file =paste0("Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
  tabu_qoliqoli_dvar_benthic_traj <- tabu_qoliqoli_dvar_data[[3]]
  finalavgcov$tabu_qoliqoli_dvar[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(tabu_qoliqoli_dvar_benthic_traj$final_coralcover)
  over30cov$tabu_qoliqoli_dvar[over30cov$graz == j & finalavgcov$conn == i] <- length(tabu_qoliqoli_dvar_benthic_traj$final_coralcover[tabu_qoliqoli_dvar_benthic_traj$final_coralcover > 0.3])
  
  #tabu 5km
  tabu_0.05_dvar_data <- readRDS(file =paste0("Final_Simulations/MainTextSims/MainText_Tabu5km_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
  tabu_0.05_dvar_benthic_traj <- tabu_0.05_dvar_data[[3]]
  finalavgcov$tabu_0.05_dvar[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(tabu_0.05_dvar_benthic_traj$final_coralcover)
  over30cov$tabu_0.05_dvar[over30cov$graz == j & finalavgcov$conn == i] <- length(tabu_0.05_dvar_benthic_traj$final_coralcover[tabu_0.05_dvar_benthic_traj$final_coralcover > 0.3])
  
  #10% alt sed
  altsed_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
  altsed_benthic_traj <- altsed_data[[3]]
  finalavgcov$altsedten[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed_benthic_traj$final_coralcover)
  over30cov$altsedten[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed_benthic_traj$final_coralcover[altsed_benthic_traj$final_coralcover > 0.3])
  
  #25% alt sed
  altsed25_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
  altsed25_benthic_traj <- altsed25_data[[3]]
  finalavgcov$altsedtwentyfive[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed25_benthic_traj$final_coralcover)
  over30cov$altsedtwentyfive[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed25_benthic_traj$final_coralcover[altsed25_benthic_traj$final_coralcover > 0.3])
  
  #10% + qoli qoli
  altsed_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
  altsed_qoliqoli_benthic_traj <- altsed_qoliqoli_dvar_data[[3]]
  finalavgcov$altsed10_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed_qoliqoli_benthic_traj$final_coralcover)
  over30cov$altsed10_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed_qoliqoli_benthic_traj$final_coralcover[altsed_qoliqoli_benthic_traj$final_coralcover > 0.3])
  
  #25% + qoli qoli 
  altsed25_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
  altsed25_qoliqoli_benthic_traj <- altsed25_qoliqoli_dvar_data[[3]]
  finalavgcov$altsed25_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed25_qoliqoli_benthic_traj$final_coralcover)
  over30cov$altsed25_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed25_qoliqoli_benthic_traj$final_coralcover[altsed25_qoliqoli_benthic_traj$final_coralcover > 0.3])
  
  tabu_qoliqoli_dvar_data <- tabu_0.05_dvar_data <- altsed_data <- altsed25_data <- altsed_qoliqoli_dvar_data <- altsed25_qoliqoli_dvar_data <- NA
  next
  }
  
  #other two connectivity scenarios
    #tabu qoli qoli
    k = 1
    q = 1
    tabu_qoliqoli_dvar_data <- readRDS(file=paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/",connectivs[i],"connectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    tabu_qoliqoli_dvar_benthic_traj <- tabu_qoliqoli_dvar_data[[3]]
    finalavgcov$tabu_qoliqoli_dvar[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(tabu_qoliqoli_dvar_benthic_traj$final_coralcover)
    over30cov$tabu_qoliqoli_dvar[over30cov$graz == j & finalavgcov$conn == i] <- length(tabu_qoliqoli_dvar_benthic_traj$final_coralcover[tabu_qoliqoli_dvar_benthic_traj$final_coralcover > 0.3])
    
    #tabu 5km
    k=3
    q=1
    tabu_0.05_dvar_data <- readRDS(file =paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/",connectivs[i],"connectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    tabu_0.05_dvar_benthic_traj <- tabu_0.05_dvar_data[[3]]
    finalavgcov$tabu_0.05_dvar[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(tabu_0.05_dvar_benthic_traj$final_coralcover)
    over30cov$tabu_0.05_dvar[over30cov$graz == j & finalavgcov$conn == i] <- length(tabu_0.05_dvar_benthic_traj$final_coralcover[tabu_0.05_dvar_benthic_traj$final_coralcover > 0.3])
    
    #10% alt sed
    k=1
    q=2
    altsed_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/",connectivs[i],"connectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed_benthic_traj <- altsed_data[[3]]
    finalavgcov$altsedten[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed_benthic_traj$final_coralcover)
    over30cov$altsedten[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed_benthic_traj$final_coralcover[altsed_benthic_traj$final_coralcover > 0.3])
    
    #25% alt sed
    k=1
    q=3
    altsed25_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/",connectivs[i],"connectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed25_benthic_traj <- altsed25_data[[3]]
    finalavgcov$altsedtwentyfive[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed25_benthic_traj$final_coralcover)
    over30cov$altsedtwentyfive[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed25_benthic_traj$final_coralcover[altsed25_benthic_traj$final_coralcover > 0.3])
    
    #10% + qoli qoli
    k=2
    q=2
    altsed_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/",connectivs[i],"connectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed_qoliqoli_benthic_traj <- altsed_qoliqoli_dvar_data[[3]]
    finalavgcov$altsed10_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed_qoliqoli_benthic_traj$final_coralcover)
    over30cov$altsed10_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed_qoliqoli_benthic_traj$final_coralcover[altsed_qoliqoli_benthic_traj$final_coralcover > 0.3])
    
    #25% + qoli qoli 
    k=2
    q=3
    altsed25_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/",connectivs[i],"connectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed25_qoliqoli_benthic_traj <- altsed25_qoliqoli_dvar_data[[3]]
    finalavgcov$altsed25_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == i] <- mean(altsed25_qoliqoli_benthic_traj$final_coralcover)
    over30cov$altsed25_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == i] <- length(altsed25_qoliqoli_benthic_traj$final_coralcover[altsed25_qoliqoli_benthic_traj$final_coralcover > 0.3])
    
    tabu_qoliqoli_dvar_data <- tabu_0.05_dvar_data <- altsed_data <- altsed25_data <- altsed_qoliqoli_dvar_data <- altsed25_qoliqoli_dvar_data <- NA
  }
}

#DIDNT EDIT BELOW HERE

over30cov_percent <- (over30cov/numreefs)*100

#average across scenarios
mean(finalavgcov$altsedten) #0.08454472
mean(finalavgcov$altsedtwentyfive) #0.1081352
mean(finalavgcov$tabu_qoliqoli_dvar) #0.07769963
mean(finalavgcov$tabu_0.05_dvar) #0.08074997
mean(finalavgcov$altsed10_qoliqolitabu) #0.08871183
mean(finalavgcov$altsed25_qoliqolitabu) #0.1145077

#average across mgmt strategies 
finalavgcov_abr <- finalavgcov[,-1]
rowMeans(finalavgcov_abr) #0.01435021 0.12290531 0.13991896
#didn't work
#mean(finalavgcov[1,c(2,3,4,5,6,7)])
#mean(finalavgcov[finalavgcov$graz == 2])
