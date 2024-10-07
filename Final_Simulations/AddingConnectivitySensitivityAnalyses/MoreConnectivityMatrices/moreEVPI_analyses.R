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
connectivs <- c("weighted", 10,15,20,30,55,65)
buffsize <- c("original", "qoliqoli", "5km") #buffsize <- c(0.02,"qoliqoli") 
altsed <- c("baseline", "10_alt", "25_alt")

#LOAD IN THE FINAL VALUES
finalavgcov <- data.frame(graz = rep(seq(1,3,1), each = length(connectivs)), conn = connectivs, altsedten = NA, altsedtwentyfive = NA, tabu_qoliqoli_dvar = NA,  tabu_0.05_dvar = NA, altsed10_qoliqolitabu = NA, altsed25_qoliqolitabu = NA)
over30cov <- data.frame(graz = rep(seq(1,3,1), each = length(connectivs)), conn = connectivs, altsedten = NA, altsedtwentyfive = NA, tabu_qoliqoli_dvar = NA,  tabu_0.05_dvar = NA, altsed10_qoliqolitabu = NA, altsed25_qoliqolitabu = NA)
for(j in 1:3){ #look across the 3 grazing rates, only looking at the dvar ones
  for(i in 1:length(connectivs)){
    if(i == 1){
      #tabu qoli qoli
      tabu_qoliqoli_dvar_data <- readRDS(file =paste0("Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
      tabu_qoliqoli_dvar_benthic_traj <- tabu_qoliqoli_dvar_data[[3]]
      finalavgcov$tabu_qoliqoli_dvar[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(tabu_qoliqoli_dvar_benthic_traj$final_coralcover)
      over30cov$tabu_qoliqoli_dvar[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(tabu_qoliqoli_dvar_benthic_traj$final_coralcover[tabu_qoliqoli_dvar_benthic_traj$final_coralcover > 0.3])
      
      #tabu 5km
      tabu_0.05_dvar_data <- readRDS(file =paste0("Final_Simulations/MainTextSims/MainText_Tabu5km_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
      tabu_0.05_dvar_benthic_traj <- tabu_0.05_dvar_data[[3]]
      finalavgcov$tabu_0.05_dvar[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(tabu_0.05_dvar_benthic_traj$final_coralcover)
      over30cov$tabu_0.05_dvar[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(tabu_0.05_dvar_benthic_traj$final_coralcover[tabu_0.05_dvar_benthic_traj$final_coralcover > 0.3])
      
      #10% alt sed
      altsed_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
      altsed_benthic_traj <- altsed_data[[3]]
      finalavgcov$altsedten[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed_benthic_traj$final_coralcover)
      over30cov$altsedten[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed_benthic_traj$final_coralcover[altsed_benthic_traj$final_coralcover > 0.3])
      
      #25% alt sed
      altsed25_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
      altsed25_benthic_traj <- altsed25_data[[3]]
      finalavgcov$altsedtwentyfive[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed25_benthic_traj$final_coralcover)
      over30cov$altsedtwentyfive[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed25_benthic_traj$final_coralcover[altsed25_benthic_traj$final_coralcover > 0.3])
      
      #10% + qoli qoli
      altsed_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
      altsed_qoliqoli_benthic_traj <- altsed_qoliqoli_dvar_data[[3]]
      finalavgcov$altsed10_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed_qoliqoli_benthic_traj$final_coralcover)
      over30cov$altsed10_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed_qoliqoli_benthic_traj$final_coralcover[altsed_qoliqoli_benthic_traj$final_coralcover > 0.3])
      
      #25% + qoli qoli 
      altsed25_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
      altsed25_qoliqoli_benthic_traj <- altsed25_qoliqoli_dvar_data[[3]]
      finalavgcov$altsed25_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed25_qoliqoli_benthic_traj$final_coralcover)
      over30cov$altsed25_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed25_qoliqoli_benthic_traj$final_coralcover[altsed25_qoliqoli_benthic_traj$final_coralcover > 0.3])
      
      tabu_qoliqoli_dvar_data <- tabu_0.05_dvar_data <- altsed_data <- altsed25_data <- altsed_qoliqoli_dvar_data <- altsed25_qoliqoli_dvar_data <- NA
      next
    }
    
    if(i == 2){
    #tabu qoli qoli
    k = 2
    q = 1
    tabu_qoliqoli_dvar_data <- readRDS(file=paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    tabu_qoliqoli_dvar_benthic_traj <- tabu_qoliqoli_dvar_data[[3]]
    finalavgcov$tabu_qoliqoli_dvar[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(tabu_qoliqoli_dvar_benthic_traj$final_coralcover)
    over30cov$tabu_qoliqoli_dvar[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(tabu_qoliqoli_dvar_benthic_traj$final_coralcover[tabu_qoliqoli_dvar_benthic_traj$final_coralcover > 0.3])
    
    #tabu 5km
    k=3
    q=1
    tabu_0.05_dvar_data <- readRDS(file =paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    tabu_0.05_dvar_benthic_traj <- tabu_0.05_dvar_data[[3]]
    finalavgcov$tabu_0.05_dvar[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(tabu_0.05_dvar_benthic_traj$final_coralcover)
    over30cov$tabu_0.05_dvar[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(tabu_0.05_dvar_benthic_traj$final_coralcover[tabu_0.05_dvar_benthic_traj$final_coralcover > 0.3])
    
    #10% alt sed
    k=1
    q=2
    altsed_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed_benthic_traj <- altsed_data[[3]]
    finalavgcov$altsedten[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed_benthic_traj$final_coralcover)
    over30cov$altsedten[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed_benthic_traj$final_coralcover[altsed_benthic_traj$final_coralcover > 0.3])
    
    #25% alt sed
    k=1
    q=3
    altsed25_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed25_benthic_traj <- altsed25_data[[3]]
    finalavgcov$altsedtwentyfive[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed25_benthic_traj$final_coralcover)
    over30cov$altsedtwentyfive[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed25_benthic_traj$final_coralcover[altsed25_benthic_traj$final_coralcover > 0.3])
    
    #10% + qoli qoli
    k=2
    q=2
    altsed_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed_qoliqoli_benthic_traj <- altsed_qoliqoli_dvar_data[[3]]
    finalavgcov$altsed10_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed_qoliqoli_benthic_traj$final_coralcover)
    over30cov$altsed10_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed_qoliqoli_benthic_traj$final_coralcover[altsed_qoliqoli_benthic_traj$final_coralcover > 0.3])
    
    #25% + qoli qoli 
    k=2
    q=3
    altsed25_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
    altsed25_qoliqoli_benthic_traj <- altsed25_qoliqoli_dvar_data[[3]]
    finalavgcov$altsed25_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed25_qoliqoli_benthic_traj$final_coralcover)
    over30cov$altsed25_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed25_qoliqoli_benthic_traj$final_coralcover[altsed25_qoliqoli_benthic_traj$final_coralcover > 0.3])
    
    tabu_qoliqoli_dvar_data <- tabu_0.05_dvar_data <- altsed_data <- altsed25_data <- altsed_qoliqoli_dvar_data <- altsed25_qoliqoli_dvar_data <- NA
    next
    }
    
    #tabu qoli qoli
    k = 2
    q = 1
    tabu_qoliqoli_dvar_data <- readRDS(file=paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[i],"PLD_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
    tabu_qoliqoli_dvar_benthic_traj <- tabu_qoliqoli_dvar_data[[3]]
    finalavgcov$tabu_qoliqoli_dvar[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(tabu_qoliqoli_dvar_benthic_traj$final_coralcover)
    over30cov$tabu_qoliqoli_dvar[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(tabu_qoliqoli_dvar_benthic_traj$final_coralcover[tabu_qoliqoli_dvar_benthic_traj$final_coralcover > 0.3])
    
    #tabu 5km
    k=3
    q=1
    tabu_0.05_dvar_data <- readRDS(file =paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[i],"PLD_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
    tabu_0.05_dvar_benthic_traj <- tabu_0.05_dvar_data[[3]]
    finalavgcov$tabu_0.05_dvar[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(tabu_0.05_dvar_benthic_traj$final_coralcover)
    over30cov$tabu_0.05_dvar[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(tabu_0.05_dvar_benthic_traj$final_coralcover[tabu_0.05_dvar_benthic_traj$final_coralcover > 0.3])
    
    #10% alt sed
    k=1
    q=2
    altsed_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[i],"PLD_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
    altsed_benthic_traj <- altsed_data[[3]]
    finalavgcov$altsedten[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed_benthic_traj$final_coralcover)
    over30cov$altsedten[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed_benthic_traj$final_coralcover[altsed_benthic_traj$final_coralcover > 0.3])
    
    #25% alt sed
    k=1
    q=3
    altsed25_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[i],"PLD_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
    altsed25_benthic_traj <- altsed25_data[[3]]
    finalavgcov$altsedtwentyfive[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed25_benthic_traj$final_coralcover)
    over30cov$altsedtwentyfive[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed25_benthic_traj$final_coralcover[altsed25_benthic_traj$final_coralcover > 0.3])
    
    #10% + qoli qoli
    k=2
    q=2
    altsed_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[i],"PLD_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
    altsed_qoliqoli_benthic_traj <- altsed_qoliqoli_dvar_data[[3]]
    finalavgcov$altsed10_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed_qoliqoli_benthic_traj$final_coralcover)
    over30cov$altsed10_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed_qoliqoli_benthic_traj$final_coralcover[altsed_qoliqoli_benthic_traj$final_coralcover > 0.3])
    
    #25% + qoli qoli 
    k=2
    q=3
    altsed25_qoliqoli_dvar_data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[i],"PLD_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
    altsed25_qoliqoli_benthic_traj <- altsed25_qoliqoli_dvar_data[[3]]
    finalavgcov$altsed25_qoliqolitabu[finalavgcov$graz == j & finalavgcov$conn == connectivs[i]] <- mean(altsed25_qoliqoli_benthic_traj$final_coralcover)
    over30cov$altsed25_qoliqolitabu[over30cov$graz == j & finalavgcov$conn == connectivs[i]] <- length(altsed25_qoliqoli_benthic_traj$final_coralcover[altsed25_qoliqoli_benthic_traj$final_coralcover > 0.3])
    
    
    
    tabu_qoliqoli_dvar_data <- tabu_0.05_dvar_data <- altsed_data <- altsed25_data <- altsed_qoliqoli_dvar_data <- altsed25_qoliqoli_dvar_data <- NA
  }
}
