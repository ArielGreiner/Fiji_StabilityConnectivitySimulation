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

###How much better could our decision (re: management intervention) be if we knew the grazing rate?
#just want the final average coral cover across a variety of scenarios

numreefs <- 75
r <- 125 #rate coral recruits onto/overgrows turf algae
gamma <- 900 #rate macroalgae recruits onto/overgrows turf algae

#LOAD IN THE FINAL VALUES
finalavgcov <- data.frame(graz = seq(1,4,1), altsed = NA, tabu_0.02_dvar = NA,  tabu_0.05_dvar = NA, tabu_qoliqoli_dvar = NA, altsed_0.02tabu = NA, altsed_qoliqolitabu = NA)
over30cov <- data.frame(graz = seq(1,4,1), altsed = NA, tabu_0.02_dvar = NA,  tabu_0.05_dvar = NA, tabu_qoliqoli_dvar = NA, altsed_0.02tabu = NA, altsed_qoliqolitabu = NA)
for(j in 1:4){ #look across the 4 grazing rates, only looking at the dvar ones
  #tabu 2km
  tabu_0.02_dvar_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.02degreebuffer/TabuScenario0.02_fullsimrun_allherbs_dval_sed_determined_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
  tabu_0.02_dvar_benthic_traj <- tabu_0.02_dvar_data[[3]]
  finalavgcov$tabu_0.02_dvar[finalavgcov$graz == j] <- mean(tabu_0.02_dvar_benthic_traj$final_coralcover)
  over30cov$tabu_0.02_dvar[over30cov$graz == j] <- length(tabu_0.02_dvar_benthic_traj$final_coralcover[tabu_0.02_dvar_benthic_traj$final_coralcover > 0.3])

  #tabu 5km
  tabu_0.05_dvar_data <- readRDS(file =paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.05degreebuffer/TabuScenario0.05_fullsimrun_allherbs_dval_sed_determined_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
  tabu_0.05_dvar_benthic_traj <- tabu_0.05_dvar_data[[3]]
  finalavgcov$tabu_0.05_dvar[finalavgcov$graz == j] <- mean(tabu_0.05_dvar_benthic_traj$final_coralcover)
  over30cov$tabu_0.05_dvar[over30cov$graz == j] <- length(tabu_0.05_dvar_benthic_traj$final_coralcover[tabu_0.05_dvar_benthic_traj$final_coralcover > 0.3])
  
  #tabu qoli qoli
  tabu_qoliqoli_dvar_data <- readRDS(file =paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/qoliqolidegreebuffer/TabuScenarioqoliqoli_fullsimrun_allherbs_dval_sed_determined_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_7.1.2022.rds"))
  tabu_qoliqoli_dvar_benthic_traj <- tabu_qoliqoli_dvar_data[[3]]
  finalavgcov$tabu_qoliqoli_dvar[finalavgcov$graz == j] <- mean(tabu_qoliqoli_dvar_benthic_traj$final_coralcover)
  over30cov$tabu_qoliqoli_dvar[over30cov$graz == j] <- length(tabu_qoliqoli_dvar_benthic_traj$final_coralcover[tabu_qoliqoli_dvar_benthic_traj$final_coralcover > 0.3])
  
  #10% alt sed
  altsed_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/alt/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_alt_5.10.2022.rds"))
  altsed_benthic_traj <- altsed_data[[3]]
  finalavgcov$altsed[finalavgcov$graz == j] <- mean(altsed_benthic_traj$final_coralcover)
  over30cov$altsed[over30cov$graz == j] <- length(altsed_benthic_traj$final_coralcover[altsed_benthic_traj$final_coralcover > 0.3])
  
  #10% + 2km
  altsed_0.02_dvar_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AltSed_NewTabu_7.2022/MixedScenario_Tabu0.02_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_7.4.2022.rds"))
  altsed_0.02_benthic_traj <- altsed_0.02_dvar_data[[3]]
  finalavgcov$altsed_0.02tabu[finalavgcov$graz == j] <- mean(altsed_0.02_benthic_traj$final_coralcover)
  over30cov$altsed_0.02tabu[over30cov$graz == j] <- length(altsed_0.02_benthic_traj$final_coralcover[altsed_0.02_benthic_traj$final_coralcover > 0.3])
  
  #10% + qoli qoli
  altsed_qoliqoli_dvar_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AltSed_NewTabu_7.2022/MixedScenario_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_7.4.2022.rds"))
  altsed_qoliqoli_benthic_traj <- altsed_qoliqoli_dvar_data[[3]]
  finalavgcov$altsed_qoliqolitabu[finalavgcov$graz == j] <- mean(altsed_qoliqoli_benthic_traj$final_coralcover)
  over30cov$altsed_qoliqolitabu[over30cov$graz == j] <- length(altsed_qoliqoli_benthic_traj$final_coralcover[altsed_qoliqoli_benthic_traj$final_coralcover > 0.3])
}

over30cov_percent <- (over30cov/75)*100

#average across scenarios
mean(finalavgcov$altsed) #0.262182
mean(finalavgcov$tabu_0.02_dvar) #0.2601483
mean(finalavgcov$tabu_0.05_dvar) #0.2701899
mean(finalavgcov$tabu_qoliqoli_dvar) #0.2600327
mean(finalavgcov$altsed_0.02tabu) #0.2739645
mean(finalavgcov$altsed_qoliqolitabu) #0.2737683

#average across mgmt strategies 
finalavgcov_abr <- finalavgcov[,-1]
rowMeans(finalavgcov_abr) #0.01507326 0.05093359 0.48396836 0.51688199
#didn't work
#mean(finalavgcov[1,c(2,3,4,5,6,7)])
#mean(finalavgcov[finalavgcov$graz == 2])

#if i remove the 2km strategies
finalavgcov_abr_no2km <- finalavgcov_abr[,-c(2,5)]
rowMeans(finalavgcov_abr_no2km) #0.01480605 0.05028077 0.48420414 0.51688199
(max(finalavgcov_abr_no2km[1,]) + max(finalavgcov_abr_no2km[2,]) + max(finalavgcov_abr_no2km[3,]) + max(finalavgcov_abr_no2km[4,]))/4 #0.2745655

