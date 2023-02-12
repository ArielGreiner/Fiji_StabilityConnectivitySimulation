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

###What effect did the management scenarios have on the system?
#want to look at this across the 4 base scenarios and across d = 0.24 and d allowed to vary
#only want to look at the sedimentation scenario compared with the 'd allowed to vary' scenario

numreefs <- 75

#LOAD IN THE FINAL VALUES
#guide <- matrix(data = c(1,2,3,4,1,2,3,4,1,1,1,1,2,2,2,2), nrow = 8, ncol = 2) #col1 defines grazing rate, col2 defines which d scenario
finalcover <- list()

for(j in 1:4){ #dim(guide)[1]
#j <- guide[i,1] #grazing scenario
#k <- guide[i,2] #d scenario
r <- 125 #rate coral recruits onto/overgrows turf algae
gamma <- 900 #rate macroalgae recruits onto/overgrows turf algae
finalcov <- data.frame(reef = seq(1,numreefs,1), base_coral = NA, base_malg = NA, base_dvar_coral = NA, base_dvar_malg = NA, altsed_coral = NA, altsed_malg = NA, tabu_0.02_dsame_coral = NA, tabu_0.02_dsame_malg = NA, tabu_0.02_dvar_coral = NA, tabu_0.02_dvar_malg = NA, tabu_0.05_dsame_coral = NA, tabu_0.05_dsame_malg = NA, tabu_0.05_dvar_coral = NA, tabu_0.05_dvar_malg = NA)

#Load in the base scenario
k = 1
#datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
basedata <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/SimRound3_3.2022/Paramboth_varinitcond/FullSimRun/fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_3.27.2022.rds"))
base_benthic_traj <- basedata[[3]]
finalcov$base_coral <- base_benthic_traj$final_coralcover
finalcov$base_malg <- base_benthic_traj$final_malgcover

k = 2 #d allowed to vary
#datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
base_dvar_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/base/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_base_5.10.2022.rds"))
base_dvar_benthic_traj <- base_dvar_data[[3]]
finalcov$base_dvar_coral <- base_dvar_benthic_traj$final_coralcover
finalcov$base_dvar_malg <- base_dvar_benthic_traj$final_malgcover

#load altered sedimentation scenario
altsed_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/alt/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_alt_5.10.2022.rds"))
altsed_benthic_traj <- altsed_data[[3]]
finalcov$altsed_coral <- altsed_benthic_traj$final_coralcover
finalcov$altsed_malg <- altsed_benthic_traj$final_malgcover

#load 0.02 tabu scenario
#dval = 0.24
dvalz <- c("all0.24", "sed_determined")
k=1
tabu_0.02_dsame_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.02degreebuffer/TabuScenario0.02_fullsimrun_allherbs_dval_",dvalz[k],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds")) #4.26.2022
tabu_0.02_dsame_benthic_traj <- tabu_0.02_dsame_data[[3]]
finalcov$tabu_0.02_dsame_coral <- tabu_0.02_dsame_benthic_traj$final_coralcover
finalcov$tabu_0.02_dsame_malg <- tabu_0.02_dsame_benthic_traj$final_malgcover

#dval allowed to vary
k=2
tabu_0.02_dvar_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.02degreebuffer/TabuScenario0.02_fullsimrun_allherbs_dval_",dvalz[k],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
tabu_0.02_dvar_benthic_traj <- tabu_0.02_dvar_data[[3]]
finalcov$tabu_0.02_dvar_coral <- tabu_0.02_dvar_benthic_traj$final_coralcover
finalcov$tabu_0.02_dvar_malg <- tabu_0.02_dvar_benthic_traj$final_malgcover

#load 0.05 tabu scenario 
k=1
tabu_0.05_dsame_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.05degreebuffer/TabuScenario0.05_fullsimrun_allherbs_dval_",dvalz[k],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
tabu_0.05_dsame_benthic_traj <- tabu_0.05_dsame_data[[3]]
finalcov$tabu_0.05_dsame_coral <- tabu_0.05_dsame_benthic_traj$final_coralcover
finalcov$tabu_0.05_dsame_malg <- tabu_0.05_dsame_benthic_traj$final_malgcover

#dval allowed to vary
k=2
tabu_0.05_dvar_data <- readRDS(file =paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.05degreebuffer/TabuScenario0.05_fullsimrun_allherbs_dval_",dvalz[k],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
tabu_0.05_dvar_benthic_traj <- tabu_0.05_dvar_data[[3]]
finalcov$tabu_0.05_dvar_coral <- tabu_0.05_dvar_benthic_traj$final_coralcover
finalcov$tabu_0.05_dvar_malg <- tabu_0.05_dvar_benthic_traj$final_malgcover

finalcover[[j]] <- finalcov

#just in case
base_benthic_traj <- base_dvar_benthic_traj <- altsed_benthic_traj <- tabu_0.02_dsame_benthic_traj <- tabu_0.02_dvar_benthic_traj <- tabu_0.05_dsame_benthic_traj <- tabu_0.05_dvar_benthic_traj <- NULL
basedata <- base_dvar_data <- altsed_data <- tabu_0.02_dsame_data <- tabu_0.02_dvar_data <- tabu_0.05_dsame_data <- tabu_0.05_dvar_data <- NULL
}

#finalcov <- data.frame(reef = seq(1,numreefs,1), base_coral = NA, base_malg = NA, base_dvar_coral = NA, base_dvar_malg = NA, altsed_coral = NA, altsed_malg = NA, tabu_dsame_coral = NA, tabu_dsame_malg = NA, tabu_dvar_coral = NA, tabu_dvar_malg = NA)
#want to compare: base_coral to tabu_dsame_coral, base_dvar_coral to altsed_coral and base_dvar_coral to tabu_dvar_coral
genres_coral <- genres_malg <- ratiocover <- list() #i dont think these genres's do much
ratiocover_0.02_newtabus <- ratiocover_0.02_oldtabus <- ratiocover_0.05_newtabus <- ratiocover_0.05_oldtabus <- list()
#load in the 0.02 tabus
#load in the master dataset with sediment values and extended tabu grazing levels
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_5.10.2022.RData")

#newly in tabus - new_grazinglevel is different from comb_grazinglevel and not listed as being in a tabu
#which reefs are 'newly in tabus'?
benthicfish_masterdataset_sed$newtabu_0.02 <- 0
for(i in 1:numreefs){
  #not in a tabu
  if(benthicfish_masterdataset_sed$mgmt_abr[i] != "TABU"){
    if(benthicfish_masterdataset_sed$comb_grazinglevel[i] != benthicfish_masterdataset_sed$new_grazinglevel[i]){
      benthicfish_masterdataset_sed$newtabu_0.02[i] <- 1
    }
  }
}
newtabus_0.02 <- which(benthicfish_masterdataset_sed$newtabu_0.02 > 0)
oldtabus_0.02 <- which(benthicfish_masterdataset_sed$mgmt_abr == "TABU")

benthicfish_masterdataset_sed <- NULL
#load in the 0.05 tabus
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.05_5.26.2022.RData")
#which reefs are 'newly in tabus'?
benthicfish_masterdataset_sed$newtabu_0.05 <- 0
for(i in 1:numreefs){
  #not in a tabu
  if(benthicfish_masterdataset_sed$mgmt_abr[i] != "TABU"){
    if(benthicfish_masterdataset_sed$comb_grazinglevel[i] != benthicfish_masterdataset_sed$new_grazinglevel[i]){
      benthicfish_masterdataset_sed$newtabu_0.05[i] <- 1
    }
  }
}
newtabus_0.05 <- which(benthicfish_masterdataset_sed$newtabu_0.05 > 0)
oldtabus_0.05 <- which(benthicfish_masterdataset_sed$mgmt_abr == "TABU")


gen_results_coral <- data.frame(graz = c(1,2,3,4), avgcoral_base = NA, avgcoral_base_dvar = NA, avgcoral_altsed = NA, avgcoral_0.02_tabu_dsame = NA, avgcoral_0.02_tabu_dvar = NA, avgcoral_0.05_tabu_dsame = NA, avgcoral_0.05_tabu_dvar = NA, thirtycoral_base = NA, thirtycoral_base_dvar = NA, thirtycoral_altsed = NA, thirtycoral_0.02_tabu_dsame = NA, thirtycoral_0.02_tabu_dvar = NA, thirtycoral_0.05_tabu_dsame = NA, thirtycoral_0.05_tabu_dvar = NA, ratio_base = NA, ratio_base_dvar = NA, ratio_altsed = NA, ratio_0.02_tabu_dsame = NA, ratio_0.02_tabu_dvar = NA, ratio_0.05_tabu_dsame = NA, ratio_0.05_tabu_dvar = NA)

gen_results_malg <- data.frame(graz = c(1,2,3,4), avgmalg_base = NA, avgmalg_base_dvar = NA, avgmalg_altsed = NA, avgmalg_0.02_tabu_dsame = NA, avgmalg_0.02_tabu_dvar = NA, avgmalg_0.05_tabu_dsame = NA, avgmalg_0.05_tabu_dvar = NA, thirtymalg_base = NA, thirtymalg_base_dvar = NA, thirtymalg_altsed = NA, thirtymalg_0.02_tabu_dsame = NA, thirtymalg_0.02_tabu_dvar = NA, thirtymalg_0.05_tabu_dsame = NA, thirtymalg_0.05_tabu_dvar = NA)

gen_results_coral_0.02_newtabus <- data.frame(graz = c(1,2,3,4), avgcoral_base = NA, avgcoral_base_dvar = NA, avgcoral_altsed = NA, avgcoral_tabu_dsame = NA, avgcoral_tabu_dvar = NA, thirtycoral_base = NA, thirtycoral_base_dvar = NA, thirtycoral_altsed = NA, thirtycoral_tabu_dsame = NA, thirtycoral_tabu_dvar = NA, ratio_base = NA, ratio_base_dvar = NA, ratio_altsed = NA, ratio_tabu_dsame = NA, ratio_tabu_dvar = NA)

gen_results_malg_0.02_newtabus <- data.frame(graz = c(1,2,3,4), avgmalg_base = NA, avgmalg_base_dvar = NA, avgmalg_altsed = NA, avgmalg_tabu_dsame = NA, avgmalg_tabu_dvar = NA, thirtymalg_base = NA, thirtymalg_base_dvar = NA, thirtymalg_altsed = NA, thirtymalg_tabu_dsame = NA, thirtymalg_tabu_dvar = NA)

gen_results_coral_0.02_oldtabus <- data.frame(graz = c(1,2,3,4), avgcoral_base = NA, avgcoral_base_dvar = NA, avgcoral_altsed = NA, avgcoral_tabu_dsame = NA, avgcoral_tabu_dvar = NA, thirtycoral_base = NA, thirtycoral_base_dvar = NA, thirtycoral_altsed = NA, thirtycoral_tabu_dsame = NA, thirtycoral_tabu_dvar = NA, ratio_base = NA, ratio_base_dvar = NA, ratio_altsed = NA, ratio_tabu_dsame = NA, ratio_tabu_dvar = NA)

gen_results_malg_0.02_oldtabus <- data.frame(graz = c(1,2,3,4), avgmalg_base = NA, avgmalg_base_dvar = NA, avgmalg_altsed = NA, avgmalg_tabu_dsame = NA, avgmalg_tabu_dvar = NA, thirtymalg_base = NA, thirtymalg_base_dvar = NA, thirtymalg_altsed = NA, thirtymalg_tabu_dsame = NA, thirtymalg_tabu_dvar = NA)

gen_results_coral_0.05_newtabus <- data.frame(graz = c(1,2,3,4), avgcoral_base = NA, avgcoral_base_dvar = NA, avgcoral_altsed = NA, avgcoral_tabu_dsame = NA, avgcoral_tabu_dvar = NA, thirtycoral_base = NA, thirtycoral_base_dvar = NA, thirtycoral_altsed = NA, thirtycoral_tabu_dsame = NA, thirtycoral_tabu_dvar = NA, ratio_base = NA, ratio_base_dvar = NA, ratio_altsed = NA, ratio_tabu_dsame = NA, ratio_tabu_dvar = NA)

gen_results_malg_0.05_newtabus <- data.frame(graz = c(1,2,3,4), avgmalg_base = NA, avgmalg_base_dvar = NA, avgmalg_altsed = NA, avgmalg_tabu_dsame = NA, avgmalg_tabu_dvar = NA, thirtymalg_base = NA, thirtymalg_base_dvar = NA, thirtymalg_altsed = NA, thirtymalg_tabu_dsame = NA, thirtymalg_tabu_dvar = NA)

gen_results_coral_0.05_oldtabus <- data.frame(graz = c(1,2,3,4), avgcoral_base = NA, avgcoral_base_dvar = NA, avgcoral_altsed = NA, avgcoral_tabu_dsame = NA, avgcoral_tabu_dvar = NA, thirtycoral_base = NA, thirtycoral_base_dvar = NA, thirtycoral_altsed = NA, thirtycoral_tabu_dsame = NA, thirtycoral_tabu_dvar = NA, ratio_base = NA, ratio_base_dvar = NA, ratio_altsed = NA, ratio_tabu_dsame = NA, ratio_tabu_dvar = NA)

gen_results_malg_0.05_oldtabus <- data.frame(graz = c(1,2,3,4), avgmalg_base = NA, avgmalg_base_dvar = NA, avgmalg_altsed = NA, avgmalg_tabu_dsame = NA, avgmalg_tabu_dvar = NA, thirtymalg_base = NA, thirtymalg_base_dvar = NA, thirtymalg_altsed = NA, thirtymalg_tabu_dsame = NA, thirtymalg_tabu_dvar = NA)

for(i in 1:4){
finalcovr <- finalcover[[i]]
finalcovr_0.05_newtabus <- finalcovr[newtabus_0.05,]
finalcovr_0.05_oldtabus <- finalcovr[oldtabus_0.05,]
finalcovr_0.02_newtabus <- finalcovr[newtabus_0.02,]
finalcovr_0.02_oldtabus <- finalcovr[oldtabus_0.02,]

###Q1: Did average coral cover increase?
#print(paste("In scenario", i, "average final coral cover in the base scenario is",mean(finalcovr$base_coral),", in the base_dvar scenario is", mean(finalcovr$base_dvar_coral),"in the altsed scenario is", mean(finalcovr$altsed_coral),"in the tabu_dsame scenario is", mean(finalcovr$tabu_dsame_coral),"in the tabu_dvar scenario is", mean(finalcovr$tabu_dvar_coral)))
gen_results_coral$avgcoral_base[gen_results_coral$graz==i] <- mean(finalcovr$base_coral)
gen_results_coral$avgcoral_base_dvar[gen_results_coral$graz==i] <- mean(finalcovr$base_dvar_coral)
gen_results_coral$avgcoral_altsed[gen_results_coral$graz==i] <- mean(finalcovr$altsed_coral)
gen_results_coral$avgcoral_0.02_tabu_dsame[gen_results_coral$graz==i] <- mean(finalcovr$tabu_0.02_dsame_coral)
gen_results_coral$avgcoral_0.02_tabu_dvar[gen_results_coral$graz==i] <- mean(finalcovr$tabu_0.02_dvar_coral)
gen_results_coral$avgcoral_0.05_tabu_dsame[gen_results_coral$graz==i] <- mean(finalcovr$tabu_0.05_dsame_coral)
gen_results_coral$avgcoral_0.05_tabu_dvar[gen_results_coral$graz==i] <- mean(finalcovr$tabu_0.05_dvar_coral)

gen_results_coral_0.02_newtabus$avgcoral_base[gen_results_coral_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$base_coral)
gen_results_coral_0.02_newtabus$avgcoral_base_dvar[gen_results_coral_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$base_dvar_coral)
gen_results_coral_0.02_newtabus$avgcoral_altsed[gen_results_coral_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$altsed_coral)
gen_results_coral_0.02_newtabus$avgcoral_tabu_dsame[gen_results_coral_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$tabu_0.02_dsame_coral)
gen_results_coral_0.02_newtabus$avgcoral_tabu_dvar[gen_results_coral_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$tabu_0.02_dvar_coral)

gen_results_coral_0.05_newtabus$avgcoral_base[gen_results_coral_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$base_coral)
gen_results_coral_0.05_newtabus$avgcoral_base_dvar[gen_results_coral_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$base_dvar_coral)
gen_results_coral_0.05_newtabus$avgcoral_altsed[gen_results_coral_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$altsed_coral)
gen_results_coral_0.05_newtabus$avgcoral_tabu_dsame[gen_results_coral_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$tabu_0.05_dsame_coral)
gen_results_coral_0.05_newtabus$avgcoral_tabu_dvar[gen_results_coral_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$tabu_0.05_dvar_coral)

gen_results_coral_0.02_oldtabus$avgcoral_base[gen_results_coral_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$base_coral)
gen_results_coral_0.02_oldtabus$avgcoral_base_dvar[gen_results_coral_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$base_dvar_coral)
gen_results_coral_0.02_oldtabus$avgcoral_altsed[gen_results_coral_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$altsed_coral)
gen_results_coral_0.02_oldtabus$avgcoral_tabu_dsame[gen_results_coral_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$tabu_0.02_dsame_coral)
gen_results_coral_0.02_oldtabus$avgcoral_tabu_dvar[gen_results_coral_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$tabu_0.02_dvar_coral)

gen_results_coral_0.05_oldtabus$avgcoral_base[gen_results_coral_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$base_coral)
gen_results_coral_0.05_oldtabus$avgcoral_base_dvar[gen_results_coral_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$base_dvar_coral)
gen_results_coral_0.05_oldtabus$avgcoral_altsed[gen_results_coral_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$altsed_coral)
gen_results_coral_0.05_oldtabus$avgcoral_tabu_dsame[gen_results_coral_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$tabu_0.05_dsame_coral)
gen_results_coral_0.05_oldtabus$avgcoral_tabu_dvar[gen_results_coral_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$tabu_0.05_dvar_coral)

#print(paste("In scenario", i, "average final malg cover in the base scenario is",mean(finalcovr$base_malg),", in the base_dvar scenario is", mean(finalcovr$base_dvar_malg),"in the altsed scenario is", mean(finalcovr$altsed_malg),"in the tabu_dsame scenario is", mean(finalcovr$tabu_dsame_malg),"in the tabu_dvar scenario is", mean(finalcovr$tabu_dvar_malg)))
gen_results_malg$avgmalg_base[gen_results_malg$graz==i] <- mean(finalcovr$base_malg)
gen_results_malg$avgmalg_base_dvar[gen_results_malg$graz==i] <- mean(finalcovr$base_dvar_malg)
gen_results_malg$avgmalg_altsed[gen_results_malg$graz==i] <- mean(finalcovr$altsed_malg)
gen_results_malg$avgmalg_0.02_tabu_dsame[gen_results_malg$graz==i] <- mean(finalcovr$tabu_0.02_dsame_malg)
gen_results_malg$avgmalg_0.02_tabu_dvar[gen_results_malg$graz==i] <- mean(finalcovr$tabu_0.02_dvar_malg)
gen_results_malg$avgmalg_0.05_tabu_dsame[gen_results_malg$graz==i] <- mean(finalcovr$tabu_0.05_dsame_malg)
gen_results_malg$avgmalg_0.05_tabu_dvar[gen_results_malg$graz==i] <- mean(finalcovr$tabu_0.05_dvar_malg)

gen_results_malg_0.02_newtabus$avgmalg_base[gen_results_malg_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$base_malg)
gen_results_malg_0.02_newtabus$avgmalg_base_dvar[gen_results_malg_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$base_dvar_malg)
gen_results_malg_0.02_newtabus$avgmalg_altsed[gen_results_malg_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$altsed_malg)
gen_results_malg_0.02_newtabus$avgmalg_tabu_dsame[gen_results_malg_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$tabu_0.02_dsame_malg)
gen_results_malg_0.02_newtabus$avgmalg_tabu_dvar[gen_results_malg_0.02_newtabus$graz==i] <- mean(finalcovr_0.02_newtabus$tabu_0.02_dvar_malg)

gen_results_malg_0.05_newtabus$avgmalg_base[gen_results_malg_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$base_malg)
gen_results_malg_0.05_newtabus$avgmalg_base_dvar[gen_results_malg_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$base_dvar_malg)
gen_results_malg_0.05_newtabus$avgmalg_altsed[gen_results_malg_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$altsed_malg)
gen_results_malg_0.05_newtabus$avgmalg_tabu_dsame[gen_results_malg_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$tabu_0.05_dsame_malg)
gen_results_malg_0.05_newtabus$avgmalg_tabu_dvar[gen_results_malg_0.05_newtabus$graz==i] <- mean(finalcovr_0.05_newtabus$tabu_0.05_dvar_malg)

gen_results_malg_0.02_oldtabus$avgmalg_base[gen_results_malg_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$base_malg)
gen_results_malg_0.02_oldtabus$avgmalg_base_dvar[gen_results_malg_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$base_dvar_malg)
gen_results_malg_0.02_oldtabus$avgmalg_altsed[gen_results_malg_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$altsed_malg)
gen_results_malg_0.02_oldtabus$avgmalg_tabu_dsame[gen_results_malg_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$tabu_0.02_dsame_malg)
gen_results_malg_0.02_oldtabus$avgmalg_tabu_dvar[gen_results_malg_0.02_oldtabus$graz==i] <- mean(finalcovr_0.02_oldtabus$tabu_0.02_dvar_malg)

gen_results_malg_0.05_oldtabus$avgmalg_base[gen_results_malg_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$base_malg)
gen_results_malg_0.05_oldtabus$avgmalg_base_dvar[gen_results_malg_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$base_dvar_malg)
gen_results_malg_0.05_oldtabus$avgmalg_altsed[gen_results_malg_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$altsed_malg)
gen_results_malg_0.05_oldtabus$avgmalg_tabu_dsame[gen_results_malg_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$tabu_0.05_dsame_malg)
gen_results_malg_0.05_oldtabus$avgmalg_tabu_dvar[gen_results_malg_0.05_oldtabus$graz==i] <- mean(finalcovr_0.05_oldtabus$tabu_0.05_dvar_malg)

###Q2: Do more reefs have >30% coral cover?
gen_results_coral$thirtycoral_base[gen_results_coral$graz==i] <- length(finalcovr$base_coral[which(finalcovr$base_coral > 0.3)])
gen_results_coral$thirtycoral_base_dvar[gen_results_coral$graz==i] <- length(finalcovr$base_dvar_coral[which(finalcovr$base_dvar_coral > 0.3)])
gen_results_coral$thirtycoral_altsed[gen_results_coral$graz==i] <- length(finalcovr$altsed_coral[which(finalcovr$altsed_coral > 0.3)])
gen_results_coral$thirtycoral_0.02_tabu_dsame[gen_results_coral$graz==i] <- length(finalcovr$tabu_0.02_dsame_coral[which(finalcovr$tabu_0.02_dsame_coral > 0.3)])
gen_results_coral$thirtycoral_0.02_tabu_dvar[gen_results_coral$graz==i] <- length(finalcovr$tabu_0.02_dvar_coral[which(finalcovr$tabu_0.02_dvar_coral > 0.3)])
gen_results_coral$thirtycoral_0.05_tabu_dsame[gen_results_coral$graz==i] <- length(finalcovr$tabu_0.05_dsame_coral[which(finalcovr$tabu_0.05_dsame_coral > 0.3)])
gen_results_coral$thirtycoral_0.05_tabu_dvar[gen_results_coral$graz==i] <- length(finalcovr$tabu_0.05_dvar_coral[which(finalcovr$tabu_0.05_dvar_coral > 0.3)])


gen_results_malg$thirtymalg_base[gen_results_malg$graz==i] <- length(finalcovr$base_malg[which(finalcovr$base_malg > 0.3)])
gen_results_malg$thirtymalg_base_dvar[gen_results_malg$graz==i] <- length(finalcovr$base_dvar_malg[which(finalcovr$base_dvar_malg > 0.3)])
gen_results_malg$thirtymalg_altsed[gen_results_malg$graz==i] <- length(finalcovr$altsed_malg[which(finalcovr$altsed_malg > 0.3)])
gen_results_malg$thirtymalg_0.02_tabu_dsame[gen_results_malg$graz==i] <- length(finalcovr$tabu_0.02_dsame_coral[which(finalcovr$tabu_0.02_dsame_malg > 0.3)])
gen_results_malg$thirtymalg_0.02_tabu_dvar[gen_results_malg$graz==i] <- length(finalcovr$base_0.02_dvar_malg[which(finalcovr$tabu_0.02_dvar_malg > 0.3)])
gen_results_malg$thirtymalg_0.05_tabu_dsame[gen_results_malg$graz==i] <- length(finalcovr$tabu_0.05_dsame_coral[which(finalcovr$tabu_0.05_dsame_malg > 0.3)])
gen_results_malg$thirtymalg_0.05_tabu_dvar[gen_results_malg$graz==i] <- length(finalcovr$base_0.05_dvar_malg[which(finalcovr$tabu_0.05_dvar_malg > 0.3)])

gen_results_coral_0.02_newtabus$thirtycoral_base[gen_results_coral_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$base_coral[which(finalcovr_0.02_newtabus$base_coral > 0.3)])
gen_results_coral_0.02_newtabus$thirtycoral_base_dvar[gen_results_coral_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$base_dvar_coral[which(finalcovr_0.02_newtabus$base_dvar_coral > 0.3)])
gen_results_coral_0.02_newtabus$thirtycoral_altsed[gen_results_coral_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$altsed_coral[which(finalcovr_0.02_newtabus$altsed_coral > 0.3)])
gen_results_coral_0.02_newtabus$thirtycoral_tabu_dsame[gen_results_coral_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$tabu_0.02_dsame_coral[which(finalcovr_0.02_newtabus$tabu_0.02_dsame_coral > 0.3)])
gen_results_coral_0.02_newtabus$thirtycoral_tabu_dvar[gen_results_coral_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$tabu_0.02_dvar_coral[which(finalcovr_0.02_newtabus$tabu_0.02_dvar_coral > 0.3)])

gen_results_coral_0.05_newtabus$thirtycoral_base[gen_results_coral_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$base_coral[which(finalcovr_0.05_newtabus$base_coral > 0.3)])
gen_results_coral_0.05_newtabus$thirtycoral_base_dvar[gen_results_coral_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$base_dvar_coral[which(finalcovr_0.05_newtabus$base_dvar_coral > 0.3)])
gen_results_coral_0.05_newtabus$thirtycoral_altsed[gen_results_coral_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$altsed_coral[which(finalcovr_0.05_newtabus$altsed_coral > 0.3)])
gen_results_coral_0.05_newtabus$thirtycoral_tabu_dsame[gen_results_coral_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$tabu_0.05_dsame_coral[which(finalcovr_0.05_newtabus$tabu_0.05_dsame_coral > 0.3)])
gen_results_coral_0.05_newtabus$thirtycoral_tabu_dvar[gen_results_coral_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$tabu_0.05_dvar_coral[which(finalcovr_0.05_newtabus$tabu_0.05_dvar_coral > 0.3)])

gen_results_malg_0.02_newtabus$thirtymalg_base[gen_results_malg_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$base_malg[which(finalcovr_0.02_newtabus$base_malg > 0.3)])
gen_results_malg_0.02_newtabus$thirtymalg_base_dvar[gen_results_malg_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$base_dvar_malg[which(finalcovr_0.02_newtabus$base_dvar_malg > 0.3)])
gen_results_malg_0.02_newtabus$thirtymalg_altsed[gen_results_malg_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$altsed_malg[which(finalcovr_0.02_newtabus$altsed_malg > 0.3)])
gen_results_malg_0.02_newtabus$thirtymalg_tabu_dsame[gen_results_malg_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$tabu_0.02_dsame_malg[which(finalcovr_0.02_newtabus$tabu_0.02_dsame_malg > 0.3)])
gen_results_malg_0.02_newtabus$thirtymalg_tabu_dvar[gen_results_malg_0.02_newtabus$graz==i] <- length(finalcovr_0.02_newtabus$tabu_0.02_dvar_malg[which(finalcovr_0.02_newtabus$tabu_0.02_dvar_malg > 0.3)])

gen_results_malg_0.05_newtabus$thirtymalg_base[gen_results_malg_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$base_malg[which(finalcovr_0.05_newtabus$base_malg > 0.3)])
gen_results_malg_0.05_newtabus$thirtymalg_base_dvar[gen_results_malg_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$base_dvar_malg[which(finalcovr_0.05_newtabus$base_dvar_malg > 0.3)])
gen_results_malg_0.05_newtabus$thirtymalg_altsed[gen_results_malg_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$altsed_malg[which(finalcovr_0.05_newtabus$altsed_malg > 0.3)])
gen_results_malg_0.05_newtabus$thirtymalg_tabu_dsame[gen_results_malg_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$tabu_0.05_dsame_malg[which(finalcovr_0.05_newtabus$tabu_0.05_dsame_malg > 0.3)])
gen_results_malg_0.05_newtabus$thirtymalg_tabu_dvar[gen_results_malg_0.05_newtabus$graz==i] <- length(finalcovr_0.05_newtabus$tabu_0.05_dvar_malg[which(finalcovr_0.05_newtabus$tabu_0.05_dvar_malg > 0.3)])

gen_results_coral_0.02_oldtabus$thirtycoral_base[gen_results_coral_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$base_coral[which(finalcovr_0.02_oldtabus$base_coral > 0.3)])
gen_results_coral_0.02_oldtabus$thirtycoral_base_dvar[gen_results_coral_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$base_dvar_coral[which(finalcovr_0.02_oldtabus$base_dvar_coral > 0.3)])
gen_results_coral_0.02_oldtabus$thirtycoral_altsed[gen_results_coral_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$altsed_coral[which(finalcovr_0.02_oldtabus$altsed_coral > 0.3)])
gen_results_coral_0.02_oldtabus$thirtycoral_tabu_dsame[gen_results_coral_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$tabu_0.02_dsame_coral[which(finalcovr_0.02_oldtabus$tabu_0.02_dsame_coral > 0.3)])
gen_results_coral_0.02_oldtabus$thirtycoral_tabu_dvar[gen_results_coral_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$tabu_0.02_dvar_coral[which(finalcovr_0.02_oldtabus$tabu_0.02_dvar_coral > 0.3)])

gen_results_coral_0.05_oldtabus$thirtycoral_base[gen_results_coral_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$base_coral[which(finalcovr_0.05_oldtabus$base_coral > 0.3)])
gen_results_coral_0.05_oldtabus$thirtycoral_base_dvar[gen_results_coral_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$base_dvar_coral[which(finalcovr_0.05_oldtabus$base_dvar_coral > 0.3)])
gen_results_coral_0.05_oldtabus$thirtycoral_altsed[gen_results_coral_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$altsed_coral[which(finalcovr_0.05_oldtabus$altsed_coral > 0.3)])
gen_results_coral_0.05_oldtabus$thirtycoral_tabu_dsame[gen_results_coral_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$tabu_0.05_dsame_coral[which(finalcovr_0.05_oldtabus$tabu_0.05_dsame_coral > 0.3)])
gen_results_coral_0.05_oldtabus$thirtycoral_tabu_dvar[gen_results_coral_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$tabu_0.05_dvar_coral[which(finalcovr_0.05_oldtabus$tabu_0.05_dvar_coral > 0.3)])

gen_results_malg_0.02_oldtabus$thirtymalg_base[gen_results_malg_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$base_malg[which(finalcovr_0.02_oldtabus$base_malg > 0.3)])
gen_results_malg_0.02_oldtabus$thirtymalg_base_dvar[gen_results_malg_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$base_dvar_malg[which(finalcovr_0.02_oldtabus$base_dvar_malg > 0.3)])
gen_results_malg_0.02_oldtabus$thirtymalg_altsed[gen_results_malg_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$altsed_malg[which(finalcovr_0.02_oldtabus$altsed_malg > 0.3)])
gen_results_malg_0.02_oldtabus$thirtymalg_tabu_dsame[gen_results_malg_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$tabu_0.02_dsame_malg[which(finalcovr_0.02_oldtabus$tabu_0.02_dsame_malg > 0.3)])
gen_results_malg_0.02_oldtabus$thirtymalg_tabu_dvar[gen_results_malg_0.02_oldtabus$graz==i] <- length(finalcovr_0.02_oldtabus$tabu_0.02_dvar_malg[which(finalcovr_0.02_oldtabus$tabu_0.02_dvar_malg > 0.3)])

gen_results_malg_0.05_oldtabus$thirtymalg_base[gen_results_malg_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$base_malg[which(finalcovr_0.05_oldtabus$base_malg > 0.3)])
gen_results_malg_0.05_oldtabus$thirtymalg_base_dvar[gen_results_malg_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$base_dvar_malg[which(finalcovr_0.05_oldtabus$base_dvar_malg > 0.3)])
gen_results_malg_0.05_oldtabus$thirtymalg_altsed[gen_results_malg_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$altsed_malg[which(finalcovr_0.05_oldtabus$altsed_malg > 0.3)])
gen_results_malg_0.05_oldtabus$thirtymalg_tabu_dsame[gen_results_malg_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$tabu_0.05_dsame_malg[which(finalcovr_0.05_oldtabus$tabu_0.05_dsame_malg > 0.3)])
gen_results_malg_0.05_oldtabus$thirtymalg_tabu_dvar[gen_results_malg_0.05_oldtabus$graz==i] <- length(finalcovr_0.05_oldtabus$tabu_0.05_dvar_malg[which(finalcovr_0.05_oldtabus$tabu_0.05_dvar_malg > 0.3)])

###Q3: Are more reefs trending towards coral > macroalgae with the mgmt? 
#make a dataframe that gives the C:M ratio for all of the reefs under each situation, if >1 means that coral > malg cover
ratiocovr <- data.frame(reef = seq(1,numreefs,1), baseratio = NA, base_dvar_ratio = NA, altsed_ratio = NA, tabu_0.02_dsame_ratio = NA, tabu_0.02_dvar_ratio = NA, tabu_0.05_dsame_ratio = NA, tabu_0.05_dvar_ratio = NA)
ratiocovr_0.02_newtabus <- data.frame(reef = seq(1,length(newtabus_0.02),1), baseratio = NA, base_dvar_ratio = NA, altsed_ratio = NA, tabu_dsame_ratio = NA, tabu_dvar_ratio = NA)
ratiocovr_0.02_oldtabus <- data.frame(reef = seq(1,length(oldtabus_0.02),1), baseratio = NA, base_dvar_ratio = NA, altsed_ratio = NA, tabu_dsame_ratio = NA, tabu_dvar_ratio = NA)
ratiocovr_0.05_newtabus <- data.frame(reef = seq(1,length(newtabus_0.05),1), baseratio = NA, base_dvar_ratio = NA, altsed_ratio = NA, tabu_dsame_ratio = NA, tabu_dvar_ratio = NA)
ratiocovr_0.05_oldtabus <- data.frame(reef = seq(1,length(oldtabus_0.05),1), baseratio = NA, base_dvar_ratio = NA, altsed_ratio = NA, tabu_dsame_ratio = NA, tabu_dvar_ratio = NA)

ratiocovr$baseratio <- (1+finalcovr$base_coral)/(1+finalcovr$base_malg)
ratiocovr$base_dvar_ratio <- (1+finalcovr$base_dvar_coral)/(1+finalcovr$base_dvar_malg)
ratiocovr$altsed_ratio <- (1+finalcovr$altsed_coral)/(1+finalcovr$altsed_malg)
ratiocovr$tabu_0.02_dsame_ratio <- (1+finalcovr$tabu_0.02_dsame_coral)/(1+finalcovr$tabu_0.02_dsame_malg)
ratiocovr$tabu_0.02_dvar_ratio <- (1+finalcovr$tabu_0.02_dvar_coral)/(1+finalcovr$tabu_0.02_dvar_malg)
ratiocovr$tabu_0.05_dsame_ratio <- (1+finalcovr$tabu_0.05_dsame_coral)/(1+finalcovr$tabu_0.05_dsame_malg)
ratiocovr$tabu_0.05_dvar_ratio <- (1+finalcovr$tabu_0.05_dvar_coral)/(1+finalcovr$tabu_0.05_dvar_malg)

ratiocovr_0.02_newtabus$baseratio <- (1+finalcovr_0.02_newtabus$base_coral)/(1+finalcovr_0.02_newtabus$base_malg)
ratiocovr_0.02_newtabus$base_dvar_ratio <- (1+finalcovr_0.02_newtabus$base_dvar_coral)/(1+finalcovr_0.02_newtabus$base_dvar_malg)
ratiocovr_0.02_newtabus$altsed_ratio <- (1+finalcovr_0.02_newtabus$altsed_coral)/(1+finalcovr_0.02_newtabus$altsed_malg)
ratiocovr_0.02_newtabus$tabu_dsame_ratio <- (1+finalcovr_0.02_newtabus$tabu_0.02_dsame_coral)/(1+finalcovr_0.02_newtabus$tabu_0.02_dsame_malg)
ratiocovr_0.02_newtabus$tabu_dvar_ratio <- (1+finalcovr_0.02_newtabus$tabu_0.02_dvar_coral)/(1+finalcovr_0.02_newtabus$tabu_0.02_dvar_malg)

ratiocovr_0.02_oldtabus$baseratio <- (1+finalcovr_0.02_oldtabus$base_coral)/(1+finalcovr_0.02_oldtabus$base_malg)
ratiocovr_0.02_oldtabus$base_dvar_ratio <- (1+finalcovr_0.02_oldtabus$base_dvar_coral)/(1+finalcovr_0.02_oldtabus$base_dvar_malg)
ratiocovr_0.02_oldtabus$altsed_ratio <- (1+finalcovr_0.02_oldtabus$altsed_coral)/(1+finalcovr_0.02_oldtabus$altsed_malg)
ratiocovr_0.02_oldtabus$tabu_dsame_ratio <- (1+finalcovr_0.02_oldtabus$tabu_0.02_dsame_coral)/(1+finalcovr_0.02_oldtabus$tabu_0.02_dsame_malg)
ratiocovr_0.02_oldtabus$tabu_dvar_ratio <- (1+finalcovr_0.02_oldtabus$tabu_0.02_dvar_coral)/(1+finalcovr_0.02_oldtabus$tabu_0.02_dvar_malg)

ratiocovr_0.05_newtabus$baseratio <- (1+finalcovr_0.05_newtabus$base_coral)/(1+finalcovr_0.05_newtabus$base_malg)
ratiocovr_0.05_newtabus$base_dvar_ratio <- (1+finalcovr_0.05_newtabus$base_dvar_coral)/(1+finalcovr_0.05_newtabus$base_dvar_malg)
ratiocovr_0.05_newtabus$altsed_ratio <- (1+finalcovr_0.05_newtabus$altsed_coral)/(1+finalcovr_0.05_newtabus$altsed_malg)
ratiocovr_0.05_newtabus$tabu_dsame_ratio <- (1+finalcovr_0.05_newtabus$tabu_0.05_dsame_coral)/(1+finalcovr_0.05_newtabus$tabu_0.05_dsame_malg)
ratiocovr_0.05_newtabus$tabu_dvar_ratio <- (1+finalcovr_0.05_newtabus$tabu_0.05_dvar_coral)/(1+finalcovr_0.05_newtabus$tabu_0.05_dvar_malg)

ratiocovr_0.05_oldtabus$baseratio <- (1+finalcovr_0.05_oldtabus$base_coral)/(1+finalcovr_0.05_oldtabus$base_malg)
ratiocovr_0.05_oldtabus$base_dvar_ratio <- (1+finalcovr_0.05_oldtabus$base_dvar_coral)/(1+finalcovr_0.05_oldtabus$base_dvar_malg)
ratiocovr_0.05_oldtabus$altsed_ratio <- (1+finalcovr_0.05_oldtabus$altsed_coral)/(1+finalcovr_0.05_oldtabus$altsed_malg)
ratiocovr_0.05_oldtabus$tabu_dsame_ratio <- (1+finalcovr_0.05_oldtabus$tabu_0.05_dsame_coral)/(1+finalcovr_0.05_oldtabus$tabu_0.05_dsame_malg)
ratiocovr_0.05_oldtabus$tabu_dvar_ratio <- (1+finalcovr_0.05_oldtabus$tabu_0.05_dvar_coral)/(1+finalcovr_0.05_oldtabus$tabu_0.05_dvar_malg)

#can then sum the ratios across all of the reefs to see the trends
gen_results_coral$ratio_base[gen_results_coral$graz==i] <- numreefs - length(!which(ratiocovr$baseratio < 1)) #sum(ratiocovr$baseratio)
gen_results_coral$ratio_base_dvar[gen_results_coral$graz==i] <- numreefs - length(!which(ratiocovr$base_dvar_ratio < 1))
gen_results_coral$ratio_altsed[gen_results_coral$graz==i] <- numreefs - length(!which(ratiocovr$altsed_ratio < 1))
gen_results_coral$ratio_0.02_tabu_dsame[gen_results_coral$graz==i] <- numreefs - length(!which(ratiocovr$tabu_0.02_dsame_ratio < 1))
gen_results_coral$ratio_0.02_tabu_dvar[gen_results_coral$graz==i] <- numreefs - length(!which(ratiocovr$tabu_0.02_dvar_ratio < 1))
gen_results_coral$ratio_0.05_tabu_dsame[gen_results_coral$graz==i] <- numreefs - length(!which(ratiocovr$tabu_0.05_dsame_ratio < 1))
gen_results_coral$ratio_0.05_tabu_dvar[gen_results_coral$graz==i] <- numreefs - length(!which(ratiocovr$tabu_0.05_dvar_ratio < 1))

gen_results_coral_0.02_newtabus$ratio_base[gen_results_coral_0.02_newtabus$graz==i] <- length(newtabus_0.02) - length(!which(ratiocovr_0.02_newtabus$baseratio<1))
gen_results_coral_0.02_newtabus$ratio_base_dvar[gen_results_coral_0.02_newtabus$graz==i] <- length(newtabus_0.02) - length(!which(ratiocovr_0.02_newtabus$base_dvar_ratio<1))
gen_results_coral_0.02_newtabus$ratio_altsed[gen_results_coral_0.02_newtabus$graz==i] <- length(newtabus_0.02) - length(!which(ratiocovr_0.02_newtabus$altsed_ratio<1))
gen_results_coral_0.02_newtabus$ratio_tabu_dsame[gen_results_coral_0.02_newtabus$graz==i] <- length(newtabus_0.02) - length(!which(ratiocovr_0.02_newtabus$tabu_dsame_ratio<1))
gen_results_coral_0.02_newtabus$ratio_tabu_dvar[gen_results_coral_0.02_newtabus$graz==i] <- length(newtabus_0.02) - length(!which(ratiocovr_0.02_newtabus$tabu_dvar_ratio<1))

gen_results_coral_0.02_oldtabus$ratio_base[gen_results_coral_0.02_oldtabus$graz==i] <- length(oldtabus_0.02) - length(!which(ratiocovr_0.02_oldtabus$baseratio<1))
gen_results_coral_0.02_oldtabus$ratio_base_dvar[gen_results_coral_0.02_oldtabus$graz==i] <- length(oldtabus_0.02) - length(!which(ratiocovr_0.02_oldtabus$base_dvar_ratio<1))
gen_results_coral_0.02_oldtabus$ratio_altsed[gen_results_coral_0.02_oldtabus$graz==i] <- length(oldtabus_0.02) - length(!which(ratiocovr_0.02_oldtabus$altsed_ratio<1))
gen_results_coral_0.02_oldtabus$ratio_tabu_dsame[gen_results_coral_0.02_oldtabus$graz==i] <- length(oldtabus_0.02) - length(!which(ratiocovr_0.02_oldtabus$tabu_dsame_ratio<1))
gen_results_coral_0.02_oldtabus$ratio_tabu_dvar[gen_results_coral_0.02_oldtabus$graz==i] <- length(oldtabus_0.02) - length(!which(ratiocovr_0.02_oldtabus$tabu_dvar_ratio<1))

gen_results_coral_0.05_newtabus$ratio_base[gen_results_coral_0.05_newtabus$graz==i] <- length(newtabus_0.05) - length(!which(ratiocovr_0.05_newtabus$baseratio<1))
gen_results_coral_0.05_newtabus$ratio_base_dvar[gen_results_coral_0.05_newtabus$graz==i] <- length(newtabus_0.05) - length(!which(ratiocovr_0.05_newtabus$base_dvar_ratio<1))
gen_results_coral_0.05_newtabus$ratio_altsed[gen_results_coral_0.05_newtabus$graz==i] <- length(newtabus_0.05) - length(!which(ratiocovr_0.05_newtabus$altsed_ratio<1))
gen_results_coral_0.05_newtabus$ratio_tabu_dsame[gen_results_coral_0.05_newtabus$graz==i] <- length(newtabus_0.05) - length(!which(ratiocovr_0.05_newtabus$tabu_dsame_ratio<1))
gen_results_coral_0.05_newtabus$ratio_tabu_dvar[gen_results_coral_0.05_newtabus$graz==i] <- length(newtabus_0.05) - length(!which(ratiocovr_0.05_newtabus$tabu_dvar_ratio<1))

gen_results_coral_0.05_oldtabus$ratio_base[gen_results_coral_0.05_oldtabus$graz==i] <- length(oldtabus_0.05) - length(!which(ratiocovr_0.05_oldtabus$baseratio<1))
gen_results_coral_0.05_oldtabus$ratio_base_dvar[gen_results_coral_0.05_oldtabus$graz==i] <- length(oldtabus_0.05) - length(!which(ratiocovr_0.05_oldtabus$base_dvar_ratio<1))
gen_results_coral_0.05_oldtabus$ratio_altsed[gen_results_coral_0.05_oldtabus$graz==i] <- length(oldtabus_0.05) - length(!which(ratiocovr_0.05_oldtabus$altsed_ratio<1))
gen_results_coral_0.05_oldtabus$ratio_tabu_dsame[gen_results_coral_0.05_oldtabus$graz==i] <- length(oldtabus_0.05) - length(!which(ratiocovr_0.05_oldtabus$tabu_dsame_ratio<1))
gen_results_coral_0.05_oldtabus$ratio_tabu_dvar[gen_results_coral_0.05_oldtabus$graz==i] <- length(oldtabus_0.05) - length(!which(ratiocovr_0.05_oldtabus$tabu_dvar_ratio<1))
#genres_coral[[i]] <- gen_results_coral[gen_results_coral$graz==i,] 
#genres_malg[[i]] <- gen_results_malg[gen_results_malg$graz==i,]
ratiocover[[i]] <- ratiocovr

###Tabu Q1: Did the fate of the reefs that are newly in tabus improve? (C>M now, >30% C now, etc)
#take the subset of the dataframes made above that just includes the reefs in the newtabus
###Tabu Q2: Did the fate of reefs that were in tabus before improve at all? (might be a bit weird to assess)
#take the subset of the dataframes made above that just includes the reefs originally in the tabus
##^ both of these answered above

#these lines are kinda pointless
ratiocover_0.02_newtabus[[i]] <- ratiocovr[newtabus_0.02,] 
ratiocover_0.02_oldtabus[[i]] <- ratiocovr[oldtabus_0.02,]
ratiocover_0.05_newtabus[[i]] <- ratiocovr[newtabus_0.05,] 
ratiocover_0.05_oldtabus[[i]] <- ratiocovr[oldtabus_0.05,]
}

###Sedimentation Q1: Are the reefs in the red zones better off after the intervention? 

##finalavgcoralcover <- data.frame(graz_scenario = c(1,2,3,4), base = NA, base_dvar = NA, altsed = NA, tabu_samed = NA, tabu_dvar = NA)
#finalavgmalgcover <- data.frame(graz_scenario = c(1,2,3,4), base = NA, base_dvar = NA, altsed = NA, tabu_samed = NA, tabu_dvar = NA)
#numreefs_30cc <- data.frame(graz_scenario = c(1,2,3,4), base = NA, base_dvar = NA, altsed = NA, tabu_samed = NA, tabu_dvar = NA)


#plotting things
#note: finalcover has the final coral and malg cover data for all of the reefs, ratiocover has the final ratio data
#finalcover, finalcov <- data.frame(reef = seq(1,numreefs,1), base_coral = NA, base_malg = NA, base_dvar_coral = NA, base_dvar_malg = NA, altsed_coral = NA, altsed_malg = NA, tabu_dsame_coral = NA, tabu_dsame_malg = NA, tabu_dvar_coral = NA, tabu_dvar_malg = NA)
#ratiocover, ratiocovr <- data.frame(reef = seq(1,numreefs,1), baseratio = NA, base_dvar_ratio = NA, altsed_ratio = NA, tabu_dsame_ratio = NA, tabu_dvar_ratio = NA)
scens <- c("base", "base_dvar", "altsed", "tabu_0.02_dsame", "tabu_0.02_dvar", "tabu_0.05_dsame", "tabu_0.05_dvar")
finalcov_restr <- data.frame(graz = rep(c(1,2,3,4),each=length(scens)*numreefs), scenario = rep(scens, each = numreefs), reef = seq(1,numreefs,1), coral_cover=NA, malg_cover = NA, ratio = NA)

#to make the violin plots need to re-structure the dataframes
#this re-structuring could replace some of what's above...
for(j in 1:4){ 
  for(k in 1:length(scens)){
  r <- 125 #rate coral recruits onto/overgrows turf algae
  gamma <- 900 #rate macroalgae recruits onto/overgrows turf algae
  ratiocover_dummy <- ratiocover[[j]]
  
  #load in data
  if(k == 1){
  #Load in the base scenario
  data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/SimRound3_3.2022/Paramboth_varinitcond/FullSimRun/fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_3.27.2022.rds"))
  benthic_traj <- data[[3]]
  finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- ratiocover_dummy$baseratio
  }
  
  if(k == 2){
  #d allowed to vary
  #datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
  data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/base/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_base_5.10.2022.rds"))
  benthic_traj <- data[[3]]
  finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- ratiocover_dummy$base_dvar_ratio
  }
  
  if(k == 3){
    data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/alt/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_alt_5.10.2022.rds"))
    benthic_traj <- data[[3]]
    finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- ratiocover_dummy$altsed_ratio
  }
  
  if(k==4){
  #load tabu scenario
  #dval = 0.24
  data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.02degreebuffer/TabuScenario0.02_fullsimrun_allherbs_dval_all0.24_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
  benthic_traj <- data[[3]]
  finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- ratiocover_dummy$tabu_0.02_dsame_ratio
  }
  
  if(k == 5){
  #dval allowed to vary
  data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.02degreebuffer/TabuScenario0.02_fullsimrun_allherbs_dval_sed_determined_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
  benthic_traj <- data[[3]]
  finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- ratiocover_dummy$tabu_0.02_dvar_ratio
  }
  
  if(k==6){
    #load tabu scenario
    #dval = 0.24
    data <- readRDS(file =paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.05degreebuffer/TabuScenario0.05_fullsimrun_allherbs_dval_all0.24_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
    benthic_traj <- data[[3]]
    finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- ratiocover_dummy$tabu_0.05_dsame_ratio
  }
  
  if(k == 7){
    #dval allowed to vary
    data <- readRDS(file =paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.05degreebuffer/TabuScenario0.05_fullsimrun_allherbs_dval_sed_determined_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
    benthic_traj <- data[[3]]
    finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- ratiocover_dummy$tabu_0.05_dvar_ratio
  }
  
  finalcov_restr$coral_cover[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- benthic_traj$final_coralcover
  finalcov_restr$malg_cover[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- benthic_traj$final_malgcover
  
  
  #just in case
  benthic_traj <- NULL
  data <- NULL
}
}

#violin plot time - used: http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization, https://ggplot2.tidyverse.org/reference/geom_jitter.html, 
#define a function
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

coralcover_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr, aes(x=scenario, y=coral_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Coral Cover")+
  scale_x_discrete(limits=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","altsed","tabu_0.02_dvar", "tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00", "#E69F00","#b2d6eb","#56B4E9","#56B4E9", "#56B4E9"),breaks=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","altsed","tabu_0.02_dvar", "tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(coralcover_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/coralcover_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

malgcover_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr, aes(x=scenario, y=malg_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover")+
  scale_x_discrete(limits=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","altsed","tabu_0.02_dvar", "tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00", "#E69F00","#b2d6eb","#56B4E9","#56B4E9", "#56B4E9"),breaks=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","altsed","tabu_0.02_dvar", "tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(malgcover_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/malgcover_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

ratiocover_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr, aes(x=scenario, y=ratio,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Ratio of Coral:Macroalgae Cover on Reefs")+
  scale_x_discrete(limits=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","altsed","tabu_0.02_dvar", "tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00", "#E69F00","#b2d6eb","#56B4E9","#56B4E9", "#56B4E9"),breaks=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","altsed","tabu_0.02_dvar", "tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 1, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(ratiocover_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/ratiocover_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)


###just new tabus (and just tabu scenarios, no alt sed scenarios)
#finalcov_restr <- data.frame(graz = rep(c(1,2,3,4),each=length(scens)*numreefs), scenario = rep(scens, each = numreefs), reef = seq(1,numreefs,1), coral_cover=NA, malg_cover = NA, ratio = NA)
finalcov_restr_0.02_newtabus <- finalcov_restr[finalcov_restr$reef %in% newtabus_0.02 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.02_dsame","tabu_0.02_dvar"),]
finalcov_restr_0.02_newtabus$scenario <- as.character(finalcov_restr_0.02_newtabus$scenario)
finalcov_restr_0.02_newtabus$scenario[finalcov_restr_0.02_newtabus$scenario == "base"] <- "base_0.02"
finalcov_restr_0.02_newtabus$scenario[finalcov_restr_0.02_newtabus$scenario == "base_dvar"] <- "base_dvar_0.02"
finalcov_restr_0.05_newtabus <- finalcov_restr[finalcov_restr$reef %in% newtabus_0.05 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.05_dsame","tabu_0.05_dvar"),]
finalcov_restr_0.05_newtabus$scenario <- as.character(finalcov_restr_0.05_newtabus$scenario)
finalcov_restr_0.05_newtabus$scenario[finalcov_restr_0.05_newtabus$scenario == "base"] <- "base_0.05"
finalcov_restr_0.05_newtabus$scenario[finalcov_restr_0.05_newtabus$scenario == "base_dvar"] <- "base_dvar_0.05"
finalcov_restr_newtabus <- rbind(finalcov_restr_0.02_newtabus, finalcov_restr_0.05_newtabus)

coralcover_newtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_newtabus, aes(x=scenario, y=coral_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Coral Cover, New Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(coralcover_newtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/coralcover_newtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

malgcover_newtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_newtabus, aes(x=scenario, y=malg_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover, New Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(malgcover_newtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/malgcover_newtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

ratiocover_newtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_newtabus, aes(x=scenario, y=ratio,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Ratio of Coral:Macroalgae Cover on Reefs, New Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 1, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(ratiocover_newtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/ratiocover_newtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

###just old tabus (and just tabu scenarios, no alt sed scenarios)
#NOTE: old tabus in 0.02 and 0.05 are the same! 
finalcov_restr_oldtabus <- finalcov_restr[finalcov_restr$reef %in% oldtabus_0.02 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.02_dsame", "tabu_0.05_dsame","tabu_0.02_dvar","tabu_0.05_dvar"),]

coralcover_oldtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_oldtabus, aes(x=scenario, y=coral_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Coral Cover, Old Tabus Only")+
  scale_x_discrete(limits=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","tabu_0.02_dvar","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00", "#E69F00","#b2d6eb","#56B4E9","#56B4E9"),breaks=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","tabu_0.02_dvar","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(coralcover_oldtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/coralcover_oldtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

malgcover_oldtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_oldtabus, aes(x=scenario, y=malg_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover, Old Tabus Only")+
  scale_x_discrete(limits=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","tabu_0.02_dvar","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00", "#E69F00","#b2d6eb","#56B4E9","#56B4E9"),breaks=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","tabu_0.02_dvar","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(malgcover_oldtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/malgcover_oldtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

ratiocover_oldtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_oldtabus, aes(x=scenario, y=ratio,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Ratio of Coral:Macroalgae Cover on Reefs, Old Tabus Only")+
  scale_x_discrete(limits=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","tabu_0.02_dvar","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00", "#E69F00","#b2d6eb","#56B4E9","#56B4E9"),breaks=c("base", "tabu_0.02_dsame", "tabu_0.05_dsame", "base_dvar","tabu_0.02_dvar","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 1, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(ratiocover_oldtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/ratiocover_oldtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

###just tabus (and just tabu scenarios, no alt sed scenarios)
alltabus_0.02 <- c(newtabus_0.02,oldtabus_0.02)
finalcov_restr_0.02_alltabus <- finalcov_restr[finalcov_restr$reef %in% alltabus_0.02 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.02_dsame","tabu_0.02_dvar"),]
finalcov_restr_0.02_alltabus$scenario <- as.character(finalcov_restr_0.02_alltabus$scenario)
finalcov_restr_0.02_alltabus$scenario[finalcov_restr_0.02_alltabus$scenario == "base"] <- "base_0.02"
finalcov_restr_0.02_alltabus$scenario[finalcov_restr_0.02_alltabus$scenario == "base_dvar"] <- "base_dvar_0.02"
alltabus_0.05 <- c(newtabus_0.05,oldtabus_0.05)
finalcov_restr_0.05_alltabus <- finalcov_restr[finalcov_restr$reef %in% alltabus_0.05 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.05_dsame","tabu_0.05_dvar"),]
finalcov_restr_0.05_alltabus$scenario <- as.character(finalcov_restr_0.05_alltabus$scenario)
finalcov_restr_0.05_alltabus$scenario[finalcov_restr_0.05_alltabus$scenario == "base"] <- "base_0.05"
finalcov_restr_0.05_alltabus$scenario[finalcov_restr_0.05_alltabus$scenario == "base_dvar"] <- "base_dvar_0.05"
finalcov_restr_alltabus <- rbind(finalcov_restr_0.02_alltabus, finalcov_restr_0.05_alltabus)

coralcover_alltabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_alltabus, aes(x=scenario, y=coral_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Coral Cover, Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(coralcover_alltabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/coralcover_alltabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

malgcover_alltabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_alltabus, aes(x=scenario, y=malg_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover, Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(malgcover_alltabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/malgcover_alltabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

ratiocover_alltabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_alltabus, aes(x=scenario, y=ratio,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Ratio of Coral:Macroalgae Cover on Reefs, Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 1, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(ratiocover_alltabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/ratiocover_alltabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

###non-tabus
nontabus <- seq(1,75,1)
nontabus_0.02 <- nontabus[-alltabus_0.02]
finalcov_restr_0.02_nontabus <- finalcov_restr[finalcov_restr$reef %in% nontabus_0.02 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.02_dsame","tabu_0.02_dvar"),]
finalcov_restr_0.02_nontabus$scenario <- as.character(finalcov_restr_0.02_nontabus$scenario)
finalcov_restr_0.02_nontabus$scenario[finalcov_restr_0.02_nontabus$scenario == "base"] <- "base_0.02"
finalcov_restr_0.02_nontabus$scenario[finalcov_restr_0.02_nontabus$scenario == "base_dvar"] <- "base_dvar_0.02"
nontabus_0.05 <- nontabus[-alltabus_0.05]
finalcov_restr_0.05_nontabus <- finalcov_restr[finalcov_restr$reef %in% nontabus_0.05 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.05_dsame","tabu_0.05_dvar"),]
finalcov_restr_0.05_nontabus$scenario <- as.character(finalcov_restr_0.05_nontabus$scenario)
finalcov_restr_0.05_nontabus$scenario[finalcov_restr_0.05_nontabus$scenario == "base"] <- "base_0.05"
finalcov_restr_0.05_nontabus$scenario[finalcov_restr_0.05_nontabus$scenario == "base_dvar"] <- "base_dvar_0.05"
finalcov_restr_nontabus <- rbind(finalcov_restr_0.02_nontabus, finalcov_restr_0.05_nontabus)

coralcover_nontabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_nontabus, aes(x=scenario, y=coral_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Coral Cover, Non-Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(coralcover_nontabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/coralcover_nontabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

malgcover_nontabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_nontabus, aes(x=scenario, y=malg_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover, Non-Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(malgcover_nontabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/malgcover_nontabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

ratiocover_nontabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_nontabus, aes(x=scenario, y=ratio,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Ratio of Coral:Macroalgae Cover on Reefs, Non-Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 1, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(ratiocover_nontabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/ratiocover_nontabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022.png"), bg = "transparent", height = 10, width = 10)

#checking why the 0.05 tabu reefs go down in coral cover...how many of the newtabus_0.05 and oldtabus_0.05 have lower grazing levels than before?
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.05_5.26.2022.RData")
#benthicfish_masterdataset_sed[newtabus_0.05,c(19,28)] #33 reefs newly in tabus
#new tabu reefs with lower grazing levels: 10, 31, 37, 38, 44, 64, 69, 70, 72 -> 9/33 are lower
#benthicfish_masterdataset_sed[oldtabus_0.05,c(19,28)] #18 reefs originally in tabus -> 6 same, 7 higher, 5 lower
#old tabu reefs with the same grazing levels: 3, 6, 26, 39, 47, 55, 
#old tabu reefs with higher grazing levels: 8, 28, 33, 41, 50, 73, 75
#old tabu reefs with lower grazing levels: 9, 42, 46, 49, 67

#in contrast, the 0.02 tabu reefs:
#new tabu reefs: 3/17 are lower, old tabu reefs: different but lower - 2/18, different but higher - 5/18, rest are the same
#=> so in conclusion, proportionally more of the 0.05 reefs in tabus have lower grazing levels

###looking at how each reef fares before/after management
#plot managed-base for each reef, ratio and coral cover, for every scenario
#make a column that lets me colour by newtabu, oldtabu, everything else? 
tabus_0.02 <- seq(1,75,1)
tabus_0.02[newtabus_0.02] <- "newtabu"
tabus_0.02[oldtabus_0.02] <- "oldtabu"
tabus_0.02[tabus_0.02 %in% seq(1,75,1)] <- "n"

tabus_0.05 <- seq(1,75,1)
tabus_0.05[newtabus_0.05] <- "newtabu"
tabus_0.05[oldtabus_0.05] <- "oldtabu"
tabus_0.05[tabus_0.05 %in% seq(1,75,1)] <- "n"

scenz <- c("tabu0.02_base", "tabu0.02_basedvar", "tabu0.05_base", "tabu0.05_basedvar", "altsed_basedvar")

finalcov_changes <- data.frame(graz = rep(c(1,2,3,4), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*4)), reef = rep(rep(seq(1,75,1),each = length(scenz)),4), identity_0.02 = rep(rep(tabus_0.02,each = length(scenz)),4), identity_0.05 = rep(rep(tabus_0.05,each = length(scenz)),4), coral_cover = NA, malg_cover = NA, ratio = NA)

finalcov_percentchange <- data.frame(graz = rep(c(1,2,3,4), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*4)), reef = rep(rep(seq(1,75,1),each = length(scenz)),4), identity_0.02 = rep(rep(tabus_0.02,each = length(scenz)),4), identity_0.05 = rep(rep(tabus_0.05,each = length(scenz)),4), coral_cover = NA, malg_cover = NA)

for(i in 1:4){
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[1]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.02_dsame"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[1]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.02_dsame"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[1]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.02_dsame"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[2]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.02_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[2]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.02_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[2]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.02_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[3]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dsame"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[3]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dsame"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[3]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dsame"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[4]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[4]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[4]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "altsed"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "altsed"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "altsed"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
}

ggplot(finalcov_changes, aes(x = scenario, y = coral_cover*100)) +
  geom_point()+
  ylab("Change in Coral Cover")+
  facet_wrap(~graz)

ggplot(finalcov_changes, aes(x = scenario, y = ratio)) +
  geom_point()+
  ylab("Change in C:M Ratio")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~graz)

#this doesn't even plot because of small number problems (small #/even smaller # -> really huge values, not plotting)
ggplot(finalcov_percentchange, aes(x = scenario, y = coral_cover)) +
  geom_point()+
  ylab("Percent Change in Coral Cover")+
  facet_wrap(~graz)

finalcov_changes_0.02 <- finalcov_changes[finalcov_changes$scenario %in% c("tabu0.02_base", "tabu0.02_basedvar"),]

ggplot(finalcov_changes_0.02, aes(x = scenario, y = coral_cover*100, color = identity_0.02)) +
  geom_point()+
  ylab("Change in Coral Cover")+
  facet_wrap(~graz)

ggplot(finalcov_changes_0.02, aes(x = scenario, y = ratio, color = identity_0.02)) +
  geom_point()+
  ylab("Change in C:M Ratio")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~graz)

finalcov_changes_0.05 <- finalcov_changes[finalcov_changes$scenario %in% c("tabu0.05_base", "tabu0.05_basedvar"),]

ggplot(finalcov_changes_0.05, aes(x = scenario, y = coral_cover*100, color = identity_0.05)) +
  geom_point()+
  ylab("Change in Coral Cover")+
  facet_wrap(~graz)

ggplot(finalcov_changes_0.05, aes(x = scenario, y = ratio, color = identity_0.05)) +
  geom_point()+
  ylab("Change in C:M Ratio")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~graz)

####this needs to be moved somewhere else later
#plot how final coral cover (base scenario) relates to immi, emi, grazing level, sedimentation level
#finalcov_restr, benthicfish_masterdataset_sed
#NEED TO MULTIPLY COMB_GRAZING LEVEL BY THAT FACTOR - see below for code to generate those values and pc_val and pm_val
par(mfrow = c(2, 2))
plot(benthicfish_masterdataset_sed$comb_grazinglevel, finalcov_restr$coral_cover[finalcov_restr$graz == 1 & finalcov_restr$scenario == "base"], ylab = "coral cover", xlab = "grazing level", main = "empirical graz level")
plot(benthicfish_masterdataset_sed$comb_grazinglevel*MD_scale, finalcov_restr$coral_cover[finalcov_restr$graz == 2 & finalcov_restr$scenario == "base"], ylab = "coral cover", xlab = "grazing level", main = "median graz = 0.1")
plot(benthicfish_masterdataset_sed$comb_grazinglevel*ASS_scale, finalcov_restr$coral_cover[finalcov_restr$graz == 3 & finalcov_restr$scenario == "base"], ylab = "coral cover", xlab = "grazing level", main = "median graz = 0.3")
plot(benthicfish_masterdataset_sed$comb_grazinglevel*CD_scale, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base"],ylab = "coral cover", xlab = "grazing level", main = "median graz = 0.5")

dev.off()

#mortality level - only makes sense for the base_dvar scenario
par(mfrow = c(2, 2))
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 1 & finalcov_restr$scenario == "base_dvar"], x=benthicfish_masterdataset_sed$sediment_alt, ylab = "coral cover", xlab = "natural coral mortality", main = "empirical graz level")
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 2 & finalcov_restr$scenario == "base_dvar"], x=benthicfish_masterdataset_sed$sediment_alt, ylab = "coral cover", xlab = "natural coral mortality", main = "median graz = 0.1")
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 3 & finalcov_restr$scenario == "base_dvar"], x=benthicfish_masterdataset_sed$sediment_alt, ylab = "coral cover", xlab = "natural coral mortality", main = "median graz = 0.3")
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base_dvar"], x=benthicfish_masterdataset_sed$sediment_alt, ylab = "coral cover", xlab = "natural coral mortality", main = "median graz = 0.5")

dev.off()

#immigration level
par(mfrow = c(2, 2))
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 1 & finalcov_restr$scenario == "base"], x=pc_val, ylab = "coral cover", xlab = "coral input level", main = "empirical graz level")
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 2 & finalcov_restr$scenario == "base"], x=pc_val, ylab = "coral cover", xlab = "coral input level", main = "median graz = 0.1")
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 3 & finalcov_restr$scenario == "base"], x=pc_val, ylab = "coral cover", xlab = "coral input level", main = "median graz = 0.3")
plot(y=finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base"], x=pc_val, ylab = "coral cover", xlab = "coral input level", main = "median graz = 0.5")

dev.off()


###why do the scenario 4 results for the base scenario/base tabu0.02/base tabu0.05 have those lines?
##base first
plot(finalcov_restr$reef[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base"], finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base"])
finalcov_restr[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base",]
group_sixty <- c(10,13,14,15,23,25,27,28,34,38,60,61,63,65,67,71,74,75)
group_seventyfive <- c(3,5,6,12,35,39,41,46,51,58,59,66)
#roughly 60%:10,13,14,15,23,25,27,28,34,38,60,61,63,65?,67,71,74,75
#roughly 75%:3,5,6,12,35,39,41,46,51,58,59,66

#they all vary in initial coral cover, malg cover, turf cover, grazing level, location, mgmt level etc 
benthicfish_masterdataset_sed[group_sixty,]
benthicfish_masterdataset_sed[group_seventyfive,]

####check connectivity values
#see below for the code to generate pm_val and pc_val
pc_val[group_sixty] #they all have the same coral input level 
pm_val[group_sixty] #vary in malg input levels though
pc_val[group_seventyfive] #same coral input level again
pm_val[group_seventyfive] #vary in malg input levels though
plot(finalcov_restr$reef[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base"], pc_val) #two distinct non-zero lines here

plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base"])

###tabu_0.02_dsame
finalcov_restr[finalcov_restr$graz == 4 & finalcov_restr$scenario == "tabu_0.02_dsame",]
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "tabu_0.02_dsame"]) #such a clean line, again

###tabu_0.05_dsame
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "tabu_0.05_dsame"]) #such a clean line, again

#just to see, what about the ones where sed varies? these show more variation with coral immi level but are weirdly identical with each other
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "base_dvar"])
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "altsed"])
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "tabu_0.02_dvar"])
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 4 & finalcov_restr$scenario == "tabu_0.05_dvar"])

#what about other scenarios?
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 3 & finalcov_restr$scenario == "base"]) #okay
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 2 & finalcov_restr$scenario == "base"])
plot(pc_val, finalcov_restr$coral_cover[finalcov_restr$graz == 1 & finalcov_restr$scenario == "base"])

###load in connectivity + fish abundance + benthic cover data

#herbivore abundance + benthic cover data 
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged.RData") #generated in Fiji_status/Fish Belt Vis/2021_fishbelt_thesisproject/FishBelt_generatefishoverviewmaps.Rmd

#remove VIR10 from 2018 since we have a 2020 one, i.e. row 63
benthicfish_masterdataset <- wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged[-63,]
#removing NS3 because the connectivity for that site is wrong since the coordinates were/are wrong
benthicfish_masterdataset <- benthicfish_masterdataset[-47,]


#convert herbivore abundance to a number between 0->1 by assigning 1 to the largest herbivore abundance recorded in the Fijian MERMAID database (as of 11.25.2021, out of 657 sample events, from Emily)
#look at: https://labs.eemb.ucsb.edu/burkepile/deron/research/herbivory-herbivore-diversity-and-ecosystem-function
largest_malgherbabundance <- 1573.33
#5172.61 is the max detritivore density recorded as of 11.25.2021, out of 657 sample events, from Emily
largest_detherbabundance <- 5172.61

#extract the sites
sitevector <- benthicfish_masterdataset$site

###looking at detritivore herbivore densities

#only the >2016 herbivore data from wcs fiji sites 
#save(wcsonly_fiji_fishbelt_current, file = here("Fish Belt Vis","2021_fishbelt_thesisproject", "wcsonly_current_fiji_herbivoredata.RData"))
load("~/GitHub/WCSFiji2020Internship/Fiji-status/Fish Belt Vis/2021_fishbelt_thesisproject/wcsonly_current_fiji_herbivoredata.RData")
benthicfish_masterdataset_detherbs <- benthicfish_masterdataset
wcsonly_fiji_fishbelt_current_jointsitesonly <- wcsonly_fiji_fishbelt_current[wcsonly_fiji_fishbelt_current$site %in% sitevector,]
wcsonly_fiji_fishbelt_current_jointsitesonly <- wcsonly_fiji_fishbelt_current_jointsitesonly[-10,] #removing VIR10 2018
#are the sites in the same order? no
#setequal(wcsonly_fiji_fishbelt_current_jointsitesonly$site, benthicfish_masterdataset_detherbs$site)
#wcsonly_fiji_fishbelt_current_jointsitesonly$site %in% benthicfish_masterdataset_detherbs$site #(all TRUE both ways)
#wcsonly_fiji_fishbelt_current_jointsitesonly %>%
#  distinct(site) #only 75 distinct sites, two KB17 data points (keep the 2019 one bc only have 2019 benthic data) #<- a 1.25.2022 change
wcsonly_fiji_fishbelt_current_jointsitesonly <- wcsonly_fiji_fishbelt_current_jointsitesonly[-49,] #before: -17 when removing the 2019 one
wcsonly_fiji_fishbelt_current_jointsitesonly_abr <- wcsonly_fiji_fishbelt_current_jointsitesonly %>%
  select(site, biomass_kgha_trophic_group_avg_herbivore_detritivore)
benthicfish_masterdataset_detherbs <- merge(benthicfish_masterdataset_detherbs, wcsonly_fiji_fishbelt_current_jointsitesonly_abr, by = "site")

benthicfish_masterdataset_detherbs$malg_density <- benthicfish_masterdataset_detherbs$biomass_kgha_trophic_group_avg_herbivore_macroalgae
benthicfish_masterdataset_detherbs$malg_density[is.na(benthicfish_masterdataset_detherbs$malg_density)] <- 0
benthicfish_masterdataset_detherbs$comb_grazinglevel <- (benthicfish_masterdataset_detherbs$biomass_kgha_trophic_group_avg_herbivore_detritivore + benthicfish_masterdataset_detherbs$malg_density)/(largest_detherbabundance+largest_malgherbabundance)

benthicfish_masterdataset <- benthicfish_masterdataset_detherbs 

#scale it so the grazing rate either has a mean of 0.1, 0.3 or 0.5
median(benthicfish_masterdataset$comb_grazinglevel) 
MD_scale <- 0.1/median(benthicfish_masterdataset$comb_grazinglevel) #if multiply by 2.338038 will get a median of 0.1
ASS_scale <- 0.3/median(benthicfish_masterdataset$comb_grazinglevel) #7.014113
CD_scale <- 0.5/median(benthicfish_masterdataset$comb_grazinglevel) #11.690188
#checking 
#median(benthicfish_masterdataset$grazinglevel*CD_scale) #0.1, 0.3, 0.5 confirmed
scalingfactors <- c(1,MD_scale, ASS_scale, CD_scale)

#get 'turf' cover by 1-(coral_cover)-(malg_cover)
benthicfish_masterdataset$malg_cover <- round((benthicfish_masterdataset$percent_cover_benthic_category_avg_macroalgae/100),4) #chose 4 because that's as accurate/precise/? as the measurement was initially
benthicfish_masterdataset$coral_cover <- round((benthicfish_masterdataset$percent_cover_benthic_category_avg_hard_coral/100),4)
benthicfish_masterdataset$turf_cover <- 1 - benthicfish_masterdataset$coral_cover - benthicfish_masterdataset$malg_cover

#extract the sites
sitevector <- benthicfish_masterdataset$site

#coral connectivity matrix
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/weightedavgconnmat.RData") 
#^weightedavgconnmat - for all sites
#subset it to only the 76 sites with both benthic cover and fish abundance data
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData") #newcoordinates; abridged coordinate file that corresponds with conn mat above, site order-wise
coordinates <- newcoordinates
jointsite_rows <- which(coordinates$site %in% sitevector)
jointsite_coral_weightedavgconnmat <- weightedavgconnmat[jointsite_rows, jointsite_rows]

#macroalgae connectivity matrix 
#(coral conn mat divided by some very large number)
#jointsite_malg_weightedavgconnmat <- jointsite_coral_weightedavgconnmat/100 #100 chosen arbitrarily

#take the summed (across months/years) coral connectivity matrix with a PLD of 5
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
malg_smallPLD_connmat <- Conn_Mat_Sum[[1]]
jointsite_malg_smallPLDconnmat <- malg_smallPLD_connmat[jointsite_rows, jointsite_rows]

pc_val <- pm_val <- rep(NA,length(sitevector))
for(i in 1:75){
  pm_val[i] <- sum(jointsite_coral_weightedavgconnmat[i,]) #sum of inputs to i (coral), sum row i
  #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
  pc_val[i] <- sum(jointsite_malg_smallPLDconnmat[i,]) #sum of inputs to i (macroalgae)
}




####OLD CODE
finalcov_restr_0.02_oldtabus <- finalcov_restr[finalcov_restr$reef %in% oldtabus_0.02 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.02_dsame","tabu_0.02_dvar"),]
finalcov_restr_0.02_oldtabus$scenario <- as.character(finalcov_restr_0.02_oldtabus$scenario)
finalcov_restr_0.02_oldtabus$scenario[finalcov_restr_0.02_oldtabus$scenario == "base"] <- "base_0.02"
finalcov_restr_0.02_oldtabus$scenario[finalcov_restr_0.02_oldtabus$scenario == "base_dvar"] <- "base_dvar_0.02"
finalcov_restr_0.05_oldtabus <- finalcov_restr[finalcov_restr$reef %in% oldtabus_0.05 & finalcov_restr$scenario %in% c("base","base_dvar","tabu_0.05_dsame","tabu_0.05_dvar"),]
finalcov_restr_0.05_oldtabus$scenario <- as.character(finalcov_restr_0.05_oldtabus$scenario)
finalcov_restr_0.05_oldtabus$scenario[finalcov_restr_0.05_oldtabus$scenario == "base"] <- "base_0.05"
finalcov_restr_0.05_oldtabus$scenario[finalcov_restr_0.05_oldtabus$scenario == "base_dvar"] <- "base_dvar_0.05"
finalcov_restr_oldtabus <- rbind(finalcov_restr_0.02_oldtabus, finalcov_restr_0.05_oldtabus)

coralcover_oldtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_oldtabus, aes(x=scenario, y=coral_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Coral Cover, Old Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(coralcover_oldtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/coralcover_oldtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022_old.png"), bg = "transparent", height = 10, width = 10)

malgcover_oldtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_oldtabus, aes(x=scenario, y=malg_cover*100,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover, Old Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(malgcover_oldtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/malgcover_oldtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022_old.png"), bg = "transparent", height = 10, width = 10)

ratiocover_oldtabus_mgmtstressorimpactassessment_summary <- ggplot(finalcov_restr_oldtabus, aes(x=scenario, y=ratio,fill=scenario)) + 
  geom_violin(color = NA)+
  ggtitle("Ratio of Coral:Macroalgae Cover on Reefs, Old Tabus Only")+
  scale_x_discrete(limits=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  scale_fill_manual(values=c("#e8d4a7","#E69F00","#e8d4a7", "#E69F00","#b2d6eb","#56B4E9","#b2d6eb","#56B4E9"),breaks=c("base_0.02", "tabu_0.02_dsame","base_0.05", "tabu_0.05_dsame", "base_dvar_0.02","tabu_0.02_dvar","base_dvar_0.05","tabu_0.05_dvar"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 1, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(ratiocover_oldtabus_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/ratiocover_oldtabus_mgmtstressorimpactassessment_summary_altsed_5.30.2022_old.png"), bg = "transparent", height = 10, width = 10)



