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

numreefs <- 75
r <- 6500 #rate coral recruits onto/overgrows turf algae
gamma <- 1900 #rate macroalgae recruits onto/overgrows turf algae
zerocutoff <- 0.01

scens <- c("base_dvar", "10altsed", "25altsed", "10altsed_scattered", "25altsed_scattered", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "tabuqoliqoli_10sedscat_dvar", "tabuqoliqoli_25sedscat_dvar")
finalcov_restr <- data.frame(graz = rep(c(1,2,3),each=length(scens)*numreefs), scenario = rep(scens, each = numreefs), reef = seq(1,numreefs,1), coral_cover=NA, malg_cover = NA, ratio = NA, numzerocoral = NA)

#to make the violin plots need to re-structure the dataframes
#this re-structuring could replace some of what's above...
for(j in 1:3){ 
  for(k in 1:length(scens)){
    
    #load in data
    #base_dvar
    if(k == 1){
      #datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuoriginal_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #10altsed
    if(k == 2){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #25altsed
    if(k == 3){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #10altsed_scattered
    if(k == 4){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/ScatteredSedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor10percent_5.6.2023.rds"))
    }
    
    #25altsed_scattered
    if(k == 5){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/ScatteredSedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor25percent_5.6.2023.rds"))
    }
    
    #tabu_qoliqoli_dvar
    if(k == 6){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #tabu_0.05_dvar
    if(k == 7){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabu5km_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #10% + qoli qoli
    if(k == 8){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #25% + qoli qoli
    if(k == 9){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #10altsed_scattered w tabu graz
    if(k == 10){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/ScatteredSedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor10per_wgraz_5.6.2023.rds"))
    }
    
    #25altsed_scattered w tabu graz
    if(k == 11){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/ScatteredSedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor25per_wgraz_5.6.2023.rds"))
    }
    
    benthic_traj <- data[[3]]
    finalcov_restr$coral_cover[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- benthic_traj$final_coralcover
    finalcov_restr$malg_cover[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- benthic_traj$final_malgcover
    finalcov_restr$ratio[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- (1+benthic_traj$final_coralcover)/(1+benthic_traj$final_malgcover)
    finalcov_restr$numzerocoral[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k]] <- numreefs - length(finalcov_restr$reef[finalcov_restr$graz == j & finalcov_restr$scenario == scens[k] & finalcov_restr$coral_cover > zerocutoff])
    
    #just in case
    benthic_traj <- NULL
    data <- NULL
  }
}


#beeswarm plots instead? see: https://github.com/eclarke/ggbeeswarm

# New facet label names
grazinglevels.labs <- c("Low", "Medium", "High")
names(grazinglevels.labs) <- c("1", "2","3")
#grazinglevels.labs <- c("Empirical", "Pessimistic", "Middling", "Optimistic")
#names(grazinglevels.labs) <- c("1", "2", "3","4")

coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=coral_cover*100,colour=scenario, alpha = 0.9)) + 
  geom_quasirandom()+
  #geom_violin(color = NA)+
  ggtitle("Coral Cover, dvar only")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed_scattered", "25altsed_scattered", "10altsed", "25altsed", "tabuqoliqoli_10sedscat_dvar", "tabuqoliqoli_25sedscat_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1-qq", "1-5km", "2-10scat", "2-25scat", "2-10", "2-25", "3-scatlow", "3-scathigh", "3-low", "3-high"), name = "Management Strategy")+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94eba7","#56e95d", "#94cceb", "#56B4E9","#eb8fe6","#e956e9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed_scattered", "25altsed_scattered", "10altsed", "25altsed", "tabuqoliqoli_10sedscat_dvar", "tabuqoliqoli_25sedscat_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1-qq = 2km tabu increase within qoliqoli", "1-5km = 5km tabu increase", "2-10scat", "2-25scat", "2-10 = 10% Water Quality Improvement", "2-25 = 25% Water Quality Improvement", "3-scatlow", "3-scathigh", "3-low = Low Mixed", "3-high = High Mixed"), name = "Management Strategy")+ 
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/FinalPlots/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=malg_cover*100,colour=scenario, alpha = 0.9)) + 
  geom_quasirandom()+
  #geom_violin(color = NA)+
  ggtitle("Malg Cover, dvar only")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed_scattered", "25altsed_scattered", "10altsed", "25altsed", "tabuqoliqoli_10sedscat_dvar", "tabuqoliqoli_25sedscat_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1-qq", "1-5km", "2-10scat", "2-25scat", "2-10", "2-25", "3-scatlow", "3-scathigh", "3-low", "3-high"), name = "Management Strategy")+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94eba7","#56e95d", "#94cceb", "#56B4E9","#eb8fe6","#e956e9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed_scattered", "25altsed_scattered", "10altsed", "25altsed", "tabuqoliqoli_10sedscat_dvar", "tabuqoliqoli_25sedscat_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1-qq = 2km tabu increase within qoliqoli", "1-5km = 5km tabu increase", "2-10scat", "2-25scat", "2-10 = 10% Water Quality Improvement", "2-25 = 25% Water Quality Improvement", "3-scatlow", "3-scathigh", "3-low = Low Mixed", "3-high = High Mixed"), name = "Management Strategy")+ 
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/FinalPlots/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5.6.2023.png"), bg = "transparent", height = 10, width = 10)



###looking at how each reef fares before/after management
#plot managed-base for each reef, ratio and coral cover, for every scenario
#make a column that lets me colour by newtabu, oldtabu, everything else? 

#load in the qoli qoli tabus
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_6.30.2022.RData")
#newly in tabus - new_grazinglevel is different from comb_grazinglevel and not listed as being in a tabu
#which reefs are 'newly in tabus'?
benthicfish_masterdataset_sed$newtabu_qoliqoli <- 0
for(i in 1:numreefs){
  #not in a tabu
  if(benthicfish_masterdataset_sed$mgmt_abr[i] == "LMMA"){
    if(benthicfish_masterdataset_sed$comb_grazinglevel[i] != benthicfish_masterdataset_sed$new_grazinglevel[i]){
      benthicfish_masterdataset_sed$newtabu_qoliqoli[i] <- 1
    }
  }
}
newtabus_qoliqoli <- which(benthicfish_masterdataset_sed$newtabu_qoliqoli > 0)
oldtabus_qoliqoli <- which(benthicfish_masterdataset_sed$mgmt_abr != "LMMA")

#which new tabus have different grazing rates than before? can't be the same bc two reefs can't have identical fish biomasses
benthicfish_masterdataset_sed[newtabus_qoliqoli,c(19,28)]
#lower: 10,37,72
#higher: 2,12,13,25,32,35,36,40,45,48,53,54

#which old tabus have different grazing rates than before?
benthicfish_masterdataset_sed[oldtabus_qoliqoli,c(19,28)]
#lower: 42,49,67,
#same:3,6,8,9,26,28,33,39,46,47,55,63,64,66
#higher: 41,50,73,75

qqlowerreefs <- c(10,37,41,50,72,73,75)

benthicfish_masterdataset_sed <- NULL

#load in the 0.05 tabus
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.05_5.26.2022.RData")
#which reefs are 'newly in tabus'?
benthicfish_masterdataset_sed$newtabu_0.05 <- 0
for(i in 1:numreefs){
  #not in a tabu
  if(benthicfish_masterdataset_sed$mgmt_abr[i] == "LMMA"){
    if(benthicfish_masterdataset_sed$comb_grazinglevel[i] != benthicfish_masterdataset_sed$new_grazinglevel[i]){
      benthicfish_masterdataset_sed$newtabu_0.05[i] <- 1
    }
  }
}
newtabus_0.05 <- which(benthicfish_masterdataset_sed$newtabu_0.05 > 0)
oldtabus_0.05 <- which(benthicfish_masterdataset_sed$mgmt_abr != "LMMA")

#which new tabus have lower grazing rates than before? can't be the same bc two reefs can't have identical fish biomasses
benthicfish_masterdataset_sed[newtabus_0.05,c(19,28)]
#lower: 10,31,37,38,44,69,70,72
#higher: 2,5,7,12,13,14,15,25,27,29,32,34,35,36,40,43,45,48,52,53,54,68,71
benthicfish_masterdataset_sed[oldtabus_0.05,c(19,28)]
#lower: 9,42,46,49,64,67
#same: 3,6,26,39,47,55,66
#higher: 8,28,33,41,50,63,73,75

fivelowerreefs <- c(9,10,31,37,38,42,44,46,49,64,67,69,70,72)

benthicfish_masterdataset_sed <- NULL

tabus_qoliqoli <- seq(1,75,1)
tabus_qoliqoli[newtabus_qoliqoli] <- "newtabu"
tabus_qoliqoli[oldtabus_qoliqoli] <- "oldtabu"
tabus_qoliqoli[tabus_qoliqoli %in% seq(1,75,1)] <- "n"

tabus_0.05 <- seq(1,75,1)
tabus_0.05[newtabus_0.05] <- "newtabu"
tabus_0.05[oldtabus_0.05] <- "oldtabu"
tabus_0.05[tabus_0.05 %in% seq(1,75,1)] <- "n"


scenz <- c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar", "altsed10scat_qoliqoli_basedvar", "altsed25scat_qoliqoli_basedvar")

finalcov_changes <- data.frame(graz = rep(c(1,2,3), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*3)), reef = rep(rep(seq(1,75,1),each = length(scenz)),3), identity_qoliqoli = rep(rep(tabus_qoliqoli,each = length(scenz)),3), identity_0.05 = rep(rep(tabus_0.05,each = length(scenz)),3), coral_cover = NA, malg_cover = NA, ratio = NA)

finalcov_percentchange <- data.frame(graz = rep(c(1,2,3), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*3)), reef = rep(rep(seq(1,75,1),each = length(scenz)),3), identity_qoliqoli = rep(rep(tabus_qoliqoli,each = length(scenz)),3), identity_0.05 = rep(rep(tabus_0.05,each = length(scenz)),3), coral_cover = NA, malg_cover = NA)

#scens <- c("base_dvar", "10altsed", "tabu_0.02_dvar", "tabu_0.05_dvar", "tabu_qoliqoli_dvar", "tabu0.02_10sed_dvar", "tabuqoliqoli_10sed_dvar")
for(i in 1:3){
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[1]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_qoliqoli_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[1]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_qoliqoli_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[1]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_qoliqoli_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[1]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_qoliqoli_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[2]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[2]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[2]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[2]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabu_0.05_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[3]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[3]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[3]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[3]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[4]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[4]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[4]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[4]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed_scattered"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed_scattered"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed_scattered"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "10altsed_scattered"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed_scattered"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed_scattered"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed_scattered"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "25altsed_scattered"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[7]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[7]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[7]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[7]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[8]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[8]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[8]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[8]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[9]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sedscat_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[9]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sedscat_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[9]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sedscat_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[9]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sedscat_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[10]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sedscat_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[10]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sedscat_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[10]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sedscat_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[10]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sedscat_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
}

# New facet label names
grazinglevels.labs <- c("Low", "Medium", "High")
names(grazinglevels.labs) <- c("1", "2","3")
#grazinglevels.labs <- c("Empirical", "Pessimistic", "Middling", "Optimistic")
#names(grazinglevels.labs) <- c("1", "2", "3","4")

changeincoralcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = coral_cover*100, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10scat_qoliqoli_basedvar", "altsed25scat_qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq", "1-5km", "2-10scat", "2-25scat", "2-10", "2-25", "3-scatlow", "3-scathigh", "3-low", "3-high"), name = "Management Strategy")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq", "1-5km", "2-10", "2-25", "2-10scat", "2-25scat", "3-low", "3-high"), name = "Management Strategy")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94eba7","#56e95d", "#94cceb", "#56B4E9","#eb8fe6","#e956e9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10scat_qoliqoli_basedvar", "altsed25scat_qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq = 2km tabu increase within qoliqoli", "1-5km = 5km tabu increase", "2-10scat", "2-25scat", "2-10 = 10% Water Quality Improvement", "2-25 = 25% Water Quality Improvement", "3-scatlow", "3-scathigh", "3-low = Low Mixed", "3-high = High Mixed"), name = "Management Strategy")+
  ylab("Change in % Coral Cover")+
  geom_hline(yintercept = 0, color = "black")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/FinalPlots/changeincoralcover_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 5)

changeinmalgcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = malg_cover*100, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10scat_qoliqoli_basedvar", "altsed25scat_qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq", "1-5km", "2-10scat", "2-25scat", "2-10", "2-25", "3-scatlow", "3-scathigh", "3-low", "3-high"), name = "Management Strategy")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq", "1-5km", "2-10", "2-25", "2-10scat", "2-25scat", "3-low", "3-high"), name = "Management Strategy")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94eba7","#56e95d", "#94cceb", "#56B4E9","#eb8fe6","#e956e9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10scat_qoliqoli_basedvar", "altsed25scat_qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq = 2km tabu increase within qoliqoli", "1-5km = 5km tabu increase", "2-10scat", "2-25scat", "2-10 = 10% Water Quality Improvement", "2-25 = 25% Water Quality Improvement", "3-scatlow", "3-scathigh", "3-low = Low Mixed", "3-high = High Mixed"), name = "Management Strategy")+
  ylab("Change in % Macroalgae Cover")+
  geom_hline(yintercept = 0, color = "black")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/FinalPlots/changeinmalgcover_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 5)

changeinCMratio_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = ratio, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10scat_qoliqoli_basedvar", "altsed25scat_qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq", "1-5km", "2-10scat", "2-25scat", "2-10", "2-25", "3-scatlow", "3-scathigh", "3-low", "3-high"), name = "Management Strategy")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq", "1-5km", "2-10", "2-25", "2-10scat", "2-25scat", "3-low", "3-high"), name = "Management Strategy")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94eba7","#56e95d", "#94cceb", "#56B4E9","#eb8fe6","#e956e9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10scat_basedvar", "altsed25scat_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10scat_qoliqoli_basedvar", "altsed25scat_qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1-qq = 2km tabu increase within qoliqoli", "1-5km = 5km tabu increase", "2-10scat", "2-25scat", "2-10 = 10% Water Quality Improvement", "2-25 = 25% Water Quality Improvement", "3-scatlow", "3-scathigh", "3-low = Low Mixed", "3-high = High Mixed"), name = "Management Strategy")+
  ylab("Change in % Macroalgae Cover")+
  ylab("Change in C:M Ratio")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinCMratio_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/ScatteredSedInterventions/FinalPlots/changeinCMratio_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 5)
