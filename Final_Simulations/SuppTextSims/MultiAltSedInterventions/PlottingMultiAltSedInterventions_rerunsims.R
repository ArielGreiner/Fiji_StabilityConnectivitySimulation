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

scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed")
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
    
    #tabu_qoliqoli_dvar
    if(k == 4){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #tabu_0.05_dvar
    if(k == 5){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabu5km_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #10% + qoli qoli
    if(k == 6){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #25% + qoli qoli
    if(k == 7){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds"))
    }
    
    #30%
    if(k == 8){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor3_5.6.2023.rds"))
    }
    
    #40%
    if(k == 9){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor4_5.6.2023.rds"))
    }
    
    #50%
    if(k == 10){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor5_5.6.2023.rds"))
    }
    
    #60%
    if(k == 11){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor6_5.6.2023.rds"))
    }
    
    #70%
    if(k == 12){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor7_5.6.2023.rds"))
    }
    
    #80%
    if(k == 13){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor8_5.6.2023.rds"))
    }
    
    #90%
    if(k == 14){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor9_5.6.2023.rds"))
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
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"), labels=c("base", "qq", "5km", "10", "25", "qq10", "qq25","30", "40", "50", "60", "70", "80", "90"))+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"), labels=c("base", "qq = 2km tabu increase within qoliqoli", "5km = 5km tabu increase", "10 = 10% Water Quality Improvement", "25 = 25% Water Quality Improvement", "qq10 = Low Mixed", "qq25 = High Mixed", "30 = 30% Water Quality Improvement", "40 = 40% Water Quality Improvement", "50 = 50% Water Quality Improvement", "60 = 60% Water Quality Improvement", "70 = 70% Water Quality Improvement", "80 = 80% Water Quality Improvement", "90 = 90% Water Quality Improvement"), name = "Management Strategy")+ 
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/FinalPlots/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=malg_cover*100,colour=scenario, alpha = 0.9)) + 
  geom_quasirandom()+
  #geom_violin(color = NA)+
  ggtitle("Malg Cover, dvar only")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"), labels=c("base", "qq", "5km", "10", "25", "qq10", "qq25","30", "40", "50", "60", "70", "80", "90"))+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"), labels=c("base", "qq = 2km tabu increase within qoliqoli", "5km = 5km tabu increase", "10 = 10% Water Quality Improvement", "25 = 25% Water Quality Improvement", "qq10 = Low Mixed", "qq25 = High Mixed", "30 = 30% Water Quality Improvement", "40 = 40% Water Quality Improvement", "50 = 50% Water Quality Improvement", "60 = 60% Water Quality Improvement", "70 = 70% Water Quality Improvement", "80 = 80% Water Quality Improvement", "90 = 90% Water Quality Improvement"), name = "Management Strategy")+ 
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/FinalPlots/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5.6.2023.png"), bg = "transparent", height = 10, width = 10)



###looking at how each reef fares before/after management
#plot managed-base for each reef, ratio and coral cover, for every scenario
#make a column that lets me colour by newtabu, oldtabu, everything else? 

###looking at how each reef fares before/after management
scenz <- c("altsed10_basedvar", "altsed25_basedvar","tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar")

finalcov_changes <- data.frame(graz = rep(c(1,2,3), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*3)), reef = rep(rep(seq(1,75,1),each = length(scenz)),3), coral_cover = NA, malg_cover = NA, ratio = NA)

finalcov_percentchange <- data.frame(graz = rep(c(1,2,3), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*3)), reef = rep(rep(seq(1,75,1),each = length(scenz)),3), coral_cover = NA, malg_cover = NA)

for(i in 1:3){ 
  for(j in 1:length(scenz)){
    #scens[1] = "base_dvar"
    finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[j]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == scens[(j+1)]] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == scens[1]]
    finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[j]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == scens[(j+1)]] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == scens[1]]
    finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[j]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == scens[(j+1)]] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == scens[1]]
    finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[j]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == scens[(j+1)]] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == scens[1]])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == scens[1]])*100
  }
}

# New facet label names
grazinglevels.labs <- c("Low", "Medium", "High")
names(grazinglevels.labs) <- c("1", "2","3")
#grazinglevels.labs <- c("Empirical", "Pessimistic", "Middling", "Optimistic")
#names(grazinglevels.labs) <- c("1", "2", "3","4")

changeincoralcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = coral_cover*100, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq", "5km", "10", "25", "qq10", "qq25","30", "40", "50", "60", "70", "80", "90"))+
  scale_colour_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq = 2km tabu increase within qoliqoli", "5km = 5km tabu increase", "10 = 10% Water Quality Improvement", "25 = 25% Water Quality Improvement", "qq10 = Low Mixed", "qq25 = High Mixed", "30 = 30% Water Quality Improvement", "40 = 40% Water Quality Improvement", "50 = 50% Water Quality Improvement", "60 = 60% Water Quality Improvement", "70 = 70% Water Quality Improvement", "80 = 80% Water Quality Improvement", "90 = 90% Water Quality Improvement"), name = "Management Strategy")+
  ylab("Change in % Coral Cover")+
  geom_hline(yintercept = 0, color = "black")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/FinalPlots/changeincoralcover_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

changeinmalgcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = malg_cover*100, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq", "5km", "10", "25", "qq10", "qq25","30", "40", "50", "60", "70", "80", "90"))+
  scale_colour_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq = 2km tabu increase within qoliqoli", "5km = 5km tabu increase", "10 = 10% Water Quality Improvement", "25 = 25% Water Quality Improvement", "qq10 = Low Mixed", "qq25 = High Mixed", "30 = 30% Water Quality Improvement", "40 = 40% Water Quality Improvement", "50 = 50% Water Quality Improvement", "60 = 60% Water Quality Improvement", "70 = 70% Water Quality Improvement", "80 = 80% Water Quality Improvement", "90 = 90% Water Quality Improvement"), name = "Management Strategy")+
  ylab("Change in % Macroalgae Cover")+
  geom_hline(yintercept = 0, color = "black")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/SuppTextSims/MultiAltSedInterventions/FinalPlots/changeinmalgcover_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

#should i do this for the scattered sed interventions also? nah
#11.9.2022: how many reefs have less than 1% coral cover under each grazing scenario, under each intervention?
finalcov_restr_lessone <- finalcov_restr
finalcov_restr_lessone <- finalcov_restr_lessone[finalcov_restr_lessone$coral_cover < 0.01,]
#now look at the lengths of each category
#scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed")
#30altsed: low = 63, medium = 39, high = 39
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "30altsed" & finalcov_restr_lessone$graz == 3])
#40altsed: low = 59, medium = 35, high = 35
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "40altsed" & finalcov_restr_lessone$graz == 3])
#50altsed: low = 49, medium = 35, high = 35
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "50altsed" & finalcov_restr_lessone$graz == 3])
#60altsed: low = 45, medium = 34, high = 34
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "60altsed" & finalcov_restr_lessone$graz == 3])
#70altsed: low = 35, medium = 32, high = 32
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "70altsed" & finalcov_restr_lessone$graz == 3])
#80altsed: low = 33, medium = 31, high = 31
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "80altsed" & finalcov_restr_lessone$graz == 3])
#90altsed: low = 30, medium = 30, high = 30
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 3])

#at least 30 reefs with less than 1% coral cover, are they the same reefs? yes
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 3] %in% 
  finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "30altsed" & finalcov_restr_lessone$graz == 3] #all true
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 3] %in%
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "40altsed" & finalcov_restr_lessone$graz == 3] #all true
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 3] %in%
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "50altsed" & finalcov_restr_lessone$graz == 3] #all true
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 3] %in%
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "60altsed" & finalcov_restr_lessone$graz == 3] #all true
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 3] %in%
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "70altsed" & finalcov_restr_lessone$graz == 3] #all true
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 3] %in%
finalcov_restr_lessone$reef[finalcov_restr_lessone$scenario == "80altsed" & finalcov_restr_lessone$graz == 3] #all true
