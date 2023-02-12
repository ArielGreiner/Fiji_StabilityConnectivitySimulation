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
#tabu 2km, tabu 5km, tabu qoli qoli, 10% alt sed, 2km+10%, qoli qoli + 10%
#the 10-90% sedimentation plots are already done, but could move them into this document?

numreefs <- 75
r <- 125 #rate coral recruits onto/overgrows turf algae
gamma <- 900 #rate macroalgae recruits onto/overgrows turf algae
zerocutoff <- 0.01

scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed")
finalcov_restr <- data.frame(graz = rep(c(1,2,3,4),each=length(scens)*numreefs), scenario = rep(scens, each = numreefs), reef = seq(1,numreefs,1), coral_cover=NA, malg_cover = NA, ratio = NA, numzerocoral = NA)

#to make the violin plots need to re-structure the dataframes
#this re-structuring could replace some of what's above...
for(j in 1:4){ 
  for(k in 1:length(scens)){
    
    #load in data
    #base_dvar
    if(k == 1){
      #datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/base/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_base_5.10.2022.rds"))
    }
    
    #10altsed
    if(k == 2){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/alt/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_alt_5.10.2022.rds"))
    }
    
    #25altsed
    if(k == 3){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AltSed_NewTabu_7.2022/MixedScenario_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_8.11.2022.rds"))
    }
    
    #tabu_qoliqoli_dvar
    if(k == 4){
      data <- readRDS(file =paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/qoliqolidegreebuffer/TabuScenarioqoliqoli_fullsimrun_allherbs_dval_sed_determined_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_7.1.2022.rds"))
    }
    
    #tabu_0.05_dvar
    if(k == 5){
      data <- readRDS(file =paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/TabuScenarios_4.5.2022/0.05degreebuffer/TabuScenario0.05_fullsimrun_allherbs_dval_sed_determined_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.26.2022.rds"))
    }
    
    #10% + qoli qoli
    if(k == 6){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AltSed_NewTabu_7.2022/MixedScenario_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_7.4.2022.rds"))
    }
    
    #25% + qoli qoli
    if(k == 7){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AltSed_NewTabu_7.2022/MixedScenario_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_8.11.2022.rds"))}
    
    #30%
    if(k == 8){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor3_5.16.2022.rds"))
    }
    
    #40%
    if(k == 9){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor4_5.16.2022.rds"))
    }
    
    #50%
    if(k == 10){
    data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor5_5.16.2022.rds"))
    }
    
    #60%
    if(k == 11){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor6_5.16.2022.rds"))
    }
    
    #70%
    if(k == 12){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor7_5.16.2022.rds"))
    }
    
    #80%
    if(k == 13){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor8_5.16.2022.rds"))
    }
    
    #90%
    if(k == 14){
      data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor9_5.16.2022.rds"))
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

# New facet label names
grazinglevels.labs <- c("Low", "Medium", "High")
names(grazinglevels.labs) <- c("2", "3","4")

finalcov_restr <- finalcov_restr[finalcov_restr$graz != 1,]

coralcover_mgmtstressorimpactassessmentfull_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=coral_cover*100,colour=scenario, alpha = 0.7)) + 
  geom_quasirandom()+
  #geom_violin(color = NA)+
  ggtitle("Coral Cover, dvar only")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"), labels=c("base", "qq", "5km", "10", "25", "qq10", "qq25","30", "40", "50", "60", "70", "80", "90"))+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"), labels=c("base", "qq = 2km tabu increase within qoliqoli", "5km = 5km tabu increase", "10 = 10% Water Quality Improvement", "25 = 25% Water Quality Improvement", "qq10 = Low Mixed", "qq25 = High Mixed", "30 = 30% Water Quality Improvement", "40 = 40% Water Quality Improvement", "50 = 50% Water Quality Improvement", "60 = 60% Water Quality Improvement", "70 = 70% Water Quality Improvement", "80 = 80% Water Quality Improvement", "90 = 90% Water Quality Improvement"), name = "Management Intervention")+ 
  geom_hline(yintercept = 30, color = "red")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(coralcover_mgmtstressorimpactassessmentfull_summary_beeswarm, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/plots_dvarmgmtonly/morealtsed/coralcover_mgmtstressorimpactassessmentfull_summary_beeswarm_moresed_11.8.2022.png"), bg = "transparent", height = 10, width = 10)


###looking at how each reef fares before/after management
scenz <- c("altsed10_basedvar", "altsed25_basedvar","tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar")

finalcov_changes <- data.frame(graz = rep(c(1,2,3,4), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*4)), reef = rep(rep(seq(1,75,1),each = length(scenz)),4), coral_cover = NA, malg_cover = NA, ratio = NA)

finalcov_percentchange <- data.frame(graz = rep(c(1,2,3,4), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*4)), reef = rep(rep(seq(1,75,1),each = length(scenz)),4), coral_cover = NA, malg_cover = NA)

#scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed")
for(i in 2:4){ #2-4 because removed graz = 1
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
names(grazinglevels.labs) <- c("2", "3","4")

finalcov_changes <- finalcov_changes[finalcov_changes$graz != 1,]


changeincoralcover <- ggplot(finalcov_changes, aes(x = scenario, y = coral_cover*100)) +
  geom_point(aes(colour = scenario))+
  ylab("Change in % Coral Cover")+
  scale_x_discrete(limits=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq", "5km", "10", "25", "qq10", "qq25","30", "40", "50", "60", "70", "80", "90"))+
  scale_colour_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq = 2km tabu increase within qoliqoli", "5km = 5km tabu increase", "10 = 10% Water Quality Improvement", "25 = 25% Water Quality Improvement", "qq10 = Low Mixed", "qq25 = High Mixed", "30 = 30% Water Quality Improvement", "40 = 40% Water Quality Improvement", "50 = 50% Water Quality Improvement", "60 = 60% Water Quality Improvement", "70 = 70% Water Quality Improvement", "80 = 80% Water Quality Improvement", "90 = 90% Water Quality Improvement"), name = "Management Intervention")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/plots_dvarmgmtonly/morealtsed/changeincoralcover_mgmtstressorimpactassessment_summary_fullsed_11.8.2022.png"), bg = "transparent", height = 10, width = 10)

changeinmalgcover <- ggplot(finalcov_changes, aes(x = scenario, y = malg_cover*100)) +
  geom_point(aes(colour = scenario))+
  ylab("Change in % Macroalgal Cover")+
  scale_x_discrete(limits=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq", "5km", "10", "25", "qq10", "qq25","30", "40", "50", "60", "70", "80", "90"))+
  scale_colour_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qq = 2km tabu increase within qoliqoli", "5km = 5km tabu increase", "10 = 10% Water Quality Improvement", "25 = 25% Water Quality Improvement", "qq10 = Low Mixed", "qq25 = High Mixed", "30 = 30% Water Quality Improvement", "40 = 40% Water Quality Improvement", "50 = 50% Water Quality Improvement", "60 = 60% Water Quality Improvement", "70 = 70% Water Quality Improvement", "80 = 80% Water Quality Improvement", "90 = 90% Water Quality Improvement"), name = "Management Intervention")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/plots_dvarmgmtonly/morealtsed/changeinmalgcover_mgmtstressorimpactassessment_summary_fullsed_11.8.2022.png"), bg = "transparent", height = 10, width = 10)

#11.9.2022: how many reefs have less than 1% coral cover under each grazing scenario, under each intervention?
finalcov_restr_lessone <- finalcov_restr
finalcov_restr_lessone <- finalcov_restr_lessone[finalcov_restr_lessone$coral_cover < 0.01,]
#now look at the lengths of each category
#scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar", "30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed")
#10altsed: low = 43, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "10altsed" & finalcov_restr_lessone$graz == 4])
#25altsed: low = 32, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "25altsed" & finalcov_restr_lessone$graz == 4])
#tabu_qoliqoli_dvar: low = 41, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabu_qoliqoli_dvar" & finalcov_restr_lessone$graz == 4])
#tabu_0.05_dvar: low = 33, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabu_0.05_dvar" & finalcov_restr_lessone$graz == 4])
#tabuqoliqoli_10sed_dvar: low = 32, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabuqoliqoli_10sed_dvar" & finalcov_restr_lessone$graz == 4])
#tabuqoliqoli_25sed_dvar: low = 30, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabuqoliqoli_25sed_dvar" & finalcov_restr_lessone$graz == 2])
#30altsed: low = 31, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "30altsed" & finalcov_restr_lessone$graz == 4])
#40altsed: low = 30, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "40altsed" & finalcov_restr_lessone$graz == 4])
#50altsed: low = 29, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "50altsed" & finalcov_restr_lessone$graz == 4])
#60altsed: low = 28, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "60altsed" & finalcov_restr_lessone$graz == 4])
#70altsed: low = 28, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "70altsed" & finalcov_restr_lessone$graz == 4])
#80altsed: low = 28, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "80altsed" & finalcov_restr_lessone$graz == 4])
#90altsed: low = 28, medium = 28, high = 28
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "90altsed" & finalcov_restr_lessone$graz == 4])



#need to update from here

changeinCMratio <- ggplot(finalcov_changes, aes(x = scenario, y = ratio)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed","30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"))+
  scale_colour_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8","#2ca4e8"),breaks=c("tabu_qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar","altsed25_basedvar","altsed10_qoliqoli_basedvar","altsed25_qoliqoli_basedvar","altsed30_basedvar", "altsed40_basedvar", "altsed50_basedvar", "altsed60_basedvar", "altsed70_basedvar", "altsed80_basedvar", "altsed90_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed","30altsed", "40altsed", "50altsed", "60altsed", "70altsed", "80altsed", "90altsed"))+
  ylab("Change in C:M Ratio")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~graz)
ggsave(changeinCMratio, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/plots_dvarmgmtonly/morealtsed/changeinCMratio_mgmtstressorimpactassessment_summary_fullsed_8.29.2022.png"), bg = "transparent", height = 10, width = 10)


#####old

###looking at how each reef fares before/after management
#plot managed-base for each reef, ratio and coral cover, for every scenario
#make a column that lets me colour by newtabu, oldtabu, everything else? 
###if want to keep this up, need to also add a qoliqoli one...

#load in the 0.02 tabus
#load in the master dataset with sediment values and extended tabu grazing levels
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_5.10.2022.RData")
#benthicfish_masterdataset_sed_abrabr <- benthicfish_masterdataset_sed %>%
#  select(site, longitude, latitude, sample_date)
#write.csv(benthicfish_masterdataset_sed_abrabr, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/75Reefs_latlongdate.csv")
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
benthicfish_masterdataset_sed <- NULL

tabus_0.02 <- seq(1,75,1)
tabus_0.02[newtabus_0.02] <- "newtabu"
tabus_0.02[oldtabus_0.02] <- "oldtabu"
tabus_0.02[tabus_0.02 %in% seq(1,75,1)] <- "n"

tabus_0.05 <- seq(1,75,1)
tabus_0.05[newtabus_0.05] <- "newtabu"
tabus_0.05[oldtabus_0.05] <- "oldtabu"
tabus_0.05[tabus_0.05 %in% seq(1,75,1)] <- "n"

scenz <- c("tabu0.02_basedvar", "tabu0.05_basedvar", "qoliqoli_basedvar", "altsed10_basedvar", "altsed10_2km_basedvar", "altsed10_qoliqoli_basedvar", "altsed50_basedvar", "altsed90_basedvar")

finalcov_changes <- data.frame(graz = rep(c(1,2,3,4), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*4)), reef = rep(rep(seq(1,75,1),each = length(scenz)),4), identity_0.02 = rep(rep(tabus_0.02,each = length(scenz)),4), identity_0.05 = rep(rep(tabus_0.05,each = length(scenz)),4), coral_cover = NA, malg_cover = NA, ratio = NA)

finalcov_percentchange <- data.frame(graz = rep(c(1,2,3,4), each = (length(scenz)*numreefs)), scenario = rep(scenz,(numreefs*4)), reef = rep(rep(seq(1,75,1),each = length(scenz)),4), identity_0.02 = rep(rep(tabus_0.02,each = length(scenz)),4), identity_0.05 = rep(rep(tabus_0.05,each = length(scenz)),4), coral_cover = NA, malg_cover = NA)
