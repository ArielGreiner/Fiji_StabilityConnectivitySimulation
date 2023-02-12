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

#load in the master dataset with sediment values and extended tabu grazing levels
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_5.10.2022.RData")

###What effect did the management scenarios have on the system?
#want to look at this across the 4 base scenarios and across d = 0.24 and d allowed to vary
#only want to look at the sedimentation scenario compared with the 'd allowed to vary' scenario

numreefs <- 75

#LOAD IN THE FINAL VALUES
#guide <- matrix(data = c(1,2,3,4,1,2,3,4,1,1,1,1,2,2,2,2), nrow = 8, ncol = 2) #col1 defines grazing rate, col2 defines which d scenario
finalcover <- list()

#multisedscenario
sedlevels <- 9
#saveRDS(datalist, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor",l,"_5.16.2022.rds"))

for(j in 1:4){ #dim(guide)[1]
  #j <- guide[i,1] #grazing scenario
  #k <- guide[i,2] #d scenario
  r <- 125 #rate coral recruits onto/overgrows turf algae
  gamma <- 900 #rate macroalgae recruits onto/overgrows turf algae
  finalcov <- data.frame(sedlevel = rep(seq(0,sedlevels,1),each=numreefs), reef = rep(seq(1,numreefs,1),sedlevels+1), coral=NA, malg = NA)
  #finalcov <- data.frame(reef = seq(1,numreefs,1), base_dvar_coral = NA, base_dvar_malg = NA, altsed_coral = NA, altsed_malg = NA, altsed2_coral = NA, altsed2_malg = NA, altsed3_coral = NA, altsed3_malg = NA)
  
  base_data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/base/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_base_5.10.2022.rds"))
  base_benthic_traj <- base_data[[3]]
  finalcov$coral[finalcov$sedlevel == 0] <- base_benthic_traj$final_coralcover
  finalcov$malg[finalcov$sedlevel == 0] <- base_benthic_traj$final_malgcover
  
  for(l in 1:9){
  data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_sedfactor",l,"_5.16.2022.rds"))
  benthic_traj <- data[[3]]
  finalcov$coral[finalcov$sedlevel == l] <- benthic_traj$final_coralcover
  finalcov$malg[finalcov$sedlevel == l] <- benthic_traj$final_malgcover
  
  #just in case
  benthic_traj <- data <- NULL
  }
  finalcover[[j]] <- finalcov
  
  #just in case
  finalcov <- NULL
}

#want to compare: base_dvar_coral to the different levels of altsed_coral
#ratiocover <- list() 

gen_results <- data.frame(graz = c(1,2,3,4), sedlevel = rep(seq(0,sedlevels,1), each = 4), avg_coral_cover=NA, avg_malg_cover = NA, thirty_coral_cover=NA, thirty_malg_cover = NA, ratio = NA)
#gen_results_coral <- data.frame(graz = c(1,2,3,4), avgcoral_base = NA, avgcoral_base_dvar = NA, avgcoral_altsed = NA, avgcoral_tabu_dsame = NA, avgcoral_tabu_dvar = NA, thirtycoral_base = NA, thirtycoral_base_dvar = NA, thirtycoral_altsed = NA, thirtycoral_tabu_dsame = NA, thirtycoral_tabu_dvar = NA, ratio_base = NA, ratio_base_dvar = NA, ratio_altsed = NA, ratio_tabu_dsame = NA, ratio_tabu_dvar = NA)

#gen_results_malg <- data.frame(graz = c(1,2,3,4), avgmalg_base = NA, avgmalg_base_dvar = NA, avgmalg_altsed = NA, avgmalg_tabu_dsame = NA, avgmalg_tabu_dvar = NA, thirtymalg_base = NA, thirtymalg_base_dvar = NA, thirtymalg_altsed = NA, thirtymalg_tabu_dsame = NA, thirtymalg_tabu_dvar = NA)

#make a dataframe that gives the C:M ratio for all of the reefs under each situation, if >1 means that coral > malg cover
ratiocovr <- data.frame(graz = rep(c(1,2,3,4), each = (1+sedlevels)*numreefs), sedlevel = rep(seq(0,sedlevels,1), each = numreefs), reef = rep(seq(1,numreefs,1),(1+sedlevels)*4), ratio = NA)

for(i in 1:4){
  finalcovr <- finalcover[[i]]
  for(l in 0:9){
  
  ###Q1: Did average coral cover increase?
  gen_results$avg_coral_cover[gen_results$graz==i & gen_results$sedlevel == l] <- mean(finalcovr$coral[finalcovr$sedlevel == l])
  gen_results$avg_malg_cover[gen_results$graz==i & gen_results$sedlevel == l] <- mean(finalcovr$malg[finalcovr$sedlevel == l])
  
  ###Q2: Do more reefs have >30% coral cover?
  gen_results$thirty_coral_cover[gen_results$graz==i & gen_results$sedlevel == l] <- length(which(finalcovr$coral[finalcovr$sedlevel == l] > 0.3))
  gen_results$thirty_malg_cover[gen_results$graz==i & gen_results$sedlevel == l] <- length(which(finalcovr$malg[finalcovr$sedlevel == l] > 0.3))
  
  ###Q3: Are more reefs trending towards coral > macroalgae with the mgmt?
  #do it for each reef first
  #ratiocovr <- data.frame(graz = rep(c(1,2,3,4), each = (1+sedlevels)*numreefs), sedlevel = rep(seq(0,sedlevels,1), each = numreefs), reef = rep(seq(1,numreefs,1),(1+sedlevels)*4), ratio = NA)
  ratiocovr$ratio[ratiocovr$graz==i & ratiocovr$sedlevel == l] <- (1+finalcovr$coral[finalcovr$sedlevel == l])/(1+finalcovr$malg[finalcovr$sedlevel == l])
  
  #can then sum the ratios across all of the reefs to see the trends
  gen_results$ratio[gen_results$graz==i & gen_results$sedlevel == l] <- numreefs - length(!which(ratiocovr$ratio[ratiocovr$graz==i & ratiocovr$sedlevel == l] < 1))
}
}

#plotting - violin plots maybe? to start off
finalcover_restr <- data.frame(graz = rep(c(1,2,3,4),each=numreefs*(sedlevels+1)), sedlevel = rep(seq(0,sedlevels,1),each=numreefs), reef = rep(seq(1,numreefs,1),sedlevels+1), coral_cover=NA, malg_cover = NA)

#this should be fine since the dataframe is the same as finalcover
for(i in 1:4){
finalcover_restr$coral_cover[finalcover_restr$graz == i] <- finalcover[[i]]$coral
finalcover_restr$malg_cover[finalcover_restr$graz == i] <- finalcover[[i]]$malg
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

coralcover_multised_mgmtstressorimpactassessment_summary <- ggplot(finalcover_restr, aes(x=as.factor(sedlevel*10), y=coral_cover*100,fill=as.factor(sedlevel*10))) +
  geom_violin(color = NA)+
  ggtitle("Coral Cover")+
  #scale_x_discrete(limits=c("base", "tabu_dsame", "base_dvar","altsed","tabu_dvar"))+
  #scale_fill_manual(values=c("#56B4E9", "#E69F00", "#56B4E9","#E69F00","#56B4E9"))+
  guides(fill=guide_legend(title="% Increase in Water Quality"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(coralcover_multised_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/coralcover_multised_mgmtstressorimpactassessment_summary_altsed_5.25.2022.png"), bg = "transparent", height = 10, width = 10)

malgcover_multised_mgmtstressorimpactassessment_summary <- ggplot(finalcover_restr, aes(x=as.factor(sedlevel*10), y=malg_cover*100,fill=as.factor(sedlevel*10))) +
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover")+
  #scale_x_discrete(limits=c("base", "tabu_dsame", "base_dvar","altsed","tabu_dvar"))+
  #scale_fill_manual(values=c("#56B4E9", "#E69F00", "#56B4E9","#E69F00","#56B4E9"))+
  guides(fill=guide_legend(title="% Increase in Water Quality"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(malgcover_multised_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/malgcover_multised_mgmtstressorimpactassessment_summary_altsed_5.25.2022.png"), bg = "transparent", height = 10, width = 10)

ggplot(finalcover_restr[finalcover_restr$graz == 3,], aes(x=as.factor(sedlevel*10), y=malg_cover*100,fill=as.factor(sedlevel*10))) +
  geom_violin(color = NA)+
  ggtitle("Macroalgae Cover")+
  #scale_x_discrete(limits=c("base", "tabu_dsame", "base_dvar","altsed","tabu_dvar"))+
  #scale_fill_manual(values=c("#56B4E9", "#E69F00", "#56B4E9","#E69F00","#56B4E9"))+
  guides(fill=guide_legend(title="% Increase in Water Quality"))+
  #geom_boxplot(width=0.1)
  geom_hline(yintercept = 30, color = "red")+
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid


ratiocover_multised_mgmtstressorimpactassessment_summary <- ggplot(ratiocovr, aes(x=as.factor(sedlevel*10), y=ratio,fill=as.factor(sedlevel*10))) +
  geom_violin(color = NA)+
  ggtitle("Ratio of Coral:Macroalgae Cover on Reefs")+
  #scale_x_discrete(limits=c("base", "tabu_dsame", "base_dvar","altsed","tabu_dvar"))+
  #scale_fill_manual(values=c("#56B4E9", "#E69F00", "#56B4E9","#E69F00","#56B4E9"))+
  guides(fill=guide_legend(title="% Increase in Water Quality"))+
  #geom_boxplot(width=0.1)
  stat_summary(fun.data=data_summary, color = "black")+
  geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) #looks horrid
  facet_wrap(~graz)
ggsave(ratiocover_multised_mgmtstressorimpactassessment_summary, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/MultiAltSedScenario_5.2022/ratiocover_multised_mgmtstressorimpactassessment_summary_altsed_5.25.2022.png"), bg = "transparent", height = 10, width = 10)

###looking at how each reef fares before/after management
#plot managed-base for each reef, ratio and coral cover, for every scenario

finalcov_changes <- data.frame(graz = rep(c(1,2,3,4), each = (sedlevels*numreefs)), scenario = rep(seq(1,9,1),(numreefs*4)), reef = rep(rep(seq(1,75,1),each = sedlevels),4), coral_cover = NA, malg_cover = NA, ratio = NA)

finalcov_percentchange <- data.frame(graz = rep(c(1,2,3,4), each = (sedlevels*numreefs)), scenario = rep(seq(1,9,1),(numreefs*4)), reef = rep(rep(seq(1,75,1),each = sedlevels),4), coral_cover = NA, malg_cover = NA)

for(i in 1:4){
  for(j in 1:sedlevels){
    finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == j] <- finalcover_restr$coral_cover[finalcover_restr$graz == i & finalcover_restr$sedlevel == j] - finalcover_restr$coral_cover[finalcover_restr$graz == i & finalcover_restr$sedlevel == 0]
    
    finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == j] <- ratiocovr$ratio[ratiocovr$graz == i & ratiocovr$sedlevel == j] - ratiocovr$ratio[ratiocovr$graz == i & ratiocovr$sedlevel == 0]
    
    finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == j] <- ((finalcover_restr$coral_cover[finalcover_restr$graz == i & finalcover_restr$sedlevel == j] - finalcover_restr$coral_cover[finalcover_restr$graz == i & finalcover_restr$sedlevel == 0])/finalcover_restr$coral_cover[finalcover_restr$graz == i & finalcover_restr$sedlevel == 0])*100  
  }
}

ggplot(finalcov_changes, aes(x = scenario*10, y = coral_cover*100)) +
  geom_point()+
  scale_x_continuous(name = "water qual improvement %", breaks = c(10,20,30,40,50,60,70,80,90))+
  ylab("Change in Coral Cover")+
  facet_wrap(~graz)

ggplot(finalcov_changes, aes(x = scenario*10, y = ratio)) +
  geom_point()+
  scale_x_continuous(name = "water qual improvement %", breaks = c(10,20,30,40,50,60,70,80,90))+
  ylab("Change in C:M Ratio")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~graz)
