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

scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")
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
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "2", "3", "4", "5", "6"), name = "Management Intervention")+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+ 
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+ 
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=malg_cover*100,colour=scenario, alpha = 0.9)) + 
  geom_quasirandom()+
  #geom_violin(color = NA)+
  ggtitle("Malg Cover, dvar only")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "2", "3", "4", "5", "6"), name = "Management Intervention")+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+ 
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+ 
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5.6.2023.png"), bg = "transparent", height = 10, width = 10)



###looking at how each reef fares before/after management
#plot managed-base for each reef, ratio and coral cover, for every scenario
#make a column that lets me colour by newtabu, oldtabu, everything else? 

#load in the qoli qoli tabus
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_5.6.2023.RData")
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

##11.22.2022: plotting the new/original/non-tabus on maps for the qoliqoli variation
#worldmap <- map_data ("world", wrap = c(0, 360))
#benthicfish_masterdataset_sed$qoliqolimgmt <- "none"
#benthicfish_masterdataset_sed$qoliqolimgmt[benthicfish_masterdataset_sed$newtabu_qoliqoli > 0] <- "new"
#benthicfish_masterdataset_sed$qoliqolimgmt[benthicfish_masterdataset_sed$mgmt_abr != "LMMA"] <- "original"

#qoliqolitabureefs <- ggplot(aes(x = long, y = lat), data = worldmap) + 
#  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
#  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("qoliqoli tabu Reefs"))+
#  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, colour = qoliqolimgmt))+
#  scale_color_manual(values = c("grey","red", "purple"), name = " ", breaks = c("none", "original","new"), labels = c("Not in a tabu","Originally in a tabu","Newly in a tabu"))+
#scale_color_identity(name="Included", labels=c("yes","no"), guide="legend")+
#  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
#  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
#  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
#                     breaks = seq(160, 190, 10),
#                     labels = c(160, 170, "180/-180", -170)) +
#  coord_equal() +  theme_bw()
#ggsave(qoliqolitabureefs, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/qoliqolitabureefs_11.22.2022.png"), bg = "transparent", height = 10, width = 10)

benthicfish_masterdataset_sed <- NULL

#load in the 0.05 tabus
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.05_5.6.2023.RData")
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

##11.22.2022: plotting the new/original/non-tabus on maps for the 5km variation
#worldmap <- map_data ("world", wrap = c(0, 360))
#benthicfish_masterdataset_sed$fivekmmgmt <- "none"
#benthicfish_masterdataset_sed$fivekmmgmt[benthicfish_masterdataset_sed$newtabu_0.05 > 0] <- "new"
#benthicfish_masterdataset_sed$fivekmmgmt[benthicfish_masterdataset_sed$mgmt_abr != "LMMA"] <- "original"

#fivekmtabureefs <- ggplot(aes(x = long, y = lat), data = worldmap) + 
#  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
#  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("5km tabu Reefs"))+
#  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, colour = fivekmmgmt))+
#  scale_color_manual(values = c("grey","red", "purple"), name = " ", breaks = c("none", "original","new"), labels = c("Not in a tabu","Originally in a tabu","Newly in a tabu"))+
#scale_color_identity(name="Included", labels=c("yes","no"), guide="legend")+
#  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
#  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
#  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
#                     breaks = seq(160, 190, 10),
#                     labels = c(160, 170, "180/-180", -170)) +
#  coord_equal() +  theme_bw()
#ggsave(fivekmtabureefs, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/fivekmtabureefs_11.22.2022.png"), bg = "transparent", height = 10, width = 10)

benthicfish_masterdataset_sed <- NULL

tabus_qoliqoli <- seq(1,75,1)
tabus_qoliqoli[newtabus_qoliqoli] <- "newtabu"
tabus_qoliqoli[oldtabus_qoliqoli] <- "oldtabu"
tabus_qoliqoli[tabus_qoliqoli %in% seq(1,75,1)] <- "n"

tabus_0.05 <- seq(1,75,1)
tabus_0.05[newtabus_0.05] <- "newtabu"
tabus_0.05[oldtabus_0.05] <- "oldtabu"
tabus_0.05[tabus_0.05 %in% seq(1,75,1)] <- "n"


scenz <- c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar")

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
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[5]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
  
  finalcov_changes$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$malg_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$malg_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_changes$ratio[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$ratio[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"]
  finalcov_percentchange$coral_cover[finalcov_changes$graz == i & finalcov_changes$scenario == scenz[6]] <- ((finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] - finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])/finalcov_restr$coral_cover[finalcov_restr$graz == i & finalcov_restr$scenario == "base_dvar"])*100
}

# New facet label names
grazinglevels.labs <- c("Low", "Medium", "High")
names(grazinglevels.labs) <- c("1", "2","3")
#grazinglevels.labs <- c("Empirical", "Pessimistic", "Middling", "Optimistic")
#names(grazinglevels.labs) <- c("1", "2", "3","4")

changeincoralcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = coral_cover*100, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1", "2", "3", "4","5", "6"), name = "Management Intervention")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  ylab("Change in % Coral Cover")+
  geom_hline(yintercept = 0, color = "black")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeincoralcover_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 5)

changeinmalgcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = malg_cover*100, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1", "2", "3", "4","5", "6"), name = "Management Intervention")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  ylab("Change in % Macroalgae Cover")+
  geom_hline(yintercept = 0, color = "black")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeinmalgcover_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 5)

changeinCMratio_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = ratio, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1", "2", "3", "4","5", "6"), name = "Management Intervention")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  ylab("Change in C:M Ratio")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinCMratio_w25_abr, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeinCMratio_w25_abr_mgmtstressorimpactassessment_summary_5.6.2023.png"), bg = "transparent", height = 10, width = 5)

#looking at the tabu designations
finalcov_changes_qq <- finalcov_changes[finalcov_changes$scenario %in% c("qoliqoli_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"),]

changeincoralcover_w25_abr_qoliqolicoloured <- ggplot(finalcov_changes_qq, aes(x = scenario, y = coral_cover*100, color = identity_qoliqoli, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ylab("Change in % Coral Cover")+
  scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1","5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu","qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "black")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr_qoliqolicoloured, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeincoralcover_w25_abr_qoliqolicoloured_5.6.2023.png"), bg = "transparent", height = 10, width = 5)

changeinmalgcover_w25_abr_qoliqolicoloured <- ggplot(finalcov_changes_qq, aes(x = scenario, y = malg_cover*100, color = identity_qoliqoli, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ylab("Change in % Macroalgae Cover")+
  scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1","5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu","qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "black")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr_qoliqolicoloured, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeinmalgcover_w25_abr_qoliqolicoloured_5.6.2023.png"), bg = "transparent", height = 10, width = 5)


changeinCMratio_w25_abr_qoliqolicoloured <- ggplot(finalcov_changes_qq, aes(x = scenario, y = ratio, color = identity_qoliqoli, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ylab("Change in C:M Ratio")+
  scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1","5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu","qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinCMratio_w25_abr_qoliqolicoloured, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeinCMratio_w25_abr_qoliqolicoloured_5.6.2023.png"), bg = "transparent", height = 10, width = 5)


finalcov_changes_0.05 <- finalcov_changes[finalcov_changes$scenario %in% c("tabu0.05_basedvar"),]

changeincoralcover_w25_abr_tabu5coloured <- ggplot(finalcov_changes_0.05, aes(x = scenario, y = coral_cover*100, color = identity_0.05, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ylab("Change in % Coral Cover")+
  scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "black")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr_tabu5coloured, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeincoralcover_w25_abr_tabu5coloured_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

changeinmalgcover_w25_abr_tabu5coloured <- ggplot(finalcov_changes_0.05, aes(x = scenario, y = malg_cover*100, color = identity_0.05, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ylab("Change in % Macroalgae Cover")+
  scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "black")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr_tabu5coloured, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeinmalgcover_w25_abr_tabu5coloured_5.6.2023.png"), bg = "transparent", height = 10, width = 10)


changeinCMratio_w25_abr_tabu5coloured <- ggplot(finalcov_changes_0.05, aes(x = scenario, y = ratio, color = identity_0.05, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ylab("Change in C:M Ratio")+
  scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinCMratio_w25_abr_tabu5coloured, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/changeinCMratio_w25_abr_tabu5coloured_5.6.2023.png"), bg = "transparent", height = 10, width = 5)

#the ones that tip backwards, how many of them are NOT the reefs that had a lower grazing rate post tabu enlargement? there's a handful
#View(finalcov_changes_qq[!(finalcov_changes_qq$reef %in% qqlowerreefs),])
#View(finalcov_changes_qq[!(finalcov_changes_0.05$reef %in% fivelowerreefs),])

#back to the coral cover plot, curious what that looks like coloured by tabu designation
finalcov_restr$identity_qoliqoli <- rep(tabus_qoliqoli,3)
finalcov_restr$identity_0.05 <- rep(tabus_0.05,3)

finalcov_restr_qqtabu <- finalcov_restr[finalcov_restr$scenario %in% c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"),]
coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus <- ggplot(finalcov_restr_qqtabu, aes(x=scenario, y=coral_cover*100,colour=identity_qoliqoli, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle("Coral Cover, dvar only, qoli qoli tabus")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus <- ggplot(finalcov_restr_qqtabu, aes(x=scenario, y=malg_cover*100,colour=identity_qoliqoli, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle("Coral Cover, dvar only, qoli qoli tabus")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

finalcov_restr_5tabu <- finalcov_restr[finalcov_restr$scenario %in% c("base_dvar", "tabu_0.05_dvar"),]
coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus <- ggplot(finalcov_restr_5tabu, aes(x=scenario, y=coral_cover*100,colour=identity_0.05, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle("Coral Cover, dvar only, 5km tabus")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_0.05_dvar"), labels=c("base", "5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus <- ggplot(finalcov_restr_5tabu, aes(x=scenario, y=malg_cover*100,colour=identity_0.05, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle("Coral Cover, dvar only, 5km tabus")+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_0.05_dvar"), labels=c("base", "5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "green4")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus_5.6.2023.png"), bg = "transparent", height = 10, width = 10)


###plot how final coral cover (base scenario) relates to immi, emi, grazing level, sedimentation level (see below for the calculation of the associated things)
#finalcov_restr, benthicfish_masterdataset_sed
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sedimentlvls_5.6.2023.RData") #benthicfish_masterdataset_sed
benthicfish_masterdataset_sed_og <- benthicfish_masterdataset_sed

#grazing level 
#add a column for grazing level into the dataframe, since the 75 reefs are always in the same order this should work
#finalcov_restr$grazinglevel[finalcov_restr$graz == 1] <- benthicfish_masterdataset_sed$comb_grazinglevel
finalcov_restr$grazinglevel[finalcov_restr$graz == 1] <- benthicfish_masterdataset_sed$comb_grazinglevel*MD_scale
finalcov_restr$grazinglevel[finalcov_restr$graz == 2] <- benthicfish_masterdataset_sed$comb_grazinglevel*ASS_scale
finalcov_restr$grazinglevel[finalcov_restr$graz == 3] <- benthicfish_masterdataset_sed$comb_grazinglevel*CD_scale

#the interventions that change the grazing rates of some of the reefs need to be plotted with their appropriate grazing rates
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_5.6.2023.RData") #benthicfish_masterdataset_sed
benthicfish_masterdataset_sed_qoliqoli <- benthicfish_masterdataset_sed
benthicfish_masterdataset_sed <- benthicfish_masterdataset_sed_og
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.05_5.6.2023.RData") #benthicfish_masterdataset_sed
benthicfish_masterdataset_sed_5km <- benthicfish_masterdataset_sed
benthicfish_masterdataset_sed <- benthicfish_masterdataset_sed_og

#finalcov_restr$grazinglevel[finalcov_restr$graz == 1 & finalcov_restr$scenario %in% c("tabu_qoliqoli_dvar","tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")] <- benthicfish_masterdataset_sed_qoliqoli$new_grazinglevel
finalcov_restr$grazinglevel[finalcov_restr$graz == 1 & finalcov_restr$scenario %in% c("tabu_qoliqoli_dvar","tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")] <- benthicfish_masterdataset_sed_qoliqoli$new_grazinglevel*MD_scale
finalcov_restr$grazinglevel[finalcov_restr$graz == 2 & finalcov_restr$scenario %in% c("tabu_qoliqoli_dvar","tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")] <- benthicfish_masterdataset_sed_qoliqoli$new_grazinglevel*ASS_scale
finalcov_restr$grazinglevel[finalcov_restr$graz == 3 & finalcov_restr$scenario %in% c("tabu_qoliqoli_dvar","tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")] <- benthicfish_masterdataset_sed_qoliqoli$new_grazinglevel*CD_scale

#finalcov_restr$grazinglevel[finalcov_restr$graz == 1 & finalcov_restr$scenario == "tabu_0.05_dvar"] <- benthicfish_masterdataset_sed_5km$new_grazinglevel
finalcov_restr$grazinglevel[finalcov_restr$graz == 1 & finalcov_restr$scenario == "tabu_0.05_dvar"] <- benthicfish_masterdataset_sed_5km$new_grazinglevel*MD_scale
finalcov_restr$grazinglevel[finalcov_restr$graz == 2 & finalcov_restr$scenario == "tabu_0.05_dvar"] <- benthicfish_masterdataset_sed_5km$new_grazinglevel*ASS_scale
finalcov_restr$grazinglevel[finalcov_restr$graz == 3 & finalcov_restr$scenario == "tabu_0.05_dvar"] <- benthicfish_masterdataset_sed_5km$new_grazinglevel*CD_scale

#it seems like the different management strategies don't really change things, so let's colour by grazing level?
coralcover_grazinglevel_dvaronlyabr_w25_grazcol <- ggplot(finalcov_restr, aes(x = grazinglevel, y = coral_cover*100, color = as.factor(graz))) +
  geom_point(size=3)+
  ylab("% Coral Cover")+
  xlab("Grazing Rate")+
  #scale_color_manual(values=c("#d7191c","#fdae61","#abdda4","#2b83ba"),breaks=c(1,2,3,4), labels=c("Empirical Grazing Scenario", "Pessimistic Grazing Scenario", "Middling Grazing Scenario","Optimistic Grazing Scenario"))+
  scale_color_manual(values=c("#fc8d59","#ffffbf","#91cf60"),breaks=c(1,2,3), labels=c("Low", "Medium","High"), name = "Grazing Scenario")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20)) #"#d7191c","#fdae61","#abdda4"
#facet_wrap(~graz)
ggsave(coralcover_grazinglevel_dvaronlyabr_w25_grazcol, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/coralcover_grazinglevel_dvaronlyabr_w25_grazcol_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

malgcover_grazinglevel_dvaronlyabr_w25_grazcol <- ggplot(finalcov_restr, aes(x = grazinglevel, y = malg_cover*100, color = as.factor(graz))) +
  geom_point(size=3)+
  ylab("% Macroalgae Cover")+
  xlab("Grazing Rate")+
  #scale_color_manual(values=c("#d7191c","#fdae61","#abdda4","#2b83ba"),breaks=c(1,2,3,4), labels=c("Empirical Grazing Scenario", "Pessimistic Grazing Scenario", "Middling Grazing Scenario","Optimistic Grazing Scenario"))+
  scale_color_manual(values=c("#fc8d59","#ffffbf","#91cf60"),breaks=c(1,2,3), labels=c("Low", "Medium","High"), name = "Grazing Scenario")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
#facet_wrap(~graz)
ggsave(malgcover_grazinglevel_dvaronlyabr_w25_grazcol, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/malgcover_grazinglevel_dvaronlyabr_w25_grazcol_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

#coral cover/grazing rate separated by grazing level
coralcover_grazinglevel_dvaronlyabr_w25_grazcol_sepbygraz <- ggplot(finalcov_restr, aes(x = grazinglevel, y = coral_cover*100, color = as.factor(graz))) +
  geom_point(size=3)+
  ylab("% Coral Cover")+
  xlab("Grazing Rate")+
  #scale_color_manual(values=c("#d7191c","#fdae61","#abdda4","#2b83ba"),breaks=c(1,2,3,4), labels=c("Empirical Grazing Scenario", "Pessimistic Grazing Scenario", "Middling Grazing Scenario","Optimistic Grazing Scenario"))+
  scale_color_manual(values=c("#fc8d59","#ffffbf","#91cf60"),breaks=c(1,2,3), labels=c("Low", "Medium","High"), name = "Grazing Scenario")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))+
  facet_wrap(~graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(coralcover_grazinglevel_dvaronlyabr_w25_grazcol_sepbygraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/coralcover_grazinglevel_dvaronlyabr_w25_grazcol_sepbygraz_5.6.2023.png"), bg = "transparent", height = 10, width = 10)


#mortality level
#add a column for mortality level into the dataframe, since the 75 reefs are always in the same order this should work
#11.9.2022 - since im altering the mortality level of the reefs for some of the scenarios, need to reflect that here
finalcov_restr$mortalitylevel[finalcov_restr$scenario == "base_dvar"] <- benthicfish_masterdataset_sed$sediment_altalt
finalcov_restr$mortalitylevel[finalcov_restr$scenario == "tabu_qoliqoli_dvar"] <- benthicfish_masterdataset_sed$sediment_altalt
finalcov_restr$mortalitylevel[finalcov_restr$scenario == "tabu_0.05_dvar"] <- benthicfish_masterdataset_sed$sediment_altalt
finalcov_restr$mortalitylevel[finalcov_restr$scenario == "10altsed"] <- benthicfish_masterdataset_sed$sediment_altalt*(1-0.1)
finalcov_restr$mortalitylevel[finalcov_restr$scenario == "tabuqoliqoli_10sed_dvar"] <- benthicfish_masterdataset_sed$sediment_altalt*(1-0.1)
finalcov_restr$mortalitylevel[finalcov_restr$scenario == "25altsed"] <- benthicfish_masterdataset_sed$sediment_altalt*(1-0.25)
finalcov_restr$mortalitylevel[finalcov_restr$scenario == "tabuqoliqoli_25sed_dvar"] <- benthicfish_masterdataset_sed$sediment_altalt*(1-0.25)

#it seems like the different management strategies don't really change things, so let's colour by grazing level?
coralcover_mortalitylevel_dvaronlyabr_w25_grazcol <- ggplot(finalcov_restr, aes(x = mortalitylevel, y = coral_cover*100, color = as.factor(graz))) +
  geom_point(size=3)+
  ylab("% Coral Cover")+
  xlab("Mortality Rate")+ 
  #scale_color_manual(values=c("#d7191c","#fdae61","#abdda4","#2b83ba"),breaks=c(1,2,3,4), labels=c("Empirical Grazing Scenario", "Pessimistic Grazing Scenario", "Middling Grazing Scenario","Optimistic Grazing Scenario"))+
  scale_color_manual(values=c("#fc8d59","#ffffbf","#91cf60"),breaks=c(1,2,3), labels=c("Low", "Medium","High"), name = "Grazing Scenario")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
#facet_wrap(~graz)
ggsave(coralcover_mortalitylevel_dvaronlyabr_w25_grazcol, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/coralcover_mortalitylevel_dvaronlyabr_w25_grazcol_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

malgcover_mortalitylevel_dvaronlyabr_w25_grazcol <- ggplot(finalcov_restr, aes(x = mortalitylevel, y = malg_cover*100, color = as.factor(graz))) +
  geom_point(size=3)+
  ylab("% Macroalgae Cover")+
  xlab("Mortality Rate")+ 
  #scale_color_manual(values=c("#d7191c","#fdae61","#abdda4","#2b83ba"),breaks=c(1,2,3,4), labels=c("Empirical Grazing Scenario", "Pessimistic Grazing Scenario", "Middling Grazing Scenario","Optimistic Grazing Scenario"))+
  scale_color_manual(values=c("#fc8d59","#ffffbf","#91cf60"),breaks=c(1,2,3), labels=c("Low", "Medium","High"), name = "Grazing Scenario")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
#facet_wrap(~graz)
ggsave(malgcover_mortalitylevel_dvaronlyabr_w25_grazcol, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/malgcover_mortalitylevel_dvaronlyabr_w25_grazcol_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

#immigration - emigration level
#probably makes most sense to plot immigration-emigration rather than either one independently
#pc_val - pcout_val for coral
#add a column for immi-emi level into the dataframe, since the 75 reefs are always in the same order this should work
finalcov_restr$coralmovementlevel <- pc_val - pcout_val
finalcov_restr$malgmovementlevel <- pm_val - pmout_val

#colour by grazing level instead?
coralcover_coralmovementlevel_dvaronlyabr_w25_grazcol <- ggplot(finalcov_restr, aes(x = coralmovementlevel, y = coral_cover*100, color = as.factor(graz))) +
  geom_point(size=3)+
  ylab("Coral Cover")+
  xlab("Coral Net Migration")+
  #scale_color_manual(values=c("#d7191c","#fdae61","#abdda4","#2b83ba"),breaks=c(1,2,3,4), labels=c("Empirical Grazing Scenario", "Pessimistic Grazing Scenario", "Middling Grazing Scenario","Optimistic Grazing Scenario"))+
  scale_color_manual(values=c("#fc8d59","#ffffbf","#91cf60"),breaks=c(1,2,3), labels=c("Low", "Medium","High"), name = "Grazing Scenario")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
#facet_wrap(~graz)
ggsave(coralcover_coralmovementlevel_dvaronlyabr_w25_grazcol, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/coralcover_coralmovementlevel_dvaronlyabr_w25_grazcol_5.6.2023.png"), bg = "transparent", height = 10, width = 10)

##NEED TO MAKE finalcov_restr_baseonly from finalcov_restr BEFORE RUN THIS CODE (made below)

finalcov_restr_baseonly <- finalcov_restr[finalcov_restr$scenario == "base_dvar",]

#Add lat/long to finalcov_restr_baseonly, since the reefs are always in the same order this should work? (it'll just x3)
finalcov_restr_baseonly$long.recenter <- benthicfish_masterdataset_sed$long.recenter
finalcov_restr_baseonly$latitude <- benthicfish_masterdataset_sed$latitude

#normal rounding rules first
finalcov_restr_baseonly$coralcovr_abr <- round_any((finalcov_restr_baseonly$coral_cover*100),10) 
#for those where the un-rounded versions are >50, call them >50
finalcov_restr_baseonly$coralcovr_abr[which(finalcov_restr_baseonly$coral_cover > 0.5)] <- 60

worldmap <- map_data ("world", wrap = c(0, 360))

#final coral cover
#would be great if could put all of these into one figure
#maybe use this: https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
for(i in 1:3){ 
  RoundedFinalCoralCov_basedvar_consistentcolours <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "black") + #colour = "grey65"
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Coral Cover basedvar, Graz ", i))+
    geom_point(data = finalcov_restr_baseonly[finalcov_restr_baseonly$graz == i,], aes(x = long.recenter, y = latitude, color = as.factor(coralcovr_abr)))+
    scale_color_manual(values = c("grey","#3288bd","#66c2a5","#abdda4","#e6f598","#fee08b","#d53e4f"),breaks = c(0,10,20,30,40,50,60), labels = c("0","10","20","30","40","50",">50"), name = "Rounded Final % Coral Cover")+
    #scale_color_manual(values=c("black","#5e4fa2","#3288bd","#66c2a5","#abdda4","#e6f598","#fee08b","#fdae61","#f46d43","#d53e4f","#9e0142"),breaks=c(seq(0,100,10)))+
    #scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-18.5,-16.5)) +  #c(-19,-16)) #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 1) #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(RoundedFinalCoralCov_basedvar_consistentcolours, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/FinalPlots/RoundedFinalCoralCov_basedvar_consistentcolours_graz",i,"_5.6.2023_blk.png"), bg = "transparent", height = 10, width = 10)
  
  #RoundedFinalCoralCov_basedvar_consistentcolours <-  
  #    ggplot(aes(x = long, y = lat), data = worldmap) + 
  #    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  #    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Coral Cover basedvar, Graz ", i))+
  #    geom_point(data = finalcov_restr[finalcov_restr$scenario == "base_dvar" & finalcov_restr$graz == i,], aes(x = long.recenter, y = latitude, color = as.factor(round_any((coral_cover*100),10))))+
  #    scale_color_manual(values=c("black","#5e4fa2","#3288bd","#66c2a5","#abdda4","#e6f598","#fee08b","#fdae61","#f46d43","#d53e4f","#9e0142"),breaks=c(seq(0,100,10)))+
  #scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
  #    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  #    scale_y_continuous(limits = c(-18.5,-16.5)) +  #c(-19,-16)) #c(-20,-15) #c(-19,-17)
  #    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 1) #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
  #                       breaks = seq(160, 190, 10),
  #                       labels = c(160, 170, "180/-180", -170)) +
  #    coord_equal() +  theme_bw()
  #  ggsave(RoundedFinalCoralCov_basedvar_consistentcolours, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/plots_dvarmgmtonly/RoundedFinalCoralCov_basedvar_consistentcolours_graz",i,"_8.9.2022.png"), bg = "transparent", height = 10, width = 10)
}

#11.9.2022: how many reefs have less than 1% coral cover under each grazing scenario, under each intervention?
finalcov_restr_lessone <- finalcov_restr
finalcov_restr_lessone <- finalcov_restr_lessone[finalcov_restr_lessone$coral_cover < 0.01,]
#now look at the lengths of each category
#scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")
#base_dvar: low = 71, medium = 46, high = 44
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "base_dvar" & finalcov_restr_lessone$graz == 3])
#10altsed: low = 69, medium = 45, high = 43
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "10altsed" & finalcov_restr_lessone$graz == 3])
#25altsed: low = 64, medium = 41, high = 40
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "25altsed" & finalcov_restr_lessone$graz == 3])
#tabu_qoliqoli_dvar: low = 66, medium = 46, high = 44
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabu_qoliqoli_dvar" & finalcov_restr_lessone$graz == 3])
#tabu_0.05_dvar: low = 66, medium = 45, high = 44
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabu_0.05_dvar" & finalcov_restr_lessone$graz == 3])
#tabuqoliqoli_10sed_dvar: low = 64, medium = 45, high = 43
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabuqoliqoli_10sed_dvar" & finalcov_restr_lessone$graz == 3])
#tabuqoliqoli_25sed_dvar: low = 61, medium = 41, high = 40
length(finalcov_restr_lessone$coral_cover[finalcov_restr_lessone$scenario == "tabuqoliqoli_25sed_dvar" & finalcov_restr_lessone$graz == 3])

#11.23.2022: How many reefs have less than 10% coral cover under the baseline simulation under each grazing scenario?
finalcov_restr_lessten <- finalcov_restr
finalcov_restr_lessten <- finalcov_restr_lessten[finalcov_restr_lessten$coral_cover < 0.1,]
#base_dvar: low = 75, medium = 60, high = 56
length(finalcov_restr_lessten$coral_cover[finalcov_restr_lessten$scenario == "base_dvar" & finalcov_restr_lessten$graz == 3])

#6.8.2023: How many reefs have more than 30% coral cover under the baseline simulation under each grazing scenario?
finalcov_restr_morethirty <- finalcov_restr
finalcov_restr_morethirty <- finalcov_restr_morethirty[finalcov_restr_morethirty$coral_cover > 0.3,]
#base_dvar: low = 0, medium = 10, high = 12
length(finalcov_restr_morethirty$coral_cover[finalcov_restr_morethirty$scenario == "base_dvar" & finalcov_restr_morethirty$graz == 3])



###################
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

pc_val <- pm_val <- rep(NA,length(sitevector)) #immigration
pcout_val <- pmout_val <- rep(NA,length(sitevector)) #emigration
for(i in 1:75){
  pm_val[i] <- sum(jointsite_coral_weightedavgconnmat[i,]) #sum of inputs to i (coral), sum row i
  #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
  pc_val[i] <- sum(jointsite_malg_smallPLDconnmat[i,]) #sum of inputs to i (macroalgae)
  pcout_val[i] <- sum(jointsite_coral_weightedavgconnmat[,i])
  pmout_val[i] <- sum(jointsite_malg_smallPLDconnmat[,i])
}

