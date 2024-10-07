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

numreefs <- 75
r <- 6500 #rate coral recruits onto/overgrows turf algae
gamma <- 1900 #rate macroalgae recruits onto/overgrows turf algae
connectivs <- c(10,15,30,55,65) #don't need the original weighted one, already made those plots
buffsize <- c("original", "qoliqoli", "5km") #buffsize <- c(0.02,"qoliqoli") 
altsed <- c("baseline", "10_alt", "25_alt")
zerocutoff <- 0.01

scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")
finalcov_restr_full <- data.frame(connectiv = rep(connectivs, each = length(scens)*numreefs*3), graz = rep(rep(c(1,2,3),each=length(scens)*numreefs), length(connectivs)), scenario = rep(rep(scens, each = numreefs), length(connectivs)), reef = rep(seq(1,numreefs,1), length(connectivs)), coral_cover=NA, malg_cover = NA, ratio = NA, numzerocoral = NA)

#to make the violin plots need to re-structure the dataframes
for(l in 1:length(connectivs)){
for(j in 1:3){ 
  for(k in 1:length(scens)){
    
    if(l==1){
      #load in data
      #base_dvar
      if(k == 1){ 
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabuoriginal_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
      }
      
      #10altsed
      if(k == 2){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
      }
      
      #25altsed
      if(k == 3){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
      }
      
      #tabu_qoliqoli_dvar
      if(k == 4){
       data <- readRDS(file=paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
        
      }
      
      #tabu_0.05_dvar
      if(k == 5){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabu5km_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
      }
      
      #10% + qoli qoli
      if(k == 6){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
      }
      
      #25% + qoli qoli
      if(k == 7){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/smallconnectivity_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.2.2024.rds"))
      }
    }
    
    if(l > 1){
      #load in data
      #base_dvar
      if(k == 1){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[l],"PLD_Tabuoriginal_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
      }
      
      #10altsed
      if(k == 2){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[l],"PLD_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
      }
      
      #25altsed
      if(k == 3){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[l],"PLD_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
      }
      
      #tabu_qoliqoli_dvar
      if(k == 4){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[l],"PLD_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
      }
      
      #tabu_0.05_dvar
      if(k == 5){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[l],"PLD_Tabu5km_fullsimrun_allherbs_dval_baseline_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
      }
      
      #10% + qoli qoli
      if(k == 6){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[l],"PLD_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
      }
      
      #25% + qoli qoli
      if(k == 7){
        data <- readRDS(file = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/MoreConnectivityMatrices/",connectivs[l],"PLD_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.13.2024.rds"))
      }
    }
    
    benthic_traj <- data[[3]]
    finalcov_restr_full$coral_cover[finalcov_restr_full$graz == j & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$connectiv == connectivs[l]] <- benthic_traj$final_coralcover
    finalcov_restr_full$malg_cover[finalcov_restr_full$graz == j & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$connectiv == connectivs[l]] <- benthic_traj$final_malgcover
    finalcov_restr_full$ratio[finalcov_restr_full$graz == j & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$connectiv == connectivs[l]] <- (1+benthic_traj$final_coralcover)/(1+benthic_traj$final_malgcover)
    finalcov_restr_full$numzerocoral[finalcov_restr_full$graz == j & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$connectiv == connectivs[l]] <- numreefs - length(finalcov_restr_full$reef[finalcov_restr_full$graz == j & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$connectiv == connectivs[l] & finalcov_restr_full$coral_cover > zerocutoff])
    
    #just in case
    benthic_traj <- NULL
    data <- NULL
  }
}
}


#beeswarm plots

for(l in 1:length(connectivs)){
  
finalcov_restr <- finalcov_restr_full[finalcov_restr_full$connectiv == connectivs[l],]

# New facet label names
grazinglevels.labs <- c("Low", "Medium", "High")
names(grazinglevels.labs) <- c("1", "2","3")

coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=coral_cover*100,colour=scenario, alpha = 0.9)) + 
  geom_quasirandom()+
  #geom_violin(color = NA)+
  ggtitle(paste("Coral Cover, dvar only, ",connectivs[l],"PLD"))+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "2", "3", "4", "5", "6"), name = "Management Intervention")+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+ 
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+ 
  geom_hline(yintercept = 30, color = "red")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_7.3.2024.png"), bg = "transparent", height = 10, width = 10)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=malg_cover*100,colour=scenario, alpha = 0.9)) + 
  geom_quasirandom()+
  #geom_violin(color = NA)+
  ggtitle(paste("Malg Cover, dvar only,",connectivs[l],"PLD"))+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "2", "3", "4", "5", "6"), name = "Management Intervention")+
  scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+ 
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_colour_manual(values=c("black","#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("base_dvar", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "10altsed", "25altsed", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "5km tabu", "10altsed", "25altsed", "qoliqoli_10sed", "qoliqoli_25sed"))+ 
  geom_hline(yintercept = 30, color = "red")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
#facet_wrap(~graz)
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_7.3.2024.png"), bg = "transparent", height = 10, width = 10)



###looking at how each reef fares before/after management
#plot managed-base for each reef, ratio and coral cover, for every scenario
#make a column that lets me colour by newtabu, oldtabu, everything else? 

#load in the qoli qoli tabus
load("Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_5.6.2023.RData")
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
load("Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.05_5.6.2023.RData")
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
  ggtitle(paste("Change in Coral Cover, dvar only,",connectivs[l],"PLD"))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1", "2", "3", "4","5", "6"), name = "Management Intervention")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  ylab("Change in % Coral Cover")+
  geom_hline(yintercept = 0, color = "red")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/changeincoralcover_w25_abr_mgmtstressorimpactassessment_summary_7.3.2024.png"), bg = "transparent", height = 7, width = 5)

changeinmalgcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = malg_cover*100, alpha = 0.9)) +
  geom_point(aes(colour = scenario))+
  ggtitle(paste("Change in Malg Cover, dvar only,",connectivs[l],"PLD"))+
  scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1", "2", "3", "4","5", "6"), name = "Management Intervention")+
  scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  #scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
  ylab("Change in % Macroalgae Cover")+
  geom_hline(yintercept = 0, color = "red")+
  #facet_wrap(~graz)
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/changeinmalgcover_w25_abr_mgmtstressorimpactassessment_summary_7.3.2024.png"), bg = "transparent", height = 7, width = 5)


#looking at the tabu designations
finalcov_changes_qq <- finalcov_changes[finalcov_changes$scenario %in% c("qoliqoli_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"),]

changeincoralcover_w25_abr_qoliqolicoloured <- ggplot(finalcov_changes_qq, aes(x = scenario, y = coral_cover*100, color = identity_qoliqoli, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ggtitle(paste("Change in % Coral Cover, dvar only,",connectivs[l],"PLD"))+
  ylab("Change in % Coral Cover")+
  scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1","5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu","qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr_qoliqolicoloured, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/changeincoralcover_w25_abr_qoliqolicoloured_7.3.2024.png"), bg = "transparent", height = 7, width = 5)

changeinmalgcover_w25_abr_qoliqolicoloured <- ggplot(finalcov_changes_qq, aes(x = scenario, y = malg_cover*100, color = identity_qoliqoli, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ggtitle(paste("Change in Malg Cover, dvar only,",connectivs[l],"PLD"))+
  ylab("Change in % Macroalgae Cover")+
  scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1","5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu","qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr_qoliqolicoloured, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/changeinmalgcover_w25_abr_qoliqolicoloured_7.3.2024.png"), bg = "transparent", height = 7, width = 5)

finalcov_changes_0.05 <- finalcov_changes[finalcov_changes$scenario %in% c("tabu0.05_basedvar"),]

changeincoralcover_w25_abr_tabu5coloured <- ggplot(finalcov_changes_0.05, aes(x = scenario, y = coral_cover*100, color = identity_0.05, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ggtitle(paste("Change in % Coral Cover, dvar only,",connectivs[l],"PLD"))+
  ylab("Change in % Coral Cover")+
  scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeincoralcover_w25_abr_tabu5coloured, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/changeincoralcover_w25_abr_tabu5coloured_7.3.2024.png"), bg = "transparent", height = 7, width = 5)

changeinmalgcover_w25_abr_tabu5coloured <- ggplot(finalcov_changes_0.05, aes(x = scenario, y = malg_cover*100, color = identity_0.05, alpha = 0.9)) +
  geom_quasirandom(dodge.width=0.5)+
  ggtitle(paste("Change in Malg Cover, dvar only,",connectivs[l],"PLD"))+
  ylab("Change in % Macroalgae Cover")+
  scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 0, color = "red")+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(changeinmalgcover_w25_abr_tabu5coloured, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/changeinmalgcover_w25_abr_tabu5coloured_7.3.2024.png"), bg = "transparent", height = 7, width = 5)

#the ones that tip backwards, how many of them are NOT the reefs that had a lower grazing rate post tabu enlargement? 
#View(finalcov_changes_qq[!(finalcov_changes_qq$reef %in% qqlowerreefs),])
#View(finalcov_changes_qq[!(finalcov_changes_0.05$reef %in% fivelowerreefs),])

#back to the coral cover plot, curious what that looks like coloured by tabu designation
finalcov_restr$identity_qoliqoli <- rep(tabus_qoliqoli,3)
finalcov_restr$identity_0.05 <- rep(tabus_0.05,3)

finalcov_restr_qqtabu <- finalcov_restr[finalcov_restr$scenario %in% c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"),]
coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus <- ggplot(finalcov_restr_qqtabu, aes(x=scenario, y=coral_cover*100,colour=identity_qoliqoli, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle(paste("Coral Cover, dvar only, qoli qoli tabus,",connectivs[l],"PLD"))+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "red")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus_7.3.2024.png"), bg = "transparent", height = 10, width = 7)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus <- ggplot(finalcov_restr_qqtabu, aes(x=scenario, y=malg_cover*100,colour=identity_qoliqoli, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle(paste("Malg Cover, dvar only, qoli qoli tabus,",connectivs[l],"PLD"))+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "5", "6"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "red")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus_7.3.2024.png"), bg = "transparent", height = 10, width = 7)

finalcov_restr_5tabu <- finalcov_restr[finalcov_restr$scenario %in% c("base_dvar", "tabu_0.05_dvar"),]
coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus <- ggplot(finalcov_restr_5tabu, aes(x=scenario, y=coral_cover*100,colour=identity_0.05, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle(paste("Coral Cover, dvar only, 5km tabus,",connectivs[l],"PLD"))+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_0.05_dvar"), labels=c("base", "5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "red")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus_7.3.2024.png"), bg = "transparent", height = 10, width = 5)

malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus <- ggplot(finalcov_restr_5tabu, aes(x=scenario, y=malg_cover*100,colour=identity_0.05, alpha = 0.9)) + 
  geom_quasirandom(dodge.width=0.5)+
  #geom_violin(color = NA)+
  ggtitle(paste("Malg Cover, dvar only, 5km tabus,",connectivs[l],"PLD"))+
  scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
  scale_x_discrete(limits=c("base_dvar", "tabu_0.05_dvar"), labels=c("base", "5km tabu increase"), name = "Management Intervention")+
  #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
  scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
  geom_hline(yintercept = 30, color = "red")+
  #stat_summary(fun.data=data_summary, color = "black")+
  #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
  facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus, filename = paste0("Final_Simulations/AddingConnectivitySensitivityAnalyses/BeeswarmPlots/",connectivs[l],"PLD/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus_7.3.2024.png"), bg = "transparent", height = 10, width = 5)

}

