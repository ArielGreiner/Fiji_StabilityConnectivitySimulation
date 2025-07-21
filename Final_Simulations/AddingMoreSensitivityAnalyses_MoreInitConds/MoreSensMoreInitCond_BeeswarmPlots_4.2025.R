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
library(cowplot) #library(ggpubr)
#library(Hmisc) #won't work
library(ggbeeswarm)
#devtools::install_github("eclarke/ggbeeswarm")

###What effect did the management scenarios have on the system?
#only looking at the dvar scenarios
#tabu qoli qoli, tabu 5km, 10% alt sed, 25% alt sed, qoli qoli + 10%, qoli qoli + 25%

setwd("~/Dropbox/Github/PhDProjects/Fiji_StabilityConnectivitySimulation/")

a_vals <- c(0.05,0.07,0.15,0.2) #only need the new values
d_meds <- c(0.12,0.16,0.36,0.48) #only need the new values
initconds <- c(0.1,0.8,0.1,0.8)
initconds_more <- c(0.01,0.98)
varying_a <- data.frame(a = rep(a_vals, each = 3), d = 0.24, g = rep(c(1,2,3),length(a_vals)), C_init = NA, M_init = NA, vary = NA)
varying_d <- data.frame(a = 0.1, d = rep(d_meds, each = 3), g = rep(c(1,2,3),length(d_meds)), C_init = NA, M_init = NA, vary = NA)
varying_initcond <- data.frame(a = 0.1, d = 0.24, g = rep(c(1,2,3),length(initconds)), C_init = rep(initconds, each = 3), M_init = rep(c(0.8,0.1,0.8,0.1), each = 3), vary = rep(c("n","y"),each = 3*length(initconds)))
varying_initcond_more <- data.frame(a = 0.1, d = 0.24, g = rep(c(1,2,3),length(initconds_more)), C_init = rep(initconds_more, each = 3), M_init = rep(c(0.98,0.01), each = 3), vary = "n")
paramsies <- rbind(varying_a, varying_d, varying_initcond,varying_initcond_more)
#3 instead of 'length(scalingfactors)


numreefs <- 75
r <- 6500 #rate coral recruits onto/overgrows turf algae
gamma <- 1900 #rate macroalgae recruits onto/overgrows turf algae
buffsize <- c("original", "qoliqoli", "5km") 
altsed <- c("baseline", "10_alt", "25_alt")
zerocutoff <- 0.01

scens <- c("base_dvar", "10altsed", "25altsed", "tabu_qoliqoli_dvar", "tabu_0.05_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar")
finalcov_restr_full <- data.frame(sensitivs = rep(seq(1,((dim(paramsies)[1])/3),1), each = length(scens)*numreefs*3), graz = rep(rep(c(1,2,3),each=length(scens)*numreefs), ((dim(paramsies)[1])/3)), scenario = rep(rep(scens, each = numreefs), ((dim(paramsies)[1])/3)), reef = rep(seq(1,numreefs,1), ((dim(paramsies)[1])/3)), coral_cover=NA, malg_cover = NA, ratio = NA, numzerocoral = NA)
#^ /3 because don't need every grazing scenario
sensitv <- c(rep(seq(1,18,1), each = 3))

#saveRDS(datalist, file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_4.9.2025.rds")) 

#to make the violin plots need to re-structure the dataframes
for(j in 1:dim(paramsies)[1]){
    for(k in 1:length(scens)){
      
    if(j < 25){
      inity <- "default"
      date <- "2.20.2025"}
    if(j > 24 & j < 37){
      inity <- paramsies$C_init[j]
      date <- "2.20.2025"
      }
    if(j > 36 & j < 49){
      inity <- paste0("vary",paramsies$C_init[j])
      date <- "4.9.2025"
    }
    if(j > 36 & j < 49){
      inity <- paste0("vary",paramsies$C_init[j])
      date <- "4.9.2025"
    }
    if(j > 48){
      inity <- paramsies$C_init[j]
      date <- "5.15.2025"
    }
    if(j < 49){
        #load in data
        #base_dvar
        if(k == 1){ 
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds"))
        }
      
        #10altsed
        if(k == 2){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds"))
        }
        
        #25altsed
        if(k == 3){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #tabu_qoliqoli_dvar
        if(k == 4){
          data <- readRDS(file=paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #tabu_0.05_dvar
        if(k == 5){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabu5km_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #10% + qoli qoli
        if(k == 6){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #25% + qoli qoli
        if(k == 7){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",j,"_",inity,"Cinit_",date,".rds")) 
      }
    }
    if(j > 48){
      sf <- j-48
        #load in data
        #base_dvar
        if(k == 1){ 
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds"))
        }
        
        #10altsed
        if(k == 2){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds"))
        }
        
        #25altsed
        if(k == 3){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuoriginal_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #tabu_qoliqoli_dvar
        if(k == 4){
          data <- readRDS(file=paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #tabu_0.05_dvar
        if(k == 5){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabu5km_fullsimrun_allherbs_dval_baseline_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #10% + qoli qoli
        if(k == 6){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_10_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
        }
        
        #25% + qoli qoli
        if(k == 7){
          data <- readRDS(file = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/SI_Tabuqoliqoli_fullsimrun_allherbs_dval_25_alt_a",paramsies$a[j],"d",paramsies$d[j],"_scalingfactor",sf,"_",inity,"Cinit_",date,".rds")) 
        }
      }
      benthic_traj <- data[[3]]
      finalcov_restr_full$coral_cover[finalcov_restr_full$graz == paramsies$g[j] & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$sensitivs == sensitv[j]] <- benthic_traj$final_coralcover
      finalcov_restr_full$malg_cover[finalcov_restr_full$graz == paramsies$g[j] & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$sensitivs == sensitv[j]] <- benthic_traj$final_malgcover
      finalcov_restr_full$ratio[finalcov_restr_full$graz == paramsies$g[j] & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$sensitivs == sensitv[j]] <- (1+benthic_traj$final_coralcover)/(1+benthic_traj$final_malgcover)
      finalcov_restr_full$numzerocoral[finalcov_restr_full$graz == paramsies$g[j] & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$sensitivs == sensitv[j]] <- numreefs - length(finalcov_restr_full$reef[finalcov_restr_full$graz == paramsies$g[j] & finalcov_restr_full$scenario == scens[k] & finalcov_restr_full$sensitivs == sensitv[j] & finalcov_restr_full$coral_cover > zerocutoff])
      
      #just in case
      benthic_traj <- NULL
      data <- NULL
      date <- NULL
      inity <- NULL 
    }
  }

#will need to change this if change the sensitivity analysis at all
sensitvs <- data.frame(sens = seq(1,max(sensitv),1) ,a = c(0.05,0.07,0.15,0.2,rep(0.1,14)), d = c(rep(0.24,4),0.12,0.16,0.36,0.48, rep(0.24,10)), C_init = c(rep(NA,8),rep(c(0.1,0.8),4),0.01,0.98), M_init = c(rep(NA,8),rep(c(0.8,0.1),4),0.98,0.01), vary = c(rep(NA,8), rep("n",4), rep("y",4),rep("n",2)))

#beeswarm plots

for(l in 1:dim(sensitvs)[1]){
  if(l < 5){
    scenie <- "varying_a"
    inity <- "default"
  }
  if(l > 4 & l < 9){
    scenie <- "varying_d"
    inity <- "default"
  }
  if(l > 8 & l < 13){
    scenie <- "new_initcond"
    inity <- sensitvs$C_init[l]
  }
  if(l > 12 & l < 17){
    scenie <- "newvar_initcond"
    inity <- paste0("vary",sensitvs$C_init[l])
  }
  if(l > 16){ #note: left it as 4.15.2025 because didn't see the point in changing the date just to reflect the addition of more initial condition options
    scenie <- "new_initcond"
    inity <- sensitvs$C_init[l]
  }
  
  
  finalcov_restr <- finalcov_restr_full[finalcov_restr_full$sensitivs == sensitvs$sens[l],]
  
  # New facet label names
  grazinglevels.labs <- c("Low", "Medium", "High")
  names(grazinglevels.labs) <- c("1", "2","3")
  
  coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=coral_cover*100,colour=scenario, alpha = 0.9)) + 
    geom_quasirandom()+
    #geom_violin(color = NA)+
    ggtitle(paste("Coral Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
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
  ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 10, width = 10)
  
  malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm <- ggplot(finalcov_restr, aes(x=scenario, y=malg_cover*100,colour=scenario, alpha = 0.9)) + 
    geom_quasirandom()+
    #geom_violin(color = NA)+
    ggtitle(paste("Malg Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
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
  ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 10, width = 10)
  
  
  
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
    ggtitle(paste("Change in Coral Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1", "2", "3", "4","5", "6"), name = "Management Intervention")+
    scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
    #scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
    ylab("Change in % Coral Cover")+
    geom_hline(yintercept = 0, color = "black")+
    #facet_wrap(~graz)
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(changeincoralcover_w25_abr, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/changeincoralcover_w25_abr_mgmtstressorimpactassessment_summary_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 7, width = 5)
  
  changeinmalgcover_w25_abr <- ggplot(finalcov_changes, aes(x = scenario, y = malg_cover*100, alpha = 0.9)) +
    geom_point(aes(colour = scenario))+
    ggtitle(paste("Change in Malg Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1", "2", "3", "4","5", "6"), name = "Management Intervention")+
    scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1 = 2km tabu increase within qoliqoli", "2 = 5km tabu increase", "3 = 10% Water Quality Improvement", "4 = 25% Water Quality Improvement", "5 = Low Mixed", "6 = High Mixed"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
    #scale_color_manual(values=c("#FFC000", "#E69F00","#94cceb", "#56B4E9","#c790fc", "#AB56FC"),breaks=c("qoliqoli_basedvar", "tabu0.05_basedvar", "altsed10_basedvar", "altsed25_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu", "5km tabu", "10altsed", "25altsed","qoliqoli_10sed", "qoliqoli_25sed"))+
    ylab("Change in % Macroalgae Cover")+
    geom_hline(yintercept = 0, color = "black")+
    #facet_wrap(~graz)
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(changeinmalgcover_w25_abr, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/changeinmalgcover_w25_abr_mgmtstressorimpactassessment_summary_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 7, width = 5)
  
  
  #looking at the tabu designations
  finalcov_changes_qq <- finalcov_changes[finalcov_changes$scenario %in% c("qoliqoli_basedvar", "altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"),]
  
  changeincoralcover_w25_abr_qoliqolicoloured <- ggplot(finalcov_changes_qq, aes(x = scenario, y = coral_cover*100, color = identity_qoliqoli, alpha = 0.9)) +
    geom_quasirandom(dodge.width=0.5)+
    ggtitle(paste("Change in % Coral Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    ylab("Change in % Coral Cover")+
    scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1","5", "6"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu","qoliqoli_10sed", "qoliqoli_25sed"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 0, color = "black")+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(changeincoralcover_w25_abr_qoliqolicoloured, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/changeincoralcover_w25_abr_qoliqolicoloured_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 7, width = 5)
  
  changeinmalgcover_w25_abr_qoliqolicoloured <- ggplot(finalcov_changes_qq, aes(x = scenario, y = malg_cover*100, color = identity_qoliqoli, alpha = 0.9)) +
    geom_quasirandom(dodge.width=0.5)+
    ggtitle(paste("Change in Malg Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    ylab("Change in % Macroalgae Cover")+
    scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("1","5", "6"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("qoliqoli_basedvar","altsed10_qoliqoli_basedvar", "altsed25_qoliqoli_basedvar"), labels=c("qoliqoli tabu","qoliqoli_10sed", "qoliqoli_25sed"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 0, color = "black")+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(changeinmalgcover_w25_abr_qoliqolicoloured, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/changeinmalgcover_w25_abr_qoliqolicoloured_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 7, width = 5)
  
  finalcov_changes_0.05 <- finalcov_changes[finalcov_changes$scenario %in% c("tabu0.05_basedvar"),]
  
  changeincoralcover_w25_abr_tabu5coloured <- ggplot(finalcov_changes_0.05, aes(x = scenario, y = coral_cover*100, color = identity_0.05, alpha = 0.9)) +
    geom_quasirandom(dodge.width=0.5)+
    ggtitle(paste("Change in % Coral Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    ylab("Change in % Coral Cover")+
    scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu increase"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 0, color = "black")+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(changeincoralcover_w25_abr_tabu5coloured, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/changeincoralcover_w25_abr_tabu5coloured_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 7, width = 5)
  
  changeinmalgcover_w25_abr_tabu5coloured <- ggplot(finalcov_changes_0.05, aes(x = scenario, y = malg_cover*100, color = identity_0.05, alpha = 0.9)) +
    geom_quasirandom(dodge.width=0.5)+
    ggtitle(paste("Change in Malg Cover, dvar only, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    ylab("Change in % Macroalgae Cover")+
    scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu increase"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("5km tabu"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 0, color = "black")+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(changeinmalgcover_w25_abr_tabu5coloured, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/changeinmalgcover_w25_abr_tabu5coloured_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 7, width = 5)
  
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
    ggtitle(paste("Coral Cover, dvar only, qoli qoli tabus, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
    scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "5", "6"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 30, color = "green4")+
    #stat_summary(fun.data=data_summary, color = "black")+
    #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 10, width = 7)
  
  malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus <- ggplot(finalcov_restr_qqtabu, aes(x=scenario, y=malg_cover*100,colour=identity_qoliqoli, alpha = 0.9)) + 
    geom_quasirandom(dodge.width=0.5)+
    #geom_violin(color = NA)+
    ggtitle(paste("Malg Cover, dvar only, qoli qoli tabus, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
    scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "1", "5", "6"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 30, color = "green4")+
    #stat_summary(fun.data=data_summary, color = "black")+
    #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 10, width = 7)
  
  finalcov_restr_5tabu <- finalcov_restr[finalcov_restr$scenario %in% c("base_dvar", "tabu_0.05_dvar"),]
  coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus <- ggplot(finalcov_restr_5tabu, aes(x=scenario, y=coral_cover*100,colour=identity_0.05, alpha = 0.9)) + 
    geom_quasirandom(dodge.width=0.5)+
    #geom_violin(color = NA)+
    ggtitle(paste("Coral Cover, dvar only, 5km tabus, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Coral Cover")+
    scale_x_discrete(limits=c("base_dvar", "tabu_0.05_dvar"), labels=c("base", "5km tabu increase"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 30, color = "green4")+
    #stat_summary(fun.data=data_summary, color = "black")+
    #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 10, width = 5)
  
  malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus <- ggplot(finalcov_restr_5tabu, aes(x=scenario, y=malg_cover*100,colour=identity_0.05, alpha = 0.9)) + 
    geom_quasirandom(dodge.width=0.5)+
    #geom_violin(color = NA)+
    ggtitle(paste("Malg Cover, dvar only, 5km tabus, a=",sensitvs$a[l],"d=",sensitvs$d[l],"initconds",inity))+
    scale_y_discrete(limits=c(0,10,30,50,75,100), name = "% Macroalgae Cover")+
    scale_x_discrete(limits=c("base_dvar", "tabu_0.05_dvar"), labels=c("base", "5km tabu increase"), name = "Management Intervention")+
    #scale_x_discrete(limits=c("base_dvar", "tabu_qoliqoli_dvar", "tabuqoliqoli_10sed_dvar", "tabuqoliqoli_25sed_dvar"), labels=c("base", "qoliqoli tabu", "qoliqoli_10sed", "qoliqoli_25sed"))+
    scale_color_manual(values=c("#a6611a","#018571","#80cdc1"),breaks=c("n","newtabu","oldtabu"),labels=c("Not in a Tabu", "Newly in a Tabu", "Originally in a Tabu"), name = "")+
    geom_hline(yintercept = 30, color = "green4")+
    #stat_summary(fun.data=data_summary, color = "black")+
    #geom_jitter(shape=16, size = 0.2, position=position_jitter(0.2))+
    facet_wrap(~ graz, labeller = labeller(graz = grazinglevels.labs))
  ggsave(malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/malgcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_4.15.2025.png"), bg = "transparent", height = 10, width = 5)
  
  #making combo plots for Appendix S4, Appendix S8
  aplot <- coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm + theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  bplot <- changeincoralcover_w25_abr + theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  cplot <- changeinmalgcover_w25_abr + theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  
  dplot <- coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_qqtabus + theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  eplot <- changeincoralcover_w25_abr_qoliqolicoloured + theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  fplot <- changeinmalgcover_w25_abr_qoliqolicoloured + theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  
  gplot <- coralcover_mgmtstressorimpactassessmentabrw25_summary_beeswarm_5tabus + scale_x_discrete(limits=c("base_dvar", "tabu_0.05_dvar"), labels=c("base", "2"), name = "Management Intervention") + theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  hplot <- changeincoralcover_w25_abr_tabu5coloured + scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("2"), name = "Management Intervention")+ theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  iplot <- changeinmalgcover_w25_abr_tabu5coloured + scale_x_discrete(limits=c("tabu0.05_basedvar"), labels=c("2"), name = "Management Intervention")+ theme(legend.position="none", axis.title.x=element_blank()) + labs(title = NULL)
  
  fullplot <- plot_grid(aplot, bplot, cplot, dplot, eplot, fplot, gplot, hplot, iplot,
            labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)"),
            ncol = 3, nrow = 3)
  ggsave(fullplot, filename = paste0("Final_Simulations/AddingMoreSensitivityAnalyses_MoreInitConds/BeeswarmPlots/",scenie,"/fullplot_beeswarm_a",sensitvs$a[l],"d",sensitvs$d[l],"initconds",inity,"_6.27.2025.png"), bg = "transparent", height = 13, width = 12)
}

