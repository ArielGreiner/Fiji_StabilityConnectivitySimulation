###load in libraries
library(scales)
library(deSolve)
library(geometry)
library(fields)
#library(optparse)
library(maps)
library(rgdal)
library(ggplot2)
library(viridis)
library(dplyr)
library(plyr)

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
#NEW 5.6.2023: divide the malg herb density by the malg herb max and the det herb density by the det herb max
benthicfish_masterdataset_detherbs$comb_grazinglevel <- (benthicfish_masterdataset_detherbs$biomass_kgha_trophic_group_avg_herbivore_detritivore/largest_detherbabundance) + (benthicfish_masterdataset_detherbs$malg_density/largest_malgherbabundance)
#OLD: benthicfish_masterdataset_detherbs$comb_grazinglevel <- (benthicfish_masterdataset_detherbs$biomass_kgha_trophic_group_avg_herbivore_detritivore + benthicfish_masterdataset_detherbs$malg_density)/(largest_detherbabundance+largest_malgherbabundance)

benthicfish_masterdataset <- benthicfish_masterdataset_detherbs 

#REMOVED THIS BIT
#scale it so the grazing rate either has a mean of 0.1, 0.3 or 0.5
#median(benthicfish_masterdataset$comb_grazinglevel) 
MD_scale <- 0.1/median(benthicfish_masterdataset$comb_grazinglevel) #if multiply by 2.338038 will get a median of 0.1
ASS_scale <- 0.3/median(benthicfish_masterdataset$comb_grazinglevel) #7.014113
CD_scale <- 0.5/median(benthicfish_masterdataset$comb_grazinglevel) #11.690188
#checking 
#median(benthicfish_masterdataset$grazinglevel*CD_scale) #0.1, 0.3, 0.5 confirmed
scalingfactors <- c(MD_scale, ASS_scale, CD_scale)

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

###probably just need this line, but loading in the stuff ^^ above just in case as well ##moved into for loop
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_5.10.2022.RData")
#update 7.1.2022 - qoli qoli
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_6.30.2022.RData")
#benthicfish_masterdataset <- benthicfish_masterdataset_sed

#need to scale the new_grazinglevel #5.31.2022: NO! scale using comb_grazinglevel instead
#scale it so the grazing rate either has a mean of 0.1, 0.3 or 0.5
#median(benthicfish_masterdataset$new_grazinglevel) 
#MD_scale <- 0.1/median(benthicfish_masterdataset$new_grazinglevel) #if multiply by 2.338038 will get a median of 0.1
#ASS_scale <- 0.3/median(benthicfish_masterdataset$new_grazinglevel) #7.014113
#CD_scale <- 0.5/median(benthicfish_masterdataset$new_grazinglevel) #11.690188
#checking 
#median(benthicfish_masterdataset$grazinglevel*CD_scale) #0.1, 0.3, 0.5 confirmed
#scalingfactors <- c(1,MD_scale, ASS_scale, CD_scale)
#####

#cycle through the three different scaling factors, tabu and sediment scenarios
#baseline: original x original (q=1,k=1)
#tabu1: qoliqoli x original (q=1,k=2)
#tabu2: 5km x original (q=1, k=3)
#sed10: original x 10% (q=2, k=1)
#sed25: original x 25% (q=3,k=1)
#lowmixed: qoliqoli x 10% (q=2, k=2)
#highmixed: qoliqoli x 25% (q=3, k=2)
#q=1,k=1 - yes; q=1,k=2 - yes; q=1,k=3 - yes; q=2,k=1 - yes; q=2,k=2 - yes; q=2,k=3 - NO; q=3,k=1 - yes; q=3,k=2 - yes; q=3,k=3 - NO

buffsize <- c("original", "qoliqoli", "5km") #buffsize <- c(0.02,"qoliqoli") 
altsed <- c("baseline", "10_alt", "25_alt")
#altsed <- "25_alt" #altsed <- c("normalalt", "10_alt")
for(q in 1:length(altsed)){ 
  for(k in 1:length(buffsize)){
    for(j in 1:length(scalingfactors)){
      
      if(q == 2 & k == 3){next}
      if(q == 3 & k == 3){next}
      
      if(k == 1){ #no buffer #OLD: 2km buffer
        load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sedimentlvls_5.6.2023.RData") #no buffer, benthicfish_masterdataset_sed
        benthicfish_masterdataset <- benthicfish_masterdataset_sed
        g_val <- benthicfish_masterdataset$comb_grazinglevel*scalingfactors[j] 
      }
      if(k == 2){ #qoli qoli det'd buffer
        load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_5.6.2023.RData")
        benthicfish_masterdataset <- benthicfish_masterdataset_sed
        g_val <- benthicfish_masterdataset$new_grazinglevel*scalingfactors[j] 
      }
      
      if(k == 3){ #5km buffer
        load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.05_5.6.2023.RData")
        benthicfish_masterdataset <- benthicfish_masterdataset_sed
        g_val <- benthicfish_masterdataset$new_grazinglevel*scalingfactors[j] 
      }
      
      if(altsed[q] == "baseline"){
        q_i <- 1
      }
      
      if(altsed[q] == "10_alt"){
        q_i <- 2
      }
      
      if(altsed[q] == "25_alt"){
        q_i <- 3.5
      }
      
      ###load in other parameters
      #Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
      #Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
      a <- 0.1 #rate macroalgae overgrows coral
      d_val <- benthicfish_masterdataset$sediment_altalt*(1-(0.1*(q_i-1))) #if q_i = 3.5 -> multiplying by 0.75
      r <- 6500 #rate coral recruits onto/overgrows turf algae
      gamma <- 1900 #rate macroalgae recruits onto/overgrows turf algae
      #g_val <- benthicfish_masterdataset$new_grazinglevel*scalingfactors[j] 
      
      
      pc_vec <- pm_vec <- list()
      for(i in 1:length(sitevector)){
        pc_vec[[i]] <- jointsite_coral_weightedavgconnmat[i,] #all inputs to i (coral)
        pm_vec[[i]] <- jointsite_malg_smallPLDconnmat[i,] #all inputs to i (macroalgae)
      }
      
      #referred to: https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r and https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r 
      #if want to code in Julia, can refer to: https://github.com/colebrookson/trait-based-rewiring and https://github.com/colebrookson/trait-based-rewiring/blob/main/src/reprexs/large-system.jl 
      MultipatchMumby_Original <- function(t,state,parameters){
        with(as.list(c(state,parameters)),{
          Ma <- state[1:l]
          Co <- state[(l+1):(2*l)]
          Tu <- state[((l*2)+1):(3*l)]
          dMa <- dCo <- dTu <- rep(NA,l)
          for(u in 1:l){
            dMa[u] <- a*Ma[u]*Co[u] - (g[u]*Ma[u])/(Ma[u]+Tu[u]) + gamma*sum(p_m[[u]]*Ma)*Tu[u]
            dCo[u] <- r*sum(p_c[[u]]*Co)*Tu[u] - d[u]*Co[u] - a*Ma[u]*Co[u] 
            dTu[u] <- (g[u]*Ma[u])/(Ma[u]+Tu[u]) + d[u]*Co[u] - (r*sum(p_c[[u]]*Co) + gamma*sum(p_m[[u]]*Ma))*Tu[u]
          }
          list(c(dMa,dCo,dTu)) 
        })
      }
      
      times <- seq(0,2000, by = 0.1)
      npoints <- length(times)
      
      mumbytrajectories <- data.frame(reefnum = rep(1:length(sitevector), each = npoints), sitename = rep(sitevector, each = npoints), reef_lat = rep(benthicfish_masterdataset$latitude, each = npoints), reef_long = rep(benthicfish_masterdataset$long.recenter, each = npoints), M = NA, C = NA, Tu = NA, TimeStep = rep(1:npoints, length(sitevector))) 
      
      #CalcTrajectories <- function(parameters,pc_val,pm_val,g_val,times,mumbytrajectories,benthicfish_masterdataset,MultipatchMumby_Original, sitevector){
      #Elmhirst parameter model 
      parameters <- list(a <- 0.1, d <- d_val, g <- g_val, r <- 6500, gamma <- 1900, p_c <- pc_vec, p_m <- pm_vec, l <- length(sitevector))
      #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
      state <- c(Ma = benthicfish_masterdataset$malg_cover, Co = benthicfish_masterdataset$coral_cover, Tu = benthicfish_masterdataset$turf_cover)
      print(paste("In trajectory calculation function, scaling factor",j))
      start_time <- Sys.time()
      out <- lsode(y = state, times = times, func = MultipatchMumby_Original, parms = parameters)	
      end_time <- Sys.time()
      end_time - start_time
      print("Done")
      for(i in 1:length(sitevector)){ 
        mumbytrajectories$M[mumbytrajectories$reefnum == i] <- out[,(i+1)]
        mumbytrajectories$C[mumbytrajectories$reefnum == i] <- out[,(length(sitevector)+i+1)]
        mumbytrajectories$Tu[mumbytrajectories$reefnum == i] <- out[,((length(sitevector)*2)+i+1)]
      }
      #return(mumbytrajectories)
      #}
      
      #need to make a dataset that just contains the first and last cover levels
      if(k==1){
        benthic_traj <- benthicfish_masterdataset %>%
          select(site, latitude, long.recenter, comb_grazinglevel) #warning: changed comb_grazinglevel to new_grazinglevel
      }
      if(k == 2){
        benthic_traj <- benthicfish_masterdataset %>%
          select(site, latitude, long.recenter, new_grazinglevel) #warning: changed comb_grazinglevel to new_grazinglevel
      }
      
      if(k == 3){
        benthic_traj <- benthicfish_masterdataset %>%
          select(site, latitude, long.recenter, new_grazinglevel) #warning: changed comb_grazinglevel to new_grazinglevel
      }
      
      for(i in 1:length(sitevector)){
        benthic_traj$init_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][1]
        benthic_traj$init_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][1]
        benthic_traj$init_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][1]
        benthic_traj$final_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][npoints]
        benthic_traj$final_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][npoints]
        benthic_traj$final_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][npoints]
      }
      datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
      saveRDS(datalist, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_5.6.2023.rds")) 
      #saveRDS(datalist, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AltSed_NewTabu_7.2022/MixedScenario_Tabu",buffsize[k],"_fullsimrun_allherbs_dval_",altsed[q],"_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_8.11.2022.rds")) 
      
      
      
      #Well let's plot this out 
      worldmap <- map_data ("world", wrap = c(0, 360))
      
      #initial coral cover
      BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10) 
      
      #final coral cover
      BaseRun_newrgamma_FinalCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Coral Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_coralcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #initial malg cover
      BaseRun_newrgamma_InitialMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #final malg cover
      BaseRun_newrgamma_FinalMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Malg Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_malgcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #removed grazing level plot
      
      #rounded versions
      #initial coral cover
      Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Coral Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_coralcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/Rounded_MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10)
      
      
      #final coral cover
      Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Coral Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_coralcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/Rounded_MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #initial malg cover
      Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Malg Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_malgcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/Rounded_MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #final malg cover
      Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Malg Cover, Scaling Factor ", j,"d = ", altsed[q], "buffer size = ", buffsize[k]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_malgcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/MainTextSims/Rounded_MainText_Tabu",buffsize[k],"_dval_",altsed[q],"_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_ScalingFactor",j,"_5.6.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #just in case
      parameters <- NULL
      state <- NULL
      out <- NULL   
      benthicfish_masterdataset <- benthicfish_masterdataset_sed <- NA
    }
  }
}