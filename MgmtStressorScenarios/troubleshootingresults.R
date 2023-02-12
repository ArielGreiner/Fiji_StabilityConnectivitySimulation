##why am i seeing those lines in scenario 4?

##try 3: what if multiply all of the coral connectivity values by 1.01 or something, just to see if the result is dependent on the connectivity values themselves?

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
library(plyr) #maybe needed for round_any?

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
scalingfactors_alt <- c(CD_scale, (CD_scale - 1), (CD_scale + 1)) #-1 -> 0.4572291 median, +1 -> 0.5427709 median

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

#cycle through the four different scaling factors

for(j in 1:length(scalingfactors_alt)){
  ###load in other parameters
  #Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
  #Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
  a <- 0.1 #rate macroalgae overgrows coral
  d <- 0.24 #rate coral dies
  r <- 125 #NEW VALUE #rate coral recruits onto/overgrows turf algae
  gamma <- 900 #NEW VALUE #rate macroalgae recruits onto/overgrows turf algae
  g_val <- benthicfish_masterdataset$comb_grazinglevel*scalingfactors_alt[j] 
  
  
  pc_val <- pm_val <- rep(NA,length(sitevector))
  for(i in 1:length(sitevector)){
    pm_val[i] <- sum(jointsite_coral_weightedavgconnmat[i,]) #sum of inputs to i (coral), sum row i
    #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
    pc_val[i] <- sum(jointsite_malg_smallPLDconnmat[i,]) #sum of inputs to i (macroalgae)
  }
  #pc_val <- pc_val*1.01
  
  #referred to: https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r and https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r 
  #if want to code in Julia, can refer to: https://github.com/colebrookson/trait-based-rewiring and https://github.com/colebrookson/trait-based-rewiring/blob/main/src/reprexs/large-system.jl 
  MultipatchMumby_Original <- function(t,state,parameters){
    with(as.list(c(state,parameters)),{
      Ma <- state[1:l]
      Co <- state[(l+1):(2*l)]
      Tu <- state[((l*2)+1):(3*l)]
      dMa <- dCo <- dTu <- rep(NA,l)
      for(k in 1:l){
        dMa[k] <- a*Ma[k]*Co[k] - (g[k]*Ma[k])/(Ma[k]+Tu[k]) + gamma*p_m[k]*sum(Ma)*Tu[k]
        dCo[k] <- p_c[k]*r*sum(Co)*Tu[k] - d*Co[k] - a*Ma[k]*Co[k] 
        dTu[k] <- (g[k]*Ma[k])/(Ma[k]+Tu[k]) + d*Co[k] - (p_c[k]*r*sum(Co) + gamma*p_m[k]*sum(Ma))*Tu[k]
      }
      list(c(dMa,dCo,dTu)) 
    })
  }
  
  times <- seq(0,2000, by = 0.1)
  npoints <- length(times)
  
  mumbytrajectories <- data.frame(reefnum = rep(1:length(sitevector), each = npoints), sitename = rep(sitevector, each = npoints), reef_lat = rep(benthicfish_masterdataset$latitude, each = npoints), reef_long = rep(benthicfish_masterdataset$long.recenter, each = npoints), M = NA, C = NA, Tu = NA, TimeStep = rep(1:npoints, length(sitevector))) 
  
  #CalcTrajectories <- function(parameters,pc_val,pm_val,g_val,times,mumbytrajectories,benthicfish_masterdataset,MultipatchMumby_Original, sitevector){
  #Elmhirst parameter model 
  parameters <- list(a <- 0.1, d <- 0.24, g <- g_val, r <- 125, gamma <- 900, p_c <- pc_val, p_m <- pm_val, l <- length(sitevector))
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
  benthic_traj <- benthicfish_masterdataset %>%
    select(site, latitude, long.recenter, comb_grazinglevel)
  
  for(i in 1:length(sitevector)){
    benthic_traj$init_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][1]
    benthic_traj$init_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][1]
    benthic_traj$init_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][1]
    benthic_traj$final_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][npoints]
    benthic_traj$final_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][npoints]
    benthic_traj$final_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][npoints]
  }
  datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
  saveRDS(datalist, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactoralt",j,"_6.21.2022.rds"))
  
  
  
  #Well let's plot this out 
  worldmap <- map_data ("world", wrap = c(0, 360))
  
  #initial coral cover
  BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Scaling Factor alt", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final coral cover
  BaseRun_newrgamma_FinalCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Coral Cover, Scaling Factor alt", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_coralcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #initial malg cover
  BaseRun_newrgamma_InitialMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Scaling Factor alt", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final malg cover
  BaseRun_newrgamma_FinalMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Malg Cover, Scaling Factor alt ", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_malgcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  
  #grazing level
  BaseRun_newrgamma_allherbivores_GrazingLevelsUsed <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("GrazingLevelsUsed, Scaling Factor alt ", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (comb_grazinglevel*scalingfactors[j])))+
    scale_color_viridis(discrete = FALSE, name = "Grazing Rate")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_allherbivores_GrazingLevelsUsed, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_GrazingLevel_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #rounded versions
  #initial coral cover
  Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Coral Cover, Gamma ", gamma,"r",r,"Scaling Factor alt",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_coralcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final coral cover
  Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Coral Cover, Gamma ", gamma,"r",r,"Scaling Factor alt",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_coralcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #initial malg cover
  Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Malg Cover, Gamma ", gamma,"r",r,"Scaling Factor alt",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_malgcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final malg cover
  Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Malg Cover, Gamma ", gamma,"r",r,"Scaling Factor",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_malgcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/altconnmat_altscalingfactors_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_ScalingFactoralt",j,"_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #just in case
  parameters <- NULL
  state <- NULL
  out <- NULL   
}

#so, how does it look
plot(seq(1,75,1), benthic_traj[,8])
plot(pc_val, benthic_traj[,8])

########

##try 2: what if all high grazing? like 0.6

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
library(plyr) #maybe needed for round_any?

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

  ###load in other parameters
  #Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
  #Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
  a <- 0.1 #rate macroalgae overgrows coral
  d <- 0.24 #rate coral dies
  r <- 125 #NEW VALUE #rate coral recruits onto/overgrows turf algae
  gamma <- 900 #NEW VALUE #rate macroalgae recruits onto/overgrows turf algae
  g_val <- rep(0.6,length(sitevector))
  
  
  pc_val <- pm_val <- rep(NA,length(sitevector))
  for(i in 1:length(sitevector)){
    pm_val[i] <- sum(jointsite_coral_weightedavgconnmat[i,]) #sum of inputs to i (coral), sum row i
    #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
    pc_val[i] <- sum(jointsite_malg_smallPLDconnmat[i,]) #sum of inputs to i (macroalgae)
  }
  
  
  #referred to: https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r and https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r 
  #if want to code in Julia, can refer to: https://github.com/colebrookson/trait-based-rewiring and https://github.com/colebrookson/trait-based-rewiring/blob/main/src/reprexs/large-system.jl 
  MultipatchMumby_Original <- function(t,state,parameters){
    with(as.list(c(state,parameters)),{
      Ma <- state[1:l]
      Co <- state[(l+1):(2*l)]
      Tu <- state[((l*2)+1):(3*l)]
      dMa <- dCo <- dTu <- rep(NA,l)
      for(k in 1:l){
        dMa[k] <- a*Ma[k]*Co[k] - (g[k]*Ma[k])/(Ma[k]+Tu[k]) + gamma*p_m[k]*sum(Ma)*Tu[k]
        dCo[k] <- p_c[k]*r*sum(Co)*Tu[k] - d*Co[k] - a*Ma[k]*Co[k] 
        dTu[k] <- (g[k]*Ma[k])/(Ma[k]+Tu[k]) + d*Co[k] - (p_c[k]*r*sum(Co) + gamma*p_m[k]*sum(Ma))*Tu[k]
      }
      list(c(dMa,dCo,dTu)) 
    })
  }
  
  times <- seq(0,2000, by = 0.1)
  npoints <- length(times)
  
  mumbytrajectories <- data.frame(reefnum = rep(1:length(sitevector), each = npoints), sitename = rep(sitevector, each = npoints), reef_lat = rep(benthicfish_masterdataset$latitude, each = npoints), reef_long = rep(benthicfish_masterdataset$long.recenter, each = npoints), M = NA, C = NA, Tu = NA, TimeStep = rep(1:npoints, length(sitevector))) 
  
  #CalcTrajectories <- function(parameters,pc_val,pm_val,g_val,times,mumbytrajectories,benthicfish_masterdataset,MultipatchMumby_Original, sitevector){
  #Elmhirst parameter model 
  parameters <- list(a <- 0.1, d <- 0.24, g <- g_val, r <- 125, gamma <- 900, p_c <- pc_val, p_m <- pm_val, l <- length(sitevector))
  #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state <- c(Ma = benthicfish_masterdataset$malg_cover, Co = benthicfish_masterdataset$coral_cover, Tu = benthicfish_masterdataset$turf_cover)
  print(paste("In trajectory calculation function, Grazing Level 0.6"))
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
  benthic_traj <- benthicfish_masterdataset %>%
    select(site, latitude, long.recenter, comb_grazinglevel)
  
  for(i in 1:length(sitevector)){
    benthic_traj$init_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][1]
    benthic_traj$init_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][1]
    benthic_traj$init_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][1]
    benthic_traj$final_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][npoints]
    benthic_traj$final_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][npoints]
    benthic_traj$final_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][npoints]
  }
  datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
  saveRDS(datalist, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_g0.6_6.21.2022.rds"))
  
  
  
  #Well let's plot this out 
  worldmap <- map_data ("world", wrap = c(0, 360))
  
  #initial coral cover
  BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Grazing Level 0.6, Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover__g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final coral cover
  BaseRun_newrgamma_FinalCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Coral Cover, Grazing Level 0.6, Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_coralcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #initial malg cover
  BaseRun_newrgamma_InitialMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Grazing Level 0.6, Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final malg cover
  BaseRun_newrgamma_FinalMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Malg Cover, Grazing Level 0.6, Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_malgcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  
  #grazing level
  BaseRun_newrgamma_allherbivores_GrazingLevelsUsed <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("GrazingLevelsUsed, Grazing Level 0.6, Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (comb_grazinglevel*scalingfactors[j])))+
    scale_color_viridis(discrete = FALSE, name = "Grazing Rate")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_allherbivores_GrazingLevelsUsed, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_GrazingLevel_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #rounded versions
  #initial coral cover
  Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Coral Cover, Gamma ", gamma,"r",r,"Grazing Level 0.6"))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_coralcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final coral cover
  Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Coral Cover, Gamma ", gamma,"r",r,"Grazing Level 0.6"))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_coralcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #initial malg cover
  Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Malg Cover, Gamma ", gamma,"r",r,"Grazing Level 0.6"))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_malgcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final malg cover
  Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Malg Cover, Gamma ", gamma,"r",r,"Grazing Level 0.6"))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_malgcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/allhighgrazing_6.21.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_g0.6_6.21.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #just in case
  parameters <- NULL
  state <- NULL
  out <- NULL  
  
#lines clearer than before
plot(seq(1,75,1), benthic_traj[,8])
plot(pc_val, benthic_traj[,8])

###########

##try 1: what if randomize the coral/malg connectivity inputs, do the reefs with the same input still behave the same?

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
library(plyr) #maybe needed for round_any?

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

#cycle through the four different scaling factors

for(j in 1:length(scalingfactors)){
  ###load in other parameters
  #Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
  #Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
  a <- 0.1 #rate macroalgae overgrows coral
  d <- 0.24 #rate coral dies
  r <- 125 #NEW VALUE #rate coral recruits onto/overgrows turf algae
  gamma <- 900 #NEW VALUE #rate macroalgae recruits onto/overgrows turf algae
  g_val <- benthicfish_masterdataset$comb_grazinglevel*scalingfactors[j] 
  
  
  pc_val <- pm_val <- rep(NA,length(sitevector))
  for(i in 1:length(sitevector)){
    pm_val[i] <- sum(jointsite_coral_weightedavgconnmat[i,]) #sum of inputs to i (coral), sum row i
    #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
    pc_val[i] <- sum(jointsite_malg_smallPLDconnmat[i,]) #sum of inputs to i (macroalgae)
  }
  
#for troubleshooting:
set.seed(2)
dummy <- seq(1,75,1)
dummy <- sample(dummy) #so i can randomize the order of the values in pm_val and pc_val but keep the order of the two of them the same
pm_val_old <- pm_val
pc_val_old <- pc_val
pm_val <- pm_val[dummy]
pc_val <- pc_val[dummy]
  
  #referred to: https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r and https://stackoverflow.com/questions/60719860/solving-a-system-of-multiple-odes-in-r 
  #if want to code in Julia, can refer to: https://github.com/colebrookson/trait-based-rewiring and https://github.com/colebrookson/trait-based-rewiring/blob/main/src/reprexs/large-system.jl 
  MultipatchMumby_Original <- function(t,state,parameters){
    with(as.list(c(state,parameters)),{
      Ma <- state[1:l]
      Co <- state[(l+1):(2*l)]
      Tu <- state[((l*2)+1):(3*l)]
      dMa <- dCo <- dTu <- rep(NA,l)
      for(k in 1:l){
        dMa[k] <- a*Ma[k]*Co[k] - (g[k]*Ma[k])/(Ma[k]+Tu[k]) + gamma*p_m[k]*sum(Ma)*Tu[k]
        dCo[k] <- p_c[k]*r*sum(Co)*Tu[k] - d*Co[k] - a*Ma[k]*Co[k] 
        dTu[k] <- (g[k]*Ma[k])/(Ma[k]+Tu[k]) + d*Co[k] - (p_c[k]*r*sum(Co) + gamma*p_m[k]*sum(Ma))*Tu[k]
      }
      list(c(dMa,dCo,dTu)) 
    })
  }
  
  times <- seq(0,2000, by = 0.1)
  npoints <- length(times)
  
  mumbytrajectories <- data.frame(reefnum = rep(1:length(sitevector), each = npoints), sitename = rep(sitevector, each = npoints), reef_lat = rep(benthicfish_masterdataset$latitude, each = npoints), reef_long = rep(benthicfish_masterdataset$long.recenter, each = npoints), M = NA, C = NA, Tu = NA, TimeStep = rep(1:npoints, length(sitevector))) 
  
  #CalcTrajectories <- function(parameters,pc_val,pm_val,g_val,times,mumbytrajectories,benthicfish_masterdataset,MultipatchMumby_Original, sitevector){
  #Elmhirst parameter model 
  parameters <- list(a <- 0.1, d <- 0.24, g <- g_val, r <- 125, gamma <- 900, p_c <- pc_val, p_m <- pm_val, l <- length(sitevector))
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
  benthic_traj <- benthicfish_masterdataset %>%
    select(site, latitude, long.recenter, comb_grazinglevel)
  
  for(i in 1:length(sitevector)){
    benthic_traj$init_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][1]
    benthic_traj$init_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][1]
    benthic_traj$init_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][1]
    benthic_traj$final_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][npoints]
    benthic_traj$final_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][npoints]
    benthic_traj$final_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][npoints]
  }
  datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
  saveRDS(datalist, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_6.17.2022.rds"))
  
  
  
  #Well let's plot this out 
  worldmap <- map_data ("world", wrap = c(0, 360))
  
  #initial coral cover
  BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Scaling Factor ", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final coral cover
  BaseRun_newrgamma_FinalCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Coral Cover, Scaling Factor ", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_coralcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #initial malg cover
  BaseRun_newrgamma_InitialMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Scaling Factor ", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final malg cover
  BaseRun_newrgamma_FinalMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Malg Cover, Scaling Factor ", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_malgcover*100)))+
    scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  
  #grazing level
  BaseRun_newrgamma_allherbivores_GrazingLevelsUsed <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("GrazingLevelsUsed, Scaling Factor ", j," Base Run r =",r," gamma =", gamma))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (comb_grazinglevel*scalingfactors[j])))+
    scale_color_viridis(discrete = FALSE, name = "Grazing Rate")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(BaseRun_newrgamma_allherbivores_GrazingLevelsUsed, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/BaseRun_r",r,"gamma",gamma,"_allherbivores_GrazingLevel_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #rounded versions
  #initial coral cover
  Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Coral Cover, Gamma ", gamma,"r",r,"Scaling Factor",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_coralcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialCoralCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final coral cover
  Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Coral Cover, Gamma ", gamma,"r",r,"Scaling Factor",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_coralcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalCoralCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #initial malg cover
  Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Malg Cover, Gamma ", gamma,"r",r,"Scaling Factor",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_malgcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_InitialMalgCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #final malg cover
  Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover <-  
    ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
    xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Malg Cover, Gamma ", gamma,"r",r,"Scaling Factor",j))+
    geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_malgcover*100),10))))+
    scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
    #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
    coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
    scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
    scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                       breaks = seq(160, 190, 10),
                       labels = c(160, 170, "180/-180", -170)) +
    coord_equal() +  theme_bw()
  ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/Troubleshooting/randomizedconnmat_6.17.2022/Rounded_BaseRun_r",r,"gamma",gamma,"_allherbivores_FinalMalgCover_ScalingFactor",j,"_6.17.2022.png"), bg = "transparent", height = 10, width = 10)
  
  #just in case
  parameters <- NULL
  state <- NULL
  out <- NULL   
}

#lines not as clear as before
plot(seq(1,75,1), benthic_traj[,8])
plot(pc_val, benthic_traj[,8])

#what does it look like when i map out the coral larval dispersal connectivity, the original arrangement definitely looks distinct
benthic_traj_conn <- benthic_traj
benthic_traj_conn$coral_input <- pc_val_old #pc_val

ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Coral Input Levels, original version"))+
  geom_point(data = benthic_traj_conn, aes(x = long.recenter, y = latitude, color = coral_input))+
  scale_color_viridis(discrete = FALSE, name = "Coral Input Level")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
