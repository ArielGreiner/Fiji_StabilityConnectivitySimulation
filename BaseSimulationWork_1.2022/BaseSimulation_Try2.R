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
benthicfish_masterdataset$grazinglevel <- benthicfish_masterdataset$biomass_kgha_trophic_group_avg_herbivore_macroalgae
benthicfish_masterdataset$grazinglevel[is.na(benthicfish_masterdataset$grazinglevel)] <- 0 #the NAs are actually 0s
benthicfish_masterdataset$grazinglevel <- benthicfish_masterdataset$grazinglevel/largest_malgherbabundance
#scale it so the grazing rate either has a mean of 0.1, 0.3 or 0.5
median(benthicfish_masterdataset$grazinglevel) #mean = 0.06458761, median = 0.01231782
hist(benthicfish_masterdataset$grazinglevel) #very not normal, will use median
MD_scale <- 0.1/median(benthicfish_masterdataset$grazinglevel) #if multiply by 8.118318 will get a median of 0.1
ASS_scale <- 0.3/median(benthicfish_masterdataset$grazinglevel)
CD_scale <- 0.5/median(benthicfish_masterdataset$grazinglevel)
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
#subset it to only the 75 sites with both benthic cover and fish abundance data
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
r <- 0.55 #rate coral recruits onto/overgrows turf algae
gamma <- 0.77 #rate macroalgae recruits onto/overgrows turf algae
g_val <- benthicfish_masterdataset$grazinglevel*scalingfactors[j] 


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
parameters <- list(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, gamma <- 0.77, p_c <- pc_val, p_m <- pm_val, l <- length(sitevector))
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
  select(site, latitude, long.recenter, grazinglevel)

for(i in 1:length(sitevector)){
  benthic_traj$init_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][1]
  benthic_traj$init_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][1]
  benthic_traj$init_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][1]
  benthic_traj$final_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][npoints]
  benthic_traj$final_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][npoints]
  benthic_traj$final_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][npoints]
}
save(out, mumbytrajectories, benthic_traj, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun2_multigraz/fullsimrun_scalingfactor",j,"_1.25.2022.RData"))


#Well let's plot this out 
worldmap <- map_data ("world", wrap = c(0, 360))

#initial coral cover
BaseRun2_multigraz_InitialCoralCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Scaling Factor ", j," Base Run 2"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun2_multigraz_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun2_multigraz/BaseRun2_multigraz_InitialCoralCover_ScalingFactor",j,"_1.25.2022.png"), bg = "transparent", height = 10, width = 10)

#final coral cover
BaseRun2_multigraz_FinalCoralCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Coral Cover, Scaling Factor ", j," Base Run 2"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_coralcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun2_multigraz_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun2_multigraz/BaseRun2_multigraz_FinalCoralCover_ScalingFactor",j,"_1.25.2022.png"), bg = "transparent", height = 10, width = 10)

#initial malg cover
BaseRun2_multigraz_InitialMalgCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Scaling Factor ", j," Base Run 2"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun2_multigraz_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun2_multigraz/BaseRun2_multigraz_InitialMalgCover_ScalingFactor",j,"_1.25.2022.png"), bg = "transparent", height = 10, width = 10)

#final malg cover
BaseRun2_multigraz_FinalMalgCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Malg Cover, Scaling Factor ", j," Base Run 2"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_malgcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun2_multigraz_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun2_multigraz/BaseRun2_multigraz_FinalMalgCover_ScalingFactor",j,"_1.25.2022.png"), bg = "transparent", height = 10, width = 10)


#grazing level
BaseRun2_multigraz_GrazingLevelsUsed <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("GrazingLevelsUsed, Scaling Factor ", j," Base Run 2"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (grazinglevel*scalingfactors[j])))+
  scale_color_viridis(discrete = FALSE, name = "Grazing Rate")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun2_multigraz_GrazingLevelsUsed, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun2_multigraz/BaseRun2_multigraz_GrazingLevelsUsed_ScalingFactor",j,"_1.25.2022.png"), bg = "transparent", height = 10, width = 10)

}

#how many sites have a grazing level of 0 (as derived from a macroalgal herbivore density of 0)
benthicfish_masterdataset %>%
  select(site,grazinglevel)

length(benthicfish_masterdataset$grazinglevel[which(benthicfish_masterdataset$grazinglevel == 0)]) #28 zeros

benthicfish_masterdataset_zeros <- benthicfish_masterdataset
benthicfish_masterdataset_zeros$zeros <- FALSE
benthicfish_masterdataset_zeros$zeros[benthicfish_masterdataset_zeros$grazinglevel == 0] <- TRUE

ZeroGrazingLevels <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("28 sites with Grazing Levels of Zero"))+
  geom_point(data = benthicfish_masterdataset_zeros, aes(x = long.recenter, y = latitude, color = zeros))+
  scale_color_viridis(discrete = TRUE, name = "Grazing Rate Zero or Not")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(ZeroGrazingLevels, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/ZeroGrazingLevels_1.25.2022.png"), bg = "transparent", height = 10, width = 10) #used to be 1.20.2022

#looking at ranges of grazing level values seen post-scaling #1.25.2022: didn't redo these histograms
hist(benthicfish_masterdataset$grazinglevel*scalingfactors[1])
hist(benthicfish_masterdataset$grazinglevel*scalingfactors[2])
hist(benthicfish_masterdataset$grazinglevel*scalingfactors[3])
hist(benthicfish_masterdataset$grazinglevel*scalingfactors[4])

quantile(benthicfish_masterdataset$grazinglevel*scalingfactors[1])
#0%        25%        50%        75%       100% 
#0.00000000 0.00000000 0.01216528 0.04464416 0.98524785 
quantile(benthicfish_masterdataset$grazinglevel*scalingfactors[2])
#0%       25%       50%       75%      100% 
#0.0000000 0.0000000 0.1000000 0.3669801 8.0988506 
quantile(benthicfish_masterdataset$grazinglevel*scalingfactors[3])
#0%      25%      50%      75%     100% 
#0.00000  0.00000  0.30000  1.10094 24.29655
quantile(benthicfish_masterdataset$grazinglevel*scalingfactors[4])
#0%       25%       50%       75%      100% 
#0.000000  0.000000  0.500000  1.834901 40.494253 


###looking at detritivore herbivore densities

#only the >2016 herbivore data from wcs fiji sites 
#save(wcsonly_fiji_fishbelt_current, file = here("Fish Belt Vis","2021_fishbelt_thesisproject", "wcsonly_current_fiji_herbivoredata.RData"))
load("~/GitHub/WCSFiji2020Internship/Fiji-status/Fish Belt Vis/2021_fishbelt_thesisproject/wcsonly_current_fiji_herbivoredata.RData")
benthicfish_masterdataset_detherbs <- benthicfish_masterdataset
wcsonly_fiji_fishbelt_current_jointsitesonly <- wcsonly_fiji_fishbelt_current[wcsonly_fiji_fishbelt_current$site %in% sitevector,]
wcsonly_fiji_fishbelt_current_jointsitesonly <- wcsonly_fiji_fishbelt_current_jointsitesonly[-10,] #removing VIR10 2018
#are the sites in the same order? no
setequal(wcsonly_fiji_fishbelt_current_jointsitesonly$site, benthicfish_masterdataset_detherbs$site)
wcsonly_fiji_fishbelt_current_jointsitesonly$site %in% benthicfish_masterdataset_detherbs$site #(all TRUE both ways)
wcsonly_fiji_fishbelt_current_jointsitesonly %>%
  distinct(site) #only 75 distinct sites, two KB17 data points (keep the more recent one, aka the 2020 one)
wcsonly_fiji_fishbelt_current_jointsitesonly <- wcsonly_fiji_fishbelt_current_jointsitesonly[-17,]
wcsonly_fiji_fishbelt_current_jointsitesonly_abr <- wcsonly_fiji_fishbelt_current_jointsitesonly %>%
  select(site, biomass_kgha_trophic_group_avg_herbivore_detritivore)
benthicfish_masterdataset_detherbs <- merge(benthicfish_masterdataset_detherbs, wcsonly_fiji_fishbelt_current_jointsitesonly_abr, by = "site")
#5172.61 is the max detritivore density recorded
largest_detherbabundance <- 5172.61
benthicfish_masterdataset_detherbs$det_grazinglevel <- benthicfish_masterdataset_detherbs$biomass_kgha_trophic_group_avg_herbivore_detritivore/largest_detherbabundance
benthicfish_masterdataset_detherbs$malg_density <- benthicfish_masterdataset_detherbs$biomass_kgha_trophic_group_avg_herbivore_macroalgae
benthicfish_masterdataset_detherbs$malg_density[is.na(benthicfish_masterdataset_detherbs$malg_density)] <- 0
benthicfish_masterdataset_detherbs$comb_grazinglevel <- (benthicfish_masterdataset_detherbs$biomass_kgha_trophic_group_avg_herbivore_detritivore + benthicfish_masterdataset_detherbs$malg_density)/(largest_detherbabundance+largest_malgherbabundance)
hist(benthicfish_masterdataset_detherbs$det_grazinglevel) #1.25.2022: didn't re-save when removed NS3
quantile(benthicfish_masterdataset_detherbs$det_grazinglevel)
#0%         25%         50%         75%        100% 
#0.002780028 0.029169027 0.043956533 0.073889197 0.347366223 
hist(benthicfish_masterdataset_detherbs$comb_grazinglevel) #1.25.2022: didn't re-save when removed NS3
quantile(benthicfish_masterdataset_detherbs$comb_grazinglevel)
#0%         25%         50%         75%        100% 
#0.002330291 0.027875285 0.042770911 0.074997702 0.366447078

