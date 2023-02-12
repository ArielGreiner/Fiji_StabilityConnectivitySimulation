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

###load in other parameters
#Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
#Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
a <- 0.1 #rate macroalgae overgrows coral
d <- 0.24 #rate coral dies
r <- 0.55 #rate coral recruits onto/overgrows turf algae
gamma <- 0.77 #rate macroalgae recruits onto/overgrows turf algae
g_val <- benthicfish_masterdataset$grazinglevel
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
    dMa[k] <- a*Ma[k]*Co[k] - (g_val[k]*Ma[k])/(Ma[k]+Tu[k]) + gamma*p_m[k]*sum(Ma)*Tu[k]
    dCo[k] <- p_c[k]*r*sum(Co)*Tu[k] - d*Co[k] - a*Ma[k]*Co[k] 
    dTu[k] <- (g_val[k]*Ma[k])/(Ma[k]+Tu[k]) + d*Co[k] - (p_c[k]*r*sum(Co) + gamma*p_m[k]*sum(Ma))*Tu[k]
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
    print("In trajectory calculation function")
    start_time <- Sys.time()
    out <- lsode(y = state, times = times, func = MultipatchMumby_Original, parms = parameters)	
    end_time <- Sys.time()
    end_time - start_time
    print(paste("first row of out = ", out[1,]))
    for(i in 1:length(sitevector)){ 
      mumbytrajectories$M[mumbytrajectories$reefnum == i] <- out[,(i+1)]
      mumbytrajectories$C[mumbytrajectories$reefnum == i] <- out[,(length(sitevector)+i+1)]
      mumbytrajectories$Tu[mumbytrajectories$reefnum == i] <- out[,((length(sitevector)*2)+i+1)]
    }
  #return(mumbytrajectories)
#}
    
#all of the 1.25.2022 are for after removing NS3    
save(out, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/fullsimrun_smallmalgPLD_1.25.2022.RData")

#save(out, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/firstfullsimrun_1.12.2022.RData")
#error message: DLSODE-  At current T (=R1), MXSTEP (=I1) steps   
#taken on this call before reaching TOUT     
#In above message, I1 = 5000

#In above message, R1 = 0.0245052

#Warning messages:
#  1: In lsode(y = state, times = times, func = MultipatchMumby_Original,  :
#               an excessive amount of work (> maxsteps ) was done, but integration was not successful - increase maxsteps
#    2: In lsode(y = state, times = times, func = MultipatchMumby_Original,  :
#                            Returning early. Results are accurate, as far as they go
#a bunch of the values are jumping over 1, some are starting over one as well, so things are definitely off

#save(out, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/secondfullsimrun_1.12.2022.RData") #14.79min, no error messages
#some of them are going over 1 still, it's weird that this time i had no error messages even tho i didn't actually fix the problem all the way?? 

#save(out, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/thirdfullsimrun_1.12.2022.RData") #14.73496 min, no error messages, not going over 1 anymore!

#save(out, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/fullsimrun_smallmalgPLD_1.13.2022.RData") #14.467 min, no error messages, not going over 1 anymore!


load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/fullsimrun_smallmalgPLD_1.25.2022.RData")

for(i in 1:length(sitevector)){ 
  mumbytrajectories$M[mumbytrajectories$reefnum == i] <- out[,(i+1)]
  mumbytrajectories$C[mumbytrajectories$reefnum == i] <- out[,(length(sitevector)+i+1)]
  mumbytrajectories$Tu[mumbytrajectories$reefnum == i] <- out[,((length(sitevector)*2)+i+1)]
}

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

#Well let's plot this out (3rd full sim run 1.12.2022, 4th full sim run 1.13.2022)
worldmap <- map_data ("world", wrap = c(0, 360))

#initial coral cover
#BaseRun_InitialCoralCover_1.13.2022 - 3rd full sim run 1.12.2022
#BaseRun_smallmalg_InitialCoralCover_1.13.2022
BaseRun_smallmalg_InitialCoralCover_1.25.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Base Run"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun_smallmalg_InitialCoralCover_1.25.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun_smallmalg_InitialCoralCover_1.25.2022.png"), bg = "transparent", height = 10, width = 10)

#final coral cover
#BaseRun_FinalCoralCover_1.13.2022
#BaseRun_smallmalg_FinalCoralCover_1.13.2022
BaseRun_smallmalg_FinalCoralCover_1.25.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Coral Cover, Base Run"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_coralcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun_smallmalg_FinalCoralCover_1.25.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun_smallmalg_FinalCoralCover_1.25.2022.png"), bg = "transparent", height = 10, width = 10)

#initial malg cover
#BaseRun_InitialMalgCover_1.13.2022
#BaseRun_smallmalg_InitialMalgCover_1.13.2022
BaseRun_smallmalg_InitialMalgCover_1.25.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Base Run"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun_smallmalg_InitialMalgCover_1.25.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun_smallmalg_InitialMalgCover_1.252022.png"), bg = "transparent", height = 10, width = 10)

#final malg cover
#BaseRun_FinalMalgCover_1.13.2022
#BaseRun_smallmalg_FinalMalgCover_1.13.2022
BaseRun_smallmalg_FinalMalgCover_1.25.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Malg Cover, Base Run"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_malgcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun_smallmalg_FinalMalgCover_1.25.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun_smallmalg_FinalMalgCover_1.25.2022.png"), bg = "transparent", height = 10, width = 10)


#grazing level
#BaseRun_GrazingLevelsUsed_1.13.2022
BaseRun_GrazingLevelsUsed_1.25.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("GrazingLevelsUsed, Base Run"))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = grazinglevel))+
  scale_color_viridis(discrete = FALSE, name = "Grazing Rate")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun_GrazingLevelsUsed_1.25.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/BaseRun_GrazingLevelsUsed_1.25.2022.png"), bg = "transparent", height = 10, width = 10)














######OLD
#functions
MultipatchMumby_Original <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dMa <- a*Ma*Co - (glvl*Ma)/(Ma+Tu) + gamma*(1-q_m)*Ma*Tu + gamma*p_m*M2*Tu
    dCo <- r*(1-q_c)*Tu*Co - d*Co - a*Ma*Co + p_c*r*C2*Tu
    dTu <- (glvl*Ma)/(Ma+Tu) - gamma*(1-q_m)*Ma*Tu + d*Co - r*(1-q_c)*Tu*Co - (p_c*r*C2 + gamma*p_m*Ma)*Tu
    dM2 <- a*M2*C2 - (g*M2)/(M2+Tu2) + gamma*(1-p_m)*M2*Tu2 + gamma*q_m*Ma*Tu2
    dC2 <- r*(1-p_c)*Tu2*C2 - d*C2 - a*M2*C2 + q_c*r*Co*Tu2
    dT2 <- (g*M2)/(M2+Tu2) - gamma*(1-p_m)*M2*Tu2 + d*C2 - r*(1-p_c)*Tu2*C2 - (gamma*q_m*Ma + q_c*r*Co)*Tu2
    list(c(dMa,dCo,dTu,dM2,dC2,dT2))
  })
}

#for this base run only going to have one trajectory


#ntrajectory <- dim(grid)[1]
times <- seq(0,2000, by = 0.1)
npoints <- length(times)
initM <- grid$Minit
initC <- grid$Cinit
initT <- grid$Tinit

#can't see the full range of trajectories without starting the values in the other patch from different starting points (can't just set M1=M2, C1=C2 re: starting points)
initM2 <- gridtwo$Minit
initC2 <- gridtwo$Cinit
initT2 <- gridtwo$Tinit

#initializing the dataframes beforehand
mumbytrajectories <- data.frame(Run=rep(1:ntrajectory, each = npoints), M1 = NA, C1 = NA, T1 = NA, M2 = NA, C2 = NA, T2 = NA, TimeStep = rep(1:npoints))

CalcTrajectories <- function(i,parameters,recruitvalue, g_val, glvl, lvl, ntrajectory,times,mumbytrajectories,initM,initC,initT,initM2,initC2,initT2,MumbyOpen_Elmhirst_2PatchExt){
  for(i in 1:ntrajectory){
    #Elmhirst parameter model 
    parameters <- c(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, gamma <- 0.77, p_c <- recruitvalue, q_c <- recruitvalue, p_m <- round(recruitvalue*lvl,4), q_m <- round(recruitvalue*lvl,4), glvl <- glvl)
    #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
    state <- c(M1 = initM[i], C1 = initC[i], Tu1 = initT[i], M2 = initM2[i], C2 = initC2[i], Tu2 = initT2[i])
    print("In trajectory calculation function")
    out <- lsode(y = state, times = times, func = MumbyOpen_Elmhirst_2PatchExt, parms = parameters)	
    print(paste("first row of out = ", out[1,]))
    mumbytrajectories[mumbytrajectories$Run == i,"M1"] <- out[,2]
    print(paste("M1 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"M1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"C1"] <- out[,3]
    print(paste("C1 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"C1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"T1"] <- out[,4]
    print(paste("T1 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"T1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"M2"] <- out[,5]
    print(paste("M2 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"M2"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"C2"] <- out[,6]
    print(paste("C2 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"C2"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"T2"] <- out[,7]
    print(paste("T2 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"T2"][1]))
    print(paste("First row of mumbytrajectories post-indexing", mumbytrajectories[1,]))
  }
  return(mumbytrajectories)
}
