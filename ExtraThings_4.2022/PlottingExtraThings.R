#load in the standard things used to run the simulations (as of 3.30.2022)
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
library(igraph)


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

worldmap <- map_data ("world", wrap = c(0, 360))
#####

#Plotting Management - 3.30.2022
###want to plot management
#benthicfish_masterdataset$management
#want to extract out the LMMA/Tabu/MPA/Open part
benthicfish_masterdataset <- benthicfish_masterdataset %>%
  mutate(management = toupper(management)) %>%
  mutate(mgmt_abr = gsub(".*_","",management)) 

#trying to re-order factor levels #update: it worked!
benthicfish_masterdataset$mgmt_abr <- factor(benthicfish_masterdataset$mgmt_abr, levels = c("OPEN","LMMA", "TABU","MPA"))

worldmap <- map_data ("world", wrap = c(0, 360))

Mgmt_75Reefs_3.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Management Levels"))+
  geom_point(data = benthicfish_masterdataset, aes(x = long.recenter, y = latitude, color = as.factor(mgmt_abr)))+
  scale_color_viridis(discrete = TRUE, name = "Management Levels")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(Mgmt_75Reefs_3.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/Mgmt75Reefs_3.30.2022.png"), bg = "transparent", height = 10, width = 10)

#4.25.2022: plotting management levels after Yash sent me updated management data
load(file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_updatedmgmtlevels_4.25.2022.RData")

Mgmt_75Reefs_4.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Management Levels"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, color = as.factor(mgmt_abr)))+
  scale_color_viridis(discrete = TRUE, name = "Management Levels")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(Mgmt_75Reefs_4.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/Mgmt75Reefs_4.25.2022.png"), bg = "transparent", height = 10, width = 10)

#8.9.2022: plotting management levels after Yash sent me updated management data, csee styles
load(file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_updatedmgmtlevels_4.25.2022.RData")

Mgmt_75Reefs_8.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Management Levels"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, color = as.factor(mgmt_abr)))+
  #scale_color_viridis(discrete = TRUE, name = "Management Levels")+
  scale_color_manual(values=c("#4472C4","#4472C4","#70AD47"),breaks=c("LMMA","MPA","TABU"))+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(Mgmt_75Reefs_8.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/Mgmt75Reefs_csee_8.9.2022.png"), bg = "transparent", height = 10, width = 10)


##plotting initial coral and macroalgae cover, using better colours
InitialCoral_75Reefs_4.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover"))+
  geom_point(data = benthicfish_masterdataset, aes(x = long.recenter, y = latitude, color = coral_cover*100))+
  scale_color_viridis(option = "plasma", discrete = FALSE, name = "Initial Coral Cover")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(InitialCoral_75Reefs_4.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/InitialCoral_75Reefs_4.2022.png"), bg = "transparent", height = 10, width = 10)

InitialMalg_75Reefs_4.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Macroalgae Cover"))+
  geom_point(data = benthicfish_masterdataset, aes(x = long.recenter, y = latitude, color = malg_cover*100))+
  scale_color_viridis(option = "cividis", discrete = FALSE, name = "Initial Macroalgae Cover")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(InitialMalg_75Reefs_4.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/InitialMalg_75Reefs_4.2022.png"), bg = "transparent", height = 10, width = 10)

#Plotting Total Herbivore Density (det herbs + malg herbs)
benthicfish_masterdataset_noNAs <- benthicfish_masterdataset
benthicfish_masterdataset_noNAs$biomass_kgha_trophic_group_avg_herbivore_macroalgae[is.na(benthicfish_masterdataset_noNAs$biomass_kgha_trophic_group_avg_herbivore_macroalgae)] <- 0
  
TotalHerbivores_75Reefs_4.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Total Herbivore Density"))+
  geom_point(data = benthicfish_masterdataset_noNAs, aes(x = long.recenter, y = latitude, color = (biomass_kgha_trophic_group_avg_herbivore_detritivore + biomass_kgha_trophic_group_avg_herbivore_macroalgae)))+
  scale_color_viridis(option = "inferno", discrete = FALSE, name = "Total Herbivore Density")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(TotalHerbivores_75Reefs_4.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/TotalHerbivores_75Reefs_4.2022.png"), bg = "transparent", height = 10, width = 10)

#Looking at retention at the different PLDs
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
retention <- rep(0,26)
dimdim <- dim(Conn_Mat_Sum[[1]])[1]
for(i in 1:26){
  for(j in 1:dimdim){
    dummy <- sum(Conn_Mat_Sum[[i]][j,]) #sum(Conn_Mat_Sum[[i]][,j]) #exact same thing
    retention[i] <- retention[i] + dummy 
  }
}
col_retention <- retention

#6.28.2022: map of inputs to each coral reef (coral and malg)
pc_val <- pm_val <- pcout_val <- pmout_val <- rep(NA,length(sitevector))
for(i in 1:length(sitevector)){
  pm_val[i] <- sum(jointsite_coral_weightedavgconnmat[i,]) #sum of inputs to i (coral), sum row i
  #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
  pc_val[i] <- sum(jointsite_malg_smallPLDconnmat[i,]) #sum of inputs to i (macroalgae)
  pcout_val[i] <- sum(jointsite_coral_weightedavgconnmat[,i])
  pmout_val[i] <- sum(jointsite_malg_smallPLDconnmat[,i])
}
benthicfish_masterdataset$coral_inputlevels <- pc_val
benthicfish_masterdataset$malg_inputlevels <- pm_val

CoralInputMap_75Reefs_6.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Coral Input Map"))+
  geom_point(data = benthicfish_masterdataset, aes(x = long.recenter, y = latitude, color = coral_inputlevels))+
  scale_color_viridis(option = "viridis", discrete = FALSE, name = "Coral Input Levels")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(CoralInputMap_75Reefs_6.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/CoralInputMap_75Reefs_6.2022.png"), bg = "transparent", height = 10, width = 10)

MalgInputMap_75Reefs_6.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Malg Input Map"))+
  geom_point(data = benthicfish_masterdataset, aes(x = long.recenter, y = latitude, color = malg_inputlevels))+
  scale_color_viridis(option = "viridis", discrete = FALSE, name = "Malg Input Levels")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(MalgInputMap_75Reefs_6.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/MalgInputMap_75Reefs_6.2022.png"), bg = "transparent", height = 10, width = 10)

#12.2022 - plotting net coral migration
benthicfish_masterdataset$netcoralmigration <- pc_val - pcout_val

NetCoralMigration_75Reefs_12.2022 <- 
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Net Coral Migration Map"))+
  geom_point(data = benthicfish_masterdataset, aes(x = long.recenter, y = latitude, color = netcoralmigration))+
  scale_color_viridis(option = "viridis", discrete = FALSE, name = "Net Coral Migration")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(NetCoralMigration_75Reefs_12.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/NetCoralMigration_75Reefs_12.2022.png"), bg = "transparent", height = 10, width = 10)




#lost the code used to generate the MalgNetworks_75Reefs_4.2022.png map and all of the double checking I did to confirm it (i think i just checked that the 551 reef subsetting down to the 75 reefs worked correctly and also i used the full matrix to make the networks and then subsetted the network IDs when putting them into the benthicfish_masterdataset)

#take the summed (across months/years) coral connectivity matrix with a PLD of 5
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
#malg_smallPLD_connmat <- Conn_Mat_Sum[[1]]
#jointsite_malg_smallPLDconnmat <- malg_smallPLD_connmat[jointsite_rows, jointsite_rows]

library(igraph)
relevantcoordinates <- coordinates
relevantcoordinates <- relevantcoordinates[which(relevantcoordinates$site %in% sitevector),]

g_malg_orig <- graph.adjacency(as.matrix(Conn_Mat_Sum[[1]]), weighted = TRUE) #need to use the full 551 reef connectivity matrix
relevantcoordinates$wnetworks_malg <- clusters(g_malg_orig,mode = "weak")$membership[jointsite_rows] #only including the 75 reefs in the model
relevantcoordinates$snetworks_malg <- clusters(g_malg_orig,mode = "strong")$membership[jointsite_rows]

worldmap <- map_data ("world", wrap = c(0, 360))

malgweaknetworks <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Macroalgae Weak Networks"))+
  geom_point(data = relevantcoordinates, aes(x = long.recenter, y = latitude, color = as.factor(wnetworks_malg)))+
  scale_color_viridis(discrete = TRUE, name = "Network #")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(malgweaknetworks, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/malgweaknetworks.png"), bg = "transparent", height = 10, width = 10)

malgstrongnetworks <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Macroalgae Strong Networks"))+
  geom_point(data = relevantcoordinates, aes(x = long.recenter, y = latitude, color = as.factor(snetworks_malg)))+
  scale_color_viridis(discrete = TRUE, name = "Network #")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(malgstrongnetworks, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/malgstrongnetworks.png"), bg = "transparent", height = 10, width = 10)

#plotting sedimentation/mortality levels
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sedimentlvls_5.10.2022.RData") #benthicfish_masterdataset_sed
worldmap <- map_data ("world", wrap = c(0, 360))

sedimentation_mortality_levels <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Mortality/Sedimentation Levels"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, color = sediment_altalt))+
  scale_color_viridis(discrete = FALSE, name = "Mortality/Sedimentation Level")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(sedimentation_mortality_levels, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/sedimentationmortalitylevels.png"), bg = "transparent", height = 10, width = 10)

#9.27.2022: Plotting map of all 551 reefs and the 75 reefs
#newcoordinates file with all 551 reef sites
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData") 
Selecting_75Reefs_9.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("75 Selected Reefs"))+
  geom_point(data = newcoordinates, aes(x = long.recenter, y = latitude, colour = "grey"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, colour = "black"))+
  scale_color_identity(name="Included", labels=c("yes","no"), guide="legend")+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(Selecting_75Reefs_9.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/Selecting_75Reefs_9.27.2022.png"), bg = "transparent", height = 10, width = 10)

#9.29.2022 - plotting relationship between initial coral cover and grazing rate, net input, mortality
#benthicfish_masterdataset has initial coral and macroalgal cover, extract the initial coral cover
#benthicfish_masterdataset_sed - comb_grazinglevel is the grazing rate, sediment_altalt is the mortality level
#coral and malg net input values
pc_val <- pm_val <- rep(NA,length(sitevector)) #immigration
pcout_val <- pmout_val <- rep(NA,length(sitevector)) #emigration
for(i in 1:75){
  pm_val[i] <- sum(jointsite_coral_weightedavgconnmat[i,]) #sum of inputs to i (coral), sum row i
  #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
  pc_val[i] <- sum(jointsite_malg_smallPLDconnmat[i,]) #sum of inputs to i (macroalgae)
  pcout_val[i] <- sum(jointsite_coral_weightedavgconnmat[,i])
  pmout_val[i] <- sum(jointsite_malg_smallPLDconnmat[,i])
}
#finalcov_restr$coralmovementlevel <- pc_val - pcout_val

#get everything into the same dataframe
benthicfish_masterdataset_initialplotting <- benthicfish_masterdataset_sed
benthicfish_masterdataset_initialplotting$initialcoral <- benthicfish_masterdataset$coral_cover
benthicfish_masterdataset_initialplotting$coral_netinput <- pc_val - pcout_val

initialcoralcover_coralnetinput <- ggplot(benthicfish_masterdataset_initialplotting, aes(x = coral_netinput, y = initialcoral*100)) +
  geom_point(size=3)+
  ylab("% Coral Cover")+
  xlab("Coral Net Input")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
#facet_wrap(~graz)
ggsave(initialcoralcover_coralnetinput, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/initialcoralcover_coralnetinput_9.29.2022.png"), bg = "transparent", height = 10, width = 10)

initialcoralcover_grazingrate <- ggplot(benthicfish_masterdataset_initialplotting, aes(x = comb_grazinglevel, y = initialcoral*100)) +
  geom_point(size=3)+
  ylab("% Coral Cover")+
  xlab("Grazing Rate")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
#facet_wrap(~graz)
ggsave(initialcoralcover_grazingrate, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/initialcoralcover_grazingrate_9.29.2022.png"), bg = "transparent", height = 10, width = 10)

initialcoralcover_mortalityrate <- ggplot(benthicfish_masterdataset_initialplotting, aes(x = sediment_altalt, y = initialcoral*100)) +
  geom_point(size=3)+
  ylab("% Coral Cover")+
  xlab("Mortality Rate")+
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
#facet_wrap(~graz)
ggsave(initialcoralcover_mortalityrate, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/initialcoralcover_mortalityrate_9.29.2022.png"), bg = "transparent", height = 10, width = 10)

#10.27.2022 - Plotting Coral Weak Networks, Clearly Delineating the 75 Reefs Chosen
#worldmap <- map_data ("world", wrap = c(0, 360))
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData") 
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/weightedavgconnmat.RData")
#relevantcoordinates <- coordinates
#relevantcoordinates <- relevantcoordinates[which(relevantcoordinates$site %in% sitevector),]

g_coral_orig <- graph.adjacency(as.matrix(weightedavgconnmat), weighted = TRUE)
newcoordinates$wnetworks_coral <- clusters(g_coral_orig,mode = "weak")$membership
newcoordinates$snetworks_coral <- clusters(g_coral_orig,mode = "strong")$membership
#want to make a column that identifies whether a reef is (a) in snetwork 3 + in the 75, (b) not in snetwork 3 + in the 75, (c) not in either
newcoordinates$in75 <- 0
newcoordinates$in75[relevantcoordinates$X] <- 1
newcoordinates$sn75 <- "not in 75 Reefs, in single reef networks"
newcoordinates$sn75[newcoordinates$snetworks_coral == 3] <- "not in 75 Reefs, in big network"
newcoordinates$sn75[newcoordinates$in75 > 0] <- "in 75 Reefs, in single reef networks"
newcoordinates$sn75[(newcoordinates$in75)*(newcoordinates$snetworks_coral) == 3] <- "in 75 Reefs, in big network"

#if just want to look at the 75 reefs: newcoordinates[relevantcoordinates$X,]
CoralStrongNetworks_75Reefs_10.2022 <- ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Coral Strong Networks + 75 Selected Reefs"))+
  geom_point(data = newcoordinates, aes(x = long.recenter, y = latitude, colour = sn75))+
  scale_color_manual(values=c("red","#000000","lightblue","grey"),breaks=c("in 75 Reefs, in single reef networks","in 75 Reefs, in big network","not in 75 Reefs, in single reef networks","not in 75 Reefs, in big network"), labels=c("in 75 Reefs, in single reef networks","in 75 Reefs, in big network","not in 75 Reefs, in single reef networks","not in 75 Reefs, in big network"), name = " ")+
  #scale_color_manual(values=c("lightblue","grey","red","#000000"),breaks=c("not in 75 Reefs, in single reef networks","not in 75 Reefs, in big network","in 75 Reefs, in single reef networks","in 75 Reefs, in big network"), labels=c("not in 75 Reefs, in single reef networks","not in 75 Reefs, in big network","in 75 Reefs, in single reef networks","in 75 Reefs, in big network"), name = " ")+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(CoralStrongNetworks_75Reefs_10.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/CoralStrongNetworks_75Reefs_10.2022.png"), bg = "transparent", height = 10, width = 10)

#basically almost every reef is in strong network 3, the rest are in networks of 1
ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Coral Strong Networks + 75 Selected Reefs"))+
  geom_point(data = newcoordinates, aes(x = long.recenter, y = latitude, colour = "pink", alpha = 0.01))+
  geom_point(data = newcoordinates[newcoordinates$snetworks_coral == 3,], aes(x = long.recenter, y = latitude, colour = "grey"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, colour = "black", alpha = 0.1))+
  #scale_color_identity(name="Included", labels=c("yes","no"), guide="legend")+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()

#NOTE: this graph does not work BUT there is truly only 1 weak network...so...
CoralWeakNetworks_75Reefs_10.2022 <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Coral Strong Networks + 75 Selected Reefs"))+
  geom_point(data = newcoordinates, aes(x = long.recenter, y = latitude, colour = factor(snetworks_coral)))+
  #geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, colour = "black"))+
  #scale_color_identity(name="Included", labels=c("yes","no"), guide="legend")+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(CoralWeakNetworks_75Reefs_10.2022, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ExtraThings_4.2022/CoralWeakNetworks_75Reefs_10.2022.png"), bg = "transparent", height = 10, width = 10)

#11.9.2022 - How many reefs coral cover increases/decreases over time under the base_dvar scenario?
#what does the initial -> final look like for the base_dvar scenario?
basecov <- data.frame(graz = rep(c(1,2,3,4),each=2*numreefs), scenario = rep(c("initial","final"), each = numreefs), reef = seq(1,numreefs,1), coral_cover=NA, malg_cover = NA, ratio = NA)
for(j in 1:4){
  data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/AlteredSedimentScenario5.2022/base/SedimentScenario_fullsimrun_allherbs_gammaval",gamma,"r_val",r,"_scalingfactor",j,"_base_5.10.2022.rds"))
  benthic_traj <- data[[3]]
  basecov$coral_cover[basecov$graz == j & basecov$scenario == "initial"] <- benthic_traj$init_coralcover
  basecov$coral_cover[basecov$graz == j & basecov$scenario == "final"] <- benthic_traj$final_coralcover
  basecov$malg_cover[basecov$graz == j & basecov$scenario == "initial"] <- benthic_traj$init_malgcover
  basecov$malg_cover[basecov$graz == j & basecov$scenario == "final"] <- benthic_traj$final_malgcover
  basecov$ratio[basecov$graz == j & basecov$scenario == "initial"] <- (1+benthic_traj$init_coralcover)/(1+benthic_traj$init_malgcover)
  basecov$ratio[basecov$graz == j & basecov$scenario == "final"] <- (1+benthic_traj$final_coralcover)/(1+benthic_traj$final_malgcover)
}
#'low' scenario - 2 increased in coral cover, 73 decreased, none stayed the same
length(which(basecov$coral_cover[basecov$graz == 2 & basecov$scenario == "final"] - basecov$coral_cover[basecov$graz == 2 & basecov$scenario == "initial"] == 0)) 
length(which((basecov$coral_cover[basecov$graz == 2 & basecov$scenario == "final"]*100) < 1)) #43
length(which((basecov$coral_cover[basecov$graz == 2 & basecov$scenario == "final"]*100) < 10)) #66
length(which((basecov$coral_cover[basecov$graz == 2 & basecov$scenario == "final"]*100) > 30)) #1
#'medium' scenario - 46 increased in coral cover, 29 decreased, none stayed the same
length(which(basecov$coral_cover[basecov$graz == 3 & basecov$scenario == "final"] - basecov$coral_cover[basecov$graz == 3 & basecov$scenario == "initial"] == 0)) 
length(which((basecov$coral_cover[basecov$graz == 3 & basecov$scenario == "final"]*100) < 1)) #28
length(which((basecov$coral_cover[basecov$graz == 3 & basecov$scenario == "final"]*100) < 10)) #28
length(which((basecov$coral_cover[basecov$graz == 3 & basecov$scenario == "final"]*100) > 30)) #43
#'high' scenario - 47 increased in coral cover, 28 decreased, none stayed the same
length(which(basecov$coral_cover[basecov$graz == 4 & basecov$scenario == "final"] - basecov$coral_cover[basecov$graz == 4 & basecov$scenario == "initial"] < 0)) 
length(which((basecov$coral_cover[basecov$graz == 4 & basecov$scenario == "final"]*100) < 1)) #28
length(which((basecov$coral_cover[basecov$graz == 4 & basecov$scenario == "final"]*100) < 10)) #28
length(which((basecov$coral_cover[basecov$graz == 4 & basecov$scenario == "final"]*100) > 30)) #47
