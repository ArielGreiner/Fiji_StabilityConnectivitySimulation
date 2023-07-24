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

#Part 1: trying to match the coordinates i have with the stressor map polygons from https://github.com/WCS-Marine/local-reef-pressures (used to make: https://programs.wcs.org/vibrantoceans/map)
#Part 2: need to make buffers around the sites and figure out a way to see which reefs fall within said buffers as the buffer size increases, will be used to look at what happens when make the tabu's bigger

####Part 0 - Loading in the benthicfish master dataset
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

#####Part 1 (referred to: https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package/343477#343477, didn't use: https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile)

#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/localreefpressuresdata_data/allreefs.RData") #can't even view this, it's too big

#Krista helped with this code, based off of her code that she sent me 'For-loop-all-fish.r'
allreefs_WGS84 <- sf::st_read("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/localreefpressuresdata_data/allreefs_WGS84/allreefs_WGS84.shp")
ls(allreefs_WGS84)
#plot(allreefs_WGS84, border = NA)

#subset to just melanesia
melanesia_allreefsWGS84 <- allreefs_WGS84[allreefs_WGS84$Region == "Melanesia",]
#st_write(melanesia_allreefsWGS84, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/localreefpressuresdata_data/melanesia_allreefsWGS84.shp")

#5.10.2022 - trying with the non-recentred longitudes?
#subset benthicfish_masterdataset -> lat/long only dataframe
benthicfish_masterdataset_latlong_new <- benthicfish_masterdataset[,c(5,4)] #want longitude to be first
names(benthicfish_masterdataset_latlong_new) <- c("longitude","latitude") #reset the names
benthicfish_latlong_shp_new <- st_as_sf(x = benthicfish_masterdataset_latlong_new, 
                                        coords = c("longitude", "latitude"),
                                        crs = st_crs(4326))


#make this into a shape file so i can play with it in qgis
#st_write(benthicfish_latlong_shp_new, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/benthicfish_masterdataset_latlong_5.10.2022.shp")

#set benthicfish_latlong_shp to have the same crs as allreefs_WGS84, i think this messed up the coordinates tho
#benthicfish_latlong_shp_adj=st_transform(benthicfish_latlong_shp,st_crs(allreefs_WGS84)) #dont need anymore
st_crs(benthicfish_latlong_shp_new)
#st_crs(benthicfish_latlong_shp)
st_crs(melanesia_allreefsWGS84) #st_crs(allreefs_WGS84)

#matching points in benthicfish_latlong_shp with the polygons in allreefs_WGS84, sp = F to not return an sp type object (but to return an sf one...instead?), duplicate = TRUE so that one coordinate can fall in 2 polygons
#need a more up to date version of R (4.0 or higher), Krista knows how to use two versions at once
#test.pip=spatialEco::point.in.poly(x=benthicfish_latlong_shp, y=allreefs_WGS84, sp = F, duplicate = TRUE)

#this one (intersectionz) runs but a bunch of them don't intersect, seems like the lau reefs don't show up in the local reef pressures dataset? 5.10.2022 - no they do, just having 180 problems
intersectionz_new <- st_intersects(x=benthicfish_latlong_shp_new, y=melanesia_allreefsWGS84) #5.10.2022 - more of these match! still a few that fall outside
#melanesia_allreefsWGS84 or allreefs_WGS84 both give functionally the same thing (the numbers are the rows of y)
#prepared = TRUE maybe not needed? #sparse = FALSE gave a boolean matrix which was annoying

####new (5.10.2022) - with intersectionz_new only two of them didn't match, but used QGIS to figure out what grid cell they were closest to; [[7]] is closest to FID = 48540 and [[52]] is closest to FID = 47881 (add below)

#the elements of intersectionz give the row of the melanesia dataset (i.e. the polygon) associated with each reef in the benthicfish dataset
benthicfish_masterdataset$sediment_orig_new <- NA
for(i in 1:(dim(benthicfish_masterdataset)[1])){
  if(length(intersectionz_new[[i]]) > 0){
    benthicfish_masterdataset$sediment_orig_new[i] <- melanesia_allreefsWGS84$sedimnt[intersectionz_new[[i]]]  
  }
}

hist(benthicfish_masterdataset$sediment_orig_new) #not normally distributed, focus on the median

#add values for [[7]] and [[52]]
benthicfish_masterdataset$sediment_orig_new[7] <- melanesia_allreefsWGS84$sedimnt[melanesia_allreefsWGS84$OBJECTI == 48540]
benthicfish_masterdataset$sediment_orig_new[52] <- melanesia_allreefsWGS84$sedimnt[melanesia_allreefsWGS84$OBJECTI == 47881]

hist(benthicfish_masterdataset$sediment_orig_new)

#want these values to be centred at 0.24, set median = 0.24
goalmed <- 0.24
sediment_scale <- goalmed/median(benthicfish_masterdataset$sediment_orig_new)
#scale the sediment values
benthicfish_masterdataset$sediment_alt <- sediment_scale*benthicfish_masterdataset$sediment_orig_new
#sediment_alt is now the new mortality level for the reefs

hist(benthicfish_masterdataset$sediment_alt) #looks roughly the same 

#i think the 0's are causing errors, need to replace them with some other value. maybe 0.02 as that's quite a bit lower than the rest of the values? [stolen from Fung et al 2011]
benthicfish_masterdataset$sediment_altalt <- benthicfish_masterdataset$sediment_alt
benthicfish_masterdataset$sediment_altalt[which(benthicfish_masterdataset$sediment_altalt == 0)] <- 0.02

hist(benthicfish_masterdataset$sediment_altalt)

#save in a new dataframe
benthicfish_masterdataset_sed <- benthicfish_masterdataset
save(benthicfish_masterdataset_sed, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sedimentlvls_5.6.2023.RData")

#5.2023: didn't redo this because nothing changed
#plotting sediment levels 
worldmap <- map_data ("world", wrap = c(0, 360))

NatCoralMortality_wzeros <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Natural Coral Mortality Rates (Incl Zeros)"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, color = sediment_alt))+
  scale_color_viridis(discrete = FALSE, name = "Natural Coral Mortality Rate")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(NatCoralMortality_wzeros, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/BaseSedimentScenario4.2022/NatCoralMortality_wzeros_180fixed.png"), bg = "transparent", height = 10, width = 10)

#5.2023: didn't redo this because nothing changed
NatCoralMortality_nozeros <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Sediment Levels (No Zeros)"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, color = sediment_altalt))+
  scale_color_viridis(discrete = FALSE, name = "Natural Coral Mortality Rate")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(NatCoralMortality_nozeros, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/BaseSedimentScenario4.2022/NatCoralMortality_nozeros_180fixed.png"), bg = "transparent", height = 10, width = 10)


###Part 2 - play with buffers? (maybe use: https://gis.stackexchange.com/questions/394954/r-using-st-intersects-to-classify-points-inside-outside-and-within-a-buffer)

#import the data that Yash sent me - fixed province/island/district designations and fixed mgmt levels and qoliqoli IDs
updated_sitedata <- read.csv(file = "~/Dropbox/University of Toronto/Research Related/Chapter3_Round2/tabu_data/2022April24WCSsite.for.Ariel.csv")
#names(updated_sitedata)
#names(benthicfish_masterdataset_sed)
#make the updated_sitedata factor columns into characters
updated_sitedata$site <- as.character(updated_sitedata$site)
updated_sitedata$Island <- as.character(updated_sitedata$Island)
updated_sitedata$District <- as.character(updated_sitedata$District)
updated_sitedata$province <- as.character(updated_sitedata$province)
updated_sitedata$management <- as.character(updated_sitedata$management)
updated_sitedata$mgmt_abr <- as.character(updated_sitedata$mgmt_abr)
updated_sitedata$Qoliqoli_id <- as.character(updated_sitedata$Qoliqoli_id)

#i think that all of the sites are unique and the order should be the same too - yes
#identical(updated_sitedata$site, benthicfish_masterdataset_sed$site) #TRUE
benthicfish_masterdataset_sed$Island <- updated_sitedata$Island
benthicfish_masterdataset_sed$District <- updated_sitedata$District
benthicfish_masterdataset_sed$province <- updated_sitedata$province
benthicfish_masterdataset_sed$management <- updated_sitedata$management
benthicfish_masterdataset_sed$mgmt_abr <- updated_sitedata$mgmt_abr
benthicfish_masterdataset_sed$Qoliqoli_id <- updated_sitedata$Qoliqoli_id

#check that the management and mgmt_abr columns match up properly #yes they do, ty Yash
#benthicfish_masterdataset_sed %>%
#  select(management, mgmt_abr)
save(benthicfish_masterdataset_sed, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_updatedmgmtlevels_5.6.2023.RData") #used to be 5.10.2022

#5.2023: didn't redo this because nothing changed
Qoliqoli_mgmtlevels <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("QoliQoli + Management Levels"))+
  geom_point(data = benthicfish_masterdataset_sed, aes(x = long.recenter, y = latitude, color = as.factor(Qoliqoli_id), fill = as.factor(Qoliqoli_id), shape = as.factor(mgmt_abr)))+
  scale_shape_manual(values=c(19, 2, 1))+ #want TABU to be 1 and MPA to be 2
  scale_color_viridis(discrete = TRUE, name = "Management Levels")+
  guides(fill = "none")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(Qoliqoli_mgmtlevels, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/BaseSedimentScenario4.2022/Qoliqoli_mgmtlevels.png"), bg = "transparent", height = 10, width = 10)

#do the tabu/mpa reefs have a higher mean grazing levels than the LMMA ones? yes, good
mean(benthicfish_masterdataset_sed$comb_grazinglevel[benthicfish_masterdataset_sed$mgmt_abr == "LMMA"]) #0.05466122
mean(benthicfish_masterdataset_sed$comb_grazinglevel[benthicfish_masterdataset_sed$mgmt_abr != "LMMA"]) #0.0818385
median(benthicfish_masterdataset_sed$comb_grazinglevel[benthicfish_masterdataset_sed$mgmt_abr == "LMMA"]) #0.03759298
median(benthicfish_masterdataset_sed$comb_grazinglevel[benthicfish_masterdataset_sed$mgmt_abr != "LMMA"]) #0.07410976

#want to draw a buffer around the tabu reefs
#using the projected benthicfish points dataset to start (code below copied from above)
#subset benthicfish_masterdataset -> lat/long only dataframe
#benthicfish_masterdataset_latlong <- benthicfish_masterdataset[,c(6,4)] #want long.recenter to be first
#names(benthicfish_masterdataset_latlong) <- c("longitude","latitude") #reset the names
#benthicfish_latlong_shp <- st_as_sf(x = benthicfish_masterdataset_latlong, 
#                                    coords = c("longitude", "latitude"),
#                                    crs = st_crs(4326)) #just used the EPSG code directly

#make a new shape file that includes more info than just lat/long
benthicfish_masterdataset_latlong_extra <- benthicfish_masterdataset_sed[,c(6,4,26,27)] #want long.recenter to be first
names(benthicfish_masterdataset_latlong_extra) <- c("longitude","latitude","mgmt_abr","Qoliqoli_id") #reset the names
benthicfish_latlong_shp_extra <- st_as_sf(x = benthicfish_masterdataset_latlong_extra, 
                                          coords = c("longitude", "latitude"),
                                          crs = st_crs(4326)) #just used the EPSG code directly

#5.2023: didn't redo this because nothing will change about this shape file
st_write(benthicfish_latlong_shp_extra, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_latlong_mgmtinfo.shp") #5.11.2022: MAYBE REMAKE WITH LONGITUDE INSTEAD OF LONG.RECENTER FOR QGIS PURPOSES? 

#11.22.2022 - making one with longitude instead of long.recenter - actually this sort of sucks for QGIS plots lol
#benthicfish_masterdataset_latlong_extra_two <- benthicfish_masterdataset_sed[,c(5,4,26,27)] #want longitude to be first
#names(benthicfish_masterdataset_latlong_extra_two) <- c("longitude","latitude","mgmt_abr","Qoliqoli_id") #reset the names
#benthicfish_latlong_shp_extra_two <- st_as_sf(x = benthicfish_masterdataset_latlong_extra_two, 
#                                          coords = c("longitude", "latitude"),
#                                          crs = st_crs(4326)) #just used the EPSG code directly
#st_write(benthicfish_latlong_shp_extra_two, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_latlong_mgmtinfo_longitudeversion.shp")

#need to make benthicfish_masterdataset_latlong and benthicfish_latlong_shp with long.recenter because removed from above
#later could transpose all of this to longitude but i dont think that that rly makes sense
benthicfish_masterdataset_latlong <- benthicfish_masterdataset[,c(6,4)] #want long.recenter to be first
names(benthicfish_masterdataset_latlong) <- c("longitude","latitude") #reset the names
benthicfish_latlong_shp <- st_as_sf(x = benthicfish_masterdataset_latlong, 
                                    coords = c("longitude", "latitude"),
                                    crs = st_crs(4326))

#which reefs are tabu reefs
tabureefs <- which(benthicfish_masterdataset_sed$mgmt_abr != "LMMA")
tabureefs_latlong <- benthicfish_masterdataset_latlong[tabureefs,]
tabureefs_latlong_shp <- st_as_sf(x = tabureefs_latlong, 
                                  coords = c("longitude", "latitude"),
                                  crs = st_crs(4326))

#this projection is in lat/long, 1 degree = 111km, 0.05 degrees ~=~ 5km, 0.02 seemed like the best choice (see notes from 4.25.2022)
#initialsize = 0.02

tabureefs_latlong_shp_buffer = st_buffer(tabureefs_latlong_shp, 0.05) #for qoliqoli run 0.02, for 5km run 0.05
#st_write(tabureefs_latlong_shp_buffer, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/tabureefs_latlong_shp_buffer.shp") #moved to 'old qgis things' folder

#11.22.2022 - making datafiles for qgis to make figures for supp mat 
#tabureefs_latlong_shp_buffer_one = st_buffer(tabureefs_latlong_shp, 0.01) #0.05
#st_write(tabureefs_latlong_shp_buffer_one, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/tabureefs_latlong_shp_buffer_1km.shp")
#tabureefs_latlong_shp_buffer_two = st_buffer(tabureefs_latlong_shp, 0.02) #0.05
#st_write(tabureefs_latlong_shp_buffer_two, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/tabureefs_latlong_shp_buffer_2km.shp")
#tabureefs_latlong_shp_buffer_five = st_buffer(tabureefs_latlong_shp, 0.05) #0.05
#st_write(tabureefs_latlong_shp_buffer_five, "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/tabureefs_latlong_shp_buffer_5km.shp")

#now need to make note of which reefs fall within these larger tabus
tabuintersectionz <- st_intersects(x=benthicfish_latlong_shp, y=tabureefs_latlong_shp_buffer) 

#the elements of tabuintersectionz give the row of the tabureefs_latlong_shp_buffer dataset (i.e. the polygon) associated with each reef in the benthicfish dataset
#want the new grazing level for each reef, which is either - the same as before (if not in a tabu buffer, or if the tabu in question) or different (corresponding to the tabu buffer it's in or the mean of the tabu buffers it's in (if not in the same qoliqoli as either, if in the same qoliqoli as one but not the other take the qoliqoli value))
#step 1: figure out which reefs fall in the buffers of which tabus - convert the #s in tabuintersectionz to the tabureefs values
tabuintersectionz_tabureefsvalues <- list()
for(i in 1:(dim(benthicfish_masterdataset_sed)[1])){
  if(length(tabuintersectionz[[i]]) > 0){
    tabuintersectionz_tabureefsvalues[[i]] <- tabureefs[tabuintersectionz[[i]]] #re-code the values
  }else{
    tabuintersectionz_tabureefsvalues[[i]] <- i #going to take the grazing level of the original reef if not in a buffer, doing this so that the list positions correspond (might not need it)
  }
}

#checking that there are the same or more reefs in the 0.05 tabus as are in the 0.02 tabus...can confirm
#tabuintersectionz_tabureefsvalues_0.05 <- tabuintersectionz_tabureefsvalues

#step 2: set a new grazing level based on the tabu boundaries
benthicfish_masterdataset_sed$new_grazinglevel <- NA
#note: every reef is in only one QoliQoli, but multiple tabu reefs can be in the same qoli qoli
addd <- NA
for(i in 1:(dim(benthicfish_masterdataset_sed)[1])){
  #if reef i is in multiple buffers
  if(length(tabuintersectionz_tabureefsvalues[[i]]) > 1){
    addd <- rep(NA,length(tabuintersectionz_tabureefsvalues[[i]]))
    for(j in 1:length(tabuintersectionz_tabureefsvalues[[i]])){
      #if in the same qoliqoli as a buffer, take that value + record which j
      if((benthicfish_masterdataset_sed$Qoliqoli_id[i] == benthicfish_masterdataset_sed$Qoliqoli_id[tabuintersectionz_tabureefsvalues[[i]]][j])){
        benthicfish_masterdataset_sed$new_grazinglevel[i] <- benthicfish_masterdataset_sed$comb_grazinglevel[tabuintersectionz_tabureefsvalues[[i]]][j] #dont need this line
        addd[j] <- 1
      }
    }#if reef i is in multiple buffers that are all in the same qoli qoli as reef i, take the mean of all of those tabus; also using this as the method if just share a qoliqoli with 1 buffer
    if(sum(addd,na.rm = T) > 0){ #sum(c(NA,NA,NA),na.rm = T) = 0
      benthicfish_masterdataset_sed$new_grazinglevel[i] <- mean(benthicfish_masterdataset_sed$comb_grazinglevel[tabuintersectionz_tabureefsvalues[[i]]]*addd, na.rm = T)
    }
    if(is.na(benthicfish_masterdataset_sed$new_grazinglevel[i])){ #if not in the same qoliqoli as any of the buffer reefs, take the mean of all of them
      benthicfish_masterdataset_sed$new_grazinglevel[i] <- mean(benthicfish_masterdataset_sed$comb_grazinglevel[tabuintersectionz_tabureefsvalues[[i]]])  
    }
  }
  #if just of length 1 (either bc just in 1 buffer or in 0 buffers)
  if(length(tabuintersectionz_tabureefsvalues[[i]]) == 1){ 
    benthicfish_masterdataset_sed$new_grazinglevel[i] <- benthicfish_masterdataset_sed$comb_grazinglevel[tabuintersectionz_tabureefsvalues[[i]]]
  }
}

#save(benthicfish_masterdataset_sed, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_5.10.2022.RData") #old: 4.26.2022

save(benthicfish_masterdataset_sed, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.05_5.6.2023.RData")

#step 2 (qoli qoli only version): 
benthicfish_masterdataset_sed$new_grazinglevel <- NA
#note: every reef is in only one QoliQoli, but multiple tabu reefs can be in the same qoli qoli
addd <- NA
for(i in 1:(dim(benthicfish_masterdataset_sed)[1])){
  #if reef i is in multiple buffers
  if(length(tabuintersectionz_tabureefsvalues[[i]]) > 1){
    addd <- rep(NA,length(tabuintersectionz_tabureefsvalues[[i]]))
    for(j in 1:length(tabuintersectionz_tabureefsvalues[[i]])){
      #if in the same qoliqoli as a buffer, take that value + record which j
      if((benthicfish_masterdataset_sed$Qoliqoli_id[i] == benthicfish_masterdataset_sed$Qoliqoli_id[tabuintersectionz_tabureefsvalues[[i]]][j])){
        benthicfish_masterdataset_sed$new_grazinglevel[i] <- benthicfish_masterdataset_sed$comb_grazinglevel[tabuintersectionz_tabureefsvalues[[i]]][j] 
        addd[j] <- 1
      }
    }#if reef i is in multiple buffers that are all in the same qoli qoli as reef i, take the mean of all of those tabus; also using this as the method if just share a qoliqoli with 1 buffer
    if(sum(addd,na.rm = T) > 0){ #sum(c(NA,NA,NA),na.rm = T) = 0
      benthicfish_masterdataset_sed$new_grazinglevel[i] <- mean(benthicfish_masterdataset_sed$comb_grazinglevel[tabuintersectionz_tabureefsvalues[[i]]]*addd, na.rm = T)
    }
    if(is.na(benthicfish_masterdataset_sed$new_grazinglevel[i])){ #if not in the same qoliqoli as any of the buffer reefs, get the old grazing level
      benthicfish_masterdataset_sed$new_grazinglevel[i] <- benthicfish_masterdataset_sed$comb_grazinglevel[i]
    }
  }
  #if just of length 1 (either bc just in 1 buffer or in 0 buffers)
  if(length(tabuintersectionz_tabureefsvalues[[i]]) == 1){
    #if reef i is in 1 buffer that happens to be of a reef in the same qoli qoli, have it take the grazing value of that qoli qoli
    if((benthicfish_masterdataset_sed$Qoliqoli_id[i] == benthicfish_masterdataset_sed$Qoliqoli_id[tabuintersectionz_tabureefsvalues[[i]]])){ 
      benthicfish_masterdataset_sed$new_grazinglevel[i] <- benthicfish_masterdataset_sed$comb_grazinglevel[tabuintersectionz_tabureefsvalues[[i]]]
    }else{ #if reef i is in 1 buffer that is of a reef that is not in the same qoli qoli and/or in 0 buffers, take the old grazing level
      benthicfish_masterdataset_sed$new_grazinglevel[i] <- benthicfish_masterdataset_sed$comb_grazinglevel[i]
    }
  }
}

save(benthicfish_masterdataset_sed, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_5.6.2023.RData")

#oldvals <- benthicfish_masterdataset_sed$new_grazinglevel

#dummmy <- c(NA,1,1)
#dumm_my <- c(2,3,4)
#mean(dummmy*dumm_my, na.rm = T)
#checking things
#tabureefs
#[1]  3  6  8  9 26 28 33 39 41 42 46 47 49 50 55 63 64 66 67 73 75
#40,41,42; 48,49,50; 67,72,73,75 all got set properly

#benthicfish_masterdataset_sed %>%
# select(site, comb_grazinglevel, new_grazinglevel)

#benthicfish_masterdataset_sed %>%
# select(Qoliqoli_id)


