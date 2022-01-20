library(dplyr)
library(Matrix)
library(fields) 
library(igraph)
library(ggplot2)
library(viridis)
#trying to weighted average my way to something that looks like what Wood et al. 2014 did

#using the 0.001 degree radius matrices, e.g.
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.001deg/Cmatrices_2009_1.RData") #0.001 degree radius

#load in all of the matrices made in MakingConnMat_11.2021.R

#want 1 matrix, combo of all PLDs
mixed_sum_mat_allyrs <- matrix(0,nrow=559,ncol=559)
settle <- rep(NA,26) #fill in the weighted averages here
s <- rep(NA, 26) #fill in the S_t values here
#load in the summed matrices for each PLD across all years, for the 0.001 degree radius buffer
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Sum.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Avg.RData")
#^ these two should be identical

s[2] <- 1 #set S_10 = 1, S_5 = 0
for(i in 2:26){ #start from a PLD of 10, when Wood et al. 2014 say that all of the larvae are competent
  PLD <- i*5
  #S_15 – S_10 where S_15 = S_10*e^(-5 λ) 
  s[i+1] <- s[i]*exp(-PLD*(log(2)/35))
  settle[i] <- s[i+1] - s[i]
  mixed_sum_mat_allyrs <- mixed_sum_mat_allyrs + Conn_Mat_Sum[[i]]*settle[i]
}
#take the weighted average
weightedavgconnmat <- mixed_sum_mat_allyrs/sum(settle[2:26])
save(weightedavgconnmat, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/weightedavgconnmat.RData")

#make a sparse version
library(Matrix)
weightedavgconnmat_sum_sparse <- Matrix(weightedconnmat, sparse = T)

save(weightedavgconnmat_sum_sparse, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/weightedavgconnmat_sum_sparse.RData")


#let's plot some shit
library(fields)
png(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/matrix_mixedPLD.png"))
image.plot(as.matrix(weightedavgconnmat_sum_sparse), col = c("gray", tim.colors(300)), xlab = "Source Reef", ylab = "Receiving Reef", main = paste("Mixed PLD")) #Receiving Reef = x axis for the Wood et al matrix
dev.off()

#see what some maps look like?
library(igraph)
g <- graph.adjacency(as.matrix(weightedconnmat), weighted = TRUE) #8 = 40 day PLD, 12 = 60 day PLD, 26 = 130 day PLD
#clusters(g,mode="weak") one really big cluster, lots of smaller ones
coordinates <- read.csv("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/greiner_coordinatesfromalldata_10.19.2021.csv") #these are the ones that I sent to Marco, and thus are the un-shifted ones
coordinates$networks <- clusters(g,mode="weak")$membership
print(paste("mixed PLD, Number of networks = ", clusters(g,mode="weak")$no))
print(clusters(g,mode="weak")$csize)

# shift coordinates to recenter worldmap
#library(maps)
#library(rgdal)
library(ggplot2)
library(viridis)
worldmap <- map_data ("world", wrap = c(0, 360))

weightedavgpldnetworks_20092018conn <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Networks, Mixed PLD, 2009-2018 Conn"))+
  geom_point(data = coordinates, aes(x = long.recenter, y = latitude, color = networks))+
  scale_color_viridis(discrete = FALSE, name = "Network #")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(weightedavgpldnetworks_20092018conn, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/networkmap_weightedaveragePLD.png"), bg = "transparent", height = 10, width = 10)

#strong networks?
coordinates$strongnetworks <- clusters(g,mode="strong")$membership
print(clusters(g,mode="strong")$csize) #one network with 502 reefs, a bunch with just 1 reef

weightedavgpld_strongnetworks_20092018conn <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Strong Networks, Mixed PLD, 2009-2018 Conn"))+
  geom_point(data = coordinates, aes(x = long.recenter, y = latitude, color = strongnetworks))+
  scale_color_viridis(discrete = FALSE, name = "Network #")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(weightedavgpld_strongnetworks_20092018conn, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/strongnetworkmap_weightedaveragePLD.png"), bg = "transparent", height = 10, width = 10)

#Load in the dataset with the 77 sites and the fish density + coral/macroalgal cover 
#maybe do a merge based on the latitude and long.recenter? because can't regenerate the connectivity matrix based on fewer reefs, that will change the connectivity (i think??)
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged.RData") #generated in Fiji_status/Fish Belt Vis/2021_fishbelt_thesisproject/FishBelt_generatefishoverviewmaps.Rmd

coordinates_abr <- coordinates %>%
  select(X, site, latitude, long.recenter, networks, strongnetworks) %>%
  group_by(site) %>% #getting rid of the 3 duplicate sites in the coordinates dataframe that are also in the fishbenthic joint dataset (NAOBF3, NAOBT1, NAOBT3)
  slice(1)
 # mutate(lat_long.r = paste0(latitude,"_",long.recenter))

#how many sites are duplicates in 'coordinates' #just 7
#coordinates %>%
#  count(site) %>%
#  filter(n>1)
dupsites <- c("NAOBF2","NAOBF3", "NAOBF4", "NAOBT1", "NAOBT2", "NAOBT3", "VIR1")


#finding how many rows have the same lat-long #there are 8 duplicates! which are the two sites with the same coordinates, since there are only 7 duplicate sites in the database
coordinates %>%
  mutate(lat_long.r = paste0(latitude,"_",long.recenter)) %>%
  filter(!(site %in% dupsites)) %>%
  count(lat_long.r) %>%
  filter(n>1) 
# -17.3741_179.4217 <- none of the dupsites have these coordinates

coordinates %>%
  mutate(lat_long.r = paste0(latitude,"_",long.recenter)) %>%
  filter(lat_long.r == "-17.3741_179.4217") #NS3 and NT1



#wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged <- wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged %>%
#  mutate(lat_long.r = paste0(latitude,"_",long.recenter))


masterdataframe_20172020_wcsonly_fishbenthic <- merge(wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged, coordinates_abr, by = c("latitude", "long.recenter", "site"))

#why are there 80 rows in ^? seems like it's because there were 2 NAOBF3, NAOBT1, NAOBT3 sites in the list of sites that I gave Marco...
#masterdataframe_20172020_wcsonly_fishbenthic %>%
 # count(unique_ID_yr) %>%
 # filter(n>1)
#NAOBF3_2019 2
#NAOBT1_2019 2
#NAOBT3_2019 2

#masterdataframe_20172020_wcsonly_fishbenthic %>%
#  filter(unique_ID_yr == "NAOBT3_2019")
#coordinates_abr %>%
#  filter(site == "NAOBT1")

wcsonly_fishbenthic_20172020_weightedavgpld_strongnetworks <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Strong Networks, Mixed PLD, WCS Only, 20172020, FishBenthicSites"))+
  geom_point(data = masterdataframe_20172020_wcsonly_fishbenthic, aes(x = long.recenter, y = latitude, color = strongnetworks))+
  scale_color_viridis(discrete = FALSE, name = "Network #")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(wcsonly_fishbenthic_20172020_weightedavgpld_strongnetworks, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/wcsonly_fishbenthic_20172020_weightedavgpld_strongnetworks.png"), bg = "transparent", height = 10, width = 10)

wcsonly_fishbenthic_20172020_weightedavgpld_weaknetworks <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Weak Networks, Mixed PLD, WCS Only, 20172020, FishBenthicSites"))+
  geom_point(data = masterdataframe_20172020_wcsonly_fishbenthic, aes(x = long.recenter, y = latitude, color = networks))+
  scale_color_viridis(discrete = FALSE, name = "Network #")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(wcsonly_fishbenthic_20172020_weightedavgpld_weaknetworks, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/wcsonly_fishbenthic_20172020_weightedavgpld_weaknetworks.png"), bg = "transparent", height = 10, width = 10)

  



