library(dplyr)
library(Matrix)
library(sp)
library(fields) 
library(igraph)
library(ggplot2)
library(viridis)

#This will basically be a rehash of MakingConnMat_11.2021.R and MakingMixedConnMat_12.2021, just starting off by removing the duplicate sites. 
dupsites <- c("NAOBF2","NAOBF3", "NAOBF4", "NAOBT1", "NAOBT2", "NAOBT3", "VIR1", "NS3")
#Will leave NT1 and NS3 for now, since they are two separate sites even if they have the exact same latitude/longitude coordinates
#Yash changed the info for them on mermaid, NS3 had the wrong coordinates...Marco would need to redo the whole thing (potentially) to change this coordinate tho so at the moment probably best to just remove NS3 from the analysis (correct coords: NT1 - Lat  (-17.37439), Long(179.42183); NS3 - Lat (-17.33784), Long(179.44673))
#NS3 does show up!!! should i remove it??? probably??
#1.21.2022: removing NS3

#do they have the same site ordering?
#note: greiner_coordinatesfromalldata_10.19.2021 is the file that I sent Marco that has the original coordinates and he sent me back greiner_coordinatesfromalldata_11.17.2021_MA which has the shifted coordinates that he used to perform the simulation (he used the original coordinates from the 10.19.2021 file to determine the landing points), the site ordering in both of these should be the same
coordinates <- read.csv("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/greiner_coordinatesfromalldata_11.17.2021_MA.csv")
coordinates_alt <- read.csv("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/greiner_coordinatesfromalldata_10.19.2021.csv")
setequal(coordinates$site, coordinates_alt$site) #TRUE, so yes...they do have the same site ordering

#at what row are the dupsites located in
dupcoords <- which(coordinates$site %in% dupsites) #302 303 304 305 306 307 308 309 310 311 312 313 340 429 430
#need to only remove one of each duplicate
coordinates[dupcoords,] #could remove 303, 305, 307, 309, 311, 313, 340, 430 
remov_coords <- c(303,305,307,309,311,313,340,430)
newcoordinates <- coordinates_alt[-remov_coords,]
#save new coordinates files, using the 10.19.2021 coordinates since those aren't shifted
save(newcoordinates, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData")


#how do i remove just those rows and columns from a matrix?
dummy <- matrix(data = seq(1,64), nrow = 8, ncol = 8)
remov_rows <- c(2,4,6)
dummy[-remov_rows, -remov_rows] #yes this works

########from MakingConnMat_11.2021.R
#stole from the 'CreatingConnMat.R' file from the 'Sally Wood Connectivity Matrix' folder in dropbox

#what dimension are the matrices that Marco sent for each month?
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.001deg/Cmatrices_2009_1.RData") #0.001 degree radius
#str(Cmatrices[[2]][-remov_coords, -remov_coords]) #this seems to work here as well, good


#note: moved the 0.01 degree ones into a 0.01 degree folder within 'ConnectivityMatrices_11.2021', so i don't have to change everything below. will move the 0.001 degree ones into a folder later if play with other buffers later

#create lists, want 1 matrix for each PLD #later might split based on year?
sum_mat_allyrs <- sum_mat_avg_allyrs <- list()
#sum_mat_sepyrs <- sum_mat_avg_sepyrs <- list()

for(i in 1:26){
  PLD <- i*5
  #create empty matrices
  sum_mat_allyrs[[i]] <- matrix(0,nrow=(559-length(dupsites)),ncol=(559-length(dupsites)))
  sum_mat_avg_allyrs[[i]] <- matrix(0,nrow = (559-length(dupsites)), ncol = (559-length(dupsites)))
  
  #2009 - 2018 (all 12 months)
  for(year in 2009:2018){ 
    for(month in 1:12){ 
      #load(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.01degreeradius/Cmatrices_",year,"_",month,".RData"))
      load(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.001deg/Cmatrices_",year,"_",month,".RData"))
      #a <- readMat(Cmatrices[[(i+1)]]) #it's not this type of matrix?
      a <- Cmatrices[[(i+1)]] #because Cmatrices[[1]] is PLD = 0
      #remove rows and columns corresponding to the dupsites
      a <- a[-remov_coords, -remov_coords]
      sum_mat_allyrs[[i]] <- sum_mat_allyrs[[i]] + a
      sum_mat_avg_allyrs[[i]] <- sum_mat_avg_allyrs[[i]] + (a)/100
      print(paste("PLD = ", PLD, "year = ",year,"month = ", month))
      flush.console() #prints as executing, takes less time
      Cmatrices <- NULL #clear it every time, to make sure it doesn't load in twice
    }}
}
save(sum_mat_allyrs, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/sum_mat_allyrs.RData")
save(sum_mat_avg_allyrs, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/sum_mat_avg_allyrs.RData")


load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/sum_mat_allyrs.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/sum_mat_avg_allyrs.RData")
#120 = the number of matrices being summed both times (10 yrs x 12 months)
#Conn_Mat_Sum = add all of the matrices together, then divide every element of the summed matrix by 120*100
#Conn_Mat_Avg = divide every element of every matrix by 100 to turn it into a connectivity matrix (bc otherwise it is just a number of particles that made it b/w two grid cells and not a proportion) and then take the average of all 120 matrices
#might turn out to be the same thing, if not -> use the Sum one because that one should inflate the rare occurrences (as opposed to deflate) which is more accurate since only 100 particles sent out per patch
#they turn out to be the same, pretty sure
Conn_Mat_Sum <- Conn_Mat_Avg <- list()
for(i in 1:26){ #120 for 0.01 degree radius
  Conn_Mat_Sum[[i]] <- sum_mat_allyrs[[i]]/(120*100) #/(120*100)
  Conn_Mat_Avg[[i]] <- sum_mat_avg_allyrs[[i]]/120 #120
}

save(Conn_Mat_Sum, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
save(Conn_Mat_Avg, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Avg.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Avg.RData")

#create a sparse matrix - much smaller size, only works if matrix has lots of 0s because only changes the 0s...can still deal with sparse matrices as normal (add, subtract, etc) <- according to Marco
#save some sparse ones as well
library(Matrix)
Conn_Mat_Sum_sparse <- Conn_Mat_Avg_sparse <- list()
for(i in 1:26){
  Conn_Mat_Sum_sparse[[i]] <- Matrix(Conn_Mat_Sum[[i]], sparse = T)
  Conn_Mat_Avg_sparse[[i]] <- Matrix(Conn_Mat_Avg[[i]], sparse = T)
}

save(Conn_Mat_Sum_sparse, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum_sparse.RData")
save(Conn_Mat_Avg_sparse, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Avg_sparse.RData")

#note: tried recalculating the euclidean distance stuff from these ^ matrices and that one weird thing didn't get less weird...but whatever

####from MakingMixedConnMat_12.2021.R
#trying to weighted average my way to something that looks like what Wood et al. 2014 did

#using the 0.001 degree radius matrices, e.g.
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.001deg/Cmatrices_2009_1.RData") #0.001 degree radius

#load in all of the matrices made above

#want 1 matrix, combo of all PLDs
mixed_sum_mat_allyrs <- matrix(0,nrow=(559-length(dupsites)),ncol=(559-length(dupsites)))
settle <- rep(NA,26) #fill in the weighted averages here
s <- rep(NA, 26) #fill in the S_t values here
#load in the summed matrices for each PLD across all years, for the 0.001 degree radius buffer
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Avg.RData")
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
save(weightedavgconnmat, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/weightedavgconnmat.RData")

#make a sparse version
library(Matrix)
weightedavgconnmat_sum_sparse <- Matrix(weightedavgconnmat, sparse = T)

save(weightedavgconnmat_sum_sparse, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/weightedavgconnmat_sum_sparse.RData")


#let's plot some shit
#library(fields)
png(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/matrix_mixedPLD.png"))
image.plot(as.matrix(weightedavgconnmat_sum_sparse), col = c("gray", tim.colors(300)), xlab = "Source Reef", ylab = "Receiving Reef", main = paste("Mixed PLD")) #Receiving Reef = x axis for the Wood et al matrix
dev.off()

#see what some maps look like?
#library(igraph)
g <- graph.adjacency(as.matrix(weightedavgconnmat), weighted = TRUE) #8 = 40 day PLD, 12 = 60 day PLD, 26 = 130 day PLD
#clusters(g,mode="weak") one really big cluster, lots of smaller ones
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData") 
coordinates <- newcoordinates
coordinates$networks <- clusters(g,mode="weak")$membership
print(paste("mixed PLD, Number of networks = ", clusters(g,mode="weak")$no))
print(clusters(g,mode="weak")$csize)

# shift coordinates to recenter worldmap
#library(maps)
#library(rgdal)
#library(ggplot2)
#library(viridis)
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
ggsave(weightedavgpldnetworks_20092018conn, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/networkmap_weightedaveragePLD.png"), bg = "transparent", height = 10, width = 10)

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
ggsave(weightedavgpld_strongnetworks_20092018conn, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/strongnetworkmap_weightedaveragePLD.png"), bg = "transparent", height = 10, width = 10)

#Load in the dataset with the 76 sites and the fish density + coral/macroalgal cover 
#maybe do a merge based on the latitude and long.recenter? because can't regenerate the connectivity matrix based on fewer reefs, that will change the connectivity (i think??)
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_12.2021/wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged.RData") #generated in Fiji_status/Fish Belt Vis/2021_fishbelt_thesisproject/FishBelt_generatefishoverviewmaps.Rmd

masterdataframe_20172020_wcsonly_fishbenthic <- merge(wcsonly_benthicfish_jointsiteyrsonly_20172020_abridged, coordinates, by = c("latitude", "long.recenter", "site")) #76 rows :)


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
ggsave(wcsonly_fishbenthic_20172020_weightedavgpld_strongnetworks, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/wcsonly_fishbenthic_20172020_weightedavgpld_strongnetworks.png"), bg = "transparent", height = 10, width = 10)

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
ggsave(wcsonly_fishbenthic_20172020_weightedavgpld_weaknetworks, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/wcsonly_fishbenthic_20172020_weightedavgpld_weaknetworks.png"), bg = "transparent", height = 10, width = 10)