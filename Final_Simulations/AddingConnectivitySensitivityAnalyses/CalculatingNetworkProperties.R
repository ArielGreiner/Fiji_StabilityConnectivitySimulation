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
library(igraph)

setwd("~/Dropbox/Github/PhDProjects/Fiji_StabilityConnectivitySimulation/")

#removing this because realized needed to calculate the properties of the full network
#sitevector <- readRDS(file = paste0("Final_Simulations/sitevector_7.15.2024.rds")) #saving this because im tired of re-calculating it


#check some global network properties: how many networks, avg degree of nodes in the network, average shortest path length, diameter, modularity

#load in the original weighted connectivity matrix
load("BaseSimulationWork_1.2022/weightedavgconnmat.RData") 
#^weightedavgconnmat - for all sites
#subset it to only the 76 sites with both benthic cover and fish abundance data #NEED TO FIND COORDINATES BELOW
load("BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData") #newcoordinates; abridged coordinate file that corresponds with conn mat above, site order-wise
#coordinates <- newcoordinates
#jointsite_rows <- which(coordinates$site %in% sitevector)
#jointsite_coral_weightedavgconnmat <- weightedavgconnmat[jointsite_rows, jointsite_rows]

#make into a graph object
#weightedPLD_g <- graph.adjacency(as.matrix(jointsite_coral_weightedavgconnmat), weighted = TRUE)
full_weightedPLD_g <- graph.adjacency(as.matrix(weightedavgconnmat), weighted = TRUE)

#put all of the properties into this dataframe
graphs_properties <- data.frame(PLD = c("weighted",seq(10,130,5)), numweakclusters = NA, numstrongclusters = NA, numlarge_weakclusters = NA, numlarge_strongclusters = NA, avg_degree = NA, avg_shortestdistance = NA, diametr = NA)


#check properties
graphs_properties$numweakclusters[graphs_properties$PLD == "weighted"] <- clusters(full_weightedPLD_g,mode="weak")$no #1 network
graphs_properties$numstrongclusters[graphs_properties$PLD == "weighted"] <- clusters(full_weightedPLD_g, mode = "strong")$no #58 networks, most of which are 1-reef site large (except for one which has 494 reef sites)
graphs_properties$numlarge_weakclusters[graphs_properties$PLD == "weighted"] <- length(which(clusters(full_weightedPLD_g,mode="weak")$csize > 2))
graphs_properties$numlarge_strongclusters[graphs_properties$PLD == "weighted"] <- length(which(clusters(full_weightedPLD_g,mode="strong")$csize > 2))


#average shortest path lengths 
graphs_properties$avg_shortestdistance[graphs_properties$PLD == "weighted"] <- mean_distance(full_weightedPLD_g, directed = TRUE) #1.702294e-06
#diameter
graphs_properties$diametr[graphs_properties$PLD == "weighted"] <- diameter(full_weightedPLD_g,directed = TRUE,weights =E(full_weightedPLD_g)$weight) #6.412475e-05
#average degree
graphs_properties$avg_degree[graphs_properties$PLD == "weighted"] <- mean(degree(full_weightedPLD_g)) 


#Check the single-PLD connectivity matrices (PLD 10 and higher)
load("BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
singlePLD_coralconnmat <- list()
#jointsite_coralsinglePLDconnmat <- list()
singlePLD_graphs <- list()

for(i in 1:25){
  singlePLD_coralconnmat[[i]] <- Conn_Mat_Sum[[(i+1)]]
  #jointsite_coralsinglePLDconnmat[[i]] <- singlePLD_coralconnmat[[i]][jointsite_rows, jointsite_rows]
  singlePLD_graphs[[i]] <- graph.adjacency(as.matrix(singlePLD_coralconnmat[[i]]), weighted = TRUE)
  graphs_properties$numweakclusters[graphs_properties$PLD == ((i+1)*5)] <- clusters(singlePLD_graphs[[i]],mode="weak")$no 
  graphs_properties$numstrongclusters[graphs_properties$PLD == ((i+1)*5)] <- clusters(singlePLD_graphs[[i]], mode = "strong")$no
  graphs_properties$numlarge_weakclusters[graphs_properties$PLD == ((i+1)*5)] <- length(which(clusters(singlePLD_graphs[[i]],mode="weak")$csize > 2))
  graphs_properties$numlarge_strongclusters[graphs_properties$PLD == ((i+1)*5)] <- length(which(clusters(singlePLD_graphs[[i]],mode="strong")$csize > 2))
  
  #average shortest path lengths
  graphs_properties$avg_shortestdistance[graphs_properties$PLD == ((i+1)*5)] <- mean_distance(singlePLD_graphs[[i]], directed = TRUE) 
  #diameter
  graphs_properties$diametr[graphs_properties$PLD == ((i+1)*5)] <- diameter(singlePLD_graphs[[i]],directed = TRUE,weights =E(singlePLD_graphs[[i]])$weight)
  #average degree
  graphs_properties$avg_degree[graphs_properties$PLD == ((i+1)*5)] <- mean(degree(singlePLD_graphs[[i]])) 
  
}


