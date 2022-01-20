#stole from the 'CreatingConnMat.R' file from the 'Sally Wood Connectivity Matrix' folder in dropbox

#require(R.matlab) #vectors not matrices

#what dimension are the matrices that Marco sent for each month?
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.001deg/Cmatrices_2009_1.RData") #0.001 degree radius
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.01degreeradius/Cmatrices_2009_1.RData") #559 x 559
#Cmatrices[[27]]

#note: moved the 0.01 degree ones into a 0.01 degree folder within 'ConnectivityMatrices_11.2021', so i don't have to change everything below. will move the 0.001 degree ones into a folder later if play with other buffers later

#create lists, want 1 matrix for each PLD #later might split based on year?
sum_mat_allyrs <- sum_mat_avg_allyrs <- list()
#sum_mat_sepyrs <- sum_mat_avg_sepyrs <- list()

for(i in 1:26){
PLD <- i*5
#create empty matrices
sum_mat_allyrs[[i]] <- matrix(0,nrow=559,ncol=559)
sum_mat_avg_allyrs[[i]] <- matrix(0,nrow = 559, ncol = 559)

#2009 - 2018 (all 12 months)
for(year in 2009:2018){ 
  for(month in 1:12){ 
    #load(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.01degreeradius/Cmatrices_",year,"_",month,".RData"))
    load(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Cmatrices_0.001deg/Cmatrices_",year,"_",month,".RData"))
    #a <- readMat(Cmatrices[[(i+1)]]) #it's not this type of matrix?
    a <- Cmatrices[[(i+1)]] #because Cmatrices[[1]] is PLD = 0
    sum_mat_allyrs[[i]] <- sum_mat_allyrs[[i]] + a
    sum_mat_avg_allyrs[[i]] <- sum_mat_avg_allyrs[[i]] + (a)/100
    print(paste("PLD = ", PLD, "year = ",year,"month = ", month))
    flush.console() #prints as executing, takes less time
    Cmatrices <- NULL #clear it every time, to make sure it doesn't load in twice
  }}
}
save(sum_mat_allyrs, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/sum_mat_allyrs.RData")
save(sum_mat_avg_allyrs, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/sum_mat_avg_allyrs.RData")


load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/sum_mat_allyrs.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/sum_mat_avg_allyrs.RData")
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

save(Conn_Mat_Sum, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Sum.RData")
save(Conn_Mat_Avg, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Avg.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Sum.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Avg.RData")

#create a sparse matrix - much smaller size, only works if matrix has lots of 0s because only changes the 0s...can still deal with sparse matrices as normal (add, subtract, etc) <- according to Marco
#save some sparse ones as well
library(Matrix)
Conn_Mat_Sum_sparse <- Conn_Mat_Avg_sparse <- list()
for(i in 1:26){
  Conn_Mat_Sum_sparse[[i]] <- Matrix(Conn_Mat_Sum[[i]], sparse = T)
  Conn_Mat_Avg_sparse[[i]] <- Matrix(Conn_Mat_Avg[[i]], sparse = T)
}

save(Conn_Mat_Sum_sparse, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Sum_sparse.RData")
save(Conn_Mat_Avg_sparse, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Conn_Mat_Avg_sparse.RData")

#lets make some plots + collect some data
for(i in 1:26){
#plot the matrix itself
library(fields)
png(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/matrix_",i*5,"DayPLD.png"))
image.plot(as.matrix(Conn_Mat_Sum_sparse[[i]]), col = c("gray", tim.colors(300)), xlab = "Source Reef", ylab = "Receiving Reef", main = paste(i*5,"Day PLD")) #Receiving Reef = x axis for the Wood et al matrix
dev.off()

#see what some maps look like?
library(igraph)
g <- graph.adjacency(as.matrix(Conn_Mat_Sum[[i]]), weighted = TRUE) #8 = 40 day PLD, 12 = 60 day PLD, 26 = 130 day PLD
#clusters(g,mode="weak") one really big cluster, lots of smaller ones
coordinates <- read.csv("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/greiner_coordinatesfromalldata_10.19.2021.csv") #these coordinates are the shifted ones, maybe use the original one that was sent to Marco
coordinates$networks <- clusters(g,mode="weak")$membership
print(paste("PLD =", i*5," , Number of networks = ", clusters(g,mode="weak")$no))
print(clusters(g,mode="weak")$csize)

# shift coordinates to recenter worldmap
#library(maps)
#library(rgdal)
library(ggplot2)
library(viridis)
worldmap <- map_data ("world", wrap = c(0, 360))

networks_20092018conn <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Networks, PLD = ",i*5,", 2009-2018 Conn"))+
  geom_point(data = coordinates, aes(x = long.recenter, y = latitude, color = networks))+
  scale_color_viridis(discrete = FALSE, name = "Network #")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-20,-15)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 2.5), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(networks_20092018conn, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/networkmap_",i*5,"DayPLD.png"), bg = "transparent", height = 10, width = 10)
}

#well there really aren't many networks are there? let's see what these distances look like
#code from: "O1_Calculate_distances.R" from the folder "Ariel_connectivity_SallyWood50reefEEZWDPA_fromMarco"
library(Matrix)
library(sp)

load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/0.01 degree radius/Conn_Mat_Sum_sparse.RData")
a <- Conn_Mat_Sum_sparse[[6]] #going to use PLD = 30 to begin with since it seems to have the most connections
dim(a)
# 559 reefs

nonzero <- summary(a) # Gives a dataframe with the indices (row number and column number) of the nonzero entries and the value of the entry
head(nonzero)
# These indices counts row starting from 1, so it is convenient for us
# Instead, the internal representation of the sparse matrix objects count row starting from 0. 
# This will be important later, when converting values to sparse matrix


nonzero <- nonzero[,c(1:2)] # We do not need the values so we get rid of them
# A bunch of statistics:
num.nonzero <- dim(nonzero)[1] # There are 102,886 nonzero connections
# The theoretical number of possible connection is 559 * 559 = 312,481, so the connectance is:
num.nonzero / (559^2)
#These connections do not involve all reefs (the code im working from did involve all as starting/destination points)
length(unique(nonzero[,1])) #530 not 559 as starting points, due to the overlap in Marco's sim maybe? #at PLD = 5 and 40
length(unique(nonzero[,2])) #559 act as destination points though

#if all of the reefs were connected, what is the spatial Euclidean distance corresponding to these 312,481 hypothetical connections
#don't use this later, just because want to see what the dispersal kernel itself looks like and this would not show that
coordinates <- read.csv("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/greiner_coordinatesfromalldata_10.19.2021.csv")
coordinates_latlong <- data.frame(latitude = coordinates$latitude, long.recenter = coordinates$long.recenter)
euclid.dist.all <- rep(NA,(559*559))
allconnections <- data.frame(i = rep(seq(1,559),each = 559), j = rep(seq(1,559), 559)) #what if all of the reefs were connected to each other?

for (k in 1 : (559*559) ) {
  origin_reef_coord <- as.matrix(coordinates_latlong[allconnections$i[k],])    # the first argument of spDistsN1 must be a matric
  destination_reef_coord <- as.numeric(coordinates_latlong[allconnections$j[k],]) # the second argument of spDistsN1 must be numeric
  euclid.dist.all[k] <- spDistsN1(origin_reef_coord, destination_reef_coord, longlat=T) # Euclidean distance in km
}

# now we need to calculate the spatial Euclidean distance corresponding to these 102,886 nonzero connections
coordinates <- read.csv("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/greiner_coordinatesfromalldata_10.19.2021.csv")
coordinates_latlong <- data.frame(latitude = coordinates$latitude, long.recenter = coordinates$long.recenter)
euclid.dist <- rep(NA,num.nonzero)
# The following for loop takes about 5 minutes on a decent computer
for (k in 1 : num.nonzero ) {
  if (k %% 10000 == 0) {
    cat(k,"of",num.nonzero,"\n")
    flush.console()
  }
  origin_reef_coord <- as.matrix(coordinates_latlong[nonzero$i[k],])    # the first argument of spDistsN1 must be a matric
  destination_reef_coord <- as.numeric(coordinates_latlong[nonzero$j[k],]) # the second argument of spDistsN1 must be numeric
  euclid.dist[k] <- spDistsN1(origin_reef_coord, destination_reef_coord, longlat=T) # Euclidean distance in km
}

#NEED TO RECALC THESE WHEN REMOVE 0s
euclid.dist.orig <- euclid.dist
euclid.dist <- round(euclid.dist)
hist(euclid.dist)
mean(euclid.dist) #65.00374 for PLD = 30
quantile(euclid.dist)
#0%  25%  50%  75% 100% 
#0   30   57   85  359 

#removing 0s because those are just the duplicates and the self-loops
length(euclid.dist.all[euclid.dist.all == 0]) #575
hist(euclid.dist.all[euclid.dist.all > 0]) 
min(euclid.dist.all[euclid.dist.all > 0]) #0.003465457
max(euclid.dist.all[euclid.dist.all > 0]) #434.5391
euclid.dist.all.nonzero <- euclid.dist.all[euclid.dist.all > 0]
euclid.dist.all.nonzero[order(euclid.dist.all.nonzero)] #570 values less than 1km, probably why changing the buffer changed the number of networks so drastically; 12 reef sites are within 100m of each other (it used to be 575-559 self loops - 7x2 duplicate sites = 2...are two other reef sites exactly on top of each other?)
mean(euclid.dist.all.nonzero) #105.2728
quantile(euclid.dist.all.nonzero)
#0%          25%          50%     75%         100% 
#0.003465457 54.65827 87.17317  149.9236    434.5391  

#this didn't work, maybe because of the starting points/destinations mismatch?? idk
#Error in validObject(.Object) : invalid class “dgTMatrix” object: lengths of slots i and x must match
# Now we create a new sparse matrix using the indices of the nonzero entries and the calculated euclidean distances
euclid.dist.mat.T <- new("dgTMatrix",
                         i = as.integer(nonzero$i - 1),          # We subtract 1 because the internal representation of this class counts rows and columns starting from 0
                         j = as.integer(nonzero$j - 1),
                         x = euclid.dist,
                         Dim = dim(a) )              # Same dimensions as the connectivity matrix a

# Convert it into the dgCMatrix class (more efficient)
euclid.dist.mat <- as(euclid.dist.mat.T, "dgCMatrix")

# Save it
save(euclid.dist.mat,file="~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/ConnectivityMatrices_11.2021/Euclid.dist.mat.559,PLD30.RData")

# Some statistics
hist(euclid.dist.mat@x,breaks=seq(0,8000,40))
length(which(euclid.dist.mat@x>40)) / length(euclid.dist.mat@x)  # Fraction of connections that are > 40 km

