library(dplyr)
library(Matrix)
library(sp)
library(fields) 
library(igraph)
library(ggplot2)
library(viridis)

#stole directly from MixedConnMat_1.2022 and then adding censoring at the bottom

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

#how many larvae are being lost by the full system? as in how many are not landing at any reef in the system?
#the columns should sum to 1 in a closed system, check how far from 1 each of the columns are
#dim(weightedavgconnmat) #551 x 551
notclosed <- rep(NA,dim(weightedavgconnmat)[1])
for(i in 1:dim(weightedavgconnmat)[1]){
  notclosed[i] <- sum(weightedavgconnmat[,i]) 
}
#dim(weightedavgconnmat)[1] - sum(notclosed) #550.4855
#range(notclosed) #0.000000000 0.003459503 
#basically, no column is anywhere close to 1. Super open system


#make a sparse version
library(Matrix)
weightedavgconnmat_sum_sparse <- Matrix(weightedavgconnmat, sparse = T)

save(weightedavgconnmat_sum_sparse, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/weightedavgconnmat_sum_sparse.RData")

####Calculating euclidean distances of this matrix

#well there really aren't many networks are there? let's see what these distances look like
#code from: "O1_Calculate_distances.R" from the folder "Ariel_connectivity_SallyWood50reefEEZWDPA_fromMarco"
library(Matrix)
library(sp)

load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/weightedavgconnmat_sum_sparse.RData")
a <- weightedavgconnmat_sum_sparse 
dim(a)
# 551 reefs

nonzero <- summary(a) # Gives a dataframe with the indices (row number and column number) of the nonzero entries and the value of the entry
head(nonzero)
# These indices counts row starting from 1, so it is convenient for us
# Instead, the internal representation of the sparse matrix objects count row starting from 0. 
# This will be important later, when converting values to sparse matrix


nonzero <- nonzero[,c(1:2)] # We do not need the values so we get rid of them
# A bunch of statistics:
num.nonzero <- dim(nonzero)[1] # There are 35,619 nonzero connections
# The theoretical number of possible connection is 551 * 551 = 303,601, so the connectance is:
num.nonzero / (551^2)
#These connections do not involve all reefs (the code im working from did involve all as starting/destination points)
length(unique(nonzero[,1])) #499 not 551 as starting points, due to the overlap in Marco's sim maybe? 
length(unique(nonzero[,2])) #549 act as destination points though


# now we need to calculate the spatial Euclidean distance corresponding to these 35,619 nonzero connections
#from above: save(newcoordinates, file = "~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData")
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData") #newcoordinates
coordinates <- newcoordinates
coordinates_latlong <- data.frame(latitude = coordinates$latitude, long.recenter = coordinates$long.recenter)
euclid.dist <- rep(NA,num.nonzero)
# The following for loop takes about 1 minute on a decent computer
for (k in 1 : num.nonzero ) {
  if (k %% 10000 == 0) {
    cat(k,"of",num.nonzero,"\n")
    flush.console()
  }
  origin_reef_coord <- as.matrix(coordinates_latlong[nonzero$i[k],])    # the first argument of spDistsN1 must be a matric
  destination_reef_coord <- as.numeric(coordinates_latlong[nonzero$j[k],]) # the second argument of spDistsN1 must be numeric
  euclid.dist[k] <- spDistsN1(origin_reef_coord, destination_reef_coord, longlat=T) # Euclidean distance in km
}


euclid.dist.orig <- euclid.dist
euclid.dist <- round(euclid.dist)
hist(euclid.dist)
mean(euclid.dist) #49.23165
median(euclid.dist) #38
quantile(euclid.dist)
#0%  25%  50%  75% 100% 
#0   19   38   65  420 


#working for this matrix, wasn't working in 'MakingConnMat_11.2021'
# Now we create a new sparse matrix using the indices of the nonzero entries and the calculated euclidean distances
euclid.dist.mat.T <- new("dgTMatrix",
                         i = as.integer(nonzero$i - 1),          # We subtract 1 because the internal representation of this class counts rows and columns starting from 0
                         j = as.integer(nonzero$j - 1),
                         x = euclid.dist,
                         Dim = dim(a) )              # Same dimensions as the connectivity matrix a

# Convert it into the dgCMatrix class (more efficient)
euclid.dist.mat <- as(euclid.dist.mat.T, "dgCMatrix")

# Save it
save(euclid.dist.mat,file="~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Euclid.dist.mat_551.RData")

# Some statistics
hist(euclid.dist.mat@x,breaks=seq(0,500,40))
length(which(euclid.dist.mat@x>42)) / length(euclid.dist.mat@x)  # Fraction of connections that are > 40 km

###Censoring Connectivity Matrix

load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/weightedavgconnmat_sum_sparse.RData")
str(weightedavgconnmat_sum_sparse)

load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Euclid.dist.mat_551.RData")
#histogram of euclidean distances 
hist(euclid.dist.mat@x,breaks=seq(0,500,40))

# Check if the two matrices are in the same order (they should because one has been built from the other!)
all(weightedavgconnmat_sum_sparse@i == euclid.dist.mat@i) #TRUE
all(weightedavgconnmat_sum_sparse@p == euclid.dist.mat@p) #TRUE
#yes :)

#Method 1: Choose a probability threshold for the connectivity matrix
euclid_dist_threshold <- euclid.dist.mat@x
#probabilities negatively correlated with distances (lower probabilities associated with larger distances)
quantile(weightedavgconnmat_sum_sparse@x, c(0.05,0.25,0.5,0.75,0.9))
#5%          25%          50%          75%          90% 
#5.351931e-13 6.713307e-07 1.334731e-05 1.756904e-05 3.321243e-05 
weightedavgconnmat_threshold <- weightedavgconnmat_sum_sparse@x
mean(euclid_dist_threshold) #49.23165
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.0000000000001)]) #48.02541
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.00000000001)]) #47.53353
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.000000001)]) #46.3985
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.0000001)]) #44.4053
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.0000005)]) #43.66702
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.0000008)]) #42.64807
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.000001)]) #42.65154
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.0000015)]) #42.64946 #use this one
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.0000018)]) #41.47527
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.000002)]) #41.4816
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.000003)]) #41.47738
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.000005)]) #39.98194
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.00001)]) #36.4776
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.00002)]) #22.65326
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.00003)]) #19.80367 #try this one too
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.00005)]) #14.26435
mean(euclid_dist_threshold[which(weightedavgconnmat_threshold > 0.0001)]) #10.5022

#okay so that's really small
#thresholding by 0.00015% (average dispersal distance = 42km)
connmat_reduced <- weightedavgconnmat_sum_sparse
connmat_reduced@x[connmat_reduced@x <= 0.0000015] <- 0

#what do the networks look like for this reduced connectivity matrix?
g <- graph.adjacency(as.matrix(connmat_reduced), weighted = TRUE)
clusters(g,mode="weak") #$csize: 545   1   1   1   1   1   1 #not exactly smaller
clusters(g, mode = "strong") #a bunch of singletons and one that is 481 reefs big

#what happens if i just set 42km as the largest distance?
hist(euclid_dist_threshold[which(euclid_dist_threshold<43)]) #doesn't look like a dispersal kernel


#thresholding by 0.003% (average dispersal distance = 20km)
connmat_reduced <- weightedavgconnmat_sum_sparse
connmat_reduced@x[connmat_reduced@x <= 0.00003] <- 0

#what do the networks look like for this reduced connectivity matrix?
g <- graph.adjacency(as.matrix(connmat_reduced), weighted = TRUE)
clusters(g,mode="weak") #one network of 341, one network of 10, a bunch of variably sized networks
clusters(g, mode = "strong") #a bunch of singletons and one that is 176 reefs big

#what happens if i just set 42km as the largest distance?
hist(euclid_dist_threshold[which(euclid_dist_threshold<43)]) #doesn't look like a dispersal kernel