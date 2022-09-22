# Create initial_positions

library(RNetCDF)
library(raster)
library(RANN)

rm(list=ls())

data <- read.csv("greiner_coordinatesfromalldata_10.19.2021.csv")

data1 <- data.frame(lon=data$long.recenter,lat=data$latitude)
# write.table(data1,file="init1.txt",row.names=F,col.names=F)

not.in.water <- c(46,146,147,149,152,184,193,197,211,212,213,215,219,258,324,
                  325,329,330,331,339,340,341,343,398,399,400,401,402,403,404,
                  405,406,407,408,409,410,411,413,414,417,470,471,488,489,490,
                  491,497,523,526,527,528,529,530,531,532)

# Save a version of the initial positions without the drifters that are not in water
# data2 <- data1[-not.in.water,]
# write.table(data2,file="init2.txt",row.names=F,col.names=F)

# Add one column flagging the drifter not in water
niw <- rep(0,nrow(data1))
niw[not.in.water] <- 1
data1.f <- data1
data1.f$niw <- factor(niw)


# Open hydrodynamic dataset as a raster
uor <- brick("2009.nc",varname="uo", n1=1)
uor <- subset(uor,1)

# Convert raster into SpatialPointsDataFrame object
uosp <- rasterToPoints(uor,spatial=T)

# # initial positions with only  not-in-water point
# write.table(data1[data1.f$niw==1,],file="init.niw.txt",row.names=F,col.names=F)
# 
# # plot niw
# plot(uosp,col="gray",pch=1,cex=0.5)
# points(data1,col=data1.f$niw,cex=0.5,pch=16)

# Make a dataset with shifted positions for not in water
data1.sh <- data1
for (i.niw in 1 : length(not.in.water)) {
  # Find nearest neighbor of the point
  nn <- nn2(coordinates(uosp),data1[not.in.water[i.niw],])
  # Replace the coordinates of the point with those of its nearest neighbor
  data1.sh[not.in.water[i.niw],] <- coordinates(uosp)[nn$nn.idx[1],]
}
# Write new initial positions
write.table(data1.sh,file="init2.txt",row.names=F,col.names=F)
write.table(data1.f,file="initf.txt",row.names=F,col.names=F)
# Do an Ichthyop run test: all particles are in water now.


# Duplicate lines to release npart drifters per site
drifters_lon <- drifters_lat <- list()
npart <- 100
# Apply a spatial noise, normally distributed with sd = 0.0001 degrees (approx 11 meters)
# for (i in 1 : nrow(data2)) {
# drifters_lon[[i]] <- data2[i,1] + rnorm(100,0,0.0001)
# drifters_lat[[i]] <- data2[i,2] + rnorm(100,0,0.0001)
# }
# Do not apply any spatial noise
for (i in 1 : nrow(data1.sh)) {
  drifters_lon[[i]] <- rep(data1.sh[i,1],npart)
  drifters_lat[[i]] <- rep(data1.sh[i,2],npart)
}
data3 <- cbind(unlist(drifters_lon), unlist(drifters_lat))
write.table(data3,file="init3.txt",row.names=F,col.names=F)
