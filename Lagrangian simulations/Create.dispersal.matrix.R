# Calculate dispersal matrix

library(sp)
library(rgeos)
library(rgdal)
library(RNetCDF)

rm(list=ls())

# Load Ariel's sites: these will be also the recruitment sites
release <- read.csv("greiner_coordinatesfromalldata_10.19.2021.csv")

# Convert it to SpatialPoints
recr.sites <- SpatialPoints(cbind(release$long.recenter, release$latitude),
                            proj4string=CRS("+proj=longlat"))

# Create a buffer around each recruitment site: need to define a buffer size
recr.pol <- gBuffer(recr.sites, byid=T, width=0.001) # 0.001 degree is approx 110 meters; arbitrary value
# recr.pol <- gBuffer(recr.sites, byid=T, width=0.01) # 0.01 degree is approx 1110 meters; arbitrary value

# Write this to a shapefile to inspect it with QGIS
# writeOGR(SpatialPolygonsDataFrame(recr.pol,release),
#          getwd(),"recr.pol","ESRI Shapefile",overwrite_layer=T)

# Vector giving the release site of each released drifter, in the order they appear in drifters.positions
# Works when there are 100 drifters per release site and 559 release sites
release.site <- rep(c(1:559),each=100)

# Boundaries of the 559 classes, corresponding to the release sites. It will be used by the hist function below
bins <- seq(0,559)

# List of output files from Ichthyop
list.ichthyop.output <- list.files("output/")
# Each file is a release month, ex s1 is January, s2 is February etc

# Loop on years
for (year in 2009 : 2018) {
  
  # Select output files corresponding to this yeatr
  id.year <- which(substr(list.ichthyop.output,1,4) == year) # which output files correspond to this year?
  output.files <- list.ichthyop.output[id.year] # select those that correspond to this year
  output.files <- output.files[c(1,5:12,2:4)] # Order the months from 1 to 12
  output.files <- paste0("output/",output.files) # add path name to file names
  rm(id.year)
  
  # Loop on output files (months)
  for (i.month in 1 : 12) {
    
    cat("Processing year",year,"month",i.month,"\n")
    
    # Open drifter locations
    drifters <- open.nc(output.files[i.month])
    
    # Read mortality, longitude and latitude
    # mor <- var.get.nc(drifters,"mortality")
    lon <- var.get.nc(drifters,"lon")
    lat <- var.get.nc(drifters,"lat")
    # These are drifters by time matrices.
    # Number of rows = 55900, that is 559 release locations times 100 drifters
    # Number of columns = 27 time steps:
    # First column is t=0, second column t=5 days, third column t=10 days,... up to 27th column (130 days)
    # So column n is PLD = (n-1)*5 days
    
    # Transform [-180 to 180] longitudes into [0 to 360] longitudes
    lon2 <- ifelse(lon<0, lon+360, lon)
    lon <- lon2; rm(lon2)
    
    # Define list containing the connectivity matrix for the different time steps
    Cmatrices <- list()
    
    # Loop on time steps.
    # Starting from t = 2 because we do not need connectivity at time 1 (PLD = 0 days)
    for (t in 2 : 27) {
      
      cat("calculating Cmatrix for time",t,"(PLD =",(t-1)*5,"days)\n")
      
      # Create SpatialPoints object with the positions of the drifters at this time step
      drifters.positions <- SpatialPoints(cbind(lon[,t],lat[,t]),
                                          proj4string=CRS("+proj=longlat"))
      # plot(recr.pol[12],col="red",axes=T)
      # plot(drifters.positions,add=T)
      
      # Define connectivity matrix
      # Pay attention to the sense of the matrix!
      # gives the number of larvae coming from site j (column) recruited in site i (row) - NOT VICE VERSA
      Cmatrix <- matrix(NA,nrow=559,ncol=559)
      
      # Check whether drifters fall in recruitment sites:
      # produces a 55900 (drifters) by 559 (recruitment sites) matrix saying TRUE/FALSE
      recruited <- gContains(recr.pol, drifters.positions, byid =T)
      
      # For each drifter (rows): how many sites has it recruited in?
      # If other than 0 and 1, means some recruitment sites overlap
      # table(rowSums(recruited))
      
      # Loop on the columns of "recruited" (i.e. loop on recruitment sites)
      for (i.recr.site in 1 : 559) {
        
        # Which drifters have recruited in this recruitment site?
        id.larvae <- which(recruited[,i.recr.site])
        
        # What is the release site of these recruited drifters?
        recruited_release.site <- release.site[id.larvae]
        
        # Count how many drifters come from each release site: gives a vector of number of recruited drifters from site j
        num_recruited <- hist(recruited_release.site,breaks=bins,plot=F)$counts
        
        # Set line i.recr.site of Cmatrix to  the vector of number of recruited drifters from site j
        Cmatrix[i.recr.site,] <- num_recruited
      }
      
      Cmatrices[[t]] <- Cmatrix
      names(Cmatrices)[[t]] <- (t-1)*5
    }
    names(Cmatrices)[[1]] <- 0
    
    save(Cmatrices,file=paste0("Cmatrices_0.001deg/Cmatrices_",year,"_",i.month,".RData"))
    
  }
}


