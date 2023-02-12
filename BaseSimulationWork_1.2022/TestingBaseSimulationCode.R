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
#subset it to only the 76 sites with both benthic cover and fish abundance data
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/greiner_coordinatesfromalldata_dupsremoved_1.25.2022.RData") #newcoordinates; abridged coordinate file that corresponds with conn mat above, site order-wise
coordinates <- newcoordinates
jointsite_rows <- which(coordinates$site %in% sitevector)
jointsite_coral_weightedavgconnmat <- weightedavgconnmat[jointsite_rows, jointsite_rows]
jointsite_coral_weightedavgconnmat_small <- jointsite_coral_weightedavgconnmat[1:3, 1:3]

#macroalgae connectivity matrix 
#(coral conn mat divided by some very large number)
#jointsite_malg_weightedavgconnmat <- jointsite_coral_weightedavgconnmat/100 #100 chosen arbitrarily

#take the summed (across months/years) coral connectivity matrix with a PLD of 5
load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/BaseSimulationWork_1.2022/Conn_Mat_Sum.RData")
malg_smallPLD_connmat <- Conn_Mat_Sum[[1]]
jointsite_malg_smallPLDconnmat <- malg_smallPLD_connmat[jointsite_rows, jointsite_rows]
jointsite_malg_smallPLDconnmat_small <- jointsite_malg_smallPLDconnmat[1:3, 1:3]

###load in other parameters
#Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
#Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
a <- 0.1 #rate macroalgae overgrows coral
d <- 0.24 #rate coral dies
r <- 0.55 #rate coral recruits onto/overgrows turf algae
gamma <- 0.77 #rate macroalgae recruits onto/overgrows turf algae
g_val <- benthicfish_masterdataset$grazinglevel[1:3]
pc_val <- pm_val <- rep(NA,3)
for(i in 1:3){
  pm_val[i] <- sum(jointsite_coral_weightedavgconnmat_small[i,]) #sum of inputs to i (coral), sum row i
  #pc_val[i] <- sum(jointsite_malg_weightedavgconnmat[i,]) #sum of inputs to i (macroalgae)
  pc_val[i] <- sum(jointsite_malg_smallPLDconnmat_small[i,]) #sum of inputs to i (macroalgae)
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

MultipatchMumby_Tester <- function(t,state_alt,parameters_alt){
  with(as.list(c(state_alt,parameters_alt)),{
    #tried summing by addition and also by 'sum' and both give negative values in out_alt
      dMa_one <- a*Ma_one*Co_one - (g_val_one*Ma_one)/(Ma_one+Tu_one) + gamma*p_m_one*sum(c(Ma_one,Ma_two,Ma_three))*Tu_one
      dCo_one <- p_c_one*r*sum(c(Co_one,Co_two,Co_three))*Tu_one - d*Co_one - a*Ma_one*Co_one 
      dTu_one <- (g_val_one*Ma_one)/(Ma_one+Tu_one) + d*Co_one - (p_c_one*r*sum(c(Co_one,Co_two,Co_three)) + gamma*p_m_one*sum(c(Ma_one,Ma_two,Ma_three)))*Tu_one
      
      dMa_two <- a*Ma_two*Co_two - (g_val_two*Ma_two)/(Ma_two+Tu_two) + gamma*p_m_two*sum(c(Ma_one,Ma_two,Ma_three))*Tu_two
      dCo_two <- p_c_two*r*sum(c(Co_one,Co_two,Co_three))*Tu_two - d*Co_two - a*Ma_two*Co_two 
      dTu_two <- (g_val_two*Ma_two)/(Ma_two+Tu_two) + d*Co_two - (p_c_two*r*sum(c(Co_one,Co_two,Co_three)) + gamma*p_m_two*sum(c(Ma_one,Ma_two,Ma_three)))*Tu_two
      
      dMa_three <- a*Ma_three*Co_three - (g_val_three*Ma_three)/(Ma_three+Tu_three) + gamma*p_m_three*sum(c(Ma_one,Ma_two,Ma_three))*Tu_three
      dCo_three <- p_c_three*r*sum(c(Co_one,Co_two,Co_three))*Tu_three - d*Co_three - a*Ma_three*Co_three 
      dTu_three <- (g_val_three*Ma_three)/(Ma_three+Tu_three) + d*Co_three - (p_c_three*r*sum(c(Co_one,Co_two,Co_three)) + gamma*p_m_three*sum(c(Ma_one,Ma_two,Ma_three)))*Tu_three
      
    list(c(dMa_one,dCo_one,dTu_one,dMa_two,dCo_two,dTu_two,dMa_three,dCo_three,dTu_three)) 
  })
}

times <- seq(0,2000, by = 0.1)
npoints <- length(times)

mumbytrajectories_orig <- data.frame(reefnum = rep(1:3, each = npoints), sitename = rep(sitevector[1:3], each = npoints), reef_lat = rep(benthicfish_masterdataset$latitude[1:3], each = npoints), reef_long = rep(benthicfish_masterdataset$long.recenter[1:3], each = npoints), M = NA, C = NA, Tu = NA, TimeStep = rep(1:npoints, 3)) 
mumbytrajectories_tester <- data.frame(reefnum = rep(1:3, each = npoints), sitename = rep(sitevector[1:3], each = npoints), reef_lat = rep(benthicfish_masterdataset$latitude[1:3], each = npoints), reef_long = rep(benthicfish_masterdataset$long.recenter[1:3], each = npoints), M = NA, C = NA, Tu = NA, TimeStep = rep(1:npoints, 3)) 

CalcTrajectories <- function(parameters,pc_val,pm_val,g_val,times,mumbytrajectories,benthicfish_masterdataset,MultipatchMumby_Original, MultipatchMumby_Tester){
  #Elmhirst parameter model 
  parameters <- list(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, gamma <- 0.77, p_c <- pc_val, p_m <- pm_val, l <- 3)
  #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state <- c(Ma = benthicfish_masterdataset$malg_cover[1:3], Co = benthicfish_masterdataset$coral_cover[1:3], Tu = benthicfish_masterdataset$turf_cover[1:3])
  print("In trajectory calculation function")
  out <- lsode(y = state, times = times, func = MultipatchMumby_Original, parms = parameters)
  print("done calculating trajectories")
  for(i in 1:3){ 
    mumbytrajectories_orig$M[mumbytrajectories_orig$reefnum == i] <- out[,(i+1)]
    mumbytrajectories_orig$C[mumbytrajectories_orig$reefnum == i] <- out[,(3+i+1)]
    mumbytrajectories_orig$Tu[mumbytrajectories_orig$reefnum == i] <- out[,((3*2)+i+1)]
  }
  
  parameters_alt <- c(a <- 0.1, d <- 0.24, g_val_one <- g_val[1], g_val_two <- g_val[2], g_val_three <- g_val[3], r <- 0.55, gamma <- 0.77, p_c_one <- pc_val[1], p_c_two <- pc_val[2], p_c_three <- pc_val[3], p_m_one <- pm_val[1], p_m_two <- pm_val[2], p_m_three <- pm_val[3])
  #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state_alt <- c(Ma_one = benthicfish_masterdataset$malg_cover[1], 
                 Co_one = benthicfish_masterdataset$coral_cover[1],
                 Tu_one = benthicfish_masterdataset$turf_cover[1],
                 Ma_two = benthicfish_masterdataset$malg_cover[2],
                 Co_two = benthicfish_masterdataset$coral_cover[2],
                 Tu_two = benthicfish_masterdataset$turf_cover[2],
                 Ma_three = benthicfish_masterdataset$malg_cover[3],
                 Co_three = benthicfish_masterdataset$coral_cover[3], 
                 Tu_three = benthicfish_masterdataset$turf_cover[3])
  print("In alt trajectory calculation function")
  out_alt <- lsode(y = state_alt, times = times, func = MultipatchMumby_Tester, parms = parameters_alt)
  print("done calculating trajectories")
  mumbytrajectories_tester$M[mumbytrajectories_tester$reefnum == 1] <- out_alt[,2]
  mumbytrajectories_tester$C[mumbytrajectories_tester$reefnum == 1] <- out_alt[,3]
  mumbytrajectories_tester$Tu[mumbytrajectories_tester$reefnum == 1] <- out_alt[,4]
  mumbytrajectories_tester$M[mumbytrajectories_tester$reefnum == 2] <- out_alt[,5]
  mumbytrajectories_tester$C[mumbytrajectories_tester$reefnum == 2] <- out_alt[,6]
  mumbytrajectories_tester$Tu[mumbytrajectories_tester$reefnum == 2] <- out_alt[,7]
  mumbytrajectories_tester$M[mumbytrajectories_tester$reefnum == 3] <- out_alt[,8]
  mumbytrajectories_tester$C[mumbytrajectories_tester$reefnum == 3] <- out_alt[,9]
  mumbytrajectories_tester$Tu[mumbytrajectories_tester$reefnum == 3] <- out_alt[,10]

  return(mumbytrajectories_orig, mumbytrajectories_tester)
}

#now see how they compare
#out_alt is giving weird negatives, did they all start at the same value at least?
#out_alt[1,]
#out[1,]
#yes they did
out[2,]
out_alt[2,] #alt still giving negatives
mumbytrajectories_orig[1:5,] #19995:20000,
mumbytrajectories_tester[1:5,] #19995:20000,

setequal(mumbytrajectories_orig$M, mumbytrajectories_tester$M) #TRUE
setequal(mumbytrajectories_orig$C, mumbytrajectories_tester$C) #TRUE
setequal(mumbytrajectories_orig$Tu, mumbytrajectories_tester$Tu) #TRUE
# :) sweet
