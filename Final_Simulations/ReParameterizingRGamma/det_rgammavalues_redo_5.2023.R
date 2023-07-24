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

#REMOVED THIS BIT
#scale it so the grazing rate either has a mean of 0.1, 0.3 or 0.5
#median(benthicfish_masterdataset$comb_grazinglevel) 
MD_scale <- 0.1/median(benthicfish_masterdataset$comb_grazinglevel) #if multiply by 2.338038 will get a median of 0.1
ASS_scale <- 0.3/median(benthicfish_masterdataset$comb_grazinglevel) #7.014113
CD_scale <- 0.5/median(benthicfish_masterdataset$comb_grazinglevel) #11.690188
#checking 
#median(benthicfish_masterdataset$grazinglevel*CD_scale) #0.1, 0.3, 0.5 confirmed
scalingfactors <- c(MD_scale, ASS_scale, CD_scale)

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

###probably just need this line, but loading in the stuff ^^ above just in case as well ##moved into for loop
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_5.10.2022.RData")
#update 7.1.2022 - qoli qoli
#load("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/MgmtStressorScenarios/benthicfish_masterdataset_sed_tabubuffer0.02_qoliqolionly_6.30.2022.RData")
#benthicfish_masterdataset <- benthicfish_masterdataset_sed

#need to scale the new_grazinglevel #5.31.2022: NO! scale using comb_grazinglevel instead
#scale it so the grazing rate either has a mean of 0.1, 0.3 or 0.5
#median(benthicfish_masterdataset$new_grazinglevel) 
#MD_scale <- 0.1/median(benthicfish_masterdataset$new_grazinglevel) #if multiply by 2.338038 will get a median of 0.1
#ASS_scale <- 0.3/median(benthicfish_masterdataset$new_grazinglevel) #7.014113
#CD_scale <- 0.5/median(benthicfish_masterdataset$new_grazinglevel) #11.690188
#checking 
#median(benthicfish_masterdataset$grazinglevel*CD_scale) #0.1, 0.3, 0.5 confirmed
#scalingfactors <- c(1,MD_scale, ASS_scale, CD_scale)

#####

#cycle through different r/gamma values, 3 different grazing rates, baseline intervention
#round1: 
#gamma_vals <- c(50,100,250,500,900,950,1000)
#r_vals <- c(30,50,100,125,150,200,500,1000)
#g_vals <- c(0.1,0.3,0.5)
#round2
#gamma_vals <- c(50,250,500,900,950,1000)
#r_vals <- c(10000,5000,2000,1500)
#g_vals <- c(0.1,0.5) #c(0.1,0.3,0.5)
#round3
#gamma_vals <- c(650,700,800)
#r_vals <- c(2500,3000,4000)
#g_vals <- c(0.1,0.5)
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, length(r_vals)),length(g_vals))) 
#parameteers <- matrix(c(gg,rr,gammagamma), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)), ncol = 3)
#round 4
#gamma_vals <- c(900,950,1000)
#r_vals <- c(5000,7500,10000)
#g_vals <- c(0.3)
#init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#round 5 - stick two parameteers sets together, only want variable initial conditions for g=0.3 
#gamma_vals <- c(850,900,950,1000)
#r_vals <- c(4500,4750,4900)
#g_vals <- c(0.1,0.5) 
#init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptone <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(850)
#r_vals <- c(5000)
#g_vals <- c(0.1,0.5) 
#init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_pttwo <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(850,900,950,1000)
#r_vals <- c(4500,4750,4900)
#g_vals <- c(0.3) 
#init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptthree <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#only 850 hasn't been run with 5000
#gamma_vals <- c(850)
#r_vals <- c(5000)
#g_vals <- c(0.3) 
#init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptfour <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#parameteers <- rbind(parameteers_ptone,parameteers_pttwo, parameteers_ptthree,parameteers_ptfour)
#round 6 - stick two parameteers sets together, only want variable initial conditions for g=0.3 
#gamma_vals <- c(2000,5000,10000)
#r_vals <- c(4500,4750,4900)
#g_vals <- c(0.3) 
#init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptone <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(2000,5000,10000)
#r_vals <- c(4500,4750,4900)
#g_vals <- c(0.1,0.5) 
#init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_pttwo <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(900,950,1000,2000,5000,10000)
#r_vals <- c(8000,9000)
#g_vals <- c(0.3) 
#init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptthree <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(900,950,1000,2000,5000,10000)
#r_vals <- c(8000,9000)
#g_vals <- c(0.1,0.5) 
#init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptfour <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#parameteers <- rbind(parameteers_ptone,parameteers_pttwo, parameteers_ptthree,parameteers_ptfour)
#round 7 - stick two parameteers sets together, only want variable initial conditions for g=0.3 
#gamma_vals <- c(1000,1500,2000,3000,4000)
#r_vals <- c(5500,6000,6500)
#g_vals <- c(0.3) 
#init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptone <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(1000,1500,2000,3000,4000)
#r_vals <- c(5500,6000,6500)
#g_vals <- c(0.1,0.5) 
#init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_pttwo <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(3000,4000)
#r_vals <- c(5000,7500)
#g_vals <- c(0.3) 
#init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptthree <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#gamma_vals <- c(3000,4000)
#r_vals <- c(5000,7500)
#g_vals <- c(0.1,0.5) 
#init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
#gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
#rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
#gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
#initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
#parameteers_ptfour <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
#parameteers <- rbind(parameteers_ptone,parameteers_pttwo, parameteers_ptthree,parameteers_ptfour)

#round 8 - stick two parameteers sets together, only want variable initial conditions for g=0.3 
gamma_vals <- c(1600,1700,1800,1900)
r_vals <- c(4500,4750,4900,5500,6000,6500)
g_vals <- c(0.3) 
init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
parameteers_ptone <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)

gamma_vals <- c(1600,1700,1800,1900)
r_vals <- c(4500,4750,4900,5500,6000,6500)
g_vals <- c(0.1,0.5) 
init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
parameteers_pttwo <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)

gamma_vals <- c(1500)
r_vals <- c(4500,4750,4900)
g_vals <- c(0.3) 
init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
parameteers_ptthree <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)

gamma_vals <- c(1500)
r_vals <- c(4500,4750,4900)
g_vals <- c(0.1,0.5) 
init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
parameteers_ptfour <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)

gamma_vals <- c(3000,4000)
r_vals <- c(8000,9000,10000)
g_vals <- c(0.3) 
init_vals <- c(1,2,3,4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
parameteers_ptfive <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)

gamma_vals <- c(3000,4000)
r_vals <- c(8000,9000,10000)
g_vals <- c(0.1,0.5) 
init_vals <- c(4) #1 = all high coral, 2 = all high malg, 3 = 50/50, 4 = normal
gg <- c(rep(g_vals, each = (length(r_vals)*length(gamma_vals)*length(init_vals))))
rr <- c(rep(rep(r_vals, each = length(gamma_vals)*length(init_vals)),length(g_vals)))
gammagamma <- c(rep(rep(gamma_vals, each = length(init_vals)),length(g_vals)*length(r_vals))) 
initinit <- c(rep(init_vals,(length(r_vals)*length(gamma_vals)*length(g_vals))))
parameteers_ptsix <- matrix(c(gg,rr,gammagamma,initinit), nrow = (length(r_vals)*length(gamma_vals)*length(g_vals)*length(init_vals)), ncol = 4)
parameteers <- rbind(parameteers_ptone,parameteers_pttwo, parameteers_ptthree,parameteers_ptfour, parameteers_ptfive,parameteers_ptsix)

#round 8 - variable initial conditions, sometimes...again
#mixed scenario
set.seed(2)
roughlyhalf <- ceiling(length(sitevector)/2)
mixedsc_coralcover <- sample(c(rep(0.8,roughlyhalf),rep(0.01,(length(sitevector) - roughlyhalf)))) #assigning coral cover values to all reefs
mixedsc_turfcover <- rep(0.19,length(sitevector))
mixedsc_malgcover <- 1 - (mixedsc_coralcover + mixedsc_turfcover)

for(j in 64:(dim(parameteers)[1])){ 
      
      ###load in other parameters
      #Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
      #Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
      a <- 0.1 #rate macroalgae overgrows coral
      d_val <- rep(0.24, length(sitevector))
      r_val <- parameteers[j,2] #rate coral recruits onto/overgrows turf algae
      gamma_val <- parameteers[j,3] #rate macroalgae recruits onto/overgrows turf algae
      g_val <- rep(parameteers[j,1],length(sitevector)) #benthicfish_masterdataset$comb_grazinglevel*scalingfactors[j] 
      initcondsc <- parameteers[j,4]
      
      pc_vec <- pm_vec <- list()
      for(i in 1:length(sitevector)){
        pc_vec[[i]] <- jointsite_coral_weightedavgconnmat[i,] #all inputs to i (coral)
        pm_vec[[i]] <- jointsite_malg_smallPLDconnmat[i,] #all inputs to i (macroalgae)
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
            dMa[k] <- a*Ma[k]*Co[k] - (g[k]*Ma[k])/(Ma[k]+Tu[k]) + gamma*sum(p_m[[k]]*Ma)*Tu[k]
            dCo[k] <- r*sum(p_c[[k]]*Co)*Tu[k] - d[k]*Co[k] - a*Ma[k]*Co[k] 
            dTu[k] <- (g[k]*Ma[k])/(Ma[k]+Tu[k]) + d[k]*Co[k] - (r*sum(p_c[[k]]*Co) + gamma*sum(p_m[[k]]*Ma))*Tu[k]
          }
          list(c(dMa,dCo,dTu)) 
        })
      }
      
      times <- seq(0,2000, by = 0.1)
      npoints <- length(times)
      
      mumbytrajectories <- data.frame(reefnum = rep(1:length(sitevector), each = npoints), sitename = rep(sitevector, each = npoints), reef_lat = rep(benthicfish_masterdataset$latitude, each = npoints), reef_long = rep(benthicfish_masterdataset$long.recenter, each = npoints), M = NA, C = NA, Tu = NA, TimeStep = rep(1:npoints, length(sitevector))) 
      
      #CalcTrajectories <- function(parameters,pc_val,pm_val,g_val,times,mumbytrajectories,benthicfish_masterdataset,MultipatchMumby_Original, sitevector){
      #Elmhirst parameter model 
      parameters <- list(a <- 0.1, d <- d_val, g <- g_val, r <- r_val, gamma <- gamma_val, p_c <- pc_vec, p_m <- pm_vec, l <- length(sitevector))
      #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
      
      #giving M, C, T starting conditions - for round 4
      if(initcondsc == 1){ #all high coral
        state <- c(Ma = rep(0.01,length(sitevector)), Co = rep(0.8,length(sitevector)), Tu = rep(0.19,length(sitevector)))
      }
      if(initcondsc == 2){ #all high malg
        state <- c(Ma = rep(0.8,length(sitevector)), Co = rep(0.01,length(sitevector)), Tu = rep(0.19,length(sitevector)))
      }
      if(initcondsc == 3){ #mix high malg/highcoral
        state <- c(Ma = mixedsc_malgcover, Co = mixedsc_coralcover, Tu = mixedsc_turfcover)
      }
      
      if(initcondsc == 4){
        state <- c(Ma = benthicfish_masterdataset$malg_cover, Co = benthicfish_masterdataset$coral_cover, Tu = benthicfish_masterdataset$turf_cover)
      }
      
      print(paste("In trajectory calculation function, grazing value",g_val[1],"j= ", j))
      start_time <- Sys.time()
      out <- lsode(y = state, times = times, func = MultipatchMumby_Original, parms = parameters)	
      end_time <- Sys.time()
      end_time - start_time
      print("Done")
      for(i in 1:length(sitevector)){ 
        mumbytrajectories$M[mumbytrajectories$reefnum == i] <- out[,(i+1)]
        mumbytrajectories$C[mumbytrajectories$reefnum == i] <- out[,(length(sitevector)+i+1)]
        mumbytrajectories$Tu[mumbytrajectories$reefnum == i] <- out[,((length(sitevector)*2)+i+1)]
      }
      #return(mumbytrajectories)
      #}
      
      #need to make a dataset that just contains the first and last cover levels
      benthic_traj <- benthicfish_masterdataset %>%
          select(site, latitude, long.recenter, comb_grazinglevel) #warning: changed comb_grazinglevel to new_grazinglevel
     
      for(i in 1:length(sitevector)){
        benthic_traj$init_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][1]
        benthic_traj$init_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][1]
        benthic_traj$init_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][1]
        benthic_traj$final_coralcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$C[mumbytrajectories$reefnum == i][npoints]
        benthic_traj$final_malgcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$M[mumbytrajectories$reefnum == i][npoints]
        benthic_traj$final_turfcover[benthic_traj$site == sitevector[i]] <- mumbytrajectories$Tu[mumbytrajectories$reefnum == i][npoints]
      }
      datalist = list(out = out, mumbytrajectories = mumbytrajectories, benthic_traj = benthic_traj)
      saveRDS(datalist, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_fullsimrun_allherbs_gammaval",gamma_val,"r_val",r_val,"g_val",g_val[1],"_initcond",initcondsc,"_varinitconds_5.9.2023.rds"))
      
      
      #Well let's plot this out 
      worldmap <- map_data ("world", wrap = c(0, 360))
      
      #final coral cover
      BaseRun_newrgamma_FinalCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Coral Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_coralcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"FinalCoralCover_initcond",initcondsc,"_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #final malg cover
      BaseRun_newrgamma_FinalMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Final Malg Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (final_malgcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"FinalMalgCover_initcond",initcondsc,"_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #removed grazing level plot
      
      #rounded versions
      #final coral cover
      Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Coral Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_coralcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Rounded_reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"FinalCoralCover_initcond",initcondsc,"_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #final malg cover
      Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Final Malg Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((final_malgcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_FinalMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Rounded_reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"_initcond",initcondsc,"_FinalMalgCover_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
      
if(initcondsc < 4){
      #initial coral cover
      BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"_initcond",initcondsc,"_InitialCoralCover_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #initial malg cover
      BaseRun_newrgamma_InitialMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
        scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(BaseRun_newrgamma_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"_initcond",initcondsc,"_InitialMalgCover_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #initial coral cover
      Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Coral Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_coralcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Rounded_reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"_initcond",initcondsc,"_InitialCoralCover_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
      
      #initial malg cover
      Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover <-  
        ggplot(aes(x = long, y = lat), data = worldmap) + 
        geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
        xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Malg Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
        geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_malgcover*100),10))))+
        scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
        #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
        coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
        scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
        scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                           breaks = seq(160, 190, 10),
                           labels = c(160, 170, "180/-180", -170)) +
        coord_equal() +  theme_bw()
      ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Rounded_reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"_initcond",initcondsc,"_InitialMalgCover_varinitconds_5.9.2023.png"), bg = "transparent", height = 10, width = 10)
}
      
      #just in case
      parameters <- NULL
      state <- NULL
      out <- NULL   
}

#need to make a dataframe that records the proportion of patches >10%/>30%/>50% coral/malg cover
propcover <- data.frame(g = parameteers[,1], r = parameteers[,2], gamma = parameteers[,3], init_values = parameteers[,4], coral_thirty = NA, coral_fifty = NA, malg_thirty = NA, malg_fifty = NA)

for(j in 1:(dim(parameteers)[1])){
  ###load in other parameters
  #Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
  #Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
  #a <- 0.1 #rate macroalgae overgrows coral
  #d <- 0.24 #rate coral dies
  r_val <- parameteers[j,2] #rate coral recruits onto/overgrows turf algae
  gamma_val <- parameteers[j,3] #rate macroalgae recruits onto/overgrows turf algae
  g_val <- parameteers[j,1] 
  init_val <- parameteers[j,4]
  
  datalist <- readRDS(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_fullsimrun_allherbs_gammaval",gamma_val,"r_val",r_val,"g_val",g_val[1],"_initcond",init_val,"_varinitconds_5.9.2023.rds"))
  #out, mumbytrajectories, benthic_traj - benthic_traj probably the most useful
  benthic_traj <- datalist[[3]] 
  
  #note: final_coralcover and final_malgcover vary from 0 -> 1, so was multiplying by 100 above
  propcover$coral_thirty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val & propcover$init_values == init_val] <- length(benthic_traj$final_coralcover[which(benthic_traj$final_coralcover > 0.3)])/length(sitevector)
  
  propcover$coral_fifty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val & propcover$init_values == init_val] <- length(benthic_traj$final_coralcover[which(benthic_traj$final_coralcover > 0.5)])/length(sitevector)
  
  propcover$malg_thirty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val & propcover$init_values == init_val] <- length(benthic_traj$final_malgcover[which(benthic_traj$final_malgcover > 0.3)])/length(sitevector)
  
  propcover$malg_fifty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val & propcover$init_values == init_val] <- length(benthic_traj$final_malgcover[which(benthic_traj$final_malgcover > 0.5)])/length(sitevector)
  
  #just in case
  benthic_traj <- NULL
}

saveRDS(propcover, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_varinitconds_round8.rds"))
#propcover <- readRDS(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_varinitconds_round6.rds"))

#plotting propcover
#for loop to go through the initial conditions
init_vals <- c(1,2,3,4)
for(i in 1:length(init_vals)){
  propcover_lowgraz <- propcover[propcover$g == 0.1 & propcover$init_values == init_vals[i],]
  propcover_midgraz <- propcover[propcover$g == 0.3 & propcover$init_values == init_vals[i] ,]
  propcover_highgraz <- propcover[propcover$g == 0.5 & propcover$init_values == init_vals[i],]
  
  More30Malg_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(malg_thirty*75), size = as.factor(malg_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >30% Macroalgae, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Malg_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More30Malg_midgraz_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More30Coral_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(coral_thirty*75), size = as.factor(coral_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >30% Coral, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Coral_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More30Coral_midgraz_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Malg_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(malg_fifty*75), size = as.factor(malg_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >50% Macroalgae, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Malg_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More50Malg_midgraz_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Coral_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(coral_fifty*75), size = as.factor(coral_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >50% Coral, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Coral_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More50Coral_midgraz_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)

  More30Malg <- ggplot(aes(x = r, y = gamma), data = propcover_lowgraz) + 
    geom_point(aes(color = as.factor(malg_thirty*75), size = as.factor(malg_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Macroalgae")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Malg, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Malg_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More30Coral <- ggplot(aes(x = r, y = gamma), data = propcover_highgraz) + 
    geom_point(aes(color = as.factor(coral_thirty*75), size = as.factor(coral_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Coral")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Coral, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Coral_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Malg <- ggplot(aes(x = r, y = gamma), data = propcover_lowgraz) + 
    geom_point(aes(color = as.factor(malg_fifty*75), size = as.factor(malg_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Macroalgae")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Malg, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Malg_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Coral <- ggplot(aes(x = r, y = gamma), data = propcover_highgraz) + 
    geom_point(aes(color = as.factor(coral_fifty*75), size = as.factor(coral_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Coral")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Coral, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Coral_round8_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
}

#plotting all propcover - need to update with #7
load(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_round1.RData"))
propcover$init_values <- 4
propcover_one <- propcover[,c(1,2,3,8,4,5,6,7)]
propcover <- NULL
load(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_5.8.2023_round2.RData"))
propcover$init_values <- 4
propcover_two <- propcover[,c(1,2,3,8,4,5,6,7)]
propcover <- NULL
load(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_5.9.2023_round3.RData"))
propcover$init_values <- 4
propcover_three <- propcover[,c(1,2,3,8,4,5,6,7)]
propcover <- NULL
propcover_four <- readRDS(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_varinitconds_round4.rds"))
propcover_five <- readRDS(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_varinitconds_round5.rds"))
propcover_six <- readRDS(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_varinitconds_round6.rds"))
propcover_seven <- readRDS(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_varinitconds_round7.rds"))
propcover_eight <- readRDS(paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_varinitconds_round8.rds"))
propcover <- rbind(propcover_one, propcover_two, propcover_three, propcover_four, propcover_five, propcover_six, propcover_seven, propcover_eight)

#for loop to go through the initial conditions
for(i in 1:length(init_vals)){
  propcover_lowgraz <- propcover[propcover$g == 0.1 & propcover$init_values == init_vals[i],]
  propcover_midgraz <- propcover[propcover$g == 0.3 & propcover$init_values == init_vals[i] ,]
  propcover_highgraz <- propcover[propcover$g == 0.5 & propcover$init_values == init_vals[i],]
  
  More30Malg_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(malg_thirty*75), size = as.factor(malg_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >30% Macroalgae, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Malg_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More30Malg_midgraz_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More30Coral_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(coral_thirty*75), size = as.factor(coral_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >30% Coral, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Coral_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More30Coral_midgraz_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Malg_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(malg_fifty*75), size = as.factor(malg_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >50% Macroalgae, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Malg_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More50Malg_midgraz_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Coral_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
    geom_point(aes(color = as.factor(coral_fifty*75), size = as.factor(coral_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle(paste("Number of patches with >50% Coral, g = 0.3, InitVal =",init_vals[i]))+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Coral_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Initval_",init_vals[i],"More50Coral_midgraz_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More30Malg <- ggplot(aes(x = r, y = gamma), data = propcover_lowgraz) + 
    geom_point(aes(color = as.factor(malg_thirty*75), size = as.factor(malg_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Macroalgae")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Malg, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Malg_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More30Coral <- ggplot(aes(x = r, y = gamma), data = propcover_highgraz) + 
    geom_point(aes(color = as.factor(coral_thirty*75), size = as.factor(coral_thirty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Coral")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More30Coral, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Coral_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Malg <- ggplot(aes(x = r, y = gamma), data = propcover_lowgraz) + 
    geom_point(aes(color = as.factor(malg_fifty*75), size = as.factor(malg_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Macroalgae")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Malg, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Malg_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
  More50Coral <- ggplot(aes(x = r, y = gamma), data = propcover_highgraz) + 
    geom_point(aes(color = as.factor(coral_fifty*75), size = as.factor(coral_fifty*75))) +
    xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Coral")+
    scale_color_viridis(discrete = TRUE, name = "Number of Patches")
  ggsave(More50Coral, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Coral_round12345678_5.15.2023.png"), bg = "transparent", height = 10, width = 10)
  
}




###only works for rounds 1-3
#note the accidental malg_tens and coral_tens included below...
#plotting the proportion of reefs over 10/20/30/50% coral/macroalgal cover at the g=0.5/0.3/0.1 level

#need to make a dataframe that records the proportion of patches >10%/>30%/>50% coral/malg cover
propcover <- data.frame(g = gg, r = rr, gamma = gammagamma, coral_thirty = NA, coral_fifty = NA, malg_thirty = NA, malg_fifty = NA)

for(j in 1:length(rr)){
  ###load in other parameters
  #Mumby parameter model: a <- 0.1, d <- 0.44, r <- 1, gamma <- 0.8
  #Elmhirst parameter model: a <- 0.1, d <- 0.24, r <- 0.55, gamma <- 0.77
  #a <- 0.1 #rate macroalgae overgrows coral
  #d <- 0.24 #rate coral dies
  r_val <- parameteers[j,2] #rate coral recruits onto/overgrows turf algae
  gamma_val <- parameteers[j,3] #rate macroalgae recruits onto/overgrows turf algae
  g_val <- parameteers[j,1] 
  
  data <- readRDS(file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_fullsimrun_allherbs_gammaval",gamma_val,"r_val",r_val,"g_val",g_val[1],"_5.9.2023.rds"))
  #out, mumbytrajectories, benthic_traj - benthic_traj probably the most useful
  benthic_traj <- data[[3]]
  
  #note: final_coralcover and final_malgcover vary from 0 -> 1, so was multiplying by 100 above
  propcover$coral_thirty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val] <- length(benthic_traj$final_coralcover[which(benthic_traj$final_coralcover > 0.3)])/length(sitevector)
  
  propcover$coral_fifty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val] <- length(benthic_traj$final_coralcover[which(benthic_traj$final_coralcover > 0.5)])/length(sitevector)
  
  propcover$malg_thirty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val] <- length(benthic_traj$final_malgcover[which(benthic_traj$final_malgcover > 0.3)])/length(sitevector)
  
  propcover$malg_fifty[propcover$g == g_val & propcover$r == r_val & propcover$gamma == gamma_val] <- length(benthic_traj$final_malgcover[which(benthic_traj$final_malgcover > 0.5)])/length(sitevector)
  
  #just in case
  data <- NULL
  benthic_traj <- NULL
}

save(propcover, file = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/propcover_5.9.2023_round3.RData"))

#plotting propcover
propcover_lowgraz <- propcover[propcover$g == 0.1,]
propcover_midgraz <- propcover[propcover$g == 0.3,]
propcover_highgraz <- propcover[propcover$g == 0.5,]

More30Malg <- ggplot(aes(x = r, y = gamma), data = propcover_lowgraz) + 
  geom_point(aes(color = as.factor(malg_thirty*75), size = as.factor(malg_thirty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Macroalgae")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More30Malg, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Malg_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)

More30Coral <- ggplot(aes(x = r, y = gamma), data = propcover_highgraz) + 
  geom_point(aes(color = as.factor(coral_thirty*75), size = as.factor(coral_thirty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Coral")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More30Coral, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Coral_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)

More30Malg_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
  geom_point(aes(color = as.factor(malg_ten*75), size = as.factor(malg_thirty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Macroalgae, g = 0.3")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More30Malg_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Malg_midgraz_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)

More30Coral_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
  geom_point(aes(color = as.factor(coral_ten*75), size = as.factor(coral_thirty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >30% Coral, g = 0.3")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More30Coral_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More30Coral_midgraz_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)

More50Malg <- ggplot(aes(x = r, y = gamma), data = propcover_lowgraz) + 
  geom_point(aes(color = as.factor(malg_fifty*75), size = as.factor(malg_fifty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Macroalgae")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More50Malg, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Malg_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)

More50Coral <- ggplot(aes(x = r, y = gamma), data = propcover_highgraz) + 
  geom_point(aes(color = as.factor(coral_fifty*75), size = as.factor(coral_fifty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Coral")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More50Coral, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Coral_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)

More50Malg_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
  geom_point(aes(color = as.factor(malg_ten*75), size = as.factor(malg_fifty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Macroalgae, g = 0.3")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More50Malg_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Malg_midgraz_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)

More50Coral_midgraz <- ggplot(aes(x = r, y = gamma), data = propcover_midgraz) + 
  geom_point(aes(color = as.factor(coral_ten*75), size = as.factor(coral_fifty*75))) +
  xlab("r") + ylab("gamma")+ggtitle("Number of patches with >50% Coral, g = 0.3")+
  scale_color_viridis(discrete = TRUE, name = "Number of Patches")
ggsave(More50Coral_midgraz, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/More50Coral_midgraz_round3_5.9.2023.png"), bg = "transparent", height = 10, width = 10)


##################################removed initial maps bc can just regenerate them later, running out of computer space
#initial coral cover
BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Coral Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_coralcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Coral Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"InitialCoralCover_5.8.2023.png"), bg = "transparent", height = 10, width = 10)

#initial malg cover
BaseRun_newrgamma_InitialMalgCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Initial Malg Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = (init_malgcover*100)))+
  scale_color_viridis(discrete = FALSE, name = "Malg Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(BaseRun_newrgamma_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"InitialMalgCover_5.8.2023.png"), bg = "transparent", height = 10, width = 10)

#initial coral cover
Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Coral Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_coralcover*100),10))))+
  scale_color_viridis(discrete = TRUE, name = "Rounded Coral Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialCoralCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Rounded_reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"InitialCoralCover_5.8.2023.png"), bg = "transparent", height = 10, width = 10)

#initial malg cover
Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover <-  
  ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") +
  xlab("Longitude") + ylab("Latitude")+ ggtitle(paste("Rounded Initial Malg Cover, Gamma ", gamma_val,"r",r_val,"g",g_val[1]))+
  geom_point(data = benthic_traj, aes(x = long.recenter, y = latitude, color = as.factor(round_any((init_malgcover*100),10))))+
  scale_color_viridis(discrete = TRUE, name = "Rounded Malg Cover %")+
  #scale_color_manual(values = c("red","light blue"), name = "More than one year?", labels = c("no","yes"), guide = guide_legend(reverse=TRUE))+
  coord_cartesian(xlim = c(176.5,185), ylim = c(-20,-12)) +
  scale_y_continuous(limits = c(-19,-16)) + #c(-20,-15) #c(-19,-17)
  scale_x_continuous(limits = c(180 - 3, 180 + 1), #c(180 - 3, 180 + 2.5) #c(180 - 3, 180)
                     breaks = seq(160, 190, 10),
                     labels = c(160, 170, "180/-180", -170)) +
  coord_equal() +  theme_bw()
ggsave(Rounded_BaseRun_newrgamma_allherbivores_InitialMalgCover, filename = paste0("~/GitHub/PhDThesisProjects/Fiji_StabilityConnectivitySimulation/Final_Simulations/ReParameterizingRGamma/Rounded_reparam_dval_r",r_val,"gamma",gamma_val,"_allherbivores_grazing",g_val[1],"InitialMalgCover_5.8.2023.png"), bg = "transparent", height = 10, width = 10)
