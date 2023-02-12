#Mumby 1 patch model - troubleshooting
mumby_equi <- read.csv(file = "~/Documents/University of Toronto - Removed from Dropbox, on external HD/Removed 1.3.2020/MATLAB graphs/Mumby Equi Analysis/Mumby_manygvalues_MATLAB_equivals.csv")

mumby_equi$Colour <- NA
mumby_equi$Colour[mumby_equi$Stability == "stable_node"] <- 'black'
mumby_equi$Colour[mumby_equi$Stability == "unstable_node"] <- 'gold'
mumby_equi$Colour[mumby_equi$Stability == "saddle"] <- 'purple'
mumby_equi$Colour[mumby_equi$Stability == "bifurc?"] <- 'blue'

#USE THIS ONE
plot(x = mumby_equi$g, y = mumby_equi$C1, col = mumby_equi$Colour, xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover")
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black","gold","purple","blue"), pch = c(20,20))

#transparent version
library(scales)
plot(x = mumby_equi$g, y = mumby_equi$C1, col = alpha(mumby_equi$Colour,0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover")
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","blue"), 0.4), pch = c(20,20))

#analytically derived bifurcation plot (just showing the regions where the equilibrium related to the saddlenode exists)
a <- 0.1
d <- 0.44
g <- 0.3
r <- 1
gamma <- 0.8
g <- seq(0.18,0.39,by=0.01)
#g = 0.18: first time (from 0) that both M and C are positive
	#from Blackwood et al 2018, the value of this transition point = 0.17952 :)
#g = 0.39: last time (re: -> 1) that both M and C are positive
	#from Blackwood et al 2018, the value of this transition point = 0.3927272727 :)
plot(x = g, y = (-(a^2)-(a*r) + 2*a*gamma + d*gamma - sqrt((a^2 + a*r - 2*a*gamma - d*gamma)^2 - 4*(-a^2 - a*r + a*gamma)*(-a*g - g*r + a*gamma + d*gamma)))/(2*(-(a^2)-(a*r)+(a*gamma))), xlab = "Grazing", ylab = "% Coral Cover", xlim = c(0,1), ylim = c(0,1))

#plotting them both together - ok they land right on top of each other
plot(x = mumby_equi$g, y = mumby_equi$C1, col = mumby_equi$Colour, xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover")
points(x = g, y = (-(a^2)-(a*r) + 2*a*gamma + d*gamma - sqrt((a^2 + a*r - 2*a*gamma - d*gamma)^2 - 4*(-a^2 - a*r + a*gamma)*(-a*g - g*r + a*gamma + d*gamma)))/(2*(-(a^2)-(a*r)+(a*gamma))), xlab = "Grazing", ylab = "% Coral Cover", xlim = c(0,1), ylim = c(0,1), col = "red")
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?", "Analytically Derived"), col = c("black","gold","purple","blue","red"), pch = c(20,20))


