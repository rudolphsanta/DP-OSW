library(dplyr)
library(tidyverse)

T.Sizes <- c(3.3,5,8,10,15)

LCOE14.high <- c(285,251,174,166,258)
LCOE14.low <- c(266,235,162,155,243)
ATB14.high <- 136
ATB14.low <- 123

LCOE8.high <- c(194,144,88,93,145)
LCOE8.low <- c(186,137,83,87,136)
ATB8.high <- 68
ATB8.low <- 62

setEPS()
postscript("Figure 6_v2.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = T.Sizes, y = LCOE14.high, col = 'red', type = 'b', pch = 19, lwd = 2, lty = 2,
     xlab = "Turbine Size [MW]", ylab = "LCOE [USD/MWh]",
     xaxs = 'i', yaxs = 'i', las = 1, xlim = c(0,16), ylim = c(0,300))
points(x = T.Sizes, y = LCOE14.low, col ='red', type = 'b', pch = 19, lwd = 2, lty = 2)
polygon(x = c(0,0,16,16), y = c(ATB14.high, ATB14.low, ATB14.low, ATB14.high), 
        col = 'red', border = NA)
points(x = T.Sizes, y = LCOE8.high, col ='blue', type = 'b', pch = 19, lwd = 2, lty = 2)
points(x = T.Sizes, y = LCOE8.low, col ='blue', type = 'b', pch = 19, lwd = 2, lty = 2)
polygon(x = c(0,0,16,16), y = c(ATB8.high, ATB8.low, ATB8.low, ATB8.high), 
        col = 'blue', border = NA)
text(x=0.1,y=ATB14.low-1,"Wind Speed Class 14\nNREL ATB Range", col = 'red',adj=c(0,1), cex = 0.8)
text(x=0.1,y=ATB8.low-1,"Wind Speed Class 8\nNREL ATB Range", col = 'blue',adj=c(0,1), cex = 0.8)
text(x=9, y = 195, "Wind Speed Class 14\nModel Results", col = 'red', cex = 0.8)#, adj = c(1,1))
text(x=9, y = 110, "Wind Speed Class 8\nModel Results", col = 'blue', cex = 0.8)#, adj = c(0,1))

dev.off()


##############################
#Thruster Power Functions
## Define system and resulting DP.Power consumption. Fucntions from 20220523 15 MW Turbine and Functions.R
DP.Power.n <- function(force.kn, motor.diam, n = 1) { #power of n motors system, motor.diam in meters
  K <- 1250
  power <- (n/motor.diam)*(1000*force.kn/(K*n))^1.5
  return(power/1000) #in MW
}

# Data from Kongsberg for 3.8 m diameter thruster
Del.Thrust <- c(0,16,35,98,141,192,251,318,392,475,565,663,769,881) #kN
Input.Power <- c(0,11,38,178,308,489,729,1038,1425,1896,2462,3130,3909,4796)/1000 #MW
Diam <- 3.8 #m

setEPS()
postscript("Figure 3.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = Del.Thrust, y = Input.Power, pch = 19, 
     xlim = c(0,2500), ylim = c(0,15), xaxs = 'i', yaxs = 'i', las = 1,
     xlab = "Delivered Thrust [kN]", ylab = "Dynamic Positioning Power Requirement [MW]")
segments(x0 = max(Del.Thrust), y0 = 0, x1 = max(Del.Thrust), y1 = max(Input.Power), 
         lty = 2, lwd = 3, col = 'red')
segments(x0 = max(Del.Thrust)*2, y0 = 0, x1 = max(Del.Thrust)*2, y1 = DP.Power.n(max(Del.Thrust)*2,Diam, n = 2), 
         lty = 2, lwd = 3, col = 'red')

curve(DP.Power.n(x,Diam), from = 0, to = max(Del.Thrust), add = TRUE, lwd = 2)
curve(DP.Power.n(x,Diam, n = 2), from = 0, to = max(Del.Thrust)*2, add = TRUE, 
      lwd = 2, col = '#eecc16')
# curve(DP.Power.n(x,Diam, n = 3), from = 0, to = max(Del.Thrust)*3, add = TRUE, 
#       lwd = 2, col = '#0000a7')
curve(DP.Power.n(x,Diam, n = 6), from = 0, to = max(Del.Thrust)*6, add = TRUE, 
      lwd = 2, col = '#008176')
points(x = Del.Thrust, y = Input.Power, pch = 1, cex = 1.2)
text(x = 2250, y = 10, "6-Thruster \nConfiguration", col = '#008176')
text(x = 1250, y = 8, "2-Thruster \nConfiguration", col = '#eecc16')
text(x = 250, y = 4, "1-Thruster \nConfiguration", col = 'black')
text(x = max(Del.Thrust)+10, y=0.1, "1-Thruster \nConfiguration \nMax Thrust", col = 'red',
     adj = c(0,0), cex = 0.8)

text(x = max(Del.Thrust)*2+10, y=0.1, "2-Thruster \nConfiguration \nMax Thrust", col = 'red',
     adj = c(0,0), cex = 0.8)
legend("topleft", legend = c("Given Manufacturer Data Points","Functional Representation of Power v. Thrust"), 
       col = 'black', pch = c(19,NA), lty = c(NA,1), lwd = c(NA,2),
       cex = 1, bty = 'n')
dev.off()


#################################################
# OrcaFlex Validation

my_DF <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/OrcaFlex/OrcaFlex Validation Data.csv") %>%
  rename(speeds = ï..speeds)

setEPS()
postscript("Figure S3.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
plot(x = my_DF$speeds, y = my_DF$Link_1, pch = 15, ylim = c(0,1000),
     xaxs = 'i', yaxs = 'i', col = 'black', las = 1,
     xlab = 'Wind Speed [m/s]', ylab = 'Turbine Thrust Force [kN]')
points(x = my_DF$speeds, y = my_DF$DLC1.3, pch = 19, col = palette()[6], cex = 0.5)
legend('topright', legend = c("Authors' R Model Results",
                              "OrcaFlex Steady-State Results"),
       pch = c(19,15), col = c(palette()[6],'black'), bty = 'n')
dev.off()
