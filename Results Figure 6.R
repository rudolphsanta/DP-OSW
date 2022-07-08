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
postscript("Figure 6.eps", width = 7.5, height = 5)
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
text(x=0.1,y=ATB14.low,"NREL ATB Range \nWind Speed Class 14", col = 'red',adj=c(0,1))
text(x=0.1,y=ATB8.low,"NREL ATB Range \nWind Speed Class 8", col = 'blue',adj=c(0,1))
text(x=9, y = 202, "Model Results \nWind Speed Class 14", col = 'red')#, adj = c(1,1))
text(x=9, y = 110, "Model Results \nWind Speed Class 8", col = 'blue')#, adj = c(0,1))

dev.off()
