#SUM Forces


R.Vector <- function(Wind.V, Wave.V, Current.V) { #all (X,Y) vectors
  Result.V <- Wind.V + Wave.V + Current.V
  Vector.length <- sqrt(Result.V[1]^2 + Result.V[2]^2)
  angle <- atan(Result.V[2]/Result.V[1])*180/pi
  return(c(Vector.length,angle))
}

#DLC 1.3: Design Levels (single windspeed)
Wind <- c(DP.Turbine.Wind.Force(wind = 10.59),0)
Current <- c(9.7*cos(7*pi/4), 9.7*sin(7*pi/4))
Wave.low <- c(-45,0)
Wave.high <- c(50,0)

V.1.3.low <- R.Vector(Wind, Wave.low, Current)
V.1.3.high <- R.Vector(Wind, Wave.high, Current)

#DLC 6.1: Design Levels (single windspeed)
WIND <- round(as.numeric(DP.Turbine.Wind.Force(wind = 38)),0) #This function is in "20210615 Blade and Conbined Forces.R"
Wind2 <- c(WIND,0)
CUR <- round(Current_Force(speed = 0.9, eff.area = f.area.water, Cd = Cd_cyl)/1000,0) #this function and these parameters are in "Dynamic Positioning Forces 20210309.R"
Current2 <- c(CUR*cos(7*pi/4), CUR*sin(7*pi/4))

phi.wave <- seq(-90,90,by = 10)
phi.rad <- phi.wave*(pi/180)

Wave.low2.mag <- 8.2
Wave.high2.mag <- 10
V.6.1.low <- c()
for(phi in phi.rad) {
  Wave.low2 <- c(Wave.low2.mag*cos(phi), Wave.low2.mag*sin(phi))
  Low.F <- R.Vector(Wind2, Wave.low2, Current2)
  V.6.1.low <- rbind(V.6.1.low, Low.F)
}

V.6.1.low <- cbind(phi.wave, V.6.1.low) %>%
  as_tibble() %>%
  rename(wind.wave.angle = phi.wave, force.kN =  V2,final.angle =  V3)

V.6.1.high <- c()
for(phi in phi.rad) {
  Wave.high2 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
  High.F <- R.Vector(Wind2, Wave.high2, Current2)
  V.6.1.high <- rbind(V.6.1.high, High.F)
}

V.6.1.high <- cbind(phi.wave, V.6.1.high) %>%
  as_tibble() %>%
  rename(wind.wave.angle = phi.wave, force.kN =  V2,final.angle =  V3)
(max(V.6.1.high$force.kN))
 #889 kN, still about half of the 1677 kN from the one line on the NREL Paper

################################################################################
speeds <- seq(0,40,0.1)
Wind.V.static <- round(DP.Turbine.Wind.Static(speeds),2) #This function is in Dynamic Positioning Forces 20210309.R
Wind.V.spinning <- round(DP.Turbine.Wind.Spinning(speeds),2) #This function is in Dynamic Positioning Forces 20210309.R
Wind.V.eff <- round(c(DP.Turbine.Wind.Static(speeds[speeds<=3]),
                      DP.Turbine.Wind.Spinning(speeds[speeds>3 & speeds<=25]),
                      DP.Turbine.Wind.Static(speeds[speeds>25])),2)

DP.Power.n <- function(force.kn, motor.diam, n) { #power of n motors system, motor.diam in meters
  power <- (n/motor.diam)*(1000*force.kn/(K*n))^1.5
  return(power/1000) #in MW
}

Min.Force <- c() 
for(i in 1:length(wind.forces.eff)) { #was Wind.V.eff. Use wind.forces.eff from "20210615 Blade and Combined Forces.R"
  Matrix.low <- c()
  for(phi in phi.rad) {
    Wave.low2 <- c(Wave.low2.mag*cos(phi), Wave.low2.mag*sin(phi))
    Wind.low <- c(wind.forces.eff[i],0)
    Low.F <- R.Vector(Wind.low, Wave.low2, Current2)
    Matrix.low <- rbind(Matrix.low, Low.F)
  }
  Min.Force[i] <- min(Matrix.low[,1])
}

Max.Force <- c() 
for(i in 1:length(wind.forces.eff)) {
  Matrix.high <- c()
  for(phi in phi.rad) {
    Wave.high2 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
    Wind.high <- c(wind.forces.eff[i],0)
    High.F <- R.Vector(Wind.high, Wave.high2, Current2)
    Matrix.high <- rbind(Matrix.high, High.F)
  }
  Max.Force[i] <- max(Matrix.high[,1])
}
  
spin.baseline.power <- DP.Power.n(Wind.V.spinning, motor.diam = 0.92, n = 24)
stat.baseline.power <- DP.Power.n(Wind.V.static, motor.diam = 0.92, n = 24)
max.eff.power <- DP.Power.n(Max.Force, motor.diam = 0.92, n = 24)
min.eff.power <- DP.Power.n(Min.Force, motor.diam = 0.92, n = 24)

max.eff.power2 <- DP.Power.n(Max.Force, motor.diam = 1.07, n = 17)
min.eff.power2 <- DP.Power.n(Min.Force, motor.diam = 1.07, n = 17)

#Made on 06/15/2021
max.eff.power <- DP.Power.n(Max.Force, motor.diam = 4.52, n = 5)
min.eff.power2 <- DP.Power.n(Min.Force, motor.diam = 3, n = 3)

#Plot 3
#Uncomment to save the image
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))

curve(dweibull(x, shape = 2.3, scale = 10), from = 0, to = 25, lwd = 3, xaxt = 'n',
      xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
      ylab = "", cex.lab = 1.7, cex.axis = 1.7,
      main = "", col = 'blue', col.axis = 'blue', 
      ylim = c(0, 0.12))
mtext("Probability Density", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)
#Parasitic Consumption Graphs
parasitic.power1 <- DP.Power.n(Max.Force, motor.diam = 1.07, n = 86)
parasitic.power2 <- DP.Power.n(Max.Force, motor.diam = 1.98, n = 21)
parasitic.power3 <- DP.Power.n(Max.Force, motor.diam = 4.52, n = 5)
parasitic.power4 <- DP.Power.n(Max.Force, motor.diam = 3.35, n = 9)

plot(x = speeds, y = parasitic.power1, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(0,20), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
# lines(x = speeds, y = parasitic.power2, lty = 2, lwd = 3)
# lines(x = speeds, y = parasitic.power3, lty = 3, lwd = 3)
lines(x = speeds, y = parasitic.power4, lty = 4, lwd = 3)
abline(h = 15, col = 'black', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Power Requirement from Motors [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topleft', legend = c("n = 86, d = 1.07", 
                             "n = 21, d = 1.98", 
                             "n = 5, d = 4.52",
                             "n = 9, d = 3.35"), 
       col = 'black', lwd = 3, lty = c(1,2,3,4))

# dev.off() #uncomment if saving the image

# legend("topleft", 
#        legend = c("Weibull Wind Speed Distribution (Right Axis)", 
#                   "Thruster Power Requirement Bounds (Left Axis)",
#                   "Approximated Continuous Function Used to calculated Expectation"),
#        col = c('black','black','red'), lty = c(1,1,1), lwd = c(2,1,1), cex = 0.7)

###########################################################
####Calculate the parasitic losses
speeds.pwr3 <- speeds^3
speeds.pwr2 <- speeds^2
Parasitic_DF <- data.frame(speeds,speeds.pwr2,speeds.pwr3,parasitic.power1,parasitic.power2,parasitic.power3)

#REGION 1: [0, 3)
modhigh.reg1 <- lm(Parasitic_DF[speeds<3,"parasitic.power3"] ~ Parasitic_DF[speeds<3,"speeds"] +
                     Parasitic_DF[speeds<3,"speeds.pwr2"] + Parasitic_DF[speeds<3,"speeds.pwr3"])
#REGION 2: [3, 10.59)
modhigh.reg2 <- lm(Parasitic_DF[speeds>=3 & speeds<=10.59,"parasitic.power3"] ~ Parasitic_DF[speeds>=3 & speeds<=10.59,"speeds"] +
                     Parasitic_DF[speeds>=3 & speeds<=10.59,"speeds.pwr2"] + Parasitic_DF[speeds>=3 & speeds<=10.59,"speeds.pwr3"])
#REGION 3: [10.59, 25)
modhigh.reg3 <- lm(Parasitic_DF[speeds>10.59 & speeds<=25,"parasitic.power3"] ~ Parasitic_DF[speeds>10.59 & speeds<=25,"speeds"] +
                     Parasitic_DF[speeds>10.59 & speeds<=25,"speeds.pwr2"] + Parasitic_DF[speeds>10.59 & speeds<=25,"speeds.pwr3"])
#REGION 4: [25, 40]
modhigh.reg4 <- lm(Parasitic_DF[speeds>25,"parasitic.power3"] ~ Parasitic_DF[speeds>25,"speeds"] +
                     Parasitic_DF[speeds>25,"speeds.pwr2"] + Parasitic_DF[speeds>25,"speeds.pwr3"])

# modlow <- lm(Parasitic_DF[speeds>=3 & speeds<=25,"min.eff.power2"] ~ Parasitic_DF[speeds>=3 & speeds<=25,"speeds.pwr"])
# modlow2 <- lm(Parasitic_DF[speeds>25,"min.eff.power2"] ~ Parasitic_DF[speeds>25,"speeds.pwr"])
# 
# myfunlow <- function(x) {
#   prob <- dweibull(x, shape = 2.3, scale = 10)
#   pwr <- coef(modlow)[1] + coef(modlow)[2]*(x^3)
#   return(prob*pwr)
# }
myfunhigh1 <- function(x) { #x is windspeed
  prob <- dweibull(x, shape = 2.3, scale = 10)
  pwr <- as.numeric(coef(modhigh.reg1)[1] + coef(modhigh.reg1)[2]*x + coef(modhigh.reg1)[3]*x^2 + coef(modhigh.reg1)[4]*x^3)
  return(prob*pwr)
}
myfunhigh2 <- function(x) { #x is windspeed
  prob <- dweibull(x, shape = 2.3, scale = 10)
  pwr <- as.numeric(coef(modhigh.reg2)[1] + coef(modhigh.reg2)[2]*x + coef(modhigh.reg2)[3]*x^2 + coef(modhigh.reg2)[4]*x^3)
  return(prob*pwr)
}
myfunhigh3 <- function(x) { #x is windspeed
  prob <- dweibull(x, shape = 2.3, scale = 10)
  pwr <- as.numeric(coef(modhigh.reg3)[1] + coef(modhigh.reg3)[2]*x + coef(modhigh.reg3)[3]*x^2 + coef(modhigh.reg3)[4]*x^3)
  return(prob*pwr)
}
myfunhigh4 <- function(x) { #x is windspeed
  prob <- dweibull(x, shape = 2.3, scale = 10)
  pwr <- as.numeric(coef(modhigh.reg4)[1] + coef(modhigh.reg4)[2]*x + coef(modhigh.reg4)[3]*x^2 + coef(modhigh.reg4)[4]*x^3)
  return(prob*pwr)
}

# parasitic.loss.low <- 8760*integrate(myfunlow, lower = 0, upper = 40)$value
parasitic.loss.high <- 8760*(integrate(myfunhigh1, lower = 0, upper = 9)$value + 
                               integrate(myfunhigh2, lower = 9, upper = 10.59)$value + 
                               integrate(myfunhigh3, lower = 10.59, upper = 25)$value +
                               integrate(myfunhigh4, lower = 25, upper = 40)$value) #in MWh
    
##########################################################################
#Develop the effective DP turbine power curve vector

DP.Raw.Power <- sapply(speeds,Power.Curve) #Power.Curve is in "Dynamic Positioning Forces 20210309 - Debugging.R"
DP.Effective.Power <- DP.Raw.Power - parasitic.power3

#Plot 3
#Uncomment to save the image
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))

curve(dweibull(x, shape = 2.3, scale = 10), from = 0, to = 25, lwd = 3, xaxt = 'n',
      xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
      ylab = "", cex.lab = 1.7, cex.axis = 1.7,
      main = "Thrusters: n = 5, d = 4.52m, T = 1337kN, C = $80 MM", col = 'blue', col.axis = 'blue', 
      ylim = c(0, 0.12))
mtext("Probability Density", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)
#Effective Power Curve

plot(x = speeds, y = DP.Effective.Power, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-5,20), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power, col = 'green')
lines(speeds, parasitic.power3, col = 'red', lty = 2)
curve(-1*(coef(modhigh.reg1)[1] + coef(modhigh.reg1)[2]*x + coef(modhigh.reg1)[3]*x^2 + coef(modhigh.reg1)[4]*x^3), from = 0, to = 40, add = TRUE)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topleft', legend = c("n = 86, d = 1.07", "n = 21, d = 1.98", "n = 5, d = 4.52"), 
       col = 'black', lwd = 3, lty = c())

# dev.off() #uncomment if saving the image
################################################################################
library(FinancialMath)
#Sensitivity Plots
PPA <- 58 #$/MWh
#Windspeed
winds <- seq(38,50,by = 2)
wind.capex <- c(12.1,12.7,13.9,14.5,15.7,16.9,18.1)
wind.parasitic <- c(5300,5160,4930,4826,4637,4469,4317) #all based on different number of thrusters
wind.parasitic.CF <- wind.parasitic*PPA/1000000
wind.parasitic.PV <- c()
for(i in 1:length(wind.parasitic.CF)){
  wind.parasitic.PV[i] <- NPV(0,rep(wind.parasitic.CF[i],times = 25),times = seq(1,25),i=0.08)
}
wind.total <- wind.capex + wind.parasitic.PV

plot(x = winds, y = wind.total, type = 'b', pch = 17, col = 'red', cex = 2,
     ylim = c(0,22), las = 1, xaxs = 'i', yaxs = 'i', cex.lab = 1.7, cex.axis = 1.7,
     xlab = 'Wind speed [m/s]',
     ylab = '25-year Present Cost [Million USD]')
lines(x = winds, y = wind.capex, type = 'b', pch = 15, col = 'blue', cex = 2)
lines(x = winds, y = wind.parasitic.PV, type = 'b', pch = 15, col = 'green', cex = 2)
legend('topleft', legend = c("Total","CAPEX","Monetized Parasitic Losses"),
       lty = 1, pch = c(17,15,15), col = c('red','blue','green'),cex = 1)

#Current Speed
currents <- seq(0.5,1.5,by = 0.1)
current.capex <- c(10.3,10.9,10.9,11.5,12.1,12.1,12.7,13.3,14.5,15.1,15.7)
current.parasitic.20 <- c(1788,2362,3102,4085,5300,6765,8584,10705,13150,16030,19339)
current.parasitic <- c(1940,2490,3270,4191,5300,6765,8377,10207,12005,14339,16961)
current.parasitic.20.CF <- current.parasitic.20*PPA/1000000
current.parasitic.CF <- current.parasitic*PPA/1000000
current.parasitic.20.PV <- c()
for(i in 1:length(current.parasitic.20.CF)){
  current.parasitic.20.PV[i] <- NPV(0,rep(current.parasitic.20.CF[i],times = 25),times = seq(1,25),i=0.08)
}
current.total.20 <- current.capex + current.parasitic.20.PV

current.parasitic.PV <- c()
for(i in 1:length(current.parasitic.CF)){
  current.parasitic.PV[i] <- NPV(0,rep(current.parasitic.CF[i],times = 25),times = seq(1,25),i=0.08)
}
current.total <- current.capex + current.parasitic.PV

plot(x = currents, y = current.total, type = 'b', pch = 17, col = 'red', cex = 2,
     ylim = c(0,28), las = 1, xaxs = 'i', yaxs = 'i', cex.lab = 1.7, cex.axis = 1.7,
     xlab = 'Current speed [m/s]',
     ylab = '25-year PV of cost [Million USD]')
lines(x = currents, y = current.capex, type = 'b', pch = 15, col = 'blue', cex = 2)
lines(x = currents, y = current.parasitic.PV, type = 'b', pch = 15, col = 'green', cex = 2)
legend('topleft', legend = c("Total","CAPEX","Monetized Parasitic Losses"),
       lty = 1, pch = c(17,15,15), col = c('red','blue','green'),cex = 1)

#Wave Force
waves <- seq(10,50,by = 10)
wave.parasitic <- c(5300,5600,5900,6252,6600)
wave.capex <- rep(12.1,times = length(waves))
wave.parasitic.CF <- wave.parasitic*PPA/1000000
wave.parasitic.PV <- c()
for(i in 1:length(wave.parasitic.CF)){
  wave.parasitic.PV[i] <- NPV(0,rep(wave.parasitic.CF[i],times = 25),times = seq(1,25),i=0.08)
}
wave.total <- wave.capex + wave.parasitic.PV

plot(x = waves, y = wave.total, type = 'b', pch = 17, col = 'red', cex = 2,
     ylim = c(0,25), las = 1, xaxs = 'i', yaxs = 'i', cex.lab = 1.7, cex.axis = 1.7,
     xlab = 'Total Wave Force Magnitude [kN]',
     ylab = '25-year PV of cost [Million USD]')
lines(x = waves, y = wave.capex, type = 'b', pch = 15, col = 'blue', cex = 2)
lines(x = waves, y = wave.parasitic.PV, type = 'b', pch = 15, col = 'green', cex = 2)
legend('topleft', legend = c("Total","CAPEX","Monetized Parasitic Losses"),
       lty = 1, pch = c(17,15,15), col = c('red','blue','green'),cex = 1)

# Number of Thrusters
thrusters <- seq(20,30,by = 2)
thruster.capex <- c(12.1,13.3,14.5,15.7,16.9,18.1)
thruster.capex.itc <- thruster.capex*0.82
thruster.opex <- c(3.3,3.1,3.0,2.9,2.8,2.7)
thruster.opex.ptc <- c(4.0,3.8,3.6,3.5,3.4,3.2)

thruster.total <- thruster.capex + thruster.opex

thruster.total.ITC <- thruster.capex.itc + thruster.opex
thruster.total.PTC <- thruster.capex + thruster.opex.ptc

plot(x = thrusters, y = thruster.total, type = 'b', pch = 17, col = 'red', cex = 2,
     ylim = c(0,25), las = 1, xaxs = 'i', yaxs = 'i', cex.lab = 1.7, cex.axis = 1.7,
     xlab = 'Number of Thrusters',
     ylab = '25-year PV of cost [Million USD]')
lines(x = thrusters, y = thruster.capex, type = 'b', pch = 15, col = 'blue', cex = 2)
lines(x = thrusters, y = thruster.opex, type = 'b', pch = 15, col = 'green', cex = 2)
legend('topleft', legend = c("Total","CAPEX","Monetized Parasitic Losses"),
       lty = 1, pch = c(17,15,15), col = c('red','blue','green'),cex = 1)

plot(x = thrusters, y = thruster.total.ITC, type = 'b', pch = 17, col = 'red',
     ylim = c(0,25),
     xlab = 'Number of Thrusters',
     ylab = '25-year PV of cost [Million USD]')
lines(x = thrusters, y = thruster.total.PTC, type = 'b', pch = 17, col = 'blue')
legend('topleft', legend = c("PV of Total Cost with ITC (no PTC)","PV of Total Cost with PTC (no ITC)"),
       lty = 1, pch = c(17,17), col = c('red','blue'),cex = 0.8)

