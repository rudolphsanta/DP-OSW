# Rudolph Santarromana
# 5 MW Turbine
# June 2021

library(tidyverse)
library(dplyr)

setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")
## Source: Jonkman et al. (2009): Definition of a 5-MW Reference Wind Turbine for Offshore System Development

#################################################
### PARAMETERS
wind_height <- 100 #measuring height of wind speed
speeds <- seq(0,40, by = 0.1)
Cd.factor <- 1 #0.6 was used in OrcaFlex
Cd_cyl <- 0.5*Cd.factor

#WEIBULL PARAMETERS
P.shape = 2 #2.3
P.scale = 10.82 #7.95,10,10.82

### WIND TURBINE PROPERTIES
cut.in5 <- 3 #[m/s]
rated.wind5 <- 11.4 #[m/s]
cut.out5 <- 25 #[m/s]
rated.power5 <- 5 #MW

### FLOATER PROPERTIES. SAME AS 15 MW Floater
scale5.15 <- 1 #0.6 #based on hub height scale (90/150)
F.freeboard5 <- 15*scale5.15 #same floater as 15MW turbine for now
F.diam5 <- 12.5*scale5.15
F.draft5 <- 20*scale5.15
FT.diam5 <- 10*scale5.15

#Effective water-exposed floater area (whole substructure)
f.area.water5 <- (F.diam5*F.draft5*3) + (F.draft5*FT.diam5) #m^2

###TOWER PROPERTIES
#Tower dimensions
hub.height5 <- 90 #m

t.heights5 <- c(0,87.6) #m
t.diams5 <- c(6,3.87) #m

t.mod5 <- lm(t.diams5 ~ t.heights5) #Tower diameter prediction model
t.fun5 <- function(x) as.numeric(predict(t.mod5, newdata = data.frame(t.heights5 = x))) #tower diameter function given height

##PLOT of tower diameter model
plot(x = t.heights5, y = t.diams5, pch = 19, col = 'red')
lines(x = t.heights5, y = predict(t.mod5, newdata = data.frame(t.heights5 = t.heights5)))

## TOWER FORCE FUNCTION
t.fun5.force <- function(x) t.fun5(x - F.freeboard5)*((x)^(2*alpha)) #shifts the tower upward to the floater top height
Wind_Force_Tower5 <- function(v.meas, h.meas = wind_height, Cd = Cd_cyl, base = F.freeboard5) {
  if(base < 0) return(0)
  rho_air <- 1.23 #kg/m^3
  alpha <- 0.14
  A <- rho_air*Cd*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  #integrate from the tower base (at the top of the floater) to the hub height + floater height
  C <- integrate(t.fun5.force, lower = F.freeboard5, upper = hub.height5 + F.freeboard5)$value
  force <- (A/B)*C
  return(force) #in Newtons
}
Wind_Force_Tower5(10)/1000
Wind_Force_Tower10(10)/1000
################################################################################

## STATIC, ARRESTED BLADE FORCES
#Blades with varying diameter and Cd, and wind with varying speed
chord5 <- c(3.54,3.85,4.17,4.56,4.65,4.46,4.25,4.0,3.75,3.5,3.26,3.01,2.76,2.52,2.31,2.09,1.42)
thick.chord.rat5 <- c(1,1,1,rep(0.18, times = 14))

thickness5 <- chord5*thick.chord.rat5#[m]
Cd.vect5 <-   Cd.factor*c(0.5,0.5,0.36,0.0113,0.0094,0.0094,0.0087,0.0065,0.0065,0.0057,0.0057,0.0052,0.0052,0.0052,0.0052,0.0052,0.0052) #[-]
dist.vect5 <- c(0,2.87,5.6,8.33,11.75,15.85,19.95,24.05,28.15,32.25,36.35,40.45,44.55,48.65,52.75,56.16,58.9,61.6) #[m]

Wind_Force_Blade5 <- function(v.meas, h.meas = wind_height, l.elev = hub.height5) {
  alpha <- 0.14
  rho_air <- 1.23 #kg/m^3
  
  A <- rho_air*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  C <- 0
  
  for (i in 1:length(thickness5)){
    fun <- function(h) return(h^(2*alpha)*thickness5[i]*Cd.vect5[i])
    component <- integrate(fun, lower = l.elev + dist.vect5[i], upper = l.elev + dist.vect5[i+1])$value
    C <- C + component
  }
  force <- (A/B)*C
  return(force) #in Newtons
}

B.Force.static5 <- Wind_Force_Blade5(speeds)/1000 #in [kN]
max(B.Force.static5)
max(B.Force.static15)
################################################################################
##ROTOR THRUST FORCE
# Recreate Fig 9.1 in 5 MW Reference turbine document (Jonkman et al., 2009)
#REGION 1: 3 - 11.4
X.R1_5 <- c(3,7,11.4) #m/s of windspeed
Y.R1_5 <- c(179.5,410,820) #kN of thrust

X.R1.2_5 <- X.R1_5^2
X.R1.3_5 <- X.R1_5^3

mod.R1_5 <- lm(Y.R1_5 ~ X.R1_5 + X.R1.2_5)

#REGION 2: 11.4 - 25
X.R2_5 <- c(11.4,14,25) #m/s of windspeed
Y.R2_5 <- c(820,564,385) #kN of thrust

X.R2.1_5 <- X.R2_5^-1
X.R2.2_5 <- X.R2_5^-2
X.R2.3_5 <- X.R2_5^3

mod.R2_5 <- lm(Y.R2_5 ~ X.R2.1_5 + X.R2.2_5)

#plot of how model fits the data
plot(X.R1_5,Y.R1_5, pch = 19, col = 'red', xlim = c(3,25),
     xlab = "Wind Speed [m/s]", ylab = "Swept Area Thrust Force [kN]")
curve(mod.R1_5$coefficients[1] + mod.R1_5$coefficients[2]*x + mod.R1_5$coefficients[3]*x^2, 
      from = cut.in5, to = rated.wind5, add = TRUE)
points(X.R2_5,Y.R2_5, pch = 19, col = 'red')
curve(mod.R2_5$coefficients[1] + mod.R2_5$coefficients[2]*x^-1 + mod.R2_5$coefficients[3]*x^-2, #+ mod.R2$coefficients[4]*x^3,
      from = rated.wind5, to = cut.out5, add = TRUE)

Wind_Blade_Force_spinning5 <- function(wind, IN = cut.in5) { #windspeed in m/s as input only defined on [3,25] interval
  if(wind < IN | wind > cut.out5) force <- 0
  if(wind >= IN && wind <= rated.wind5) force <- as.numeric(mod.R1_5$coefficients[1] + mod.R1_5$coefficients[2]*wind + mod.R1_5$coefficients[3]*wind^2)
  if(wind > rated.wind5 && wind <= cut.out5) force <- as.numeric(mod.R2_5$coefficients[1] + mod.R2_5$coefficients[2]*wind^-1 + mod.R2_5$coefficients[3]*wind^-2)
  return(force) #returns a value in kN
}

Wind_Blade_Force_spinning5(25) 

#Total wind force on the turbine assuming same tower and substructure, only the RNA has changed.
#functions in this function are contained in "Dynamic Positioning Forces 20210309 - Debugging"
DP.Turbine.Wind.Force5 <- function(wind, eff.cut.in = cut.in5) { 
  #non-blade components
  T.Force.best <- Wind_Force_Tower5(v.meas = wind)/1000 #in kN
  #Same Floater as the 15-MW turbine
  F.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = floater.diam, u.elev = floater.freeboard)/1000 #in [kN]
  FT.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = tower.diam, u.elev = floater.freeboard)/1000 #in [kN]
  
  #blade spinning component
  B.Force.best.spinning <- Wind_Blade_Force_spinning5(wind, IN = eff.cut.in) #in kN. returns 0 when blades are static
  
  #blade static component. Uses 5 MW blade
  if(wind < eff.cut.in | wind > cut.out5) B.Force.static5 <- Wind_Force_Blade5(wind)/1000  else B.Force.static5 <- 0 #in [kN]. returns 0 when blades are spinning
  
  #Blade Total Forces vs. windspeed
  Tot.Wind.Force <- T.Force.best + 
    (3*B.Force.static5) + 
    B.Force.best.spinning + 
    (3*F.Force.height) + 
    FT.Force.height
  
  return(Tot.Wind.Force)
}
Wind.Forces5 <- sapply(speeds,DP.Turbine.Wind.Force5)

DP.Turbine.Wind.Force5(10) #693 kN, compare to 700 kN calculated by Max
DP.Turbine.Wind.Force10(10)

plot(x = speeds, y = Wind.Forces5, type = 'l')

F.Current.Force5 <- function(current) {
  #Current_Force function is in 15MW turbine file
  F.Force.Current <- Current_Force(speed = current, f.area.water5,Cd_cyl)/1000 #in kN
  return(F.Force.Current)
}

################################################################################
### COMBINED ENVIRONMENTAL FORCES
# Considers 15 MW Floater and Tower
#DLC 1.3: Design Levels (single windspeed)
Wind_5 <- c(DP.Turbine.Wind.Force5(wind = rated.wind5),0)
Current_5 <- c(F.Current.Force5(0.2)*cos(7*pi/4), F.Current.Force5(0.2)*sin(7*pi/4))
Wave.high_5 <- c(50,0)

# V.1.3.low <- R.Vector(Wind, Wave.low, Current)
V.1.3.high_5 <- R.Vector(Wind_5, Wave.high_5, Current_5)

## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DLC1.3_5.fun <- function(wind) {
  force1.3_5 <- c()
  Current_5 <- c(F.Current.Force5(0.2)*cos(7*pi/4), F.Current.Force5(0.2)*sin(7*pi/4))
  Wave.high_5 <- c(50,0)
  for(i in 1:length(wind)){
    Wind_5 <- c(DP.Turbine.Wind.Force5(wind = wind[i]),0)
    force1.3_5 <- c(force1.3_5, R.Vector(Wind_5, Wave.high_5, Current_5)[1])
  }
  return(force1.3_5)
}

DLC1.3_5 <- DLC1.3_5.fun(speeds) #RESULTING FORCES UNDER DLC1.3
(max(DLC1.3_5))

#DLC 6.1: Design Levels (single windspeed)
WIND_5 <- round(as.numeric(DP.Turbine.Wind.Force5(wind = 38)),0)
Wind2_5 <- c(WIND_5,0)
Current2_5 <- c(F.Current.Force5(0.9)*cos(7*pi/4), F.Current.Force5(0.9)*sin(7*pi/4))

phi.wave <- seq(-90,90,by = 10)
phi.rad <- phi.wave*(pi/180)

Wave.high2.mag <- 10 #magnitude of the total wind force. calculated in "Wave Drift Forces.R"

V.6.1.high_5 <- c()
for(phi in phi.rad) {
  Wave.high2_5 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
  High.F_5 <- R.Vector(Wind2_5, Wave.high2_5, Current2_5)
  V.6.1.high_5 <- rbind(V.6.1.high_5, High.F_5)
}

V.6.1.high_5 <- cbind(phi.wave, V.6.1.high_5) %>%
  as_tibble() %>%
  rename(wind.wave.angle = phi.wave, force.kN = V2,final.angle =  V3)
(max(V.6.1.high_5$force.kN))

## Given a windspeed, this function calculates the resultant force under DLC6.1 conditions
DLC6.1_5.fun <- function(wind) {
  force6.1_5 <- c() 
  Current2_5 <- c(F.Current.Force5(0.9)*cos(7*pi/4), F.Current.Force5(0.9)*sin(7*pi/4))
  
  Wave.high2.mag <- 10 #the total magnitude of the force in kN
  phi.wave <- seq(-90,90,by = 10)
  phi.rad <- phi.wave*(pi/180)
  
  for(i in 1:length(wind)) {
    Matrix.high_5 <- c()
    for(phi in phi.rad) {
      Wave.high2_5 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
      Wind.high_5 <- c(DP.Turbine.Wind.Force5(wind = wind[i]),0)
      High.F_5 <- R.Vector(Wind.high_5, Wave.high2_5, Current2_5)
      Matrix.high_5 <- rbind(Matrix.high_5, High.F_5)
    }
    force6.1_5 <- c(force6.1_5,max(Matrix.high_5[,1]))
  }
  return(force6.1_5)
}

DLC6.1_5 <- DLC6.1_5.fun(speeds)  #RESULTING FORCES UNDER DLC6.1
(tail(DLC6.1_5,1))

F.DF <- data.frame(speeds,DLC1.3_5,DLC6.1_5)
writexl::write_xlsx(F.DF, "20211018_5 MW Calculations.xlsx")

### Total Environmental Force Plot
plot(x = speeds, y = DLC6.1_5, type = 'l',
     ylim = c(0,1000), yaxs = 'i', xaxs = 'i',
     ylab = "Turbine Thrust Force [kN]",
     xlab = "Wind Speed [m/s]")
lines(x = speeds, y = DLC1.3_5, col = 'blue')
abline(h = 700)
abline(v = 15)

DLC6.1_5.fun(40) #602 kN
DLC1.3_5.fun(11.4) #904 kN. Design at this level

## Calculate Parasitic Power under DLC1.3, which depends on the thruster system
parasitic.power5.1 <- DP.Power.n(DLC1.3_5, motor.diam = 1.07, n = 28)
parasitic.power5.2 <- DP.Power.n(DLC1.3_5, motor.diam = 1.98, n = 8)
parasitic.power5.3 <- DP.Power.n(DLC1.3_5, motor.diam = 4.52, n = 3) #3
parasitic.power5.4 <- DP.Power.n(DLC1.3_5, motor.diam = 3.91, n = 3)
parasitic.power5.3.1 <- DP.Power.n(DLC1.3_5, motor.diam = 4.52, n = 1)
parasitic.power5.4.1 <- DP.Power.n(DLC1.3_5, motor.diam = 3.91, n = 1)

parasitic.power5 <- DP.Power.n(DLC1.3_5, motor.diam = 3.8, n = 2)

## PLOT
# png(filename = "20210616 Parasitic Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))

curve(dweibull(x, shape = P.shape, scale = P.scale), from = 0, to = 25, lwd = 3, xaxt = 'n',
      xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
      ylab = "", cex.lab = 1.7, cex.axis = 1.7,
      main = "", col = 'blue', col.axis = 'blue', 
      ylim = c(0, 0.12))
mtext("Probability Density", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)

plot(x = speeds, y = parasitic.power5.1, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(0,P.scale), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
lines(x = speeds, y = parasitic.power5.2, lty = 2, lwd = 3)
lines(x = speeds, y = parasitic.power5.3, lty = 3, lwd = 3)
lines(x = speeds, y = parasitic.power5.4, lty = 4, lwd = 3)
abline(h = 5, col = 'black', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Power Requirement from Motors [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topright', title = "Thruster Specs",
       legend = c("d = 1.07, T = 57 kN, P = 0.3 MW; N = 28 ($16.9MM)", 
                  "d = 1.98, T = 244 kN, P = 1.25 MW; N = 8 ($20.1MM)", 
                  "d = 4.52, T = 1281 kN, P = 7.5 MW; N = 3 ($45.3MM)",
                  "d = 3.91, T = 789 kN, P = 4.2 MW; N = 3 ($25.3MM)"), 
       col = 'black', lwd = 3, lty = c(1,2,3,4))

# dev.off() #uncomment if saving the image

######Power Curve for 5 MW Turbine
# Quadratic Region
x <- c(3,7,10,11.4)
y <- c(0,1.2,3.46,5) #in [MW]

power.mod5 <- lm(y ~ poly(x, 2))

Power.Curve.5 <- function(wind.speed) { #calculate the turbine's 'raw' power output for each wind speed
  if (wind.speed < cut.in5) 0
  else if (wind.speed <= rated.wind5) as.double(predict(power.mod5, newdata = data.frame(x=wind.speed)))
  # coef(power.mod)[1] + coef(power.mod)[2]*wind.speed + coef(power.mod)[3]*wind.speed^2
  else if (wind.speed <= cut.out5) 5
  else 0
}

##########################################################################
#Develop the effective DP turbine power curve vector
DP.Raw.Power5 <- sapply(speeds,Power.Curve.5)
DP.Effective.Power5 <- DP.Raw.Power5 - parasitic.power5.3 #5.3 looked to be the best

##PLOT
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))

curve(dweibull(x, shape = P.shape, scale = P.scale), from = 0, to = 25, lwd = 3, xaxt = 'n',
      xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
      ylab = "", cex.lab = 1.7, cex.axis = 1.7,
      main = "Thrusters: n = 5, d = 4.52m, T = 1337kN, C = $80 MM", col = 'blue', col.axis = 'blue', 
      ylim = c(0, 0.12))
mtext("Probability Density", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)
#Effective Power Curve

plot(x = speeds, y = DP.Effective.Power5, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-2,8), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power5, col = 'green')
lines(speeds, parasitic.power5.3, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")


# dev.off() #uncomment if saving the image

##PLOT: Showing Power Ratio
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))
plot(x = speeds, y = parasitic.power5.3/DP.Raw.Power5, type = 'l', lwd = 3, 
     xaxt = 'n', xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
     ylab = "", cex.lab = 1.7, cex.axis = 1.7,
     main = "", col = 'blue', col.axis = 'blue',
     ylim = c(0, 1))
mtext("Power Ratio", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)

plot(x = speeds, y = DP.Effective.Power5, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-2,8), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power5, col = 'green')
lines(speeds, parasitic.power5.3, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topright', legend = c("Power Ratio (DP Consumption/Power Generated)", "Net Power", "Generated Power", "Thruster Power Consumption"),
       lty = c(1,1,1,2), col = c('blue','black','green','red'), lwd = c(3,3,1,1))

## PLOT Parasitic Losses and Power Curve
# png(filename = "20210616 Parasitic Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))

plot(speeds, DP.Raw.Power5, type = 'l', lwd = 2,
     xaxt = 'n',xlab = "Wind Speed [m/s]", 
     xaxs = 'i', yaxs = 'i', las = 1,
     ylab = "", cex.lab = 1.7, cex.axis = 1.7,
     main = "", col = 'blue', col.axis = 'blue', 
     ylim = c(0, 8))
mtext("Raw Power Generation [MW]", side = 2, line = 3, cex = 1.7, col = "blue")

par(new = TRUE)

plot(x = speeds, y = parasitic.power5.1, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(0,8), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
lines(x = speeds, y = parasitic.power5.2, lty = 2, lwd = 3)
lines(x = speeds, y = parasitic.power5.3, lty = 3, lwd = 3)
lines(x = speeds, y = parasitic.power5.4, lty = 4, lwd = 3)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Power Requirement from Motors [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topright', title = "Thruster Specs",
       legend = c("d = 1.07, T = 57 kN, P = 0.3 MW; N = 28 ($16.9MM)", 
                  "d = 1.98, T = 244 kN, P = 1.25 MW; N = 8 ($20.1MM)", 
                  "d = 4.52, T = 1281 kN, P = 7.5 MW; N = 3 ($45.3MM)",
                  "d = 3.91, T = 789 kN, P = 4.2 MW; N = 3 ($25.3MM)"), 
       col = 'black', lwd = 3, lty = c(1,2,3,4))

## PLOT Parasitic Losses and Power Curve v2
# png(filename = "20210616 Parasitic Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))

plot(speeds, DP.Raw.Power5, type = 'l', lwd = 2,
     xaxt = 'n',xlab = "Wind Speed [m/s]", 
     xaxs = 'i', yaxs = 'i', las = 1,
     ylab = "", cex.lab = 1.7, cex.axis = 1.7,
     main = "", col = 'blue', col.axis = 'blue', 
     ylim = c(0, 8))
mtext("Raw Power Generation [MW]", side = 2, line = 3, cex = 1.7, col = "blue")

par(new = TRUE)

plot(x = speeds, y = parasitic.power5.3, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(0,8), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
lines(x = speeds, y = parasitic.power5.4, lty = 2, col = 'black', lwd = 3)
lines(x = speeds, y = parasitic.power5.3.1, lty = 1, col = 'green', lwd = 3)
lines(x = speeds, y = parasitic.power5.4.1, lty = 2, col = 'green', lwd = 3)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Power Requirement from Motors [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topright', title = "Thruster Specs",
       legend = c("d = 4.52, T = 1281 kN, P = 7.5 MW; N = 3 ($45.3MM)",
                  "d = 3.91, T = 789 kN, P = 4.2 MW; N = 3 ($25.3MM)",
                  "d = 4.52, T = 1281 kN, P = 7.5 MW; N = 1 ($15MM); No Redundancy",
                  "d = 3.91, T = 789 kN, P = 4.2 MW; N = 1 ($8.4MM); No Redundancy"),
       col = c('black','black','green','green'), lwd = 3, lty = c(1,2,1,2), cex = 0.8)

######################## Turn effective power curve into several linear models

#WEIBULL PARAMETERS
P.shape = 2 #2.3
P.scale = 10 #7.95,10,10.82

parasitic.power5 <- DP.Power.n(DLC1.3_5, motor.diam = 3.8, n = 2)

DF_Turbine5 <- data.frame(speeds,DP.Raw.Power5,Parasitic5 = parasitic.power5,DP.Effective.Power5)

#REGION 1: Before cut in wind speed
EFF.mod.R1_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 1), data = filter(DF_Turbine5,speeds <= cut.in5))
#REGION 2: Cut in wind speed until rated wind speed
EFF.mod.R2_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 3), data = filter(DF_Turbine5,speeds > cut.in5 & speeds <= rated.wind5))
#REGION 3: Rated wind speed until cut out wind speed
EFF.mod.R3_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 4), data = filter(DF_Turbine5,speeds > rated.wind5 & speeds <= cut.out5))
#REGION 4: Above cut out wind speed
EFF.mod.R4_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 2), data = filter(DF_Turbine5,speeds > cut.out5))

##PLOT: How good are these models? They look very good.
plot(x = speeds, y = DP.Effective.Power5, pch = 19, cex = 0.01)
lines(x = DF_Turbine5$speeds[speeds <= cut.in5], y = predict(EFF.mod.R1_5, newdata = filter(DF_Turbine5, speeds <= cut.in5)), col = 'red')
lines(x = DF_Turbine5$speeds[speeds > cut.in5 & speeds <= rated.wind5], y = predict(EFF.mod.R2_5, newdata = filter(DF_Turbine5, speeds > cut.in5 & speeds <= rated.wind5)), col = 'red')
lines(x = DF_Turbine5$speeds[speeds > rated.wind5 & speeds <= cut.out5], y = predict(EFF.mod.R3_5, newdata = filter(DF_Turbine5, speeds > rated.wind5 & speeds <= cut.out5)), col = 'red')
lines(x = DF_Turbine5$speeds[speeds > cut.out5], y = predict(EFF.mod.R4_5, newdata = filter(DF_Turbine5, speeds > cut.out5)), col = 'red')

############ Calculate Parasitic losses. What % goes toward thrusters?
#REGION 1: Before cut in wind speed
LOS.mod.R1_5 <- lm(Parasitic5 ~ poly(speeds, 1), data = filter(DF_Turbine5,speeds <= cut.in5))
fun1_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_5, newdata = data.frame(speeds = x)))
#REGION 2: Cut in wind speed until rated wind speed
LOS.mod.R2_5 <- lm(Parasitic5 ~ poly(speeds, 3), data = filter(DF_Turbine5,speeds > cut.in5 & speeds <= rated.wind5))
fun2_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_5, newdata = data.frame(speeds = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_5 <- lm(Parasitic5 ~ poly(speeds, 6), data = filter(DF_Turbine5,speeds > rated.wind5 & speeds <= cut.out5))
fun3_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_5, newdata = data.frame(speeds = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_5 <- lm(Parasitic5 ~ poly(speeds, 2), data = filter(DF_Turbine5,speeds > cut.out5))
fun4_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_5, newdata = data.frame(speeds = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = speeds, y = DF_Turbine5$Parasitic5, pch = 19, cex = 0.01)
lines(x = DF_Turbine5$speeds[speeds <= cut.in5], y = predict(LOS.mod.R1_5, newdata = filter(DF_Turbine5, speeds <= cut.in5)), col = 'red')
lines(x = DF_Turbine5$speeds[speeds > cut.in5 & speeds <= rated.wind5], y = predict(LOS.mod.R2_5, newdata = filter(DF_Turbine5, speeds > cut.in5 & speeds <= rated.wind5)), col = 'red')
lines(x = DF_Turbine5$speeds[speeds > rated.wind5 & speeds <= cut.out5], y = predict(LOS.mod.R3_5, newdata = filter(DF_Turbine10, speeds > rated.wind5 & speeds <= cut.out5)), col = 'red')
lines(x = DF_Turbine5$speeds[speeds > cut.out5], y = predict(LOS.mod.R4_5, newdata = filter(DF_Turbine5, speeds > cut.out5)), col = 'red')

parasitic.loss5 <- 8760*(integrate(fun1_5, lower = 0, upper = cut.in5)$value + 
                            integrate(fun2_5, lower = cut.in5, upper = rated.wind5)$value + 
                            integrate(fun3_5, lower = rated.wind5, upper = cut.out5)$value +
                            integrate(fun4_5, lower = cut.out5, upper = 40)$value) #in MWh
#Result = 10863 MWh (50%)
#Result under 6.2 cut in wind speed = 9922 MWh(46%)

############ Calculate Production losses. What is produced?
#REGION1 = 0
#REGION2
gen.fun2_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod5, newdata = data.frame(x = x)))
#REGION3
gen.fun3_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*rated.power5
#REGION4 = 0

gen5 <- 8760*(integrate(gen.fun2_5, lower = cut.in5, upper = rated.wind5)$value + 
                 integrate(gen.fun3_5, lower = rated.wind5, upper = cut.out5)$value) #in MWh
#RESULT = 21918 MWh
#Result under 6.2 cut in wind speed = 21308 MWh

(parasitic.loss5/gen5)
(gen5)
(parasitic.loss5)

################################################################################
################################################################################
## PART II: CAN WE REDUCE THE LOSSES AT LOW WIND SPEEDS?

## Find where the generation power curve first becomes greater than the parasitic loss curve
## Use models generated (in Region 2) to find this crossover point.
Crossover5 <- function(guess = 5, tolerance = 0.01, step = 0.1) { #guess crossover wind speed
  value <- guess
  cons <- predict(LOS.mod.R2_5, newdata = data.frame(speeds = value))
  prod <- predict(power.mod5, newdata = data.frame(x = value))
  net <- abs(prod - cons)
  while(net > tolerance) {
    value <- value + step
    cons <- predict(LOS.mod.R2_5, newdata = data.frame(speeds = value))
    prod <- predict(power.mod5, newdata = data.frame(x = value))
    net <- abs(prod - cons)
  }
  return(value)
}
eff.cut.in5 <- Crossover5() #6.2 m/s

plot(x = speeds, y = DP.Effective.Power5, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-2,8), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power5, col = 'green')
lines(speeds, parasitic.power5.3, col = 'red', lty = 2)
abline(v = 6.2)
axis(side = 2, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 2, line = 3.5, cex = 1.7, col = "black")

## ADJUST POWER PRODUCTION TO CUT IN ONLY WHEN PRODUCTION IS GREATER THAN LOSSES
## Change Cut in wind speed at very top to 6.2 m/s and run functions again.
## Parasitic Loss Result = 9922 MWh
## Annual Production Result = 21308 MWh 








#FUNCTIONALIZED POWER CURVES (Lines above)
#WEIBULL PARAMETERS
sensitivity_5 <- function(shape, scale, OCC.High, OCC.Low, FOM.High, FOM.Low, disc.r, years = 25, fs = 1.67) {
  P.shape = shape
  P.scale = scale
  Turbine <- 5
  #From NREL ATB 2021
  
  CRF <- 0.049 #capital recovery factor
  PFF <- 1.045 #production finance factor
  CFF <- 1.075 #construction finance factor
  
  mooring.proportion <- 0.11 #percent of project cost that mooring comprises. From James and Ros
  
  #DP Thrusters
  DP_Thrusters <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning/Kongsberg Thruster Specs/Kongsberg Thrusters.csv") %>%
  rename("Name" = "ï..Name")
  
  diameters <- DP_Thrusters$PropellerD_m
  number <- seq(1,30)

  #Initialize Empty Matrices
  MATRIX.lcoe.h <- matrix(nrow = length(number), ncol = dim(DP_Thrusters)[1])
  MATRIX.lcoe.l <- matrix(nrow = length(number), ncol = dim(DP_Thrusters)[1])
  MATRIX.ltcoe <- matrix(nrow = length(number), ncol = dim(DP_Thrusters)[1])
  MATRIX.capex <- matrix(nrow = length(number), ncol = dim(DP_Thrusters)[1])
  MATRIX.loss.ratio <- matrix(nrow = length(number), ncol = dim(DP_Thrusters)[1])
  
  for(TYPE in 1:dim(DP_Thrusters)[1]) { #for each type of DP thruster
    DP_Power <- DP_Thrusters[TYPE,2] #kW
    DP_Diam <- DP_Thrusters[TYPE,3] #m
    DP_Force <- DP_Thrusters[TYPE,5] #kN
    DP_Price <- DP_Thrusters[TYPE,7] #USD
    for(num in number) { #for each number of thrusters
      if(num*DP_Force < max(DLC1.3_5)*fs) next #if this configuration doesn't provide the max needed station keeping force, the configuration does not work, and move to next one
      
      parasitic.power5 <- DP.Power.n(DLC1.3_5, motor.diam = DP_Diam, n = num)
      
      DF_Turbine5 <- data.frame(speeds,DP.Raw.Power5,Parasitic5 = parasitic.power5,DP.Effective.Power5)
      
      #REGION 1: Before cut in wind speed
      EFF.mod.R1_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 1), data = filter(DF_Turbine5,speeds <= cut.in5))
      #REGION 2: Cut in wind speed until rated wind speed
      EFF.mod.R2_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 3), data = filter(DF_Turbine5,speeds > cut.in5 & speeds <= rated.wind5))
      #REGION 3: Rated wind speed until cut out wind speed
      EFF.mod.R3_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 4), data = filter(DF_Turbine5,speeds > rated.wind5 & speeds <= cut.out5))
      #REGION 4: Above cut out wind speed
      EFF.mod.R4_5 <- lm(DP.Effective.Power5 ~ poly(speeds, 2), data = filter(DF_Turbine5,speeds > cut.out5))
      
      ############ Calculate Parasitic losses. What % goes toward thrusters?
      #REGION 1: Before cut in wind speed
      LOS.mod.R1_5 <- lm(Parasitic5 ~ poly(speeds, 1), data = filter(DF_Turbine5,speeds <= cut.in5))
      fun1_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_5, newdata = data.frame(speeds = x)))
      #REGION 2: Cut in wind speed until rated wind speed
      LOS.mod.R2_5 <- lm(Parasitic5 ~ poly(speeds, 3), data = filter(DF_Turbine5,speeds > cut.in5 & speeds <= rated.wind5))
      fun2_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_5, newdata = data.frame(speeds = x)))
      #REGION 3: Rated wind speed until cut out wind speed
      LOS.mod.R3_5 <- lm(Parasitic5 ~ poly(speeds, 6), data = filter(DF_Turbine5,speeds > rated.wind5 & speeds <= cut.out5))
      fun3_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_5, newdata = data.frame(speeds = x)))
      #REGION 4: Above cut out wind speed
      LOS.mod.R4_5 <- lm(Parasitic5 ~ poly(speeds, 2), data = filter(DF_Turbine5,speeds > cut.out5))
      fun4_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_5, newdata = data.frame(speeds = x)))
      
      parasitic.loss5 <- 8760*(integrate(fun1_5, lower = 0, upper = cut.in5)$value + 
                                 integrate(fun2_5, lower = cut.in5, upper = rated.wind5)$value + 
                                 integrate(fun3_5, lower = rated.wind5, upper = cut.out5)$value +
                                 integrate(fun4_5, lower = cut.out5, upper = 40)$value) #in MWh
   
      ############ Calculate Production losses. What is produced?
      #REGION1 = 0
      #REGION2
      gen.fun2_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod5, newdata = data.frame(x = x)))
      #REGION3
      gen.fun3_5 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*rated.power5
      #REGION4 = 0
      
      gen5 <- 8760*(integrate(gen.fun2_5, lower = cut.in5, upper = rated.wind5)$value + 
                      integrate(gen.fun3_5, lower = rated.wind5, upper = cut.out5)$value) #in MWh
      
      net5 <- gen5 - parasitic.loss5 #annual expected net electricity output in MWh
      
      potential <- 8760*Turbine #max potential MWh per year
      
      netCF <- net5/potential #percent net CF
      
      #Populate Matrices
      DP_INV <- DP_Price*num/1000000 #MUSD. Incurred in first year, no discounting
      DP_CF <- 0 #O&M set to zero for now
      
      NPV_Cost <-FinancialMath::NPV(cf0 = DP_INV, cf = rep(DP_CF, times = years),times = seq(1,years),i = disc.r)
      NPV_Energy <- FinancialMath::NPV(cf0 = 0,cf = rep(net5,times = years),times = seq(1,years),i = disc.r)
      
      LTCOE <- -1000000*NPV_Cost/NPV_Energy #USD/MWh
      netOCC.High <- OCC.High*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000)) #in [$/kW]
      netOCC.Low <- OCC.Low*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000))
      
      LCOE.High <- (CRF*PFF*CFF*(netOCC.High)+FOM.High)*1000/(netCF*8760)
      LCOE.Low <- (CRF*PFF*CFF*(netOCC.Low)+FOM.Low)*1000/(netCF*8760)
      
      #Populate Matrices
      MATRIX.capex[num,TYPE] <- DP_INV
      MATRIX.loss.ratio[num,TYPE] <- parasitic.loss5/gen5 #annual percent of energy for DP
      MATRIX.ltcoe[num,TYPE] <- LTCOE
      MATRIX.lcoe.h[num,TYPE] <- LCOE.High
      MATRIX.lcoe.l[num,TYPE] <- LCOE.Low
    }
  }
  print(gen5/(8760*Turbine))
  return(list(MATRIX.loss.ratio, MATRIX.capex, MATRIX.ltcoe, MATRIX.lcoe.h, MATRIX.lcoe.l))
}

sensitivity_5(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
