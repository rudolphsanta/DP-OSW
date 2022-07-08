# Rudolph Santarromana
# 8 MW Turbine
# July 2021

library(tidyverse)
library(dplyr)

setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")
## Source: Desmond et al. (2016). Description of an 8 MW Reference Wind Turbine

#################################################
### PARAMETERS
wind_height <- 100 #measuring height of wind speed
speeds <- seq(0,40, by = 0.1)
Cd_cyl <- 0.5

#WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale = 10.82 #10

### WIND TURBINE PROPERTIES
cut.in8 <- 4 #[m/s]
rated.wind8 <- 11.5 #[m/s]. Says 12.5 in the report, but this doesn't seem accurate
cut.out8 <- 25 #[m/s]

rated.power8 <- 8 #[MW]

### FLOATER PROPERTIES
scale8.15 <- 1 #based on the hubheight scale (119/150)
F.freeboard8 <- 15*scale8.15 #same floater as 15MW turbine for now
F.diam8 <- 12.5*scale8.15
F.draft8 <- 20*scale8.15
FT.diam8 <- 10*scale8.15

#Effective water-exposed floater area (whole substructure)
f.area.water8 <- (F.diam8*F.draft8*3) + (F.draft8*FT.diam8) #m^2

## BLADE PROPERTIES
b.length8 <- 164/2 #m. Not given in report. taken as rotor diameter/2

###TOWER PROPERTIES
#Tower dimensions
hub.height8 <- 110 #m

t.heights8 <- c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30,32.5,35,37.5,40,42.5,45,47.5,50,52.5,55,57.5,60,62.5,65,67.5,70,72.5,75,77.5,80,82.5,85,87.5,90,92.5,95,97.5,100,102.5,105,106) #m
t.diams8 <- c(7.69,7.628,7.566,7.504,7.442,7.38,7.319,7.257,7.195,7.133,7.071,7.009,6.947,6.885,6.824,6.762,6.7,6.638,6.576,6.514,6.452,6.39,6.329,6.267,6.205,6.143,6.081,6.019,5.957,5.895,5.834,5.772,5.71,5.648,5.586,5.524,5.462,5.4,5.339,5.277,5.215,5.153,5.091,5.066) #m

t.mod8 <- lm(t.diams8 ~ t.heights8) #Tower diameter prediction model
t.fun8 <- function(x) as.numeric(predict(t.mod8, newdata = data.frame(t.heights8 = x))) #tower diameter function given height

##PLOT of tower diameter model
plot(x = t.heights8, y = t.diams8, pch = 19, col = 'red')
lines(x = t.heights8, y = predict(t.mod8, newdata = data.frame(t.heights8 = t.heights8)))

## TOWER FORCE FUNCTION
t.fun8.force <- function(x) t.fun8(x - F.freeboard8)*((x)^(2*alpha)) #shifts the tower upward to the floater top height
Wind_Force_Tower8 <- function(v.meas, h.meas = wind_height, Cd = Cd_cyl, base = F.freeboard8) {
  if(base < 0) return(0)
  rho_air <- 1.23 #kg/m^3
  alpha <- 0.14
  A <- rho_air*Cd*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  #integrate from the tower base (at the top of the floater) to the hub height + floater height
  C <- integrate(t.fun8.force, lower = F.freeboard8, upper = hub.height8 + F.freeboard8)$value
  force <- (A/B)*C
  return(force) #in Newtons
}
################################################################################

## STATIC, ARRESTED BLADE FORCES. (Without the 8 MW blade specifications, I will use the 10 MW blades and scale it)
#Blades with varying diameter and Cd, and wind with varying speed
thickness8 <- c(4.6,3.1,2.1,1.9,1.9,1.8,1.7,1.3,0.5)#[m]. Same as the 10 MW blade
Cd.vect8 <-   c(0.5,0.5,0.0067,0.0096,0.0113,0.0133,0.0146,0.0146,0.0146) #[-]. Same as the 10 MW blade
dist.vect8 <- c(0,0.11,0.211,0.27,0.301,0.33,0.36,0.48,0.72,1)*b.length8 #[m]

Wind_Force_Blade8 <- function(v.meas, h.meas = wind_height, l.elev = hub.height8) {
  alpha <- 0.14
  rho_air <- 1.23 #kg/m^3
  
  A <- rho_air*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  C <- 0
  
  for (i in 1:length(thickness8)){
    fun <- function(h) return(h^(2*alpha)*thickness8[i]*Cd.vect8[i])
    component <- integrate(fun, lower = l.elev + dist.vect10[i], upper = l.elev + dist.vect8[i+1])$value
    C <- C + component
  }
  force <- (A/B)*C
  return(force) #in Newtons
}

B.Force.static8 <- Wind_Force_Blade8(speeds)/1000 #in [kN]
max(B.Force.static5)
max(B.Force.static8)
max(B.Force.static10)
max(B.Force.static15)
################################################################################
#################################################
### ROTOR THRUST FORCE
# Recreate 3 in 8 MW Reference turbine document (Table 2 in Desmond et al., 2016)
#REGION 1: 4 - 11.1 (NOT TO RATED WINDSPEED)
X.R1_8 <- c(4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11) #m/s of windspeed
Y.R1_8 <- c(190,232,273,324,381,440,505,573,648,723,800,876,945,1014,1052) #kN of thrust

X.R1.2_8 <- X.R1_8^2
X.R1.3_8 <- X.R1_8^3

mod.R1_8 <- lm(Y.R1_8 ~ X.R1_8 + X.R1.2_8 + X.R1.3_8)

#REGION 2: 11.1 - 25
X.R2_8 <- seq(from = 11, to = 25, by = 0.5) #m/s of windspeed
Y.R2_8 <- c(1052,1028,972,905,847,801,765,730,700,668,644,624,604,587,571,557,542,528,516,505,497,486,476,472,461,454,445,442,437) #kN of thrust

X.R2.1_8 <- X.R2_8^-1
X.R2.2_8 <- X.R2_8^-2
X.R2.3_8 <- X.R2_8^-3

mod.R2_8 <- lm(Y.R2_8 ~ X.R2.1_8 + X.R2.2_8)

## Define the start of the second thrust force region
thrust.R2 <- 11.1 #via observation of the figures

#plot of how model fits the data
plot(X.R1_8,Y.R1_8, pch = 19, col = 'red', xlim = c(3,25),
     xlab = "Wind Speed [m/s]", ylab = "Rotor Thrust Force [kN]")
curve(mod.R1_8$coefficients[1] + mod.R1_8$coefficients[2]*x + mod.R1_8$coefficients[3]*x^2 + mod.R1_8$coefficients[4]*x^3, 
      from = cut.in8, to = thrust.R2, add = TRUE)
points(X.R2_8,Y.R2_8, pch = 19, col = 'red')
curve(mod.R2_8$coefficients[1] + mod.R2_8$coefficients[2]*x^-1 + mod.R2_8$coefficients[3]*x^-2, 
      from = thrust.R2, to = cut.out8, add = TRUE)

Wind_Blade_Force_spinning8 <- function(wind) { #windspeed in m/s as input only defined on [4,25] interval
  if(wind < cut.in8 | wind > cut.out8) force <- 0
  if(wind >= cut.in8 && wind <= thrust.R2) force <- as.numeric(mod.R1_8$coefficients[1] + mod.R1_8$coefficients[2]*wind + mod.R1_8$coefficients[3]*wind^2 + mod.R1_8$coefficients[4]*wind^3)
  if(wind > thrust.R2 && wind <= cut.out8) force <- as.numeric(mod.R2_8$coefficients[1] + mod.R2_8$coefficients[2]*wind^-1 + mod.R2_8$coefficients[3]*wind^-2)
  return(force) #returns a value in kN
}

Wind_Blade_Force_spinning8(11) 
Wind_Blade_Force_spinning10(11) 

#################################################
#Total wind force on the turbine assuming same tower and substructure, only the RNA has changed.
#functions in this function are contained in "Dynamic Positioning Forces 20210309 - Debugging"
DP.Turbine.Wind.Force8 <- function(wind) { 
  #non-blade components
  T.Force.best <- Wind_Force_Tower8(v.meas = wind)/1000 #in kN
  #Floater Components. Scaled Floater Size
  F.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = F.diam8, u.elev = F.freeboard8)/1000 #in [kN]
  FT.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = FT.diam8, u.elev = F.freeboard8)/1000 #in [kN]
  
  #blade spinning component
  B.Force.best.spinning <- Wind_Blade_Force_spinning8(wind) #in kN. returns 0 when blades are static
  
  #blade static component. uses the static blades geometry of the 15-MW turbine. Overestimated
  if(wind < cut.in8 | wind > cut.out8) B.Force.static8 <- Wind_Force_Blade8(wind)/1000  else B.Force.static8 <- 0 #in [kN]. returns 0 when blades are spinning
  
  #Blade Total Forces vs. windspeed
  Tot.Wind.Force <- T.Force.best + 
    (3*B.Force.static8) + 
    B.Force.best.spinning + 
    (3*F.Force.height) + 
    FT.Force.height
  
  return(Tot.Wind.Force)
}

Wind.Forces8 <- sapply(speeds,DP.Turbine.Wind.Force8)
DP.Turbine.Wind.Force10(38)
DP.Turbine.Wind.Force8(38)
DP.Turbine.Wind.Force5(38)

plot(x = speeds, y = Wind.Forces8, type = 'l')

## CURRENT FORCE MAGNITUDE
F.Current.Force8 <- function(current) {
  #Current_Force function is in 15MW turbine file
  F.Force.Current <- Current_Force(speed = current, f.area.water8,Cd_cyl)/1000 #in kN
  return(F.Force.Current)
}

################################################################################
### COMBINED ENVIRONMENTAL FORCES
# Considers 15 MW Floater and Tower
#DLC 1.3: Design Levels (single windspeed)
Wind_8 <- c(DP.Turbine.Wind.Force8(wind = rated.wind8),0)
Current_8 <- c(F.Current.Force8(0.2)*cos(7*pi/4), F.Current.Force8(0.2)*sin(7*pi/4))
Wave.high_8 <- c(50,0) #Calculated in "Wave Drift Forces.R" for the 15 MW turbine

# V.1.3.low <- R.Vector(Wind, Wave.low, Current)
V.1.3.high_8 <- R.Vector(Wind_8, Wave.high_8, Current_8)

## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DLC1.3_8.fun <- function(wind) {
  force1.3_8 <- c()
  Current_8 <- c(F.Current.Force8(0.2)*cos(7*pi/4), F.Current.Force8(0.2)*sin(7*pi/4))
  Wave.high_8 <- c(50,0)
  for(i in 1:length(wind)){
    Wind_8 <- c(DP.Turbine.Wind.Force8(wind = wind[i]),0)
    force1.3_8 <- c(force1.3_8, R.Vector(Wind_8, Wave.high_8, Current_8)[1])
  }
  return(force1.3_8)
}

DLC1.3_8 <- DLC1.3_8.fun(speeds) #RESULTING FORCES UNDER DLC1.3
(max(DLC1.3_8))

#DLC 6.1: Design Levels (single windspeed)
WIND_8 <- round(as.numeric(DP.Turbine.Wind.Force8(wind = 38)),0)
Wind2_8 <- c(WIND_8,0)
Current2_8 <- c(F.Current.Force8(0.9)*cos(7*pi/4), F.Current.Force8(0.9)*sin(7*pi/4))

phi.wave <- seq(-90,90,by = 10)
phi.rad <- phi.wave*(pi/180)

Wave.high2.mag <- 10

V.6.1.high_8 <- c()
for(phi in phi.rad) {
  Wave.high2_8 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
  High.F_8 <- R.Vector(Wind2_8, Wave.high2_8, Current2_8)
  V.6.1.high_8 <- rbind(V.6.1.high_8, High.F_8)
}

V.6.1.high_8 <- cbind(phi.wave, V.6.1.high_8) %>%
  as_tibble() %>%
  rename(wind.wave.angle = phi.wave, force.kN = V2,final.angle = V3)
(max(V.6.1.high_8$force.kN))

## Given a windspeed, this function calculates the resultant force under DLC6.1 conditions
DLC6.1_8.fun <- function(wind) {
  force6.1_8 <- c() 
  Current2_8 <- c(F.Current.Force8(0.9)*cos(7*pi/4), F.Current.Force8(0.9)*sin(7*pi/4))
  
  Wave.high2.mag <- 10 #the total magnitude of the force in kN
  phi.wave <- seq(-90,90,by = 10)
  phi.rad <- phi.wave*(pi/180)
  
  for(i in 1:length(wind)) {
    Matrix.high_8 <- c()
    for(phi in phi.rad) {
      Wave.high2_8 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
      Wind.high_8 <- c(DP.Turbine.Wind.Force8(wind = wind[i]),0)
      High.F_8 <- R.Vector(Wind.high_8, Wave.high2_8, Current2_8)
      Matrix.high_8 <- rbind(Matrix.high_8, High.F_8)
    }
    force6.1_8 <- c(force6.1_8,max(Matrix.high_8[,1]))
  }
  return(force6.1_8)
}

DLC6.1_8 <- DLC6.1_8.fun(speeds)  #RESULTING FORCES UNDER DLC6.1
(tail(DLC6.1_8,1))

### Total Environmental Force Plot
plot(x = speeds, y = DLC6.1_8, type = 'l',
     ylim = c(0,1200), yaxs = 'i', xaxs = 'i',
     ylab = "Turbine Thrust Force [kN]",
     xlab = "Wind Speed [m/s]")
lines(x = speeds, y = DLC1.3_8, col = 'blue')

##????? Why is the gap larger at the lower windspeeds and smaller later on?
##As the turbine rotor force increases, it greatly outweighs the other forces. 
##This rotor force is the same under both conditions (DLCs), and it has a greater
##and greater influence at defining the total magnitude of the resultant vector.
##Therefore, when the rotor thrust force is maximized (at the rated wind speed), 
##it defines the overwhelming majority of the total force on the turbine, while
##the contribution from current and waves is minimal, relative to the magnitude
##of the rotor thrust force. Since the rotor thrust force from wind is the same
##in both DLCs, the difference between the two DLCs at this level is minimal.
##when the force on the rotor is small (at lower wind speeds, and when the rotor is stopped),
##the contribution to the force from current and waves is much greater relative to
##the rotor force, and thus, the conditions which are different in both DLCs
##have a greater impact on the turbine total force, causing a greater difference
##between the two DLCs when the rotor thrust force is small.

DLC6.1_8.fun(38) #510 kN
DLC1.3_8.fun(thrust.R2) #1155 kN. Design at this level

Rotor_Only <- mapply(Wind_Blade_Force_spinning8, wind = speeds)

## Calculate Parasitic Power under DLC1.3, which depends on the thruster system
parasitic.power8.1 <- DP.Power.n(DLC1.3_8, motor.diam = 1.07, n = 35)
parasitic.power8.2 <- DP.Power.n(DLC1.3_8, motor.diam = 1.98, n = 9)
parasitic.power8.3 <- DP.Power.n(DLC1.3_8, motor.diam = 4.52, n = 3)
parasitic.power8.3.rotor <- DP.Power.n(Rotor_Only, motor.diam = 4.52, n = 3) #What is the parasitic power consumption of only the rotor force
parasitic.power8.4 <- DP.Power.n(DLC1.3_8, motor.diam = 3.35, n = 4)

parasitic.power8 <- DP.Power.n(DLC1.3_8, motor.diam = 3.35, n = 3)

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

plot(x = speeds, y = parasitic.power8.1, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(0,10), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
lines(x = speeds, y = parasitic.power8.2, lty = 2, lwd = 3)
lines(x = speeds, y = parasitic.power8.3, lty = 3, lwd = 3)
lines(x = speeds, y = parasitic.power8.4, lty = 4, lwd = 3)
abline(h = rated.power8, col = 'black', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Power Requirement from Motors [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topleft', legend = c("n = 35, d = 1.07", 
                             "n = 9, d = 1.98", 
                             "n = 3, d = 4.52",
                             "n = 4, d = 3.35"), 
       col = 'black', lwd = 3, lty = c(1,2,3,4))

# dev.off() #uncomment if saving the image

######Power Curve for 8 MW Turbine
# Quadratic Region
x <- seq(from = 4, to = 12.5, by = 0.5) #wind speeds [m/s]
y <- c(110,350,600,850,1140,1490,1900,2370,2900,3500,4155,4870,5630,6420,7150,7610,7865,8000)/1000 #in [MW]

power.mod8 <- lm(y ~ poly(x, 7))

#Check the model vs the points
plot(x, y)
lines(x, y = predict(power.mod8, newdata = data.frame(x = x)), col = 'red')


Power.Curve.8 <- function(wind.speed) { #calculate the turbine's 'raw' power output for each wind speed
  if (wind.speed < cut.in8) 0
  else if (wind.speed <= rated.wind8) as.double(predict(power.mod8, newdata = data.frame(x=wind.speed)))
  # coef(power.mod)[1] + coef(power.mod)[2]*wind.speed + coef(power.mod)[3]*wind.speed^2
  else if (wind.speed <= cut.out8) rated.power8
  else 0
}

##########################################################################
#Develop the effective DP turbine power curve vector
DP.Raw.Power8 <- sapply(speeds,Power.Curve.8)
DP.Effective.Power8 <- DP.Raw.Power8 - parasitic.power8 #10.3 looked to be the best

##PLOT
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))

curve(dweibull(x, shape = P.shape, scale = P.scale), from = 0, to = 25, lwd = 3, xaxt = 'n',
      xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
      ylab = "", cex.lab = 1.7, cex.axis = 1.7,
      main = "", col = 'blue', col.axis = 'blue', 
      ylim = c(0, 0.12))
mtext("Probability Density", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)
#Effective Power Curve

plot(x = speeds, y = DP.Effective.Power8, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-4,12), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power8, col = 'green')
lines(speeds, parasitic.power8.3, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")

# dev.off() #uncomment if saving the image

##PLOT: Showing Power Ratio
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))
plot(x = speeds, y = parasitic.power8.3/DP.Raw.Power8, type = 'l', lwd = 3, 
     xaxt = 'n', xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
     ylab = "", cex.lab = 1.7, cex.axis = 1.7,
     main = "", col = 'blue', col.axis = 'blue',
     ylim = c(0, 1))
mtext("Power Ratio", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)

plot(x = speeds, y = DP.Effective.Power8, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-2,15), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
abline(v = rated.wind8, col = 'blue')
abline(v = thrust.R2, col = 'red')
lines(speeds, DP.Raw.Power8, col = 'green')
lines(speeds, parasitic.power8.3, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topright', legend = c("Power Ratio (DP Consumption/Power Generated)", "Net Power", "Generated Power", "Thruster Power Consumption"),
       lty = c(1,1,1,2), col = c('blue','black','green','red'), lwd = c(3,3,1,1), cex = 0.8)

######################## Turn effective power curve into several linear models

#WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale = 10#10

parasitic.power8 <- DP.Power.n(DLC1.3_8, motor.diam = 4.1, n = 2)

DF_Turbine8 <- data.frame(speeds,DP.Raw.Power8,Parasitic8 = parasitic.power8,DP.Effective.Power8)

#REGION 1: Before cut in wind speed
EFF.mod.R1_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 1), data = filter(DF_Turbine8,speeds <= cut.in8))
#REGION 2: Cut in wind speed until rated wind speed
EFF.mod.R2_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 3), data = filter(DF_Turbine8,speeds > cut.in8 & speeds <= rated.wind8))
#REGION 3: Rated wind speed until cut out wind speed
EFF.mod.R3_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 4), data = filter(DF_Turbine8,speeds > rated.wind8 & speeds <= cut.out8))
#REGION 4: Above cut out wind speed
EFF.mod.R4_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 2), data = filter(DF_Turbine8,speeds > cut.out8))

##PLOT: How good are these models? They look very good.
plot(x = speeds, y = DP.Effective.Power8, pch = 19, cex = 0.01)
lines(x = DF_Turbine8$speeds[speeds <= cut.in8], y = predict(EFF.mod.R1_8, newdata = filter(DF_Turbine8, speeds <= cut.in8)), col = 'red')
lines(x = DF_Turbine8$speeds[speeds > cut.in8 & speeds <= rated.wind8], y = predict(EFF.mod.R2_8, newdata = filter(DF_Turbine8, speeds > cut.in8 & speeds <= rated.wind8)), col = 'red')
lines(x = DF_Turbine8$speeds[speeds > rated.wind8 & speeds <= cut.out8], y = predict(EFF.mod.R3_8, newdata = filter(DF_Turbine8, speeds > rated.wind8 & speeds <= cut.out8)), col = 'red')
lines(x = DF_Turbine8$speeds[speeds > cut.out8], y = predict(EFF.mod.R4_8, newdata = filter(DF_Turbine8, speeds > cut.out8)), col = 'red')


############ Calculate Parasitic losses. What % goes toward thrusters?
#REGION 1: Before cut in wind speed
LOS.mod.R1_8 <- lm(Parasitic8 ~ poly(speeds, 1), data = filter(DF_Turbine8,speeds <= cut.in8))
fun1_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_8, newdata = data.frame(speeds = x)))
#REGION 2: Cut in wind speed until rated wind speed
LOS.mod.R2_8 <- lm(Parasitic8 ~ poly(speeds, 3), data = filter(DF_Turbine8,speeds > cut.in8 & speeds <= thrust.R2))
fun2_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_8, newdata = data.frame(speeds = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_8 <- lm(Parasitic8 ~ poly(speeds, 5), data = filter(DF_Turbine8,speeds > thrust.R2 & speeds <= cut.out8))
fun3_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_8, newdata = data.frame(speeds = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_8 <- lm(Parasitic8 ~ poly(speeds, 2), data = filter(DF_Turbine8,speeds > cut.out8))
fun4_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_8, newdata = data.frame(speeds = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = speeds, y = DF_Turbine8$Parasitic8, pch = 19, cex = 0.01)
lines(x = DF_Turbine8$speeds[speeds <= cut.in8], y = predict(LOS.mod.R1_8, newdata = filter(DF_Turbine8, speeds <= cut.in8)), col = 'red')
lines(x = DF_Turbine8$speeds[speeds > cut.in8 & speeds <= thrust.R2], y = predict(LOS.mod.R2_8, newdata = filter(DF_Turbine8, speeds > cut.in8 & speeds <= thrust.R2)), col = 'red')
lines(x = DF_Turbine8$speeds[speeds > thrust.R2 & speeds <= cut.out8], y = predict(LOS.mod.R3_8, newdata = filter(DF_Turbine8, speeds > thrust.R2 & speeds <= cut.out8)), col = 'red')
lines(x = DF_Turbine8$speeds[speeds > cut.out8], y = predict(LOS.mod.R4_8, newdata = filter(DF_Turbine8, speeds > cut.out8)), col = 'red')

parasitic.loss8 <- 8760*(integrate(fun1_8, lower = 0, upper = cut.in8)$value + 
                            integrate(fun2_8, lower = cut.in8, upper = thrust.R2)$value + 
                            integrate(fun3_8, lower = thrust.R2, upper = cut.out8)$value +
                            integrate(fun4_8, lower = cut.out8, upper = 40)$value) #in MWh
#Result = 15308 MWh (36%)
#Result(rotor only) = 13044 MWh

############ Calculate Production losses. What is produced?
#REGION1 = 0
#REGION2
gen.fun2_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod8, newdata = data.frame(x = x)))
#REGION3
gen.fun3_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*rated.wind8
#REGION4 = 0

gen8 <- 8760*(integrate(gen.fun2_8, lower = cut.in8, upper = rated.wind8)$value + 
                 integrate(gen.fun3_8, lower = rated.wind8, upper = cut.out8)$value) #in MWh
#RESULT = 42789 MWh

(parasitic.loss8/gen8)
(gen8/(rated.power8*8760))
((gen8-parasitic.loss8)/(rated.power8*8760))
(parasitic.loss8)


################################################################################
################################################################################
## PART II: CAN WE REDUCE THE LOSSES AT LOW WIND SPEEDS?

## Find where the generation power curve first becomes greater than the parasitic loss curve
## Use models generated (in Region 2) to find this crossover point.
Crossover8 <- function(guess = 4, tolerance = 0.1, step = 0.1) { #guess crossover wind speed
  value <- guess
  cons <- predict(LOS.mod.R2_8, newdata = data.frame(speeds = value))
  prod <- predict(power.mod8, newdata = data.frame(x = value))
  net <- abs(prod - cons)
  while(net > tolerance) {
    value <- value + step
    cons <- predict(LOS.mod.R2_8, newdata = data.frame(speeds = value))
    prod <- predict(power.mod8, newdata = data.frame(x = value))
    net <- abs(prod - cons)
  }
  return(value)
}
Crossover8() #4.8 m/s

plot(x = speeds, y = DP.Effective.Power8, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-2,12), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
abline(v = 4.8)
lines(speeds, DP.Raw.Power8, col = 'green')
lines(speeds, parasitic.power8.3, col = 'red', lty = 2)
axis(side = 2, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 2, line = 3.5, cex = 1.7, col = "black")

## ADJUST POWER PRODUCTION TO CUT IN ONLY WHEN PRODUCTION IS GREATER THAN LOSSES
## Change Cut in wind speed at very top to 6.2 m/s and run functions again.
## Parasitic Loss Result = 22071 MWh (43%)
## Annual Production Result = 51598 MWh 


#FUNCTIONALIZED POWER CURVES (Lines above)
sensitivity_8 <- function(shape, scale, OCC.High, OCC.Low, FOM.High, FOM.Low, disc.r, years = 25, fs = 1.67) {
  P.shape = shape
  P.scale = scale
  Turbine <- 8
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
      if(num*DP_Force < max(DLC1.3_8)*fs) next #if this configuration doesn't provide the max needed station keeping force, the configuration does not work, and move to next one
      
      parasitic.power8 <- DP.Power.n(DLC1.3_8, motor.diam = DP_Diam, n = num)
      
      DF_Turbine8 <- data.frame(speeds,DP.Raw.Power8,Parasitic8 = parasitic.power8,DP.Effective.Power8)
      
      #REGION 1: Before cut in wind speed
      EFF.mod.R1_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 1), data = filter(DF_Turbine8,speeds <= cut.in8))
      #REGION 2: Cut in wind speed until rated wind speed
      EFF.mod.R2_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 3), data = filter(DF_Turbine8,speeds > cut.in8 & speeds <= rated.wind8))
      #REGION 3: Rated wind speed until cut out wind speed
      EFF.mod.R3_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 4), data = filter(DF_Turbine8,speeds > rated.wind8 & speeds <= cut.out8))
      #REGION 4: Above cut out wind speed
      EFF.mod.R4_8 <- lm(DP.Effective.Power8 ~ poly(speeds, 2), data = filter(DF_Turbine8,speeds > cut.out8))
      
      ############ Calculate Parasitic losses. What % goes toward thrusters?
      #REGION 1: Before cut in wind speed
      LOS.mod.R1_8 <- lm(Parasitic8 ~ poly(speeds, 1), data = filter(DF_Turbine8,speeds <= cut.in8))
      fun1_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_8, newdata = data.frame(speeds = x)))
      #REGION 2: Cut in wind speed until rated wind speed
      LOS.mod.R2_8 <- lm(Parasitic8 ~ poly(speeds, 3), data = filter(DF_Turbine8,speeds > cut.in8 & speeds <= thrust.R2))
      fun2_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_8, newdata = data.frame(speeds = x)))
      #REGION 3: Rated wind speed until cut out wind speed
      LOS.mod.R3_8 <- lm(Parasitic8 ~ poly(speeds, 5), data = filter(DF_Turbine8,speeds > thrust.R2 & speeds <= cut.out8))
      fun3_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_8, newdata = data.frame(speeds = x)))
      #REGION 4: Above cut out wind speed
      LOS.mod.R4_8 <- lm(Parasitic8 ~ poly(speeds, 2), data = filter(DF_Turbine8,speeds > cut.out8))
      fun4_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_8, newdata = data.frame(speeds = x)))

      parasitic.loss8 <- 8760*(integrate(fun1_8, lower = 0, upper = cut.in8)$value + 
                                 integrate(fun2_8, lower = cut.in8, upper = thrust.R2)$value + 
                                 integrate(fun3_8, lower = thrust.R2, upper = cut.out8)$value +
                                 integrate(fun4_8, lower = cut.out8, upper = 40)$value) #in MWh
      
      ############ Calculate Production losses. What is produced?
      #REGION1 = 0
      #REGION2
      gen.fun2_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod8, newdata = data.frame(x = x)))
      #REGION3
      gen.fun3_8 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*rated.wind8
      #REGION4 = 0
      
      gen8 <- 8760*(integrate(gen.fun2_8, lower = cut.in8, upper = rated.wind8)$value + 
                      integrate(gen.fun3_8, lower = rated.wind8, upper = cut.out8)$value) #in MWh
      
      net8 <- gen8 - parasitic.loss8 #annual expected net electricity output in MWh
      
      potential <- 8760*Turbine #max potential MWh per year
      
      netCF <- net8/potential #percent net CF
      
      #Populate Matrices
      DP_INV <- DP_Price*num/1000000 #MUSD. Incurred in first year, no discounting
      DP_CF <- 0 #O&M set to zero for now
      
      NPV_Cost <-FinancialMath::NPV(cf0 = DP_INV, cf = rep(DP_CF, times = years),times = seq(1,years),i = disc.r)
      NPV_Energy <- FinancialMath::NPV(cf0 = 0,cf = rep(net8,times = years),times = seq(1,years),i = disc.r)
      
      LTCOE <- -1000000*NPV_Cost/NPV_Energy #USD/MWh
      netOCC.High <- OCC.High*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000)) #in [$/kW]
      netOCC.Low <- OCC.Low*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000))
      
      LCOE.High <- (CRF*PFF*CFF*(netOCC.High)+FOM.High)*1000/(netCF*8760)
      LCOE.Low <- (CRF*PFF*CFF*(netOCC.Low)+FOM.Low)*1000/(netCF*8760)
      
      #Populate Matrices
      MATRIX.capex[num,TYPE] <- DP_INV
      MATRIX.loss.ratio[num,TYPE] <- parasitic.loss8/gen8 #annual percent of energy for DP
      MATRIX.ltcoe[num,TYPE] <- LTCOE
      MATRIX.lcoe.h[num,TYPE] <- LCOE.High
      MATRIX.lcoe.l[num,TYPE] <- LCOE.Low
    }
  }
  print(gen8/(8760*Turbine))
  return(list(MATRIX.loss.ratio, MATRIX.capex, MATRIX.ltcoe, MATRIX.lcoe.h, MATRIX.lcoe.l))
}

sensitivity_8(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)


