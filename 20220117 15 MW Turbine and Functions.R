# Created by Rudolph Santarromana
# Offshore Wind Stabilizing Forces
# August 2020

#####################################################################################
#####################################################################################

library(tidyverse)
library(dplyr)

# setwd("C:/Users/rudol/Desktop/Rudolph/CMU/Research/Wind Energy/Dynamic Positioning")
setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")

#####################################################################################
## PARAMETERS
speeds <- seq(0,40, by = 0.1)
wind_height <- 100 #measuring height of wind speed. could also be set to hub height

Cd_cyl <- 0.5
Cd_airfoil <- 0.09 #from IEA reference turbine data Figure 2-3
Cd_square <- 1

#WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale = 10.82 #10

par(mar = c(5, 6, 3, 6))
curve(dweibull(x, shape = 2.3, scale = 10.82), from = 0, to = 40, lwd = 3, #Class 8
      xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
      ylab = "", cex.lab = 1.7, cex.axis = 1.7,
      main = "", col = 'black', col.axis = 'black', 
      ylim = c(0, 0.12))
curve(dweibull(x, shape = 2.3, scale = 7.95), from = 0, to = 40, lwd = 3, col = 'red', add = TRUE) #Class 14
curve(dweibull(x, shape = 2.3, scale = 10), from = 0, to = 40, lwd = 3, col = 'blue', add = TRUE) #Portugal
mtext("Probability Density", side = 2, line = 4.5, cex = 1.7, col = "black")
legend('topright',c("Portuguese Wind Profile; Mean = 8.86m/s",
                    "Wind Speed Class 8; Mean = 9.59m/s",
                    "Wind Speed Class 14; Mean = 7.04m/s"), 
       col = c("blue","black", "red"), 
       cex = 1.2, lty = 1, lwd = 3)

### WIND TURBINE PROPERTIES
cut.in15 <- 3#6.8#3 #[m/s]
rated.wind15 <- 10.59 #[m/s]
cut.out15 <- 25 #[m/s]

rated.power15 <- 15

## TOWER PROPERTIES
tower.diam15 <- 10 #At the base
tower.height15 <- 150

## BLADE PROPERTIES
blade.width <- 5.8 #Maximum chord width
blade.root <- 5.2 #Blade root diameter
blade.length <- 117 #m

## FLOATER PROPERTIES
floater.diam15 <- 12.5
floater.freeboard15 <- 15
floater.draft15 <- 20

#Effective water-exposed floater area (whole substructure)
f.area.water15 <- (floater.diam15*floater.draft15*3) + (floater.draft15*tower.diam15) #m^2

#####################################################################################
# FORCE FUNCTIONS

Current_Force <- function(speed,eff.area,Cd) { #([m/s],[m^2],[-])
  rho_water <- 1025 #kg/m^3
  force <- 0.5*rho_water*(speed^2)*eff.area*Cd
  return(force) #Returns values in Newtons
}

#wind force with height on a constant cross section body. equation by ABS, 2013
Wind_Force_ConsCross <- function(v.meas, u.elev, Cd, D, h.meas = wind_height, l.elev = 0) {
  rho_air <- 1.23
  alpha <- 0.14 #from ABS, 2013
  A <- (v.meas^2)*rho_air*D*Cd
  B <- 2*(h.meas^(2*alpha))*((2*alpha)+1)
  C_H <- u.elev^((2*alpha) + 1)
  C_0 <- l.elev^((2*alpha) + 1)
  force <- (A/B)*(C_H - C_0)
  return(force) #Returns values in Newtons
}

Wind_Speed_height <- function(h, v.hub, h.hub, alpha = 0.14){ #Given by ABS, 2013
  V <- v.hub*(h/h.hub)^alpha
  return(V)
} 

#####################################################################################
# Plot to demonstrate wind profile with height
s.meas <- seq(5,20, by = 5)
hts <- seq(1,200, by = 0.5)
M <- c()
ht <- c()
for(s in s.meas) {
  i <- 1
  for(h in hts) {
    ht[i] <- Wind_Speed_height(h, s, h.hub = wind_height)
    i <- i + 1
  }
  M <- cbind(M, ht)
}
M <- cbind(hts, M) %>%
  as_tibble()

plot(x = M$V, y = M$hts, type = 'n', xlim = c(0,20), ylim = c(1,200))
lines(x = M$V, y = M$hts)
lines(x = M$Z, y = M$hts, col = 'red')
lines(x = M$V4, y = M$hts)
lines(x = M$V5, y = M$hts)


#####################################################################################
### TOWER FORCES
D_height_fun.15 <- function(h) return(-0.0026*(h-15) + 10) #h in [15,54)
D_height_fun.54 <- function(h) return(-0.029*(h-54) + 9.9) #h in [54,80)
D_height_fun.80 <- function(h) return(-0.017*(h-80) + 9.2) #h in [80,106)
D_height_fun.106 <- function(h) return(-0.026*(h-106) + 8.7) #h in [106,119)
D_height_fun.119 <- function(h) return(-0.07*(h-119) + 8.4) #h in [119,145)

#Define the piecewise function of the integrand. this is D(h)*h^(2*alpha)
alpha <- 0.14

int_fun.15 <- function(h) return((h^(2*alpha))*(-0.0026*(h-15) + 10)) #h in [15,54)
int_fun.54 <- function(h) return((h^(2*alpha))*(-0.029*(h-54) + 9.9)) #h in [54,80)
int_fun.80 <- function(h) return((h^(2*alpha))*(-0.017*(h-80) + 9.2)) #h in [80,106)
int_fun.106 <- function(h) return((h^(2*alpha))*(-0.026*(h-106) + 8.7)) #h in [106,119)
int_fun.119 <- function(h) return((h^(2*alpha))*(-0.07*(h-119) + 8.4)) #h in [119,145)

#Get the integral of the integrand over the boundaries of each sub-function
I.15 <- integrate(int_fun.15, lower = 15, upper = 54)$value
I.54 <- integrate(int_fun.54, lower = 54, upper = 80)$value
I.80 <- integrate(int_fun.80, lower = 80, upper = 106)$value
I.106 <- integrate(int_fun.106, lower = 106, upper = 119)$value
I.119 <- integrate(int_fun.119, lower = 119, upper = 145)$value

Wind_Force_Tower15 <- function(v.meas, h.meas = wind_height, Cd = Cd_cyl) {
  rho_air <- 1.23 #kg/m^3
  alpha <- 0.14
  A <- rho_air*Cd*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  C <- I.15 + I.54 + I.80 + I.106 + I.119 #sum of the integrals
  force <- (A/B)*C
  return(force) #in Newtons
}

#most accurate representation of the force on the tower. Does not include support structure below tower.
T.Force.best <- Wind_Force_Tower15(v.meas = speeds)/1000 #in kN

plot(x = speeds, y = T.Force.best, type = 'l')

#####################################################################################
## STATIC, ARRESTED BLADE FORCES
#Blades with varying diameter and Cd, and wind with varying speed
thickness15 <- c(5.2,3.7,2.4,1.8,1.5,1.2,0.9,0.7,0.6,0.5) #[m]
Cd.vect15 <- c(0.5,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09) #[-]
dist.vect15 <- c(0,11.7,23.4,35.1,46.8,58.5,70.2,81.9,93.6,105.3,117) #[m]

Wind_Force_Blade15 <- function(v.meas, h.meas = wind_height, l.elev = tower.height) {
  alpha <- 0.14
  rho_air <- 1.23 #kg/m^3
  
  A <- rho_air*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  C <- 0
  
  for (i in 1:length(thickness15)){
    fun <- function(h) return(h^(2*alpha)*thickness15[i]*Cd.vect15[i])
    component <- integrate(fun, lower = l.elev + dist.vect15[i], upper = l.elev + dist.vect15[i+1])$value
    C <- C + component
  }
  force <- (A/B)*C
  return(force) #in Newtons
}

B.Force.static15 <- Wind_Force_Blade15(speeds)/1000 #in [kN]

################################################################################
## ROTOR THRUST FORCE. SPINNING 15 MW TURBINE BLADES

# Recreate 3-1.b in 15 MW Reference turbine document (Gaertner et al., 2020)
#REGION 1: 3 - 10.59
X.R1_15 <- c(3,5,10.59) #m/s of windspeed
Y.R1_15 <- c(609.4,937.5,2789) #kN of thrust

X.R1.2_15 <- X.R1_15^2

mod.R1_15 <- lm(Y.R1_15 ~ X.R1_15 + X.R1.2_15)

#REGION 2: 10.59 - 25
X.R2_15 <- c(10.59,15,20,25) #m/s of windspeed
Y.R2_15 <- c(2789,1617,1312.5,1171.9) #kN of thrust

X.R2.1_15 <- X.R2_15^-1
X.R2.2_15 <- X.R2_15^-2
X.R2.3_15 <- X.R2_15^-3

mod.R2_15 <- lm(Y.R2_15 ~ X.R2.1_15 + X.R2.2_15 + X.R2.3_15)
# mod.R2 <- lm(Y.R2 ~ log(X.R2))


#plot of how model fits the data. It fits very well
#REGION 1
plot(X.R1_15,Y.R1_15, pch = 19, col = 'red', xlim = c(3,25), ylim = c(0,3000),
     xlab = "Wind Speed [m/s]", ylab = "Swept Area Thrust Force [kN]", xaxs = 'i', yaxs = 'i',)
curve(mod.R1_15$coefficients[1] + mod.R1_15$coefficients[2]*x + mod.R1_15$coefficients[3]*x^2, 
      from = cut.in15, to = rated.wind15, add = TRUE)
#REGION 2
points(X.R2_15,Y.R2_15, pch = 19, col = 'red')
curve(mod.R2_15$coefficients[1] + mod.R2_15$coefficients[2]*x^-1 + mod.R2_15$coefficients[3]*x^-2 + mod.R2_15$coefficients[4]*x^-3,
      from = rated.wind15, to = cut.out15, add = TRUE)

Wind_Blade_Force_spinning15 <- function(wind) { #windspeed in m/s as input only defined on [3,25] interval
  if(wind < cut.in15 | wind > cut.out15) force <- 0
  if(wind >= cut.in15 && wind <= rated.wind15) force <- as.numeric(mod.R1_15$coefficients[1] + mod.R1_15$coefficients[2]*wind + mod.R1_15$coefficients[3]*wind^2)
  if(wind > rated.wind15 && wind <= cut.out15) force <- as.numeric(mod.R2_15$coefficients[1] + mod.R2_15$coefficients[2]*wind^-1 + mod.R2_15$coefficients[3]*wind^-2 + mod.R2_15$coefficients[4]*wind^-3)
  return(force) #returns a value in kN
}

Wind_Blade_Force_spinning15(rated.wind15)
#################################################
#Total wind force on the turbine assuming same tower and substructure, only the RNA has changed.
#functions in this function are contained in "Dynamic Positioning Forces 20210309 - Debugging"
DP.Turbine.Wind.Force15 <- function(wind) { #functions in this function are contained in "Dynamic Positioning Forces 20210309 - Debugging"
  #non-blade components
  T.Force.best <- Wind_Force_Tower15(v.meas = wind)/1000 #in kN
  F.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = floater.diam15, u.elev = floater.freeboard15)/1000 #in [kN]
  FT.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = tower.diam15, u.elev = floater.freeboard15)/1000 #in [kN]
  
  #blade spinning component
  B.Force.best.spinning <- Wind_Blade_Force_spinning15(wind) #in kN. returns 0 when blades are static
  
  #blade static component
  if(wind < cut.in15 | wind > cut.out15) B.Force.static15 <- Wind_Force_Blade15(wind)/1000  else B.Force.static15 <- 0 #in [kN]. returns 0 when blades are spinning
  
  #Blade Total Forces vs. windspeed
  Tot.Wind.Force <- T.Force.best + 
    (3*B.Force.static15) + 
    B.Force.best.spinning + 
    (3*F.Force.height) + 
    FT.Force.height
  
  return(Tot.Wind.Force)
}

Wind.Forces15 <- sapply(speeds,DP.Turbine.Wind.Force15)

DP.Turbine.Wind.Force5(10) #692 kN at 10 m/s
DP.Turbine.Wind.Force10(10) #1496 kN at 10 m/s. Equal to 2.2x 5-MW turbine
DP.Turbine.Wind.Force15(10) #2571 kN at 10 m/s. Equal to 3.7x 5-MW turbine


plot(x = speeds, y = Wind.Forces15, type = 'l', 
     ylab = "Rotor Thrust Forces [kN]", xlab = "Wind Speeds [m/s]")
lines(x = speeds, y = Wind.Forces10, col = 'blue')
lines(x = speeds, y = Wind.Forces5, col = 'red')
legend('topright', legend = c('15 MW','10 MW', '5 MW'), lty = 1, col = c('black','blue','red'))
abline(v = 10)
## CURRENT FORCE MAGNITUDE
F.Current.Force15 <- function(current) {
  F.Force.Current <- Current_Force(speed = current, f.area.water15,Cd_cyl)/1000 #in kN
  return(F.Force.Current)
}

################################################################################
### COMBINED ENVIRONMENTAL FORCES
# Considers 15 MW Floater and Tower
#DLC 1.3: Design Levels (single windspeed)
Wind_15 <- c(DP.Turbine.Wind.Force15(wind = rated.wind15),0)
Current_15 <- c(F.Current.Force15(0.2)*cos(7*pi/4), F.Current.Force15(0.2)*sin(7*pi/4)) #The magnitude of this force should be calculated in another fucntion. it depends on the floater geometry
Wave.high_15 <- c(50,0)

# V.1.3.low <- R.Vector(Wind, Wave.low, Current)
V.1.3.high_15 <- R.Vector(Wind_15, Wave.high_15, Current_15)

## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DLC1.3_15.fun <- function(wind) {
  force1.3_15 <- c()
  Current_15 <- c(F.Current.Force15(0.2)*cos(7*pi/4), F.Current.Force15(0.2)*sin(7*pi/4))
  Wave.high_15 <- c(50,0) #[-45,50]
  for(i in 1:length(wind)){
    Wind_15 <- c(DP.Turbine.Wind.Force15(wind = wind[i]),0)
    force1.3_15 <- c(force1.3_15, R.Vector(Wind_15, Wave.high_15, Current_15)[1])
  }
  return(force1.3_15)
}

DLC1.3_15 <- DLC1.3_15.fun(speeds) #RESULTING FORCES UNDER DLC1.3
(max(DLC1.3_15))


#DLC 6.1: Design Levels (single windspeed)
WIND_15 <- round(as.numeric(DP.Turbine.Wind.Force15(wind = 38)),0)
Wind2_15 <- c(WIND_15,0)
Current2_15 <- c(F.Current.Force15(0.9)*cos(7*pi/4), F.Current.Force15(0.9)*sin(7*pi/4))

phi.wave <- seq(-90,90,by = 10)
phi.rad <- phi.wave*(pi/180)

Wave.high2.mag <- 10 #Magnitude of the total wave force under these conditions in kN

V.6.1.high_15 <- c()
for(phi in phi.rad) {
  Wave.high2_15 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
  High.F_15 <- R.Vector(Wind2_15, Wave.high2_15, Current2_15)
  V.6.1.high_15 <- rbind(V.6.1.high_15, High.F_15)
}

V.6.1.high_15 <- cbind(phi.wave, V.6.1.high_15) %>%
  as_tibble() %>%
  rename(wind.wave.angle = phi.wave, force.kN =  V2,final.angle =  V3)
(max(V.6.1.high_15$force.kN))

## Given a windspeed, this function calculates the resultant force under DLC6.1 conditions
DLC6.1_15.fun <- function(wind) {
  force6.1_15 <- c() 
  Current2_15 <- c(F.Current.Force15(0.9)*cos(7*pi/4), F.Current.Force15(0.9)*sin(7*pi/4))
  
  Wave.high2.mag <- 8 #the total magnitude of the force in kN
  phi.wave <- seq(-90,90,by = 10)
  phi.rad <- phi.wave*(pi/180)
  
  for(i in 1:length(wind)) {
    Matrix.high_15 <- c()
    for(phi in phi.rad) {
      Wave.high2_15 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
      Wind.high_15 <- c(DP.Turbine.Wind.Force15(wind = wind[i]),0)
      High.F_15 <- R.Vector(Wind.high_15, Wave.high2_15, Current2_15)
      Matrix.high_15 <- rbind(Matrix.high_15, High.F_15)
    }
    force6.1_15 <- c(force6.1_15,max(Matrix.high_15[,1]))
  }
  return(force6.1_15)
}

DLC6.1_15 <- DLC6.1_15.fun(speeds)  #RESULTING FORCES UNDER DLC6.1
(tail(DLC6.1_15,1)) #At the 40 m/s wind speed

### Total Environmental Force Plot
DLC6.1_5.fun(10) #755 kN (1x)
DLC6.1_10.fun(10) #1600 kN (2.1x)
DLC6.1_15.fun(10) #2724 kN (3.6x)


plot(x = speeds, y = DLC6.1_15, type = 'n',
     ylim = c(0,3000), yaxs = 'i', xaxs = 'i',
     ylab = "",
     xlab = "Wind Speed [m/s]", las = 1, cex.axis = 1.7, cex.lab = 1.7)
lines(x = speeds, y = DLC1.3_15, col = 'black', lty = 1, lwd = 2)
lines(x = speeds, y = DLC1.3_10, col = 'blue', lty = 1, lwd = 2)
# lines(x = speeds, y = DLC6.1_10, col = 'blue', lty = 1)
lines(x = speeds, y = DLC1.3_5, col = 'red', lty = 1, lwd = 2)
# lines(x = speeds, y = DLC6.1_5, col = 'red', lty = 1)
# abline(v = 10)
mtext("Turbine Thrust Force [kN]", side = 2, line = 4.5, cex = 1.7)
legend('topright', legend = c('5 MW', '10 MW', '15 MW'), lwd = 2, col = c('red','blue','black'), cex = 1.5)


legend('topright', legend = c('Total Environmental Forces under DLC 6.1','Total Environmental Forces Under DLC 1.3'),
       col = c('blue','blue'), lty = c(1,2))

Design.F.A <- max(DLC6.1_15) #2983 kN
Design.F.B <- DLC6.1_15.fun(38) #903 kN
#Choose system(s) based on these design levels

################################################################################
## Define system and resulting DP.Power consumption
DP.Power <- function(force.kn) { #power of one thruster
  power <- (1000*(force.kn/K)^(1.5))/motor.diam
  return(power/1000) #in MW
}

DP.Power.n <- function(force.kn, motor.diam, n) { #power of n motors system, motor.diam in meters
  power <- (n/motor.diam)*(1000*force.kn/(K*n))^1.5
  return(power/1000) #in MW
}

## Calculate Parasitic Power under DLC1.3, which depends on the thruster system
parasitic.power15.1 <- DP.Power.n(DLC1.3_15, motor.diam = 1.07, n = 89)
parasitic.power15.2 <- DP.Power.n(DLC1.3_15, motor.diam = 1.98, n = 22)
parasitic.power15.3 <- DP.Power.n(DLC1.3_15, motor.diam = 4.52, n = 5)
parasitic.power15.4 <- DP.Power.n(DLC1.3_15, motor.diam = 3.35, n = 9)

parasitic.power15 <- DP.Power.n(DLC1.3_15, motor.diam = 3.8, n = 6)

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

plot(x = speeds, y = parasitic.power15.1, type = 'l', lwd = 2,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(0,20), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
lines(x = speeds, y = parasitic.power15.2, lty = 2, lwd = 2)
lines(x = speeds, y = parasitic.power15.3, lty = 3, lwd = 2, col = 'blue')
lines(x = speeds, y = parasitic.power15.4, lty = 4, lwd = 2)
lines(x = speeds, y = parasitic.power15, col = 'red')
abline(h = rated.power15, col = 'black', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Power Requirement from Motors [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topright', legend = c("n = 89, d = 1.07", 
                             "n = 22, d = 1.98", 
                             "n = 5, d = 4.52",
                             "n = 9, d = 3.35"), 
       col = 'black', lwd = 2, lty = c(1,2,3,4))

# dev.off() #uncomment if saving the image

################################################################################
# Power Curve for 15-MW Turbine

# Critical Points: (3,0), (5,1.4), (7.4,5), (9.2,10), (10.59,15), (25,15) *First three points are on a quadratic

# Quadratic Region
x <- c(3,5,7.4,9.2,10.59)
y <- c(0,1.4,5,10,15)

power.mod15 <- lm(y ~ poly(x, 2))

#just to visualize the quadratic region
x.plot <- seq(min(x), max(x), length.out=100)
y.plot <- predict(power.mod15, newdata = data.frame(x = x.plot))
plot(x,y,pch = 19)
lines(x.plot, y.plot, col = "red")

Power.Curve.15 <- function(wind.speed) { #calculate the turbine's 'raw' power output for each wind speed
  if (wind.speed < cut.in15) 0
  else if (wind.speed <= rated.wind15) as.double(predict(power.mod15, newdata = data.frame(x=wind.speed)))
  # coef(power.mod)[1] + coef(power.mod)[2]*wind.speed + coef(power.mod)[3]*wind.speed^2
  else if (wind.speed <= cut.out15) rated.power15
  else 0
}

##########################################################################
#Develop the effective DP turbine power curve vector
DP.Raw.Power15 <- sapply(speeds,Power.Curve.15)
DP.Effective.Power15 <- DP.Raw.Power15 - parasitic.power15 #15.3 looked to be the best

#PLOT
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

plot(x = speeds, y = DP.Effective.Power15, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-4,20), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power15, col = 'green')
lines(speeds, parasitic.power15, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")


# dev.off() #uncomment if saving the image
##PLOT: Showing Power Ratio
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))
plot(x = speeds, y = parasitic.power15/DP.Raw.Power15, type = 'l', lwd = 3, 
     xaxt = 'n', xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
     ylab = "", cex.lab = 1.7, cex.axis = 1.7,
     main = "", col = 'blue', col.axis = 'blue',
     ylim = c(0, 1))
mtext("Power Ratio", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)

plot(x = speeds, y = DP.Effective.Power15, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-4,20), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power15, col = 'green')
lines(speeds, parasitic.power15, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend("topright",
       legend = c('Generated Power','Thruster Power Consumption','Net Power Output','Power Ratio'),
       lty = c(1,2,1,1), col = c('green','red','black','blue'), lwd = c(1,1,2,2))

# Version 2 of the immediately above
par(mar = c(4, 5, 2, 5))
plot(x = speeds, y = DP.Effective.Power15, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n', xaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7, bty = '7',
     ylim = c(-5,20), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power15, col = 'green')
lines(speeds, parasitic.power15, col = 'red', lty = 2)
axis(side = 2, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 2, line = 3, cex = 1.7, col = "black")
axis(side = 1, pos = 0, cex.axis = 1.7)
mtext("Wind Speed [m/s]", side = 1, line = -1, cex = 1.7, col = "black")

par(new = TRUE)

plot(x = speeds, y = parasitic.power15/DP.Raw.Power15, type = 'l', lwd = 3, 
     xaxt = 'n', yaxt = 'n', xlab = "", xaxs = 'i', yaxs = 'i', las = 1,
     ylab = "", cex.lab = 1.7, cex.axis = 1.7, bty = '7',
     main = "", col = 'blue', col.axis = 'blue',
     ylim = c(-0.25, 1))
axis(side = 4,at = c(0,0.2,0.4,0.6,0.8,1),col = "blue", las = 1, cex.axis = 1.7, col.axis ="blue", col.lab = "blue")
mtext("Power Ratio", side = 4, line = 3.5, cex = 1.7, col = "blue")

legend("topright",
       legend = c('Generated Power','Thruster Power Consumption','Net Power Output','Power Ratio'),
       lty = c(1,2,1,1), col = c('green','red','black','blue'), lwd = c(1,1,2,2))

##########################################################################
## Turn effective power curve into several linear models

parasitic.power15 <- DP.Power.n(DLC1.3_15, motor.diam = 3.8, n = 6)
# P.shape <- 2.3
# P.scale <- 10

DF_Turbine15 <- data.frame(speeds,DP.Raw.Power15,Parasitic15 = parasitic.power15,DP.Effective.Power15)

#REGION 1: Before cut in wind speed
EFF.mod.R1_15 <- lm(DP.Effective.Power15 ~ poly(speeds, 1), data = filter(DF_Turbine15,speeds <= cut.in15))
#REGION 2: Cut in wind speed until rated wind speed
EFF.mod.R2_15 <- lm(DP.Effective.Power15 ~ poly(speeds, 3), data = filter(DF_Turbine15,speeds > cut.in15 & speeds <= rated.wind15))
#REGION 3: Rated wind speed until cut out wind speed
EFF.mod.R3_15 <- lm(DP.Effective.Power15 ~ poly(speeds, 8), data = filter(DF_Turbine15,speeds > rated.wind15 & speeds <= cut.out15))
#REGION 4: Above cut out wind speed
EFF.mod.R4_15 <- lm(DP.Effective.Power15 ~ poly(speeds, 2), data = filter(DF_Turbine15,speeds > cut.out15))

##PLOT: How good are these models? They look very good.
plot(x = speeds, y = DP.Effective.Power15, pch = 19, cex = 0.01)
lines(x = DF_Turbine15$speeds[speeds <= cut.in15], y = predict(EFF.mod.R1_15, newdata = filter(DF_Turbine15, speeds <= cut.in15)), col = 'red')
lines(x = DF_Turbine15$speeds[speeds > cut.in15 & speeds <= rated.wind15], y = predict(EFF.mod.R2_15, newdata = filter(DF_Turbine15, speeds > cut.in15 & speeds <= rated.wind15)), col = 'red')
lines(x = DF_Turbine15$speeds[speeds > rated.wind15 & speeds <= cut.out15], y = predict(EFF.mod.R3_15, newdata = filter(DF_Turbine15, speeds > rated.wind15 & speeds <= cut.out15)), col = 'red')
lines(x = DF_Turbine15$speeds[speeds > cut.out15], y = predict(EFF.mod.R4_15, newdata = filter(DF_Turbine15, speeds > cut.out15)), col = 'red')

############ Calculate Parasitic losses. What % goes toward thrusters?
#REGION 1: Before cut in wind speed
LOS.mod.R1_15 <- lm(Parasitic15 ~ poly(speeds, 1), data = filter(DF_Turbine15,speeds < cut.in15))
fun1_15 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_15, newdata = data.frame(speeds = x)))
#REGION 2: Cut in wind speed until rated wind speed
LOS.mod.R2_15 <- lm(Parasitic15 ~ poly(speeds, 3), data = filter(DF_Turbine15,speeds > cut.in15 & speeds <= rated.wind15))
fun2_15 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_15, newdata = data.frame(speeds = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_15 <- lm(Parasitic15 ~ poly(speeds, 5), data = filter(DF_Turbine15,speeds > rated.wind15 & speeds <= cut.out15))
fun3_15 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_15, newdata = data.frame(speeds = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_15 <- lm(Parasitic15 ~ poly(speeds, 2), data = filter(DF_Turbine15,speeds > cut.out15))
fun4_15 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_15, newdata = data.frame(speeds = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = speeds, y = DF_Turbine15$Parasitic15, pch = 19, cex = 0.01)
lines(x = DF_Turbine15$speeds[speeds <= cut.in15], y = predict(LOS.mod.R1_15, newdata = filter(DF_Turbine15, speeds <= cut.in15)), col = 'red')
lines(x = DF_Turbine15$speeds[speeds > cut.in15 & speeds <= rated.wind15], y = predict(LOS.mod.R2_15, newdata = filter(DF_Turbine15, speeds > cut.in15 & speeds <= rated.wind15)), col = 'red')
lines(x = DF_Turbine15$speeds[speeds > rated.wind15 & speeds <= cut.out15], y = predict(LOS.mod.R3_15, newdata = filter(DF_Turbine15, speeds > rated.wind15 & speeds <= cut.out15)), col = 'red')
lines(x = DF_Turbine15$speeds[speeds > cut.out15], y = predict(LOS.mod.R4_15, newdata = filter(DF_Turbine15, speeds > cut.out15)), col = 'red')

parasitic.loss15 <- 8760*(integrate(fun1_15, lower = 0, upper = cut.in15)$value + 
                            integrate(fun2_15, lower = cut.in15, upper = rated.wind15)$value + 
                            integrate(fun3_15, lower = rated.wind15, upper = cut.out15)$value +
                            integrate(fun4_15, lower = cut.out15, upper = 40)$value) #in MWh
#Result = 46153 MWh (78%)

############ Calculate Production losses. What is produced?
#REGION1 = 0
#REGION2
gen.fun2_15 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod15, newdata = data.frame(x = x)))
#REGION3
gen.fun3_15 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*rated.wind15
#REGION4 = 0

gen15 <- 8760*(integrate(gen.fun2_15, lower = cut.in15, upper = rated.wind15)$value + 
                 integrate(gen.fun3_15, lower = rated.wind15, upper = cut.out15)$value) #in MWh
#RESULT = 59463 MWh

(parasitic.loss15/gen15)
(gen15)
(parasitic.loss15)

################################################################################
################################################################################
## PART II: CAN WE REDUCE THE LOSSES AT LOW WIND SPEEDS?

## Find where the generation power curve first becomes greater than the parasitic loss curve
## Use models generated (in Region 2) to find this crossover point.
Crossover15 <- function(guess = 5, tolerance = 0.1, step = 0.1) { #guess crossover wind speed
  value <- guess
  cons <- predict(LOS.mod.R2_15, newdata = data.frame(speeds = value))
  prod <- predict(power.mod15, newdata = data.frame(x = value))
  net <- abs(prod - cons)
  while(net > tolerance) {
    value <- value + step
    cons <- predict(LOS.mod.R2_15, newdata = data.frame(speeds = value))
    prod <- predict(power.mod15, newdata = data.frame(x = value))
    net <- abs(prod - cons)
  }
  return(value)
}
Crossover15() #6.8 m/s

plot(x = speeds, y = DP.Effective.Power15, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-4,20), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
abline(v = 6.8)
lines(speeds, DP.Raw.Power15, col = 'green')
lines(speeds, parasitic.power15, col = 'red', lty = 2)
axis(side = 2, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 2, line = 3.5, cex = 1.7, col = "black")

## ADJUST POWER PRODUCTION TO CUT IN ONLY WHEN PRODUCTION IS GREATER THAN LOSSES
## Change Cut in wind speed at very top to 6.2 m/s and run functions again.
## Parasitic Loss Result = 40238 MWh (72%)
## Annual Production Result = 55626 MWh 
