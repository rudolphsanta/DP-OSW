# Rudolph Santarromana
# 10 MW Turbine
# June 2021

library(tidyverse)
library(dplyr)

setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")
## Source: Bortolotti et al. (2019): IEA Wind Task 37... Reference Wind Turbines

#################################################
### PARAMETERS
wind_height <- 100 #measuring height of wind speed
speeds <- seq(0,40, by = 0.1)
Cd_cyl <- 0.5

#WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale = 10 #10

### WIND TURBINE PROPERTIES
cut.in10 <- 4#4 #[m/s]
rated.wind10 <- 10.5 #[m/s]
cut.out10 <- 25 #[m/s]

rated.power10 <- 10.5

### FLOATER PROPERTIES
scale10.15 <- 0.8 #based on the hubheight scale (119/150)
F.freeboard10 <- 15*scale10.15 #same floater as 15MW turbine for now
F.diam10 <- 12.5*scale10.15 
F.draft10 <- 20*scale10.15 
FT.diam10 <- 10*scale10.15 

#Effective water-exposed floater area (whole substructure)
f.area.water10 <- (F.diam10*F.draft10*3) + (F.draft10*FT.diam10) #m^2

## BLADE PROPERTIES
b.length10 <- 97 #m

###TOWER PROPERTIES
#Tower dimensions
hub.height10 <- 119 #m

t.heights10 <- c(0,11.5,23,34.5,46,57.5,69,80.5,92,103,115,119) #m
t.diams10 <- c(8.3,8,7.7,7.5,7.2,6.9,6.6,6.4,6.1,5.8,5.5,5.5) #m

t.mod10 <- lm(t.diams10 ~ t.heights10) #Tower diameter prediction model
t.fun10 <- function(x) as.numeric(predict(t.mod10, newdata = data.frame(t.heights10 = x))) #tower diameter function given height

##PLOT of tower diameter model
plot(x = t.heights10, y = t.diams10, pch = 19, col = 'red')
lines(x = t.heights10, y = predict(t.mod10, newdata = data.frame(t.heights10 = t.heights10)))

## TOWER FORCE FUNCTION
t.fun10.force <- function(x) t.fun10(x - F.freeboard10)*((x)^(2*alpha)) #shifts the tower upward to the floater top height
Wind_Force_Tower10 <- function(v.meas, h.meas = wind_height, Cd = Cd_cyl, base = F.freeboard10) {
  if(base < 0) return(0)
  rho_air <- 1.23 #kg/m^3
  alpha <- 0.14
  A <- rho_air*Cd*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  #integrate from the tower base (at the top of the floater) to the hub height + floater height
  C <- integrate(t.fun10.force, lower = F.freeboard10, upper = hub.height10 + F.freeboard10)$value
  force <- (A/B)*C
  return(force) #in Newtons
}
################################################################################

## STATIC, ARRESTED BLADE FORCES
#Blades with varying diameter and Cd, and wind with varying speed
thickness10 <- c(4.6,3.1,2.1,1.9,1.9,1.8,1.7,1.3,0.5)#[m]
Cd.vect10 <-   c(0.5,0.5,0.0067,0.0096,0.0113,0.0133,0.0146,0.0146,0.0146) #[-]
dist.vect10 <- c(0,0.11,0.211,0.27,0.301,0.33,0.36,0.48,0.72,1)*b.length10 #[m]

Wind_Force_Blade10 <- function(v.meas, h.meas = wind_height, l.elev = hub.height10) {
  alpha <- 0.14
  rho_air <- 1.23 #kg/m^3
  
  A <- rho_air*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  C <- 0
  
  for (i in 1:length(thickness10)){
    fun <- function(h) return(h^(2*alpha)*thickness10[i]*Cd.vect10[i])
    component <- integrate(fun, lower = l.elev + dist.vect10[i], upper = l.elev + dist.vect10[i+1])$value
    C <- C + component
  }
  force <- (A/B)*C
  return(force) #in Newtons
}

B.Force.static10 <- Wind_Force_Blade10(speeds)/1000 #in [kN]
max(B.Force.static5)
max(B.Force.static10)
max(B.Force.static15)
################################################################################
#################################################
### ROTOR THRUST FORCE
# Recreate 3-1.b in 15 MW Reference turbine document (Bortolotti et al., 20)
#REGION 1: 3 - 10.59
X.R1_10 <- c(4,5.9,9,10.5) #m/s of windspeed
Y.R1_10 <- c(284.2,600,1294,1526) #kN of thrust

X.R1.2_10 <- X.R1_10^2
X.R1.3_10 <- X.R1_10^3

mod.R1_10 <- lm(Y.R1_10 ~ X.R1_10 + X.R1.2_10 + X.R1.3_10)

#REGION 2: 10.59 - 25
X.R2_10 <- c(10.5,11.7,16.3,25) #m/s of windspeed
Y.R2_10 <- c(1526,1200,800,568.5) #kN of thrust

X.R2.1_10 <- X.R2_10^-1
X.R2.2_10 <- X.R2_10^-2
X.R2.3_10 <- X.R2_10^3

mod.R2_10 <- lm(Y.R2_10 ~ X.R2.1_10 + X.R2.2_10)
# mod.R2 <- lm(Y.R2 ~ log(X.R2))


#plot of how model fits the data
plot(X.R1_10,Y.R1_10, pch = 19, col = 'red', xlim = c(3,25),
     xlab = "Wind Speed [m/s]", ylab = "Swept Area Thrust Force [kN]")
curve(mod.R1_10$coefficients[1] + mod.R1_10$coefficients[2]*x + mod.R1_10$coefficients[3]*x^2 + mod.R1_10$coefficients[4]*x^3, 
      from = cut.in10, to = rated.wind10, add = TRUE)
points(X.R2_10,Y.R2_10, pch = 19, col = 'red')
curve(mod.R2_10$coefficients[1] + mod.R2_10$coefficients[2]*x^-1 + mod.R2_10$coefficients[3]*x^-2 , #+ mod.R2$coefficients[4]*x^3,
      from = rated.wind10, to = cut.out10, add = TRUE)

Wind_Blade_Force_spinning10 <- function(wind) { #windspeed in m/s as input only defined on [3,25] interval
  if(wind < cut.in10 | wind > cut.out10) force <- 0
  if(wind >= cut.in10 && wind <= rated.wind10) force <- as.numeric(mod.R1_10$coefficients[1] + mod.R1_10$coefficients[2]*wind + mod.R1_10$coefficients[3]*wind^2 + mod.R1_10$coefficients[4]*wind^3)
  if(wind > rated.wind10 && wind <= cut.out10) force <- as.numeric(mod.R2_10$coefficients[1] + mod.R2_10$coefficients[2]*wind^-1 + mod.R2_10$coefficients[3]*wind^-2)
  return(force) #returns a value in kN
}

Wind_Blade_Force_spinning10(11)
#################################################
#Total wind force on the turbine assuming same tower and substructure, only the RNA has changed.
#functions in this function are contained in "Dynamic Positioning Forces 20210309 - Debugging"
DP.Turbine.Wind.Force10 <- function(wind) { 
  #non-blade components
  T.Force.best <- Wind_Force_Tower10(v.meas = wind)/1000 #in kN
  #Same Floater as the 15-MW turbine
  F.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = floater.diam, u.elev = floater.freeboard)/1000 #in [kN]
  FT.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = tower.diam, u.elev = floater.freeboard)/1000 #in [kN]
  
  #blade spinning component
  B.Force.best.spinning <- Wind_Blade_Force_spinning10(wind) #in kN. returns 0 when blades are static
  
  #blade static component. uses the static blades geometry of the 15-MW turbine. Overestimated
  if(wind < cut.in10 | wind > cut.out10) B.Force.static10 <- Wind_Force_Blade10(wind)/1000  else B.Force.static10 <- 0 #in [kN]. returns 0 when blades are spinning
  
  #Blade Total Forces vs. windspeed
  Tot.Wind.Force <- T.Force.best + 
    (3*B.Force.static10) + 
    B.Force.best.spinning + 
    (3*F.Force.height) + 
    FT.Force.height
  
  return(Tot.Wind.Force)
}

Wind.Forces10 <- sapply(speeds,DP.Turbine.Wind.Force10)
DP.Turbine.Wind.Force10(38)
DP.Turbine.Wind.Force5(38)

plot(x = speeds, y = Wind.Forces10, type = 'l')

## CURRENT FORCE MAGNITUDE
F.Current.Force10 <- function(current) {
  #Current_Force function is in 15MW turbine file
  F.Force.Current <- Current_Force(speed = current, f.area.water10,Cd_cyl)/1000 #in kN
  return(F.Force.Current)
}

################################################################################
### COMBINED ENVIRONMENTAL FORCES
# Considers 15 MW Floater and Tower
#DLC 1.3: Design Levels (single windspeed)
Wind_10 <- c(DP.Turbine.Wind.Force10(wind = rated.wind10),0)
Current_10 <- c(F.Current.Force10(0.2)*cos(7*pi/4), F.Current.Force10(0.2)*sin(7*pi/4))
Wave.high_10 <- c(50,0) #Calculated in "Wave Drift Forces.R"

# V.1.3.low <- R.Vector(Wind, Wave.low, Current)
V.1.3.high_10 <- R.Vector(Wind_10, Wave.high_10, Current_10)

## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DLC1.3_10.fun <- function(wind) {
  force1.3_10 <- c()
  Current_10 <- c(F.Current.Force10(0.2)*cos(7*pi/4), F.Current.Force10(0.2)*sin(7*pi/4))
  Wave.high_10 <- c(50,0)
  for(i in 1:length(wind)){
    Wind_10 <- c(DP.Turbine.Wind.Force10(wind = wind[i]),0)
    force1.3_10 <- c(force1.3_10, R.Vector(Wind_10, Wave.high_10, Current_10)[1])
  }
  return(force1.3_10)
}

DLC1.3_10 <- DLC1.3_10.fun(speeds) #RESULTING FORCES UNDER DLC1.3

(max(DLC1.3_10))

#DLC 6.1: Design Levels (single windspeed)
WIND_10 <- round(as.numeric(DP.Turbine.Wind.Force10(wind = 38)),0)
Wind2_10 <- c(WIND_10,0)
Current2_10 <- c(F.Current.Force10(0.9)*cos(7*pi/4), F.Current.Force10(0.9)*sin(7*pi/4))

phi.wave <- seq(-90,90,by = 10)
phi.rad <- phi.wave*(pi/180)

Wave.high2.mag <- 10

V.6.1.high_10 <- c()
for(phi in phi.rad) {
  Wave.high2_10 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
  High.F_10 <- R.Vector(Wind2_10, Wave.high2_10, Current2_10)
  V.6.1.high_10 <- rbind(V.6.1.high_10, High.F_10)
}

V.6.1.high_10 <- cbind(phi.wave, V.6.1.high_10) %>%
  as_tibble() %>%
  rename(wind.wave.angle = phi.wave, force.kN =  V2,final.angle =  V3)
(max(V.6.1.high_10$force.kN))

## Given a windspeed, this function calculates the resultant force under DLC6.1 conditions
DLC6.1_10.fun <- function(wind) {
  force6.1_10 <- c() 
  Current2_10 <- c(F.Current.Force10(0.9)*cos(7*pi/4), F.Current.Force10(0.9)*sin(7*pi/4))
  
  Wave.high2.mag <- 10 #the total magnitude of the force in kN
  phi.wave <- seq(-90,90,by = 10)
  phi.rad <- phi.wave*(pi/180)
  
  for(i in 1:length(wind)) {
    Matrix.high_10 <- c()
    for(phi in phi.rad) {
      Wave.high2_10 <- c(Wave.high2.mag*cos(phi), Wave.high2.mag*sin(phi))
      Wind.high_10 <- c(DP.Turbine.Wind.Force10(wind = wind[i]),0)
      High.F_10 <- R.Vector(Wind.high_10, Wave.high2_10, Current2_10)
      Matrix.high_10 <- rbind(Matrix.high_10, High.F_10)
    }
    force6.1_10 <- c(force6.1_10,max(Matrix.high_10[,1]))
  }
  return(force6.1_10)
}

DLC6.1_10 <- DLC6.1_10.fun(speeds)  #RESULTING FORCES UNDER DLC6.1

(tail(DLC6.1_10,1))
### Total Environmental Force Plot
plot(x = speeds, y = DLC6.1_10, type = 'l',
     ylim = c(0,3000), yaxs = 'i', xaxs = 'i',
     ylab = "Turbine Thrust Force [kN]",
     xlab = "Wind Speed [m/s]")
lines(x = speeds, y = DLC1.3_10, col = 'blue')

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

## Calculate Parasitic Power under DLC1.3, which depends on the thruster system
parasitic.power10.1 <- DP.Power.n(DLC1.3_10, motor.diam = 1.07, n = 52)
parasitic.power10.2 <- DP.Power.n(DLC1.3_10, motor.diam = 1.98, n = 13)
parasitic.power10.3 <- DP.Power.n(DLC1.3_10, motor.diam = 4.52, n = 4)
parasitic.power10.4 <- DP.Power.n(DLC1.3_10, motor.diam = 3.35, n = 6)

parasitic.power10 <- DP.Power.n(DLC1.3_10, motor.diam = 2.66, n = 6)

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

plot(x = speeds, y = parasitic.power10.1, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(0,12), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
lines(x = speeds, y = parasitic.power10.2, lty = 2, lwd = 3)
lines(x = speeds, y = parasitic.power10.3, lty = 3, lwd = 3, col = 'green')
lines(x = speeds, y = parasitic.power10.4, lty = 4, lwd = 3)
lines(x = speeds, y = parasitic.power10, lwd = 3, col = 'red')

abline(h = 10.5, col = 'black', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Power Requirement from Motors [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topleft', legend = c("n = 52, d = 1.07, $31MM", 
                             "n = 13, d = 1.98, $32MM", 
                             "n = 4, d = 4.52, $60MM",
                             "n = 6, d = 3.35, $$42MM"), 
       col = 'black', lwd = 3, lty = c(1,2,3,4))

# dev.off() #uncomment if saving the image

######Power Curve for 10 MW Turbine
# Quadratic Region
x <- c(4,6,10.5)
y <- c(0,2,10.521) #in [MW]

power.mod10 <- lm(y ~ poly(x, 2))

Power.Curve.10 <- function(wind.speed) { #calculate the turbine's 'raw' power output for each wind speed
  if (wind.speed < cut.in10) 0
  else if (wind.speed <= rated.wind10) as.double(predict(power.mod10, newdata = data.frame(x=wind.speed)))
  # coef(power.mod)[1] + coef(power.mod)[2]*wind.speed + coef(power.mod)[3]*wind.speed^2
  else if (wind.speed <= cut.out10) rated.power10
  else 0
}

##########################################################################
#Develop the effective DP turbine power curve vector
DP.Raw.Power10 <- sapply(speeds,Power.Curve.10)
DP.Effective.Power10 <- DP.Raw.Power10 - parasitic.power10#10.3 looked to be the best

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

plot(x = speeds, y = DP.Effective.Power10, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-4,12), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power10, col = 'green')
lines(speeds, parasitic.power10.3, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")

# dev.off() #uncomment if saving the image

##PLOT: Showing Power Ratio
# png(filename = "20210616 Parasiti Combined.png", width = 780, height = 480)
par(mar = c(5, 6, 3, 6))
plot(x = speeds, y = parasitic.power10.3/DP.Raw.Power10, type = 'l', lwd = 3, 
     xaxt = 'n', xlab = "Wind Speed [m/s]", xaxs = 'i', yaxs = 'i', las = 1,
     ylab = "", cex.lab = 1.7, cex.axis = 1.7,
     main = "", col = 'blue', col.axis = 'blue',
     ylim = c(0, 1))
mtext("Power Ratio", side = 2, line = 4.5, cex = 1.7, col = "blue")

par(new = TRUE)

plot(x = speeds, y = DP.Effective.Power10, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-2,15), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
lines(speeds, DP.Raw.Power10, col = 'green')
lines(speeds, parasitic.power10.3, col = 'red', lty = 2)
axis(side = 4, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 4, line = 3.5, cex = 1.7, col = "black")
legend('topright', legend = c("Power Ratio (DP Consumption/Power Generated)", "Net Power", "Generated Power", "Thruster Power Consumption"),
       lty = c(1,1,1,2), col = c('blue','black','green','red'), lwd = c(3,3,1,1), cex = 0.8)

######################## Turn effective power curve into several linear models
#WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale =10.82 #10

parasitic.power10 <- DP.Power.n(DLC1.3_10, motor.diam = 3.5, n = 4)


DF_Turbine10 <- data.frame(speeds,DP.Raw.Power10,Parasitic10 = parasitic.power10,DP.Effective.Power10)

#REGION 1: Before cut in wind speed
EFF.mod.R1_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 1), data = filter(DF_Turbine10,speeds <= cut.in10))
#REGION 2: Cut in wind speed until rated wind speed
EFF.mod.R2_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 3), data = filter(DF_Turbine10,speeds > cut.in10 & speeds <= rated.wind10))
#REGION 3: Rated wind speed until cut out wind speed
EFF.mod.R3_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 4), data = filter(DF_Turbine10,speeds > rated.wind10 & speeds <= cut.out10))
#REGION 4: Above cut out wind speed
EFF.mod.R4_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 2), data = filter(DF_Turbine10,speeds > cut.out10))

##PLOT: How good are these models? They look very good.
plot(x = speeds, y = DP.Effective.Power10, pch = 19, cex = 0.01)
lines(x = DF_Turbine10$speeds[speeds <= cut.in10], y = predict(EFF.mod.R1_10, newdata = filter(DF_Turbine10, speeds <= cut.in10)), col = 'red')
lines(x = DF_Turbine10$speeds[speeds > cut.in10 & speeds <= rated.wind10], y = predict(EFF.mod.R2_10, newdata = filter(DF_Turbine10, speeds > cut.in10 & speeds <= rated.wind10)), col = 'red')
lines(x = DF_Turbine10$speeds[speeds > rated.wind10 & speeds <= cut.out10], y = predict(EFF.mod.R3_10, newdata = filter(DF_Turbine10, speeds > rated.wind10 & speeds <= cut.out10)), col = 'red')
lines(x = DF_Turbine10$speeds[speeds > cut.out10], y = predict(EFF.mod.R4_10, newdata = filter(DF_Turbine10, speeds > cut.out10)), col = 'red')


############ Calculate Parasitic losses. What % goes toward thrusters?
#REGION 1: Before cut in wind speed
LOS.mod.R1_10 <- lm(Parasitic10 ~ poly(speeds, 1), data = filter(DF_Turbine10,speeds <= cut.in10))
fun1_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_10, newdata = data.frame(speeds = x)))
#REGION 2: Cut in wind speed until rated wind speed
LOS.mod.R2_10 <- lm(Parasitic10 ~ poly(speeds, 3), data = filter(DF_Turbine10,speeds > cut.in10 & speeds <= rated.wind10))
fun2_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_10, newdata = data.frame(speeds = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_10 <- lm(Parasitic10 ~ poly(speeds, 5), data = filter(DF_Turbine10,speeds > rated.wind10 & speeds <= cut.out10))
fun3_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_10, newdata = data.frame(speeds = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_10 <- lm(Parasitic10 ~ poly(speeds, 2), data = filter(DF_Turbine10,speeds > cut.out10))
fun4_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_10, newdata = data.frame(speeds = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = speeds, y = DF_Turbine10$Parasitic10, pch = 19, cex = 0.01)
lines(x = DF_Turbine10$speeds[speeds <= cut.in10], y = predict(LOS.mod.R1_10, newdata = filter(DF_Turbine10, speeds <= cut.in10)), col = 'red')
lines(x = DF_Turbine10$speeds[speeds > cut.in10 & speeds <= rated.wind10], y = predict(LOS.mod.R2_10, newdata = filter(DF_Turbine10, speeds > cut.in10 & speeds <= rated.wind10)), col = 'red')
lines(x = DF_Turbine10$speeds[speeds > rated.wind10 & speeds <= cut.out10], y = predict(LOS.mod.R3_10, newdata = filter(DF_Turbine10, speeds > rated.wind10 & speeds <= cut.out10)), col = 'red')
lines(x = DF_Turbine10$speeds[speeds > cut.out10], y = predict(LOS.mod.R4_10, newdata = filter(DF_Turbine10, speeds > cut.out10)), col = 'red')

parasitic.loss10 <- 8760*(integrate(fun1_10, lower = 0, upper = cut.in10)$value + 
                            integrate(fun2_10, lower = cut.in10, upper = rated.wind10)$value + 
                            integrate(fun3_10, lower = rated.wind10, upper = cut.out10)$value +
                            integrate(fun4_10, lower = cut.out10, upper = 40)$value) #in MWh
#Result = 22377 MWh (43%)

############ Calculate Production losses. What is produced?
#REGION1 = 0
#REGION2
gen.fun2_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod10, newdata = data.frame(x = x)))
#REGION3
gen.fun3_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*rated.wind10
#REGION4 = 0

gen10 <- 8760*(integrate(gen.fun2_10, lower = cut.in10, upper = rated.wind10)$value + 
                 integrate(gen.fun3_10, lower = rated.wind10, upper = cut.out10)$value) #in MWh
#RESULT = 51755 MWh

(parasitic.loss10/gen10)
(gen10)
(parasitic.loss10)


################################################################################
################################################################################
## PART II: CAN WE REDUCE THE LOSSES AT LOW WIND SPEEDS?

## Find where the generation power curve first becomes greater than the parasitic loss curve
## Use models generated (in Region 2) to find this crossover point.
Crossover10 <- function(guess = 4, tolerance = 0.1, step = 0.1) { #guess crossover wind speed
  value <- guess
  cons <- predict(LOS.mod.R2_10, newdata = data.frame(speeds = value))
  prod <- predict(power.mod10, newdata = data.frame(x = value))
  net <- abs(prod - cons)
  while(net > tolerance) {
    value <- value + step
    cons <- predict(LOS.mod.R2_10, newdata = data.frame(speeds = value))
    prod <- predict(power.mod10, newdata = data.frame(x = value))
    net <- abs(prod - cons)
  }
  return(value)
}
Crossover10() #4.8 m/s

plot(x = speeds, y = DP.Effective.Power10, type = 'l', lwd = 3,
     pch = 20, xaxs ='i',yaxs='i',las = 1, yaxt = 'n',
     cex.lab = 1.7, cex.axis = 1.7,
     ylim = c(-2,12), xlim = c(0,40),
     ylab = "",
     xlab = "",
     main = "")
abline(h = 0, col = 'gray')
abline(v = 4.8)
lines(speeds, DP.Raw.Power10, col = 'green')
lines(speeds, parasitic.power10.3, col = 'red', lty = 2)
axis(side = 2, col = "black", las = 1, cex.axis = 1.7, col.axis ="black", col.lab = "black")
mtext("Net Power Output [MW]", side = 2, line = 3.5, cex = 1.7, col = "black")

## ADJUST POWER PRODUCTION TO CUT IN ONLY WHEN PRODUCTION IS GREATER THAN LOSSES
## Change Cut in wind speed at very top to 6.2 m/s and run functions again.
## Parasitic Loss Result = 22071 MWh (43%)
## Annual Production Result = 51598 MWh 



#FUNCTIONALIZED POWER CURVES (Lines above)
#WEIBULL PARAMETERS
sensitivity_10 <- function(shape, scale, OCC.High, OCC.Low, FOM.High, FOM.Low, disc.r, years = 25, fs = 1.67) {
  P.shape = shape
  P.scale = scale
  Turbine <- 10
  # #From NREL ATB 2021
  
  CRF <- 0.049 #capital recovery factor
  PFF <- 1.045 #production finance factor
  CFF <- 1.075 #construction finance factor
  
  mooring.proportion <- 0.11 #percent of project cost that mooring comprises. From James and Ros
  #DP Thrusters
  DP_Thrusters <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning/Kongsberg Thruster Specs/Kongsberg Thrusters.csv") %>%
    rename("Name" = "ï..Name")
  
  diameters <- DP_Thrusters$PropellerD_m
  number <- seq(1,30)
  
  #initialize empty matrices
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
      if(num*DP_Force < max(DLC1.3_10)*fs) next #if this configuration doesn't provide the max needed station keeping force, the configuration does not work, and move to next one
      
      parasitic.power10 <- DP.Power.n(DLC1.3_10, motor.diam = DP_Diam, n = num)
      
      DF_Turbine10 <- data.frame(speeds,DP.Raw.Power10,Parasitic10 = parasitic.power10,DP.Effective.Power10)
      
      #REGION 1: Before cut in wind speed
      EFF.mod.R1_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 1), data = filter(DF_Turbine10,speeds <= cut.in10))
      #REGION 2: Cut in wind speed until rated wind speed
      EFF.mod.R2_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 3), data = filter(DF_Turbine10,speeds > cut.in10 & speeds <= rated.wind10))
      #REGION 3: Rated wind speed until cut out wind speed
      EFF.mod.R3_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 4), data = filter(DF_Turbine10,speeds > rated.wind10 & speeds <= cut.out10))
      #REGION 4: Above cut out wind speed
      EFF.mod.R4_10 <- lm(DP.Effective.Power10 ~ poly(speeds, 2), data = filter(DF_Turbine10,speeds > cut.out10))
      
      ############ Calculate Parasitic losses. What % goes toward thrusters?
      #REGION 1: Before cut in wind speed
      LOS.mod.R1_10 <- lm(Parasitic10 ~ poly(speeds, 1), data = filter(DF_Turbine10,speeds <= cut.in10))
      fun1_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_10, newdata = data.frame(speeds = x)))
      #REGION 2: Cut in wind speed until rated wind speed
      LOS.mod.R2_10 <- lm(Parasitic10 ~ poly(speeds, 3), data = filter(DF_Turbine10,speeds > cut.in10 & speeds <= rated.wind10))
      fun2_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_10, newdata = data.frame(speeds = x)))
      #REGION 3: Rated wind speed until cut out wind speed
      LOS.mod.R3_10 <- lm(Parasitic10 ~ poly(speeds, 5), data = filter(DF_Turbine10,speeds > rated.wind10 & speeds <= cut.out10))
      fun3_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_10, newdata = data.frame(speeds = x)))
      #REGION 4: Above cut out wind speed
      LOS.mod.R4_10 <- lm(Parasitic10 ~ poly(speeds, 2), data = filter(DF_Turbine10,speeds > cut.out10))
      fun4_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_10, newdata = data.frame(speeds = x)))
     
      parasitic.loss10 <- 8760*(integrate(fun1_10, lower = 0, upper = cut.in10)$value + 
                                  integrate(fun2_10, lower = cut.in10, upper = rated.wind10)$value + 
                                  integrate(fun3_10, lower = rated.wind10, upper = cut.out10)$value +
                                  integrate(fun4_10, lower = cut.out10, upper = 40)$value) #in MWh
      
      ############ Calculate Production losses. What is produced?
      #REGION1 = 0
      #REGION2
      gen.fun2_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod10, newdata = data.frame(x = x)))
      #REGION3
      gen.fun3_10 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*rated.wind10
      #REGION4 = 0
      
      gen10 <- 8760*(integrate(gen.fun2_10, lower = cut.in10, upper = rated.wind10)$value + 
                       integrate(gen.fun3_10, lower = rated.wind10, upper = cut.out10)$value) #in MWh
      
      net10 <- gen10 - parasitic.loss10 #annual expected net electricity output in MWh
      
      potential10 <- 8760*Turbine #max potential MWh per year
      
      netCF10 <- net10/potential10 #percent net CF
      
      #Populate Matrices
      DP_INV <- DP_Price*num/1000000 #MUSD. Incurred in first year, no discounting
      DP_CF <- 0 #O&M set to zero for now
      
      NPV_Cost <-FinancialMath::NPV(cf0 = DP_INV, cf = rep(DP_CF, times = years),times = seq(1,years),i = disc.r)
      NPV_Energy <- FinancialMath::NPV(cf0 = 0,cf = rep(net10,times = years),times = seq(1,years),i = disc.r)
      
      LTCOE <- -1000000*NPV_Cost/NPV_Energy #USD/MWh
      netOCC.High <- OCC.High*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000)) #in [$/kW]
      netOCC.Low <- OCC.Low*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000))
      
      LCOE.High <- (CRF*PFF*CFF*(netOCC.High)+FOM.High)*1000/(netCF10*8760)
      LCOE.Low <- (CRF*PFF*CFF*(netOCC.Low)+FOM.Low)*1000/(netCF10*8760)
      
      #Populate Matrices
      MATRIX.capex[num,TYPE] <- DP_INV
      MATRIX.loss.ratio[num,TYPE] <- parasitic.loss10/gen10 #annual percent of energy for DP
      MATRIX.ltcoe[num,TYPE] <- LTCOE
      MATRIX.lcoe.h[num,TYPE] <- LCOE.High
      MATRIX.lcoe.l[num,TYPE] <- LCOE.Low
    }
  }
  print(gen10/(8760*Turbine))
  return(list(MATRIX.loss.ratio, MATRIX.capex, MATRIX.ltcoe, MATRIX.lcoe.h, MATRIX.lcoe.l))
}

sensitivity_10(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)

