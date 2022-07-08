# Created by Rudolph Santarromana
# August 2021
# Turbine Parasitic Losses based on Design Envelope

library(tidyverse)
library(dplyr)

## CONSTANTS
rho <- 1.225 #in [kg/m^3]
Cd_cyl <- 0.5

## WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale = 7.95 #10

## FULL INDEX
WIND <- seq(0,40, by = 0.5)

#########################

# functions for parameters 

#calculate Cp and Ct given a
Cp_a <- function(a) if(a > 0.5) return("Invalid") else 4*a*(1-a)^2
Ct_a <- function(a) if(a > 0.5) return("Invalid") else 4*a*(1-a)

#Calculate a given Cp or Ct
a_Cp <- function(Cp) {
  roots <- polyroot(c(-Cp,4,-8,4))
  roots <- roots[abs(Im(roots)) < (0.018)]
  roots <- suppressWarnings(as.numeric(roots))
  roots <- roots[roots <= 0.5]
  return(roots)
}

a_Ct <- function(Ct) {
  roots <- polyroot(c(-Ct, 4, -4))
  roots <- roots[abs(Im(roots)) < (0.0001)]
  roots <- suppressWarnings(as.numeric(roots))
  roots <- roots[roots <= 0.5]
  return(roots)
}

#Calculate Cp given Ct and vis versa. Not needed
Cp_Ct <- function(Ct) {
  a1 <- a_Ct(Ct)
  Cp <- Cp_a(a1)
  return(Cp)
}

Ct_Cp <- function(Cp) {
  a1 <- a_Ct(Cp)
  Ct <- Ct_a(a1)
  return(Ct)
}

#Calculate Thrust given Ct
Thrust <- function(Ct,area,windspeed) return(0.5*Ct*rho*area*windspeed^2)

#wind force with height on a constant cross section body. equation by ABS, 2013
#Duplicate from "20210622 15 MW Turbine and Functions.R"
Wind_Force_ConsCross <- function(v.meas, u.elev, Cd, D, h.meas = 100, l.elev = 0) {
  rho_air <- 1.23
  alpha <- 0.14 #from ABS, 2013
  A <- (v.meas^2)*rho_air*D*Cd
  B <- 2*(h.meas^(2*alpha))*((2*alpha)+1)
  C_H <- u.elev^((2*alpha) + 1)
  C_0 <- l.elev^((2*alpha) + 1)
  force <- (A/B)*(C_H - C_0)
  return(force) #Returns values in Newtons
}

#Linear extrapolation of tower (base) diameter based on reference turbines
Rturb <- c(15,10,8,5) #in [MW]
Htower <- c(150,119,110,90)
Dtower <- c(10,8.3,7.7,6)
plot(Htower,Dtower)
mod.diam <- lm(Dtower ~ Htower)
Pred.tower <- function(Htower) return(as.numeric(mod.diam$coefficients[1]+mod.diam$coefficients[2]*Htower)) #in [m]

#Linear extrapolation of static arrested blades based on turbine size
statblade5 <- mapply(Wind_Force_Blade5,v.meas = winds_T1)
statblade8 <- mapply(Wind_Force_Blade8,v.meas = winds_T1)
statblade10 <- mapply(Wind_Force_Blade10,v.meas = winds_T1)
statblade15 <- mapply(Wind_Force_Blade15,v.meas = winds_T1)
statblade3.3 <- mapply(Wind_Force_Blade15,v.meas = winds_T1)*Statblade.scale(Blength_3.3)

statblade5/statblade15 #28% of 15MW force at all wind speeds
statblade10/statblade15 #86% of 15MW force at all wind speeds
statblade8/statblade15 #64% of 15MW force at all wind speeds

turb <- c(15,10,8,5) #in [MW]
force <- c(1,0.857,0.64,0.281) #in percent of 15 MW static blade force
plot(turb,force)
mod.statblade <- lm(force ~ turb)
preds <- predict(mod.statblade)
lines(turb,preds)
Pred.statblade.force <- function(Tpower) return(as.numeric(mod.statblade$coefficients[1]+mod.statblade$coefficients[2]*Tpower)) #in [kN]

#linear extrapolation of static arrested blades based on blade length
b.length <- c(117,97,82,61.5)
plot(b.length,force)
mod.statblade2 <- lm(force ~ b.length)
preds2 <- predict(mod.statblade2)
lines(b.length,preds2)
Statblade.scale <- function(Blength) return(as.numeric(mod.statblade2$coefficients[1]+mod.statblade2$coefficients[2]*Blength)) #in [kN]

################################################################################

## Vestas V112-3.3
#(Source: https://en.wind-turbine-models.com/turbines/693-vestas-v112-3.3)
#GIVEN PARAMETERS
RatPower_3.3 <- 3.3 #in [MW]
Rdiam_3.3 <- 112 #[m]
Rarea_3.3 <- pi*(Rdiam_3.3/2)^2 #[m^2]
Theight_3.3.l <- 84
Theight_3.3.h <- 84 #84,140
C.in_3.3 <- 3 #in [m/s]
C.out_3.3 <- 25 #in [m/s]
R.wind_3.3 <- 13 #in [m/s]

#ESTIMATED PARAMETERS
Tdiam_3.3 <- Pred.tower(Theight_3.3.h)
Blength_3.3 <- Rdiam_3.3/2 #in [m]. 56 m

#ASSUMED FLOATER PARAMETERS. 15MW floater maintained
FLdiam_3.3 <- 12.5
FLfreeboard_3.3 <- 15
FLdraft_3.3 <- 20

SubArea_3.3 <- (FLdiam_3.3*FLdraft_3.3*3) + (FLdraft_3.3*Tdiam_3.3) #m^2

#######
## WIND THRUST FORCES
#Power Curve, Cp, and Ct for cut-in < wind < cut-out
DF_power_3.3 <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning/V112-3.3 Power Curve Data.csv") %>%
  rename(Wind.speed = ï..Wind.speed) %>%
  mutate(Power_MW = Power_kw/1000)

Ct_3.3 <- DF_power_3.3$Ct
winds_3.3 <- DF_power_3.3$Wind.speed

plot(DF_power_3.3$Wind.speed,DF_power_3.3$Power_MW)
abline(v = R.wind_3.3)

#Continuous function for ramp-up
y3 <- filter(DF_power_3.3, Power_MW < RatPower_3.3)
x3 <- y3$Wind.speed
y3 <- y3$Power_MW

power.mod_3.3 <- lm(y3 ~ poly(x3, 7))

# #Check the model vs the points
plot(x3, y3)
lines(x3, y = predict(power.mod_3.3, newdata = data.frame(x = x3)), col = 'red')

#fucntionalized power curve
Power.Curve.3.3 <- function(wind.speed) { #calculate the turbine's 'raw' power output for each wind speed
  if (wind.speed < C.in_3.3) 0
  else if (wind.speed <= R.wind_3.3) as.double(predict(power.mod_3.3, newdata = data.frame(x3 = wind.speed)))
  # coef(power.mod)[1] + coef(power.mod)[2]*wind.speed + coef(power.mod)[3]*wind.speed^2
  else if (wind.speed <= C.out_3.3) RatPower_3.3
  else 0
}

#ROTOR THRUST FORCES. cut-in < wind < cut-out
Thrust_3.3 <- mapply(Thrust, Ct = Ct_3.3, area = Rarea_3.3, windspeed = winds_3.3)/1000 #in [kN]

dt_3.3 <- data.frame(WIND = winds_3.3,Ct_3.3, Thrust_3.3)
plot(winds_3.3,Thrust_3.3, ylim = c(0,500), pch = 19, ylab = "Thrust Force (kN)", xlab = "Wind Speed (m/s)")

W.max.thrust3.3 <- dt_3.3[dt_3.3$Thrust_3.3==max(dt_3.3$Thrust_3.3),"WIND"] #10.5 m/s where max thrust force is felt
# Turn the spinning thrust forces into a linear model
#REGION 1: 3 - 10.5
X.R1_3.3 <- dt_3.3$WIND[dt_3.3$WIND <= W.max.thrust3.3] #m/s of windspeed
Y.R1_3.3 <- dt_3.3$Thrust_3.3[dt_3.3$WIND <= W.max.thrust3.3] #kN of thrust

X.R1.2_3.3 <- X.R1_3.3^2

mod.R1_3.3 <- lm(Y.R1_3.3 ~ X.R1_3.3 + X.R1.2_3.3)

#REGION 2: 10.5 - 25
X.R2_3.3 <- dt_3.3$WIND[dt_3.3$WIND > W.max.thrust3.3] #m/s of windspeed
Y.R2_3.3 <- dt_3.3$Thrust_3.3[dt_3.3$WIND > W.max.thrust3.3] #kN of thrust

X.R2.1_3.3 <- X.R2_3.3^-1
X.R2.2_3.3 <- X.R2_3.3^-2
X.R2.3_3.3 <- X.R2_3.3^-3

mod.R2_3.3 <- lm(Y.R2_3.3 ~ X.R2.1_3.3 + X.R2.2_3.3 + X.R2.3_3.3)

#plot of how model fits the data. It fits very well
#REGION 1
plot(X.R1_3.3,Y.R1_3.3, pch = 19, col = 'red', xlim = c(3,25), ylim = c(0,500),
     xlab = "Wind Speed [m/s]", ylab = "Swept Area Thrust Force [kN]", xaxs = 'i', yaxs = 'i',)
curve(mod.R1_3.3$coefficients[1] + mod.R1_3.3$coefficients[2]*x + mod.R1_3.3$coefficients[3]*x^2, 
      from = C.in_3.3, to = W.max.thrust3.3, add = TRUE)
#REGION 2
points(X.R2_3.3,Y.R2_3.3, pch = 19, col = 'red')
curve(mod.R2_3.3$coefficients[1] + mod.R2_3.3$coefficients[2]*x^-1 + mod.R2_3.3$coefficients[3]*x^-2 + mod.R2_3.3$coefficients[4]*x^-3,
      from = W.max.thrust3.3, to = C.out_3.3, add = TRUE)

#functionalize the models
Wind_Blade_Force_spinning3.3 <- function(wind) { #windspeed in m/s as input only defined on [3,25] interval
  if(wind < C.in_3.3 | wind > C.out_3.3) force <- 0
  if(wind >= C.in_3.3 && wind <= W.max.thrust3.3) force <- as.numeric(mod.R1_3.3$coefficients[1] + mod.R1_3.3$coefficients[2]*wind + mod.R1_3.3$coefficients[3]*wind^2)
  if(wind > W.max.thrust3.3 && wind <= C.out_3.3) force <- as.numeric(mod.R2_3.3$coefficients[1] + mod.R2_3.3$coefficients[2]*wind^-1 + mod.R2_3.3$coefficients[3]*wind^-2 + mod.R2_3.3$coefficients[4]*wind^-3)
  return(force) #returns a value in kN
}
Wind_Blade_Force_spinning3.3(10.5) #462.33

#ARRESTED BLADE FORCES. Scale from other arrested blade forces. wind > 0 and blades not spinning
StatBlade_3.3.l <- Statblade.scale(Blength_3.3)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_3.3.l)/1000
StatBlade_3.3.h <- Statblade.scale(Blength_3.3)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_3.3.h)/1000 #in [kN]

#TOWER THRUST FORCES. wind > 0
TowThrust_3.3.l <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_3.3.l, Cd = Cd_cyl, D = Tdiam_3.3)/1000 #in [kN]

TowThrust_3.3.h <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_3.3.h, Cd = Cd_cyl, D = Tdiam_3.3)/1000

#FLOATER FORCES. wind > 0
FThrust_3.3 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = FLdiam_3.3, u.elev = FLfreeboard_3.3)/1000 #in [kN]
FTThrust_3.3 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = Tdiam_3.3, u.elev = FLfreeboard_3.3)/1000 #in [kN]

SubThrust_3.3 <- (FThrust_3.3*3) + FTThrust_3.3 #in [kN]

df_3.3 <- data.frame(WIND, StatBlade_3.3.h,TowThrust_3.3.h, SubThrust_3.3)

#Wind forces DF
DF_3.3 <- merge(dt_3.3, df_3.3, by = "WIND", all = TRUE) #Table of wind forces
DF_3.3["TotThrust_3.3"] <- 0
for(row in 1:nrow(DF_3.3)){
  if(is.na(DF_3.3[row,]$Thrust_3.3)) {
    DF_3.3[row,"TotThrust_3.3"] <- sum(DF_3.3[row,4:6]) 
  } else {
    DF_3.3[row,"TotThrust_3.3"] <- sum(DF_3.3[row,c(3,5,6)])
  }
}
RotorThrust_3.3 <- DF_3.3$TotThrust_3.3 #in [kN]

#Visualize all wind thrust forces together
plot(WIND,SubThrust_3.3, ylim = c(0,1000), type = 'l')
lines(WIND, StatBlade_3.3.h, lty = 2, col = 'blue')
lines(WIND, TowThrust_3.3.h, col = 'green') #This force is reaaalllyyy high.
lines(winds_3.3, Thrust_3.3, col = 'red')
lines(WIND,RotorThrust_3.3, lwd = 2)
# lines(speeds, T.Force.best) #by comparison, the 15MW tower with tapering tower diameter.

# ####tower forces comparison
# T.Force.best.5 <- Wind_Force_Tower5(v.meas = 30)/1000 #in kN
# T.Force.best.3 <- Wind_Force_ConsCross(v.meas=30, u.elev=Theight_3.3.h, Cd=Cd_cyl, D=Tdiam_3.3, h.meas=wind_height, l.elev = FLfreeboard_3.3)/1000 #in kN


DP.Turbine.Wind.Force3.3 <- function(wind) { #functions in this function are contained in "Dynamic Positioning Forces 20210309 - Debugging"
  #non-blade components
  T.Force.best <- Wind_Force_ConsCross(v.meas=wind, u.elev=Theight_3.3.h, Cd=Cd_cyl, D=Tdiam_3.3, h.meas=wind_height, l.elev = FLfreeboard_3.3)/1000 #in kN
  F.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = FLdiam_3.3, u.elev = FLfreeboard_3.3)/1000 #in [kN]
  FT.Force.height <- Wind_Force_ConsCross(wind, Cd = Cd_cyl, D = Tdiam_3.3, u.elev = FLfreeboard_3.3)/1000 #in [kN]
  
  #blade spinning component
  B.Force.best.spinning <- Wind_Blade_Force_spinning3.3(wind) #in kN. returns 0 when blades are static
  
  #blade static component
  if(wind < C.in_3.3 | wind > C.out_3.3) B.Force.static <- Statblade.scale(Blength_3.3)*Wind_Force_Blade15(wind,h.meas = 100, l.elev=Theight_3.3.l)/1000  else B.Force.static <- 0 #in [kN]. returns 0 when blades are spinning
  
  #Blade Total Forces vs. windspeed
  Tot.Wind.Force <- T.Force.best + 
    (3*B.Force.static) + 
    B.Force.best.spinning + 
    (3*F.Force.height) + 
    FT.Force.height
  
  return(Tot.Wind.Force)
}

#######
## CURRENT THRUST FORCES. Uses Current_Force from "20210622 15 MW Turbine and Functions.R"
## The magnitude of this current force is strongly subject to the speed of the current and the effective area
SubCurr_3.3 <- Current_Force(speed = 0.2, eff.area = SubArea_3.3, Cd = Cd_cyl)/1000 #in [kN]

F.Current.Force3.3 <- function(current) {
  F.Current <- Current_Force(speed = current, eff.area=SubArea_3.3,Cd=Cd_cyl)/1000
  return(F.Current)
}

#######
## WAVE DRIFT FORCES. Uses Force.Matrix from "Wave Drift Forces.R"
## This is on the 15 MW floater geomtery
#DLC 1.3
Force.Matrix(sig.ht = 1.8, prd = 11) #[-44.9,50]
Force.Matrix(sig.ht = 1.2, prd = 14.2) #[0.85,7.53]

#DLC 6.1
Force.Matrix(sig.ht = 3.2, prd = 18.3) #[8.2,10]
Force.Matrix(sig.ht = 4.5, prd = 15.7) #[14.86,18.82]

# Choose the magnitude of the wave force based on the above
SubWave_3.3 <- 50 #in [kN]

#######
## TOTAL COMBINED FORCES. Uses functions from "20210616 Total Forces.R"
## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DP.Vector_3.3 <- DLC1.3_fun(RotorThrust_3.3,SubWave_3.3,SubCurr_3.3)
plot(WIND,DP.Vector_3.3, ylim = c(0,1000))

DLC1.3_3.3.fun <- function(wind) {
  force1.3_3.3 <- c()
  Current_3.3 <- c(F.Current.Force3.3(0.2)*cos(7*pi/4), F.Current.Force3.3(0.2)*sin(7*pi/4))
  Wave.high_3.3 <- c(50,0) #[-45,50]
  for(i in 1:length(wind)){
    Wind_3.3 <- c(DP.Turbine.Wind.Force3.3(wind = wind[i]),0) 
    force1.3_3.3 <- c(force1.3_3.3, R.Vector(Wind_3.3, Wave.high_3.3, Current_3.3)[1])
  }
  return(force1.3_3.3)
}

DLC1.3_3.3 <- DLC1.3_3.3.fun(speeds)

#######
## Convert forces to parasitic losses. Choose a thruster system first.
max(DP.Vector_3.3[1:length(DP.Vector_3.3)/2]) #DLC1.3 = 651 kN. Design at this level, although this occurs at the maximum wind speed as opposed to the rated wind speed.
tail(DP.Vector_3.3,1) #DLC6.1 = 529


parasitic.power3.3.1 <- DP.Power.n(DP.Vector_3.3, motor.diam = 3.2, n = 2) #lower one

# parasitic.powerT1.2 <- DP.Power.n(DP.Vector_T1, motor.diam = 3.35, n = 3)

plot(WIND,parasitic.power3.3.1, type = 'l', col = 'red', ylim = c(0,3))
lines(WIND,parasitic.powerT1.1, col = 'blue')
abline(v = R.wind_3.3)
abline(v = 12.5)

############ Calculate Parasitic losses. What % goes toward thrusters?

DF_3.3 <- data.frame(WIND,Parasitic_3.3 = parasitic.power3.3.1) %>%
  merge(., DF_power_3.3, by.x = "WIND", by.y = "Wind.speed", all = TRUE) %>%
  rename(Parasitic = Parasitic_3.3,
         Power = Power_MW) %>%
  transmute(WIND,Parasitic,Ct,Power) 

DF_3.3[is.na(DF_3.3)] <- 0
DF_3.3$Effective <- DF_3.3$Power - DF_3.3$Parasitic
mutate(DF_3.3, Effective = Power - Parasitic)

#Visualize the power curves
plot(DF_3.3$WIND, DF_3.3$Power, type = 'l', ylim = c(-2,3.5))
lines(DF_3.3$WIND, DF_3.3$Parasitic, col = 'blue')
lines(DF_3.3$WIND, DF_3.3$Effective, col = 'red')

Thrust.max_3.3 <- filter(DF_3.3[DF_3.3$WIND <= C.out_3.3,], Parasitic == max(Parasitic))$WIND #where max thrust occurs

#REGION 1: Before cut in wind speed
LOS.mod.R1_3.3 <- lm(Parasitic ~ poly(WIND, 1), data = filter(DF_3.3,WIND < C.in_3.3))
fun1_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_3.3, newdata = data.frame(WIND = x)))
#REGION 2: Cut in wind speed until maximum thrust force
LOS.mod.R2_3.3 <- lm(Parasitic ~ poly(WIND, 3), data = filter(DF_3.3,WIND >= C.in_3.3 & WIND <= Thrust.max_3.3))
fun2_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_3.3, newdata = data.frame(WIND = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_3.3 <- lm(Parasitic ~ poly(WIND, 5), data = filter(DF_3.3,WIND > Thrust.max_3.3 & WIND <= C.out_3.3))
fun3_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_3.3, newdata = data.frame(WIND = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_3.3 <- lm(Parasitic ~ poly(WIND, 2), data = filter(DF_3.3,WIND > C.out_3.3))
fun4_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_3.3, newdata = data.frame(WIND = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = DF_3.3$WIND, DF_3.3$Parasitic, pch = 19, cex = 0.75)
lines(x = DF_3.3$WIND[WIND < C.in_3.3], y = predict(LOS.mod.R1_3.3, newdata = filter(DF_3.3, WIND < C.in_3.3)), col = 'red')
lines(x = DF_3.3$WIND[WIND >= C.in_3.3 & WIND <= Thrust.max_3.3], y = predict(LOS.mod.R2_3.3, newdata = filter(DF_3.3, WIND >= C.in_3.3 & WIND <= Thrust.max_3.3)), col = 'red')
lines(x = DF_3.3$WIND[WIND > Thrust.max_3.3 & WIND <= C.out_3.3], y = predict(LOS.mod.R3_3.3, newdata = filter(DF_3.3, WIND > Thrust.max_3.3 & WIND <= C.out_3.3)), col = 'red')
lines(x = DF_3.3$WIND[WIND > C.out_3.3], y = predict(LOS.mod.R4_3.3, newdata = filter(DF_3.3, WIND > C.out_3.3)), col = 'red')

parasitic.loss_3.3 <- 8760*(integrate(fun1_3.3, lower = 0, upper = C.in_3.3)$value + 
                             integrate(fun2_3.3, lower = C.in_3.3, upper = Thrust.max_3.3)$value + 
                             integrate(fun3_3.3, lower = Thrust.max_3.3, upper = C.out_3.3)$value +
                             integrate(fun4_3.3, lower = C.out_3.3, upper = 40)$value) #in MWh

gen.fun2_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod_3.3, newdata = data.frame(x3 = x)))
#REGION3
gen.fun3_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*RatPower_3.3
#REGION4 = 0

gen_3.3 <- 8760*(integrate(gen.fun2_3.3, lower = C.in_3.3, upper = R.wind_3.3)$value + 
                  integrate(gen.fun3_3.3, lower = R.wind_3.3, upper = C.out_3.3)$value) #in MWh

parasitic.loss_3.3/gen_3.3#66.6%
(parasitic.loss_3.3)

#####

#FUNCTIONALIZED POWER CURVES (Lines above)
#WEIBULL PARAMETERS
sensitivity_3.3 <- function(shape, scale, OCC.High, OCC.Low, FOM.High, FOM.Low, disc.r, years = 25, fs = 1.67) {
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
  # MATRIX.CF <- matrix(nrow = length(number), ncol = dim(DP_Thrusters)[1])
  
  
  for(TYPE in 1:dim(DP_Thrusters)[1]) { #for each type of DP thruster
    DP_Power <- DP_Thrusters[TYPE,2] #kW
    DP_Diam <- DP_Thrusters[TYPE,3] #m
    DP_Force <- DP_Thrusters[TYPE,5] #kN
    DP_Price <- DP_Thrusters[TYPE,7] #USD
    for(num in number) { #for each number of thrusters
      if(num*DP_Force < max(DP.Vector_3.3)*fs) next #if this configuration doesn't provide the max needed station keeping force, the configuration does not work, and move to next one
      
      parasitic.power3.3 <- DP.Power.n(DP.Vector_3.3, motor.diam = DP_Diam, n = num) #lower one
      
      ############ Calculate Parasitic losses. What % goes toward thrusters?
      
      DF_3.3 <- data.frame(WIND,Parasitic_3.3 = parasitic.power3.3) %>%
        merge(., DF_power_3.3, by.x = "WIND", by.y = "Wind.speed", all = TRUE) %>%
        rename(Parasitic = Parasitic_3.3,
               Power = Power_MW) %>%
        transmute(WIND,Parasitic,Ct,Power) 
      
      DF_3.3[is.na(DF_3.3)] <- 0
      DF_3.3$Effective <- DF_3.3$Power - DF_3.3$Parasitic
      mutate(DF_3.3, Effective = Power - Parasitic)
      
      Thrust.max_3.3 <- filter(DF_3.3[DF_3.3$WIND <= C.out_3.3,], Parasitic == max(Parasitic))$WIND #where max thrust occurs
      
      #REGION 1: Before cut in wind speed
      LOS.mod.R1_3.3 <- lm(Parasitic ~ poly(WIND, 1), data = filter(DF_3.3,WIND < C.in_3.3))
      fun1_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_3.3, newdata = data.frame(WIND = x)))
      #REGION 2: Cut in wind speed until maximum thrust force
      LOS.mod.R2_3.3 <- lm(Parasitic ~ poly(WIND, 3), data = filter(DF_3.3,WIND >= C.in_3.3 & WIND <= Thrust.max_3.3))
      fun2_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_3.3, newdata = data.frame(WIND = x)))
      #REGION 3: Rated wind speed until cut out wind speed
      LOS.mod.R3_3.3 <- lm(Parasitic ~ poly(WIND, 5), data = filter(DF_3.3,WIND > Thrust.max_3.3 & WIND <= C.out_3.3))
      fun3_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_3.3, newdata = data.frame(WIND = x)))
      #REGION 4: Above cut out wind speed
      LOS.mod.R4_3.3 <- lm(Parasitic ~ poly(WIND, 2), data = filter(DF_3.3,WIND > C.out_3.3))
      fun4_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_3.3, newdata = data.frame(WIND = x)))
      
      parasitic.loss_3.3 <- 8760*(integrate(fun1_3.3, lower = 0, upper = C.in_3.3)$value + 
                                   integrate(fun2_3.3, lower = C.in_3.3, upper = Thrust.max_3.3)$value + 
                                   integrate(fun3_3.3, lower = Thrust.max_3.3, upper = C.out_3.3)$value +
                                   integrate(fun4_3.3, lower = C.out_3.3, upper = 40)$value) #in MWh
      
      gen.fun2_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod_3.3, newdata = data.frame(x3 = x)))
      #REGION3
      gen.fun3_3.3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*RatPower_3.3
      #REGION4 = 0
      
      gen_3.3 <- 8760*(integrate(gen.fun2_3.3, lower = C.in_3.3, upper = R.wind_3.3)$value + 
                        integrate(gen.fun3_3.3, lower = R.wind_3.3, upper = C.out_3.3)$value) #in MWh
      
      net3.3 <- gen_3.3 - parasitic.loss_3.3 #annual expected net electricity output in MWh
      
      potential <- 8760*Turbine #max potential MWh per year
      
      
      
      netCF <- net3.3/potential #percent net CF
      
      #Populate Matrices
      DP_INV <- DP_Price*num/1000000 #MUSD. Incurred in first year, no discounting
      DP_CF <- 0 #O&M set to zero for now
      
      NPV_Cost <-FinancialMath::NPV(cf0 = DP_INV, cf = rep(DP_CF, times = years),times = seq(1,years),i = disc.r)
      NPV_Energy <- FinancialMath::NPV(cf0 = 0,cf = rep(net3.3,times = years),times = seq(1,years),i = disc.r)
      
      LTCOE <- -1000000*NPV_Cost/NPV_Energy #USD/MWh
      netOCC.High <- OCC.High*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000)) #in [$/kW]
      netOCC.Low <- OCC.Low*(1-mooring.proportion)+((DP_INV*1000000)/(Turbine*1000))
      
      LCOE.High <- (CRF*PFF*CFF*(netOCC.High)+FOM.High)*1000/(netCF*8760)
      LCOE.Low <- (CRF*PFF*CFF*(netOCC.Low)+FOM.Low)*1000/(netCF*8760)
      
      #Populate Matrices
      MATRIX.capex[num,TYPE] <- DP_INV
      MATRIX.loss.ratio[num,TYPE] <- parasitic.loss_3.3/gen_3.3 #annual percent of energy for DP
      MATRIX.ltcoe[num,TYPE] <- LTCOE
      MATRIX.lcoe.h[num,TYPE] <- LCOE.High
      MATRIX.lcoe.l[num,TYPE] <- LCOE.Low
      # MATRIX.CF[num,TYPE] <- gen_3.3/(8760*RatPower_3.3)
    }
  }
  print(gen_3.3/(8760*RatPower_3.3))
  return(list(MATRIX.loss.ratio, MATRIX.capex, MATRIX.ltcoe, MATRIX.lcoe.h, MATRIX.lcoe.l))
}

sensitivity_3.3(shape = 2.3, scale = 7.95, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
