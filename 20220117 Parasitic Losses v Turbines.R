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

plot(winds_T1, statblade15, pch = 19)
points(winds_T1, statblade10, pch = 19, col = 'red')                     
points(winds_T1, statblade8, pch = 19, col = 'blue')                     
points(winds_T1, statblade5, pch = 19, col = 'green')                     

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

## Vestas V112-3.45 = T1
#(Source: https://en.wind-turbine-models.com/turbines/1247-vestas-v112-3.45#powercurve)
#GIVEN PARAMETERS
RatPower_T1 <- 3.45 #in [MW]
Rdiam_T1 <- 112 #[m]
Rarea_T1 <- pi*(Rdiam_T1/2)^2 #[m^2]
Theight_T1.l <- 60
Theight_T1.h <- 84
C.in_T1 <- 3 #in [m/s]
C.out_T1 <- 25 #in [m/s]
R.wind_T1 <- 13 #in [m/s]

#ESTIMATED PARAMETERS
Tdiam_T1 <- Pred.tower(Theight_T1.h)
Blength_T1 <- Rdiam_T1/2 #in [m]

#ASSUMED FLOATER PARAMETERS. 15MW floater maintained
FLdiam_T1 <- 12.5
FLfreeboard_T1 <- 15
FLdraft_T1 <- 20

SubArea_T1 <- (FLdiam_T1*FLdraft_T1*3) + (FLdraft_T1*Tdiam_T1) #m^2

#######
## WIND THRUST FORCES
#Power Curve, Cp, and Ct for cut-in < wind < cut-out
winds_T1 <- seq(3,25,by = 0.5) #length = 45
power_T1 <- c(7,53,123,208,309,427,567,732,927,1149,1401,1688,2006,2348,2693,3011,3252,3388,3436,3448,rep(3450,times = 25))/1000 #in [MW]
Cp_T1 <- c(0.043,0.205,0.318,0.378,0.410,0.425,0.435,0.442,0.448,0.451,0.453,0.455,0.456,0.454,0.446,0.431,0.405,0.369,0.330,0.293,0.260,0.232,0.208,0.188,0.169,0.154,0.140,0.127,0.116,0.107,0.098,0.090,0.083,0.077,0.071,0.066,0.062,0.058,0.054,0.050,0.047,0.044,0.041,0.039,0.037)
Ct_T1 <- c(0.891,0.885,0.865,0.839,0.840,0.840,0.834,0.827,0.820,0.812,0.803,0.801,0.796,0.784,0.753,0.698,0.627,0.546,0.469,0.403,0.351,0.310,0.275,0.245,0.219,0.198,0.180,0.164,0.150,0.138,0.127,0.117,0.108,0.100,0.093,0.087,0.081,0.077,0.072,0.068,0.064,0.060,0.057,0.054,0.051)
 
DF_power_T1 <- data.frame(winds_T1, power_T1, Cp_T1, Ct_T1)

plot(winds_T1,power_T1)
abline(v = R.wind_T1)

#Continuous function for ramp-up
y1 <- filter(DF_power_T1, power_T1 < RatPower_T1)
x1 <- y1$winds_T1
y1 <- y1$power_T1

power.mod_T1 <- lm(y1 ~ poly(x1, 7))

# #Check the model vs the points
# plot(x1, y1)
# lines(x1, y = predict(power.mod_T1, newdata = data.frame(x = x1)), col = 'red')

#ROTOR THRUST FORCES. cut-in < wind < cut-out
Thrust_T1 <- mapply(Thrust, Ct = Ct_T1, area = Rarea_T1, windspeed = winds_T1)/1000 #in [kN]

dt_T1 <- data.frame(WIND = winds_T1,Ct_T1, Thrust_T1)
plot(winds_T1,Thrust_T1, ylim = c(0,500))
(max(Thrust_T1))
#ARRESTED BLADE FORCES. Scale from other arrested blade forces. wind > 0 and blades not spinning
StatBlade_T1.l <- Statblade.scale(Blength_T1)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T1.l)/1000
StatBlade_T1.h <- Statblade.scale(Blength_T1)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T1.h)/1000

# RotorThrust_T1 <- c()
# for(i in WIND) {
#   if(WIND[i] < C.in_T1 | WIND[i] > C.out_T1) RotorThrust_T1[i] <- StatBlade_T1.h[i]
#   else RotorThrust_T1[i] <- Thrust()
# }

#TOWER THRUST FORCES. wind > 0
TowThrust_T1.l <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T1.l, Cd = Cd_cyl, D = Tdiam_T1)/1000 #in [kN]

TowThrust_T1.h <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T1.h, Cd = Cd_cyl, D = Tdiam_T1)/1000

#FLOATER FORCES. wind > 0
FThrust <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = FLdiam_T1, u.elev = FLfreeboard_T1)/1000 #in [kN]
FTThrust <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = Tdiam_T1, u.elev = FLfreeboard_T1)/1000 #in [kN]

SubThrust_T1 <- (FThrust*3) + FTThrust #in [kN]

df_T1 <- data.frame(WIND, StatBlade_T1.h,TowThrust_T1.h, SubThrust_T1)

#Wind forces DF
DF <- merge(dt_T1, df_T1, by = "WIND", all = TRUE) #Table of wind forces
DF["TotThrust_T1"] <- 0
for(row in 1:nrow(DF)){
  if(is.na(DF[row,]$Thrust_T1)) {
    DF[row,"TotThrust_T1"] <- sum(DF[row,4:6]) 
  } else {
    DF[row,"TotThrust_T1"] <- sum(DF[row,c(3,5,6)])
  }
}
RotorThrust_T1 <- DF$TotThrust_T1 #in [kN]

#Visualize all wind thrust forces together
plot(WIND,SubThrust_T1, ylim = c(0,round(max(Thrust_T1),-2)), type = 'l')
lines(WIND, StatBlade_T1.h, lty = 2, col = 'blue')
lines(WIND, TowThrust_T1.h, col = 'green')
lines(winds_T1, Thrust_T1, col = 'red')

#######
## CURRENT THRUST FORCES. Uses Current_Force from "20210622 15 MW Turbine and Functions.R"
## The magnitude of this current force is strongly subject to the speed of the current and the effective area
SubCurr_T1 <- Current_Force(speed = 0.9, eff.area = SubArea_T1, Cd = Cd_cyl)/1000 #in [kN]

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
SubWave_T1 <- 50 #in [kN]

#######
## TOTAL COMBINED FORCES. Uses functions from "20210616 Total Forces.R"
## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DLC1.3_fun <- function(wind,wave,current) { #wind is a vector, wave and current are constants. Magnitudes of the forces
  force1.3 <- c()
  Current <- c(current*cos(7*pi/4), current*sin(7*pi/4))
  Wave.high <- c(wave,0)
  for(i in 1:length(wind)){
    Wind <- c(wind[i],0)
    force1.3 <- c(force1.3, R.Vector(Wind, Wave.high, Current)[1])
  }
  return(force1.3)
}

DP.Vector_T1 <- DLC1.3_fun(RotorThrust_T1,SubWave_T1,SubCurr_T1)
plot(WIND,DP.Vector_T1, ylim = c(0,1000))

#######
## Convert forces to parasitic losses. Choose a thruster system first.
max(DP.Vector_T1) #675 kN. Design at this level.

parasitic.powerT1.1 <- DP.Power.n(DP.Vector_T1, motor.diam = 3.2, n = 2) #lower one
parasitic.powerT1.2 <- DP.Power.n(DP.Vector_T1, motor.diam = 3.35, n = 3)

parasitic.powerT1 <- DP.Power.n(DP.Vector_T1, motor.diam = 3.2, n = 2)

plot(WIND,parasitic.powerT1.2, type = 'l', col = 'red', ylim = c(0,3))
lines(WIND,parasitic.powerT1.1, col = 'blue')
abline(v = R.wind_T1)
abline(v = 12.5)

############ Calculate Parasitic losses. What % goes toward thrusters?
DF_T1 <- data.frame(WIND,Parasitic_T1 = parasitic.powerT1) %>%
  merge(., DF_power_T1, by.x = "WIND", by.y = "winds_T1", all = TRUE) %>%
  rename(Parasitic = Parasitic_T1,
         Power = power_T1,
         Cp = Cp_T1,
         Ct = Ct_T1) 

DF_T1[is.na(DF_T1)] <- 0
DF_T1$Effective <- DF_T1$Power - DF_T1$Parasitic
mutate(DF_T1, Effective = Power - Parasitic)

#Visualize the power curves
plot(DF_T1$WIND, DF_T1$Power, type = 'l', ylim = c(-1,3.5))
lines(DF_T1$WIND, DF_T1$Parasitic, col = 'blue')
lines(DF_T1$WIND, DF_T1$Effective, col = 'red')

Thrust.max_T1 <- filter(DF_T1, Parasitic == max(Parasitic))$WIND #where max thrust occurs

#REGION 1: Before cut in wind speed
LOS.mod.R1_T1 <- lm(Parasitic ~ poly(WIND, 1), data = filter(DF_T1,WIND < C.in_T1))
fun1_T1 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_T1, newdata = data.frame(WIND = x)))
#REGION 2: Cut in wind speed until maximum thrust force
LOS.mod.R2_T1 <- lm(Parasitic ~ poly(WIND, 3), data = filter(DF_T1,WIND >= C.in_T1 & WIND <= Thrust.max_T1))
fun2_T1 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_T1, newdata = data.frame(WIND = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_T1 <- lm(Parasitic ~ poly(WIND, 5), data = filter(DF_T1,WIND > Thrust.max_T1 & WIND <= C.out_T1))
fun3_T1 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_T1, newdata = data.frame(WIND = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_T1 <- lm(Parasitic ~ poly(WIND, 2), data = filter(DF_T1,WIND > C.out_T1))
fun4_T1 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_T1, newdata = data.frame(WIND = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = DF_T1$WIND, DF_T1$Parasitic, pch = 19, cex = 0.75)
lines(x = DF_T1$WIND[WIND < C.in_T1], y = predict(LOS.mod.R1_T1, newdata = filter(DF_T1, WIND < C.in_T1)), col = 'red')
lines(x = DF_T1$WIND[WIND >= C.in_T1 & WIND <= Thrust.max_T1], y = predict(LOS.mod.R2_T1, newdata = filter(DF_T1, WIND >= C.in_T1 & WIND <= Thrust.max_T1)), col = 'red')
lines(x = DF_T1$WIND[WIND > Thrust.max_T1 & WIND <= C.out_T1], y = predict(LOS.mod.R3_T1, newdata = filter(DF_T1, WIND > Thrust.max_T1 & WIND <= C.out_T1)), col = 'red')
lines(x = DF_T1$WIND[WIND > C.out_T1], y = predict(LOS.mod.R4_T1, newdata = filter(DF_T1, WIND > C.out_T1)), col = 'red')

parasitic.loss_T1 <- 8760*(integrate(fun1_T1, lower = 0, upper = C.in_T1)$value + 
                           integrate(fun2_T1, lower = C.in_T1, upper = Thrust.max_T1)$value + 
                           integrate(fun3_T1, lower = Thrust.max_T1, upper = C.out_T1)$value +
                           integrate(fun4_T1, lower = C.out_T1, upper = 40)$value) #in MWh

gen.fun2_T1 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod_T1, newdata = data.frame(x1 = x)))
#REGION3
gen.fun3_T1 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*RatPower_T1
#REGION4 = 0

gen_T1 <- 8760*(integrate(gen.fun2_T1, lower = C.in_T1, upper = R.wind_T1)$value + 
                integrate(gen.fun3_T1, lower = R.wind_T1, upper = C.out_T1)$value) #in MWh

parasitic.loss_T1/gen_T1#59.42%
(parasitic.loss_T1)

################################################################################

## GE 2.5-120 = T2
#(Source: https://en.wind-turbine-models.com/turbines/310-ge-general-electric-ge-2.5-120#powercurve)
#GIVEN PARAMETERS
RatPower_T2 <- 2.5 #in [MW]
Rdiam_T2 <- 120 #[m]
Rarea_T2 <- pi*(Rdiam_T2/2)^2 #[m^2]
Theight_T2.l <- 110
Theight_T2.h <- 110 #110,139
C.in_T2 <- 3 #in [m/s]
C.out_T2 <- 20 #in [m/s]
R.wind_T2 <- 12 #in [m/s]

#ESTIMATED PARAMETERS
Tdiam_T2 <- Pred.tower(Theight_T2.h)
Blength_T2 <- Rdiam_T2/2 #in [m]

#ASSUMED FLOATER PARAMETERS. 15MW floater maintained
FLdiam_T2 <- 12.5
FLfreeboard_T2 <- 15
FLdraft_T2 <- 20

SubArea_T2 <- (FLdiam_T2*FLdraft_T2*3) + (FLdraft_T2*Tdiam_T2) #m^2

#######
## WIND THRUST FORCES
#Power Curve, Cp, and Ct for cut-in < wind < cut-out
DF_power_T2 <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning/GE2.5-120 Power Curve Data.csv") %>%
  rename(Wind.speed = ï..Wind.speed) %>%
  mutate(Power_MW = Power_kw/1000)

Ct_T2 <- DF_power_T2$Ct
winds_T2 <- DF_power_T2$Wind.speed

plot(DF_power_T2$Wind.speed,DF_power_T2$Power_MW)
abline(v = R.wind_T2)

#Continuous function for ramp-up
y2 <- filter(DF_power_T2, Power_MW < RatPower_T2)
x2 <- y2$Wind.speed
y2 <- y2$Power_MW

power.mod_T2 <- lm(y2 ~ poly(x2, 7))

# #Check the model vs the points
# plot(x2, y2)
# lines(x2, y = predict(power.mod_T2, newdata = data.frame(x = x2)), col = 'red')

#ROTOR THRUST FORCES. cut-in < wind < cut-out
Thrust_T2 <- mapply(Thrust, Ct = Ct_T2, area = Rarea_T2, windspeed = winds_T2)/1000 #in [kN]

dt_T2 <- data.frame(WIND = winds_T2,Ct_T2, Thrust_T2)
plot(winds_T2,Thrust_T2, ylim = c(0,500))
#ARRESTED BLADE FORCES. Scale from other arrested blade forces. wind > 0 and blades not spinning
StatBlade_T2.l <- Statblade.scale(Blength_T2)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T2.l)/1000
StatBlade_T2.h <- Statblade.scale(Blength_T2)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T2.h)/1000


#TOWER THRUST FORCES. wind > 0
TowThrust_T2.l <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T2.l, Cd = Cd_cyl, D = Tdiam_T2)/1000 #in [kN]

TowThrust_T2.h <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T2.h, Cd = Cd_cyl, D = Tdiam_T2)/1000

#FLOATER FORCES. wind > 0
FThrust_T2 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = FLdiam_T2, u.elev = FLfreeboard_T2)/1000 #in [kN]
FTThrust_T2 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = Tdiam_T2, u.elev = FLfreeboard_T2)/1000 #in [kN]

SubThrust_T2 <- (FThrust_T2*3) + FTThrust_T2 #in [kN]

df_T2 <- data.frame(WIND, StatBlade_T2.h,TowThrust_T2.h, SubThrust_T2)

#Wind forces DF
DF_T2 <- merge(dt_T2, df_T2, by = "WIND", all = TRUE) #Table of wind forces
DF_T2["TotThrust_T2"] <- 0
for(row in 1:nrow(DF_T2)){
  if(is.na(DF_T2[row,]$Thrust_T2)) {
    DF_T2[row,"TotThrust_T2"] <- sum(DF_T2[row,4:6]) 
  } else {
    DF_T2[row,"TotThrust_T2"] <- sum(DF_T2[row,c(3,5,6)])
  }
}
RotorThrust_T2 <- DF_T2$TotThrust_T2 #in [kN]

#Visualize all separate wind thrust forces together
plot(WIND,SubThrust_T2, ylim = c(0,round(max(Thrust_T2),-2)), type = 'l')
lines(WIND, StatBlade_T2.h, lty = 2, col = 'blue')
lines(WIND, TowThrust_T2.h, col = 'green')
lines(winds_T2, Thrust_T2, col = 'red')

#######
## CURRENT THRUST FORCES. Uses Current_Force from "20210622 15 MW Turbine and Functions.R"
## The magnitude of this current force is strongly subject to the speed of the current and the effective area
SubCurr_T2 <- Current_Force(speed = 0.9, eff.area = SubArea_T2, Cd = Cd_cyl)/1000 #in [kN]

#######
## WAVE DRIFT FORCES. Uses Force.Matrix from "Wave Drift Forces.R"
## This is on the 15 MW floater geometery
#DLC 1.3
Force.Matrix(sig.ht = 1.8, prd = 11) #[-44.9,50]
Force.Matrix(sig.ht = 1.2, prd = 14.2) #[0.85,7.53]

#DLC 6.1
Force.Matrix(sig.ht = 3.2, prd = 18.3) #[8.2,10]
Force.Matrix(sig.ht = 4.5, prd = 15.7) #[14.86,18.82]

# Choose the magnitude of the wave force based on the above
SubWave_T2 <- 50 #in [kN]

#######
## TOTAL COMBINED FORCES. Uses functions from "20210616 Total Forces.R"
## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DP.Vector_T2 <- DLC1.3_fun(RotorThrust_T2,SubWave_T2,SubCurr_T2)
plot(WIND,DP.Vector_T2,ylim = c(0,1000),pch = 19, add = T)

#######
## Convert forces to parasitic losses. Choose a thruster system first.
max(DP.Vector_T2[1:length(DP.Vector_T2)/2]) #DLC1.3 = 591 kN. Design at this level although it is not max. It yields more thrusters (accounting for FS)
max(DP.Vector_T2) #DLC6.1 = 689

parasitic.powerT2.1 <- DP.Power.n(DP.Vector_T2, motor.diam = 4.1, n = 1) #lower one

parasitic.powerT2.2 <- DP.Power.n(DP.Vector_T2, motor.diam = 3.35, n = 3)

plot(WIND,parasitic.powerT2.2, type = 'l', col = 'red', ylim = c(0,3))
lines(WIND,parasitic.powerT2.1, col = 'blue')
abline(v = R.wind_T2)
abline(v = 12.5)

############ Calculate Parasitic losses. What % goes toward thrusters?

DF_T2 <- data.frame(WIND,Parasitic_T2 = parasitic.powerT2.1) %>%
  merge(., DF_power_T2, by.x = "WIND", by.y = "Wind.speed", all = TRUE) %>%
  rename(Parasitic = Parasitic_T2,
         Power = Power_MW) %>%
  transmute(WIND,Parasitic,Ct,Power)

DF_T2[is.na(DF_T2)] <- 0
DF_T2$Effective <- DF_T2$Power - DF_T2$Parasitic
mutate(DF_T2, Effective = Power - Parasitic)

#Visualize the power curves
plot(DF_T2$WIND, DF_T2$Power, type = 'l', ylim = c(-1,3.5))
lines(DF_T2$WIND, DF_T2$Parasitic, col = 'blue')
lines(DF_T2$WIND, DF_T2$Effective, col = 'red')

Thrust.max_T2 <- filter(DF_T2[DF_T2$WIND <= 20,], Parasitic == max(Parasitic))$WIND #where max thrust occurs

#REGION 1: Before cut in wind speed
LOS.mod.R1_T2 <- lm(Parasitic ~ poly(WIND, 1), data = filter(DF_T2,WIND < C.in_T2))
fun1_T2 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_T2, newdata = data.frame(WIND = x)))
#REGION 2: Cut in wind speed until maximum thrust force
LOS.mod.R2_T2 <- lm(Parasitic ~ poly(WIND, 3), data = filter(DF_T2,WIND >= C.in_T2 & WIND <= Thrust.max_T2))
fun2_T2 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_T2, newdata = data.frame(WIND = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_T2 <- lm(Parasitic ~ poly(WIND, 5), data = filter(DF_T2,WIND > Thrust.max_T2 & WIND <= C.out_T2))
fun3_T2 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_T2, newdata = data.frame(WIND = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_T2 <- lm(Parasitic ~ poly(WIND, 2), data = filter(DF_T2,WIND > C.out_T2))
fun4_T2 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_T2, newdata = data.frame(WIND = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = DF_T2$WIND, DF_T2$Parasitic, pch = 19, cex = 0.75)
lines(x = DF_T2$WIND[WIND < C.in_T2], y = predict(LOS.mod.R1_T2, newdata = filter(DF_T2, WIND < C.in_T2)), col = 'red')
lines(x = DF_T2$WIND[WIND >= C.in_T2 & WIND <= Thrust.max_T2], y = predict(LOS.mod.R2_T2, newdata = filter(DF_T2, WIND >= C.in_T2 & WIND <= Thrust.max_T2)), col = 'red')
lines(x = DF_T2$WIND[WIND > Thrust.max_T2 & WIND <= C.out_T2], y = predict(LOS.mod.R3_T2, newdata = filter(DF_T2, WIND > Thrust.max_T2 & WIND <= C.out_T2)), col = 'red')
lines(x = DF_T2$WIND[WIND > C.out_T2], y = predict(LOS.mod.R4_T2, newdata = filter(DF_T2, WIND > C.out_T2)), col = 'red')

parasitic.loss_T2 <- 8760*(integrate(fun1_T2, lower = 0, upper = C.in_T2)$value + 
                             integrate(fun2_T2, lower = C.in_T2, upper = Thrust.max_T2)$value + 
                             integrate(fun3_T2, lower = Thrust.max_T2, upper = C.out_T2)$value +
                             integrate(fun4_T2, lower = C.out_T2, upper = 40)$value) #in MWh

gen.fun2_T2 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod_T2, newdata = data.frame(x1 = x)))
#REGION3
gen.fun3_T2 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*RatPower_T2
#REGION4 = 0

gen_T2 <- 8760*(integrate(gen.fun2_T2, lower = C.in_T2, upper = R.wind_T2, subdivisions=2000)$value + 
                  integrate(gen.fun3_T2, lower = R.wind_T2, upper = C.out_T2, subdivisions=2000)$value) #in MWh

parasitic.loss_T2/gen_T2#89.54%
(parasitic.loss_T2)

################################################################################

## Vestas V112-3.3 = T3
#(Source: https://en.wind-turbine-models.com/turbines/693-vestas-v112-3.3)
#GIVEN PARAMETERS
RatPower_T3 <- 3.3 #in [MW]
Rdiam_T3 <- 112 #[m]
Rarea_T3 <- pi*(Rdiam_T3/2)^2 #[m^2]
Theight_T3.l <- 84
Theight_T3.h <- 84 #84,140
C.in_T3 <- 3 #in [m/s]
C.out_T3 <- 25 #in [m/s]
R.wind_T3 <- 13 #in [m/s]

#ESTIMATED PARAMETERS
Tdiam_T3 <- Pred.tower(Theight_T3.h)
Blength_T3 <- Rdiam_T3/2 #in [m]

#ASSUMED FLOATER PARAMETERS. 15MW floater maintained
FLdiam_T3 <- 12.5
FLfreeboard_T3 <- 15
FLdraft_T3 <- 20

SubArea_T3 <- (FLdiam_T3*FLdraft_T3*3) + (FLdraft_T3*Tdiam_T3) #m^2

#######
## WIND THRUST FORCES
#Power Curve, Cp, and Ct for cut-in < wind < cut-out
DF_power_T3 <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning/V112-3.3 Power Curve Data.csv") %>%
  rename(Wind.speed = ï..Wind.speed) %>%
  mutate(Power_MW = Power_kw/1000)

Ct_T3 <- DF_power_T3$Ct
winds_T3 <- DF_power_T3$Wind.speed

plot(DF_power_T3$Wind.speed,DF_power_T3$Power_MW)
abline(v = R.wind_T3)

#Continuous function for ramp-up
y3 <- filter(DF_power_T3, Power_MW < RatPower_T3)
x3 <- y3$Wind.speed
y3 <- y3$Power_MW

power.mod_T3 <- lm(y3 ~ poly(x3, 7))

# #Check the model vs the points
plot(x3, y3)
lines(x3, y = predict(power.mod_T3, newdata = data.frame(x = x3)), col = 'red')

#ROTOR THRUST FORCES. cut-in < wind < cut-out
Thrust_T3 <- mapply(Thrust, Ct = Ct_T3, area = Rarea_T3, windspeed = winds_T3)/1000 #in [kN]

dt_T3 <- data.frame(WIND = winds_T3,Ct_T3, Thrust_T3)
plot(winds_T3,Thrust_T3, ylim = c(0,500))
#ARRESTED BLADE FORCES. Scale from other arrested blade forces. wind > 0 and blades not spinning
StatBlade_T3.l <- Statblade.scale(Blength_T3)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T3.l)/1000
StatBlade_T3.h <- Statblade.scale(Blength_T3)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T3.h)/1000

#TOWER THRUST FORCES. wind > 0
TowThrust_T3.l <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T3.l, Cd = Cd_cyl, D = Tdiam_T3)/1000 #in [kN]

TowThrust_T3.h <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T3.h, Cd = Cd_cyl, D = Tdiam_T3)/1000

#FLOATER FORCES. wind > 0
FThrust_T3 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = FLdiam_T3, u.elev = FLfreeboard_T3)/1000 #in [kN]
FTThrust_T3 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = Tdiam_T3, u.elev = FLfreeboard_T3)/1000 #in [kN]

SubThrust_T3 <- (FThrust_T3*3) + FTThrust_T3 #in [kN]

df_T3 <- data.frame(WIND, StatBlade_T3.h,TowThrust_T3.h, SubThrust_T3)

#Wind forces DF
DF_T3 <- merge(dt_T3, df_T3, by = "WIND", all = TRUE) #Table of wind forces
DF_T3["TotThrust_T3"] <- 0
for(row in 1:nrow(DF_T3)){
  if(is.na(DF_T3[row,]$Thrust_T3)) {
    DF_T3[row,"TotThrust_T3"] <- sum(DF_T3[row,4:6]) 
  } else {
    DF_T3[row,"TotThrust_T3"] <- sum(DF_T3[row,c(3,5,6)])
  }
}
RotorThrust_T3 <- DF_T3$TotThrust_T3 #in [kN]

#Visualize all wind thrust forces together
plot(WIND,SubThrust_T3, ylim = c(0,1000), type = 'l')
lines(WIND, StatBlade_T3.h, lty = 2, col = 'blue')
lines(WIND, TowThrust_T3.h, col = 'green') #This force is reaaalllyyy high.
lines(winds_T3, Thrust_T3, col = 'red')
lines(WIND,RotorThrust_T3, lwd = 2)
# lines(speeds, T.Force.best) #by comparison, the 15MW tower with tapering tower diameter.

#######
## CURRENT THRUST FORCES. Uses Current_Force from "20210622 15 MW Turbine and Functions.R"
## The magnitude of this current force is strongly subject to the speed of the current and the effective area
SubCurr_T3 <- Current_Force(speed = 0.9, eff.area = SubArea_T3, Cd = Cd_cyl)/1000 #in [kN]

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
SubWave_T3 <- 50 #in [kN]

#######
## TOTAL COMBINED FORCES. Uses functions from "20210616 Total Forces.R"
## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DP.Vector_T3 <- DLC1.3_fun(RotorThrust_T3,SubWave_T3,SubCurr_T3)
plot(WIND,DP.Vector_T3, ylim = c(0,1000))

#######
## Convert forces to parasitic losses. Choose a thruster system first.
max(DP.Vector_T3[1:length(DP.Vector_T3)/2]) #DLC1.3 = 651 kN. Design at this level, although this occurs at the maximum wind speed as opposed to the rated wind speed.
tail(DP.Vector_T3,1) #DLC6.1 = 529


parasitic.powerT3.1 <- DP.Power.n(DP.Vector_T3, motor.diam = 3.2, n = 2) #lower one

# parasitic.powerT1.2 <- DP.Power.n(DP.Vector_T1, motor.diam = 3.35, n = 3)

plot(WIND,parasitic.powerT3.1, type = 'l', col = 'red', ylim = c(0,3))
lines(WIND,parasitic.powerT1.1, col = 'blue')
abline(v = R.wind_T3)
abline(v = 12.5)

############ Calculate Parasitic losses. What % goes toward thrusters?

DF_T3 <- data.frame(WIND,Parasitic_T3 = parasitic.powerT3.1) %>%
  merge(., DF_power_T3, by.x = "WIND", by.y = "Wind.speed", all = TRUE) %>%
  rename(Parasitic = Parasitic_T3,
         Power = Power_MW) %>%
  transmute(WIND,Parasitic,Ct,Power) 

DF_T3[is.na(DF_T3)] <- 0
DF_T3$Effective <- DF_T3$Power - DF_T3$Parasitic
mutate(DF_T3, Effective = Power - Parasitic)

#Visualize the power curves
plot(DF_T3$WIND, DF_T3$Power, type = 'l', ylim = c(-2,3.5))
lines(DF_T3$WIND, DF_T3$Parasitic, col = 'blue')
lines(DF_T3$WIND, DF_T3$Effective, col = 'red')

Thrust.max_T3 <- filter(DF_T3[DF_T3$WIND <= C.out_T3,], Parasitic == max(Parasitic))$WIND #where max thrust occurs

#REGION 1: Before cut in wind speed
LOS.mod.R1_T3 <- lm(Parasitic ~ poly(WIND, 1), data = filter(DF_T3,WIND < C.in_T3))
fun1_T3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_T3, newdata = data.frame(WIND = x)))
#REGION 2: Cut in wind speed until maximum thrust force
LOS.mod.R2_T3 <- lm(Parasitic ~ poly(WIND, 3), data = filter(DF_T3,WIND >= C.in_T3 & WIND <= Thrust.max_T3))
fun2_T3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_T3, newdata = data.frame(WIND = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_T3 <- lm(Parasitic ~ poly(WIND, 5), data = filter(DF_T3,WIND > Thrust.max_T3 & WIND <= C.out_T3))
fun3_T3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_T3, newdata = data.frame(WIND = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_T3 <- lm(Parasitic ~ poly(WIND, 2), data = filter(DF_T3,WIND > C.out_T3))
fun4_T3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_T3, newdata = data.frame(WIND = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = DF_T3$WIND, DF_T3$Parasitic, pch = 19, cex = 0.75)
lines(x = DF_T3$WIND[WIND < C.in_T3], y = predict(LOS.mod.R1_T3, newdata = filter(DF_T3, WIND < C.in_T3)), col = 'red')
lines(x = DF_T3$WIND[WIND >= C.in_T3 & WIND <= Thrust.max_T3], y = predict(LOS.mod.R2_T3, newdata = filter(DF_T3, WIND >= C.in_T3 & WIND <= Thrust.max_T3)), col = 'red')
lines(x = DF_T3$WIND[WIND > Thrust.max_T3 & WIND <= C.out_T3], y = predict(LOS.mod.R3_T3, newdata = filter(DF_T3, WIND > Thrust.max_T3 & WIND <= C.out_T3)), col = 'red')
lines(x = DF_T3$WIND[WIND > C.out_T3], y = predict(LOS.mod.R4_T3, newdata = filter(DF_T3, WIND > C.out_T3)), col = 'red')

parasitic.loss_T3 <- 8760*(integrate(fun1_T3, lower = 0, upper = C.in_T3)$value + 
                             integrate(fun2_T3, lower = C.in_T3, upper = Thrust.max_T3)$value + 
                             integrate(fun3_T3, lower = Thrust.max_T3, upper = C.out_T3)$value +
                             integrate(fun4_T3, lower = C.out_T3, upper = 40)$value) #in MWh

gen.fun2_T3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod_T3, newdata = data.frame(x3 = x)))
#REGION3
gen.fun3_T3 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*RatPower_T3
#REGION4 = 0

gen_T3 <- 8760*(integrate(gen.fun2_T3, lower = C.in_T3, upper = R.wind_T3)$value + 
                  integrate(gen.fun3_T3, lower = R.wind_T3, upper = C.out_T3)$value) #in MWh

parasitic.loss_T3/gen_T3#66.6%
(parasitic.loss_T3)

################################################################################

## GE3.4-137 = T4
#(Source: https://en.wind-turbine-models.com/turbines/1339-ge-general-electric-ge-3.4-137#powercurve)
#GIVEN PARAMETERS
RatPower_T4 <- 3.4 #in [MW]
Rdiam_T4 <- 137 #[m]
Rarea_T4 <- pi*(Rdiam_T4/2)^2 #[m^2]
Theight_T4.l <- 85
Theight_T4.h <- 85#85,155
C.in_T4 <- 3 #in [m/s]
C.out_T4 <- 25 #in [m/s]
R.wind_T4 <- 12 #in [m/s]

#ESTIMATED PARAMETERS
Tdiam_T4 <- Pred.tower(Theight_T4.h)
Blength_T4 <- Rdiam_T4/2 #in [m]

#ASSUMED FLOATER PARAMETERS. 15MW floater maintained
FLdiam_T4 <- 12.5
FLfreeboard_T4 <- 15
FLdraft_T4 <- 20

SubArea_T4 <- (FLdiam_T4*FLdraft_T4*3) + (FLdraft_T4*Tdiam_T4) #m^2

#######
## WIND THRUST FORCES
#Power Curve, Cp, and Ct for cut-in < wind < cut-out
DF_power_T4 <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning/GE3.4-137 Power Curve Data.csv") %>%
  rename(Wind.speed = ï..Wind.speed) %>%
  mutate(Power_MW = Power_kw/1000)

Ct_T4 <- DF_power_T4$Ct
winds_T4 <- DF_power_T4$Wind.speed

plot(DF_power_T4$Wind.speed,DF_power_T4$Power_MW)
abline(v = R.wind_T4)

#Continuous function for ramp-up
y4 <- filter(DF_power_T4, Power_MW <= RatPower_T4)
x4 <- y4$Wind.speed
y4 <- y4$Power_MW

power.mod_T4 <- lm(y4 ~ poly(x4, 7))

# #Check the model vs the points
plot(x4, y4)
lines(x4, y = predict(power.mod_T4, newdata = data.frame(x = x4)), col = 'red')

#ROTOR THRUST FORCES. cut-in < wind < cut-out
Thrust_T4 <- mapply(Thrust, Ct = Ct_T4, area = Rarea_T4, windspeed = winds_T4)/1000 #in [kN]

dt_T4 <- data.frame(WIND = winds_T4,Ct_T4, Thrust_T4)
plot(winds_T4,Thrust_T4,ylim = c(0,500)) #for the part beyond 20 m/s, the Ct is constant,
#thus since the wind speed is increasing, so is the thrust force. This means some other
#controls must be in place to reduce the power coefficient (perhaps some kind of rotor
#breaks), instead of the blades feathering, leading to such a behavior.

#ARRESTED BLADE FORCES. Scale from other arrested blade forces. wind > 0 and blades not spinning
StatBlade_T4.l <- Statblade.scale(Blength_T4)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T4.l)/1000
StatBlade_T4.h <- Statblade.scale(Blength_T4)*Wind_Force_Blade15(WIND,h.meas = 100, l.elev=Theight_T4.h)/1000

#TOWER THRUST FORCES. wind > 0
TowThrust_T4.l <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T4.l, Cd = Cd_cyl, D = Tdiam_T4)/1000 #in [kN]

TowThrust_T4.h <- mapply(Wind_Force_ConsCross, v.meas = WIND, 
                         u.elev = Theight_T4.h, Cd = Cd_cyl, D = Tdiam_T4)/1000

#FLOATER FORCES. wind > 0
FThrust_T4 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = FLdiam_T4, u.elev = FLfreeboard_T4)/1000 #in [kN]
FTThrust_T4 <- Wind_Force_ConsCross(WIND, Cd = Cd_cyl, D = Tdiam_T4, u.elev = FLfreeboard_T4)/1000 #in [kN]

SubThrust_T4 <- (FThrust_T4*3) + FTThrust_T4 #in [kN]

df_T4 <- data.frame(WIND, StatBlade_T4.h,TowThrust_T4.h, SubThrust_T4)

#Wind forces DF
DF_T4 <- merge(dt_T4, df_T4, by = "WIND", all = TRUE) #Table of wind forces
DF_T4["TotThrust_T4"] <- 0
for(row in 1:nrow(DF_T4)){
  if(is.na(DF_T4[row,]$Thrust_T4)) {
    DF_T4[row,"TotThrust_T4"] <- sum(DF_T4[row,4:6]) 
  } else {
    DF_T4[row,"TotThrust_T4"] <- sum(DF_T4[row,c(3,5,6)])
  }
}
RotorThrust_T4 <- DF_T4$TotThrust_T4 #in [kN]

#Visualize all wind thrust forces together
plot(WIND,SubThrust_T4, ylim = c(0,1000), type = 'l')
lines(WIND, StatBlade_T4.h, lty = 2, col = 'blue')
lines(WIND, TowThrust_T4.h, col = 'green') #This force is reaaalllyyy high.
lines(winds_T3, Thrust_T4, col = 'red')
lines(WIND,RotorThrust_T4, lwd = 2)
# lines(speeds, T.Force.best) #by comparison, the 15MW tower with tapering tower diameter.

#######
## CURRENT THRUST FORCES. Uses Current_Force from "20210622 15 MW Turbine and Functions.R"
## The magnitude of this current force is strongly subject to the speed of the current and the effective area
SubCurr_T4 <- Current_Force(speed = 0.9, eff.area = SubArea_T4, Cd = Cd_cyl)/1000 #in [kN]

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
SubWave_T4 <- 50 #in [kN]

#######
## TOTAL COMBINED FORCES. Uses functions from "20210616 Total Forces.R"
## Given a windspeed, this function calculates the resultant force under DLC1.3 conditions
DP.Vector_T4 <- DLC1.3_fun(RotorThrust_T4,SubWave_T4,SubCurr_T4)
plot(WIND,DP.Vector_T4, ylim = c(0,1000))

#######
## Convert forces to parasitic losses. Choose a thruster system first.
max(DP.Vector_T4[1:length(DP.Vector_T4)/2]) #DLC1.3 = 742 kN. Design at this level, although this occurs at the maximum wind speed as opposed to the rated wind speed.
tail(DP.Vector_T4,1) #DLC6.1 = 1090


parasitic.powerT4.1 <- DP.Power.n(DP.Vector_T4, motor.diam = 3.2, n = 17) #lower one
# parasitic.powerT1.2 <- DP.Power.n(DP.Vector_T1, motor.diam = 3.35, n = 3)

plot(WIND,parasitic.powerT4.1, type = 'l', col = 'red', ylim = c(0,3))
lines(WIND,parasitic.powerT1.1, col = 'blue')
abline(v = R.wind_T4)
abline(v = 12.5)

############ Calculate Parasitic losses. What % goes toward thrusters?

DF_T4 <- data.frame(WIND,Parasitic_T4 = parasitic.powerT4.1) %>%
  merge(., DF_power_T4, by.x = "WIND", by.y = "Wind.speed", all = TRUE) %>%
  rename(Parasitic = Parasitic_T4,
         Power = Power_MW) %>%
  transmute(WIND,Parasitic,Ct,Power) 

DF_T4[is.na(DF_T4)] <- 0
DF_T4$Effective <- DF_T4$Power - DF_T4$Parasitic
mutate(DF_T4, Effective = Power - Parasitic)

#Visualize the power curves
plot(DF_T4$WIND, DF_T4$Power, type = 'l', ylim = c(-2,3.5))
lines(DF_T4$WIND, DF_T4$Parasitic, col = 'blue')
lines(DF_T4$WIND, DF_T4$Effective, col = 'red')

Thrust.max_T4 <- filter(DF_T4[DF_T4$WIND < C.out_T4 - 10,], Parasitic == max(Parasitic))$WIND #where max thrust occurs

#REGION 1: Before cut in wind speed
LOS.mod.R1_T4 <- lm(Parasitic ~ poly(WIND, 1), data = filter(DF_T4,WIND < C.in_T4))
fun1_T4 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R1_T4, newdata = data.frame(WIND = x)))
#REGION 2: Cut in wind speed until maximum thrust force
LOS.mod.R2_T4 <- lm(Parasitic ~ poly(WIND, 3), data = filter(DF_T4,WIND >= C.in_T4 & WIND <= Thrust.max_T4))
fun2_T4 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R2_T4, newdata = data.frame(WIND = x)))
#REGION 3: Rated wind speed until cut out wind speed
LOS.mod.R3_T4 <- lm(Parasitic ~ poly(WIND, 5), data = filter(DF_T4,WIND > Thrust.max_T4 & WIND <= C.out_T4))
fun3_T4 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R3_T4, newdata = data.frame(WIND = x)))
#REGION 4: Above cut out wind speed
LOS.mod.R4_T4 <- lm(Parasitic ~ poly(WIND, 2), data = filter(DF_T4,WIND > C.out_T4))
fun4_T4 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(LOS.mod.R4_T4, newdata = data.frame(WIND = x)))

##PLOT: How good are these models? R2 is a bit off, but these are good
plot(x = DF_T4$WIND, DF_T4$Parasitic, pch = 19, cex = 0.75)
lines(x = DF_T4$WIND[WIND < C.in_T4], y = predict(LOS.mod.R1_T4, newdata = filter(DF_T4, WIND < C.in_T4)), col = 'red')
lines(x = DF_T4$WIND[WIND >= C.in_T4 & WIND <= Thrust.max_T4], y = predict(LOS.mod.R2_T4, newdata = filter(DF_T4, WIND >= C.in_T4 & WIND <= Thrust.max_T4)), col = 'red')
lines(x = DF_T4$WIND[WIND > Thrust.max_T4 & WIND <= C.out_T4], y = predict(LOS.mod.R3_T4, newdata = filter(DF_T4, WIND > Thrust.max_T4 & WIND <= C.out_T4)), col = 'red')
lines(x = DF_T4$WIND[WIND > C.out_T4], y = predict(LOS.mod.R4_T4, newdata = filter(DF_T4, WIND > C.out_T4)), col = 'red')

parasitic.loss_T4 <- 8760*(integrate(fun1_T4, lower = 0, upper = C.in_T4)$value + 
                             integrate(fun2_T4, lower = C.in_T4, upper = Thrust.max_T4)$value + 
                             integrate(fun3_T4, lower = Thrust.max_T4, upper = C.out_T4)$value +
                             integrate(fun4_T4, lower = C.out_T4, upper = 40)$value) #in MWh

gen.fun2_T4 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*as.numeric(predict(power.mod_T4, newdata = data.frame(x4 = x)))
#REGION3
gen.fun3_T4 <- function(x) dweibull(x, shape = P.shape, scale = P.scale)*RatPower_T4
#REGION4 = 0

gen_T4 <- 8760*(integrate(gen.fun2_T4, lower = C.in_T4, upper = R.wind_T4)$value + 
                  integrate(gen.fun3_T4, lower = R.wind_T4, upper = C.out_T4)$value) #in MWh

parasitic.loss_T4/gen_T4#68.1%
(parasitic.loss_T4)
