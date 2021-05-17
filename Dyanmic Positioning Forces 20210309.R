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
# FLOATING TURBINE GEOMETRY
# Simple Block Blades, under a uniform wind load w/r/t height. All dimensions in meters.

tower.diam <- 10 #Assume constant throughout tower
tower.height <- 150
blade.width <- 5.8 #Maximum chord width
blade.root <- 5.2 #Blade root diameter
blade.length <- 117 #m

floater.diam <- 12.5
floater.freeboard <- 15
floater.draft <- 20

Cd_cyl <- 0.5
Cd_airfoil <- 0.09 #from IEA reference turbine data Figure 2-3
Cd_square <- 1

#####################################################################################
# FORCE FUNCTIONS

#Uniform Load
Wind_Force_unif <- function(speed,eff.area,Cd) { #([m/s],[m^2],[-])
  rho_air <- 1.23 #kg/m^3
  force <- 0.5*rho_air*(speed^2)*eff.area*Cd
  return(force) #Returns values in Newtons
}

Current_Force <- function(speed,eff.area,Cd) { #([m/s],[m^2],[-])
  rho_water <- 1025 #kg/m^3
  force <- 0.5*rho_water*(speed^2)*eff.area*Cd
  return(force) #Returns values in Newtons
}

# #Parabolic Loads
# Wind_Speed <- function(h, v0 = 10) { #wind speed with height
#   z0 <- 0.0002
#   h.meas <- 100 #[m] Wind speed measuring height
#   v.height <- v0*log(h/z0)/log(h.meas/z0)
#   if (v.height < 0) v.height = 00
#   return(v.height)
# }

#wind force with height on a constant cross section body
Wind_Force_height <- function(v.meas, u.elev, Cd, D, h.meas = 150, l.elev = 0) {#([m/s],[-],[m],[m],[m],[m],[m])
  rho_air <- 1.23 #kg/m^3
  z0 <- 0.0002 #roughness coefficient for open water
  A <- (v.meas^2)*rho_air*D*Cd 
  B <- 2*(log(h.meas/z0)^2)
  # C_H <- (u.elev/z0)*(log(u.elev/z0)-1) #- (l.elev/z0)*(log(l.elev/z0)-1)
  C_H <- u.elev*((log(u.elev/z0)^2) - (2*log(u.elev/z0)) + 2)
  C_0 <- if (l.elev == 0) 0 else l.elev*((log(l.elev/z0)^2) - (2*log(l.elev/z0)) + 2)
  force <- (A/B)*(C_H - C_0)
  return(force) #Returns values in Newtons
}

#wind force with height on a constant cross section body. equation by ABS, 2013
Wind_Force_height2 <- function(v.meas, u.elev, Cd, D, h.meas = 150, l.elev = 0) {
  rho_air <- 1.23
  alpha <- 0.14 #from ABS, 2013
  A <- (v.meas^2)*rho_air*D*Cd
  B <- 2*(h.meas^(2*alpha))*((2*alpha)+1)
  C_H <- u.elev^((2*alpha) + 1)
  C_0 <- l.elev^((2*alpha) + 1)
  force <- (A/B)*(C_H - C_0)
  return(force) #Returns values in Newtons
}

Wind_Speed_height <- function(h, v.meas, h.meas, z0) {
  A <- if(h == 0) 0 else log(h/z0)
  B <- if(h.meas == 0) 0 else log(h.meas/z0) 
  speed <- v.meas*(A/B)
  return(speed) 
}

Wind_Speed_height2 <- function(h, v.hub, h.hub, alpha = 0.14){ #Given by ABS, 2013
  V <- v.hub*(h/h.hub)^alpha
  return(V)
} 

Wind_Force_alt <- function(v.meas,  eff.area, Cd, h.meas = 150) {
  rho_air <- 1.23 #kg/m^3
  v.fluid <- v.meas*(10/h.meas)^(1/7)
  force <- 0.5*rho_air*eff.area*Cd*v.fluid^2 
}

heights <- seq(0, 200, by = 10)
speeds <- seq(0,40, by = 1)

#####################################################################################
# Plot to demonstrate wind profile with height
s.meas <- seq(5,20, by = 5)
hts <- seq(1,200, by = 0.5)
M <- c()
ht1 <- c()
ht2 <- c()
for(s in s.meas) {
  i <- 1
  for(h in hts) {
    ht1[i] <- Wind_Speed_height(h, s, h.meas = 150, z0 = 0.0002)
    ht2[i] <- Wind_Speed_height2(h, s, h.hub = 150)
    i <- i + 1
  }
  M <- cbind(M, ht1, ht2)
}
M <- cbind(hts, M) %>%
  as_tibble()

plot(x = M$V, y = M$hts, type = 'n', xlim = c(0,20), ylim = c(1,200))
lines(x = M$V, y = M$hts)
lines(x = M$Z, y = M$hts, col = 'red')
lines(x = M$V4, y = M$hts)
lines(x = M$V5, y = M$hts)


#####################################################################################

## Forces for Uniform Wind Load
#Tower
t.area <- tower.diam*(tower.height + floater.freeboard) #m^2
T.Force.unif <- Wind_Force_unif(speeds, t.area, Cd_cyl)/1000 #in [kN]

#Blades
b.area <- blade.width*blade.length #m^2 for one blade
B.Force.unif <- Wind_Force_unif(speeds, b.area, Cd_square)/1000

#Floaters (air)
f.area <- floater.diam*floater.freeboard #m^2
F.Force.unif <- Wind_Force_unif(speeds, f.area, Cd_cyl)/1000

#Floaters (current) including the support below the tower.
f.area.water <- (floater.diam*floater.draft*3) + (floater.draft*tower.diam) #m^2
F.Force.Current <- Current_Force(speed = 0.9, f.area.water,Cd_cyl)/1000

#Combined Forces
Tot.Force.unif <- T.Force.unif + (3*B.Force.unif) + (3*F.Force.unif) + F.Force.Current

DF_Forces <- cbind(speeds, T.Force.unif, B.Force.unif, F.Force.unif, F.Force.Current, Tot.Force.unif) %>%
  as_tibble()

## Forces for Variable Wind Load
#Floaters (air)
F.Force.height <- Wind_Force_height2(speeds, Cd = Cd_cyl, D = floater.diam, u.elev = floater.freeboard)/1000 #in [kN]

#Support beneath tower
FT.Force.height <- Wind_Force_height2(speeds, Cd = Cd_cyl, D = tower.diam, u.elev = floater.freeboard)/1000 #in [kN]

#Tower with constant diameter
# T.Force.height <- Wind_Force_height(v.meas = speeds, 
#                                     u.elev = tower.height + floater.freeboard,
#                                     Cd = Cd_cyl)/1000 #in [kN]

T.Force.height <- Wind_Force_height2(v.meas = speeds,
                                     D = tower.diam,
                                     u.elev = tower.height + floater.freeboard,
                                     Cd = Cd_cyl)/1000 #in [kN]
#Tower with varying diameter
#Define the piecewise function of the tower diameters. This just gives tower diameters as a function of h
# D_height_fun.15 <- function(h) if (h<15 | h>54) 0 else return(-0.0026*(h-15) + 10) #h in [15,54)
# D_height_fun.54 <- function(h) if (h<54 | h>80) 0 else return(-0.029*(h-54) + 9.9) #h in [54,80)
# D_height_fun.80 <- function(h) if (h<80 | h>106) 0 else return(-0.017*(h-80) + 9.2) #h in [80,106)
# D_height_fun.106 <- function(h) if (h<106 | h>119) 0 else return(-0.026*(h-106) + 8.7) #h in [106,119)
# D_height_fun.119 <- function(h) if (h<119 | h>145) 0 else return(-0.07*(h-119) + 8.4) #h in [119,145)

D_height_fun.15 <- function(h) return(-0.0026*(h-15) + 10) #h in [15,54)
D_height_fun.54 <- function(h) return(-0.029*(h-54) + 9.9) #h in [54,80)
D_height_fun.80 <- function(h) return(-0.017*(h-80) + 9.2) #h in [80,106)
D_height_fun.106 <- function(h) return(-0.026*(h-106) + 8.7) #h in [106,119)
D_height_fun.119 <- function(h) return(-0.07*(h-119) + 8.4) #h in [119,145)


#Define the piecewise function of the integrand. this is D(h)*h^(2*alpha)
alpha <- 0.14
# int_fun.15 <- function(h) if (h<15 | h>54) 0 else return((h^(2*alpha))*(-0.0026*(h-15) + 10)) #h in [15,54)
# int_fun.54 <- function(h) if (h<54 | h>80) 0 else return((h^(2*alpha))*(-0.029*(h-54) + 9.9)) #h in [54,80)
# int_fun.80 <- function(h) if (h<80 | h>106) 0 else return((h^(2*alpha))*(-0.017*(h-80) + 9.2)) #h in [80,106)
# int_fun.106 <- function(h) if (h<106 | h>119) 0 else return((h^(2*alpha))*(-0.026*(h-106) + 8.7)) #h in [106,119)
# int_fun.119 <- function(h) if (h<119 | h>145) 0 else return((h^(2*alpha))*(-0.07*(h-119) + 8.4)) #h in [119,145)

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

Wind_Force_Tower <- function(v.meas, h.meas = 150, Cd = Cd_cyl) {
  rho_air <- 1.23 #kg/m^3
  alpha <- 0.14
  A <- rho_air*Cd*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  C <- I.15 + I.54 + I.80 + I.106 + I.119 #sum of the integrals
  force <- (A/B)*C
  return(force) #in Newtons
}

#most accurate representation of the force on the tower. Does not include support structure below tower.
T.Force.best <- Wind_Force_Tower(v.meas = speeds)/1000 #in kN

plot(x = speeds, y = T.Force.height, type = 'n')
lines(x = speeds, y = T.Force.height, col = 'red')
lines(x = speeds, y = T.Force.best, col = 'blue')

#Blades with varying diameter and Cd, and wind with varying speed
thickness <- c(5.2,3.7,2.4,1.8,1.5,1.2,0.9,0.7,0.6,0.5) #[m]
Cd.vect <- c(0.45,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09) #[-]
dist.vect <- c(0,11.7,23.4,35.1,46.8,58.5,70.2,81.9,93.6,105.3,117) #[m]

Wind_Force_Blade <- function(v.meas, h.meas = 150, l.elev = tower.height) {
  alpha <- 0.14
  rho_air <- 1.23 #kg/m^3
  
  A <- rho_air*(v.meas^2)
  B <- 2*(h.meas^(2*alpha))
  C <- 0
  
  for (i in 1:length(thickness)){
    fun <- function(h) return(h^(2*alpha)*thickness[i]*Cd.vect[i])
    component <- integrate(fun, lower = l.elev + dist.vect[i], upper = l.elev + dist.vect[i+1])$value
    C <- C + component
  }
  force <- (A/B)*C
  return(force)
}

B.Force.best.static <- Wind_Force_Blade(speeds)/1000 #in [kN]

#Blades, static airfoil blades, considered as constant cylinders
B.Force.height.static <- Wind_Force_height(speeds, 
                                            u.elev = tower.height + floater.freeboard + blade.length, 
                                            Cd = Cd_cyl, 
                                            l.elev = tower.height + floater.freeboard, 
                                            D = blade.root)/1000 #in [kN]
#Blades, static prismatic blades
B.Force.height.static2 <- Wind_Force_height(speeds, 
                                           u.elev = tower.height + floater.freeboard + blade.length, 
                                           Cd = Cd_square, 
                                           l.elev = tower.height + floater.freeboard, 
                                           D = blade.width)/1000 #in [kN]

#Combined Forces
#static airfoil blades
Tot.Force.height <- T.Force.height + (3*B.Force.height.static) + (3*F.Force.height) + F.Force.Current

#static prismatic blades
Tot.Force.height2 <- T.Force.height + (3*B.Force.height.static2) + (3*F.Force.height) + F.Force.Current

## Forces for Alternative Force Calculation from OCIMF
T.Force.alt <- Wind_Force_alt(speeds, t.area, Cd_cyl)/1000 #in [kN]
B.Force.alt <- Wind_Force_alt(speeds, b.area, Cd_square)/1000
F.Force.alt <- Wind_Force_alt(speeds, f.area, Cd_cyl)/1000

Tot.Force.alt <- T.Force.alt + (3*B.Force.alt) + (3*F.Force.alt) + F.Force.Current
#####################################################################################
#Blade Forces from Integral of graphs on report
spds <- c(6,7,8,9,10)
b_force <- c(2.36,3.07,4.06,5.27,6.13) #[kN]
df <- cbind(b_force, spds) %>%
  as_tibble %>%
  mutate(spds.sq = spds^2)

plot(x = spds, y = b_force)

mod.blade <- lm(b_force ~ spds.sq, data = df)
df$predict.b_Force <- coef(mod.blade)[1] + df$spds.sq*coef(mod.blade)[2]

#Plot showing how the extrapolated values fit with the function for normal \nforce on blades as a function of wind speed
plot(x = df$spds, y = df$b_force, pch = 19, xlim = c(0,25), ylim = c(0,30),
     xlab = "Wind Speed",
     ylab = "Normal Force on One Blade [kN]",
     main = "Measured values and model for normal force \n on blades as a function of wind speed")
lines(x = speeds, y = predict(mod.blade, newdata = data.frame(spds.sq = speeds^2)), col = 'red')

#####################################################################################
#New Blades force Calculation
B.Force.integral <- coef(mod.blade)[1] + (speeds^2)*coef(mod.blade)[2] #in kN

plot(x = speeds, y = B.Force.height, type = 'n',
     xlab = "Wind Speed",
     ylab = "Force on one blade [kN]",
     main = "Comparative results for the force on one blade")
lines(x = speeds, y = B.Force.height.static, col = 'red')
lines(x = speeds, y = B.Force.integral, col = 'blue')
legend("topleft", legend = c("Stalled Blades", "Airfoil Blades"),
       col = c('red','blue'), lty = c(1,1), cex = 0.8)

#####################################################################################
##In Alignment
F.Force.Current.normal <- Current_Force(speed = 0.2, f.area.water,Cd_cyl)/1000
# Spinning airfoil blades, wind speed with height, tower diameter with height
Tot.Force.best.spinning <- F.Force.Current.normal + T.Force.best + (3*B.Force.integral) + (3*F.Force.height) + FT.Force.height

# Static airfoil blades, wind speed with height, tower diameter with height
Tot.Force.best.static <- F.Force.Current.normal + T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height

RX.DLC1.3 <- Tot.Force.best.spinning[26]
DL.DLC1.3 <- RX.DLC1.3*1.67
#####################################################################################
##Mis-Aligned
theta <- 45 #degrees
theta.rad <- theta*pi/180
F.Force.Current.extr <- Current_Force(speed = 0.9, f.area.water,Cd_cyl)/1000

# Spinning airfoil blades, wind speed with height, tower diameter with height
Tot.Force.best.spinning.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                  (T.Force.best + (3*B.Force.integral) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)

# Static airfoil blades, wind speed with height, tower diameter with height
# Tot.Force.best.static.extr <- F.Force.Current + T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height
Tot.Force.best.static.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                       (T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)

RX.DLC6.1 <- Tot.Force.best.static.extr[39]
DL.DLC6.1 <- RX.DLC6.1*1.25
#####################################################################################

#Plot comparison of different wind profiles and reaction forces
plot(x = speeds, y = Tot.Force.alt, type = 'n',
     xlab = 'Wind Speed at 150m height [m/s]',
     ylab = 'Total Reaction Force [kN]',
     ylim = c(0,1600),
     main = 'Reaction Forces in the X-Y Plane vs Wind Speed \nfor 15-MW Floating Turbine Subject to DLCs')
# lines(x = speeds, y = Tot.Force.height2, col = 'orange') #static prismatic blades
# lines(x = speeds, y = Tot.Force.unif, col = 'blue') #static prismatic blades
# lines(x = speeds, y = Tot.Force.height, col = 'red', lty = 1) #static airfoil blades
# lines(x = speeds, y = Tot.Force.best, col = 'black', lty = 2) #spinning airfoil blades
lines(x = speeds, y = Tot.Force.best.spinning, col = 'red', lty = 3, lwd = 3.5)
lines(x = speeds, y = Tot.Force.best.static, col = 'red', lty = 1, lwd = 3.5)
lines(x = speeds, y = Tot.Force.best.spinning.extr, col = 'black', lty = 3, lwd = 3.5)
lines(x = speeds, y = Tot.Force.best.static.extr, col = 'black', lty = 1, lwd = 3.5)
# legend("topleft", legend = c("Uniform/Static Prismatic/Uniform Cylinder",
#                              "Varies with Height/Static Prismatic/Uniform Cylinder", 
#                              "Varies with Height/Static Cylindrical/Uniform Cylinder", 
#                              "Varies with Height/Spinning Airfoil/Uniform Cylinder",
#                              "Varies with Height/Spinning Airfoil/Tapering Cylinder",
#                              "Varies with Height/Static Airfoil/Tapering Cylinder"),
#        title = "Wind Profile/Blade Type/Tower Type",
#        col = c("blue","orange", "red", "black","black","red"), lty = c(1,1,1,2,2,1), lwd = c(1,1,1,1,3.5,3.5), cex = 0.8)
legend("topleft", legend = c("Static airfoil blades, extreme turbulence model", 
                             "Spinning airfoil blades, extreme turbulence model",
                             "Static airfoil blades, extreme wind speed model",
                             "Spinning airfoil blades, extreme wind speed model"),
       col = c("red","red","black","black"), lty = c(1,3, 1, 3), lwd = 3.5, cex = 1)

#####################################################################################

#Dynamic Positioning Motors: XU

motor.diam <- 3.6 #[m]
# max.force <- 57 #[kN]
# max.power <- 300/1000 #[MW]
n <- 3 #number of motors
K <- 1250 # Constant from American Bureau of Shipping (2020)

#####################################################################################

#Dynamic Positioning Motors: ORIGINAL

motor.diam <- 1.07 #[m]
max.force <- 57 #[kN]
max.power <- 300/1000 #[MW]
n <- 11 #number of motors
K <- 1250 # Constant from American Bureau of Shipping (2020)

#####################################################################################

DP.Power <- function(force.kn, n = n) { #power of one motor
  power <- ((1000*(force.kn/n)/K)^(1.5))/motor.diam
  return(power/1000) #in MW
}

DP.Power.n <- function(force.kn, n = 3) { #power of n motors system
  power <- (n/motor.diam)*(1000*force.kn/(K*n))^1.5
  return(power/1000) #in MW
}

forces <- seq(0,3000, by = 100)
powers <- DP.Power(forces,n = 1)

DF_Forces <- mutate(DF_Forces, 
                    Tot.Force.height = Tot.Force.height, 
                    DP.Power.unif = DP.Power.n(force.kn = Tot.Force.unif),
                    # DP.Power.best = DP.Power(Tot.Force.best),
                    # DP.Power.height = DP.Power(Tot.Force.height),
                    DP.Power.best.static = DP.Power.n(Tot.Force.best.static.extr),
                    DP.Power.best.spinning = DP.Power.n(Tot.Force.best.spinning.extr))

#Plot 1
plot(x = DF_Forces$speeds, y = DF_Forces$Tot.Force.unif, type = 'n',
     pch = 19,
     cex = 0.8,
     ylab = "Reaction Force [kN]",
     xlab = "Wind Speed [m/s]",
     main = "Zero-Order Reaction Force Based on Wind Speed")
lines(x = DF_Forces$speeds, y = DF_Forces$Tot.Force.unif)

#Plot 2
plot(x = forces, y = powers, type = 'n',
     pch = 19,
     cex = 0.8,
     xlim = c(0,1000),
     ylim = c(0,15),
     ylab = "Power Requirement from Thruster System [MW]",
     xlab = "Reaction Force [kN]",
     main = "Thruster System Power vs. Applied Force")
# lines(x = forces, y = powers)
curve(((1/motor.diam)*(1000*x/K)^1.5)/1000, from = 0, to = max.force, add = TRUE, col = 'blue', lwd = 2)
curve(((n/motor.diam)*(1000*x/(K*n))^1.5)/1000, from = 0, to = 1020, add = TRUE, col = 'green', lwd = 2)
abline(h = max.power, col = 'red') #rated power consumption [MW]
abline(v = max.force, col = 'red') #rated applied force [kN]
abline(v = 1018, col = 'blue', lty = 2)
legend("topleft", legend = c("Rated Power and Force of One DP Motor",
                              "Maximum Needed Reaction Force",
                              "Power vs. Applied Force for One Thruster",
                              "Thruster System Power vs. Applied Force"),
       col = c('red','blue','blue','green'), lty = c(1,2,1,1), lwd = c(1,1,2,2),cex = 0.8)

#Plot 3
plot(x = DF_Forces$speeds, y = DF_Forces$DP.Power.unif, 
     type = 'n',
     pch = 20,
     cex = 0.8,
     ylim = c(0,20),
     ylab = "Power Requirement from Motors [MW]",
     xlab = "Wind Speed [m/s]",
     main = "DP Power Requirement Based on Wind Speed")
# lines(x = DF_Forces$speeds, y = DF_Forces$DP.Power.unif, col = 'blue')
lines(x = DF_Forces$speeds, y = DF_Forces$DP.Power.best.static, col = 'red',lwd = 2)
lines(x = DF_Forces$speeds, y = DF_Forces$DP.Power.best.spinning, col = 'black', lty = 2, lwd = 2)
abline(h = 15, lty = 1, col = 'blue')
# abline(h = 8, lty = 3)
legend("topleft", 
       legend = c("Turbine Rated Power", 
                  # "Uniform Wind Profile, static prismatic blades",
                  "Static Turbine Blades",
                  "Spinning Turbine Blades"),
       col = c('blue','red','black'), lty = c(1,1,2), lwd = c(1,2,2), cex = 0.7)

#####################################################################################

# Mooring Costs

Anchor.cost <- function(strength.kn) 10.198*strength.kn #returns cost in USD
Chain.cost <- function(strength.kn, length) (0.0591*strength.kn - 87.6)*length #returns cost in USD

lengths <- seq(0,10000, by = 100)
MBL = 22286 #[kN]

costs <- 3*Anchor.cost(MBL) + 3*Chain.cost(MBL,lengths)

plot(x = lengths, y = costs/1000, type = 'n',
     pch = 19,
     cex = 0.8,
     ylab = "Capital Costs [Thousand USD]",
     xlab = "Chain Length [m]",
     main = "Three-Chain Mooring system cost with length \nIncluding Anchor Cost")
lines(x = lengths, y = costs/1000)
curve((3*1496*x + 3*Anchor.cost(MBL))/1000, from = 0, to = 10000, col = 'red', add = TRUE)
curve((3*1995.84*x + 3*Anchor.cost(MBL))/1000, from = 0, to = 10000, col = 'blue', add = TRUE)


#relate to depth given an angle
angle = 56.4

#####################################################################################

# Power Curve for 15-MW Turbine

# Critical Points: (3,0), (5,1.4), (7.4,5), (9.2,10), (10.59,15), (25,15) *First three points are on a quadratic

# Quadratic Region
x <- c(3,5,7.4,9.2,10.59)
y <- c(0,1.4,5,10,15)

power.mod <- lm(y ~ poly(x, 2))

x.plot <- seq(min(x), max(x), length.out=100)
y.plot <- predict(power.mod, newdata = data.frame(x = x.plot))

#just to visualize the quadratic region
plot(x,y,pch = 19)
lines(x.plot, y.plot, col = "red")

Power.Curve <- function(wind.speed) { #calculate the turbine's 'raw' power output for each wind speed
  if (wind.speed < 3) 0
  else if (wind.speed <= 10.58) as.double(predict(power.mod, newdata = data.frame(x=wind.speed)))
    # coef(power.mod)[1] + coef(power.mod)[2]*wind.speed + coef(power.mod)[3]*wind.speed^2
  else if (wind.speed <= 25) 15
  else 0
}

power.out <- sapply(speeds,Power.Curve)

#my Extrapolated Power Curve
plot(x, y, type = 'n',
     pch = 20, 
     xlab = "Wind Speed [m/s]", 
     ylab = "Turbine Output [MW]",
     main = "15-MW Reference Turbine Power Output",
     xlim = c(0,40),
     ylim = c(0,20))
lines(speeds,power.out, col = 'red', lwd = 2.5)

DF_Forces <- mutate(DF_Forces, Turbine.Power = power.out)

#Combine to see the DP turbine output
DF_Forces <- mutate(DF_Forces, 
                    DP.Turbine.Power.unif = DF_Forces$Turbine.Power - DF_Forces$DP.Power.unif,
                    # DP.Turbine.Power.height = DF_Forces$Turbine.Power - DF_Forces$DP.Power.height,
                    # DP.Turbine.Power.best = DF_Forces$Turbine.Power - DF_Forces$DP.Power.best,
                    DP.Turbine.Power.best.spinning = DF_Forces$Turbine.Power - DF_Forces$DP.Power.best.spinning,
                    DP.Turbine.Power.best.static = DF_Forces$Turbine.Power - DF_Forces$DP.Power.best.static)

#Develop the effective DP turbine power curve vector
DP.Turbine.Power.Effective <- c()
  
for(s in 1:length(DF_Forces$speeds)) {
  Raw <- DF_Forces$Turbine.Power[s]
  Cons <- -DF_Forces$DP.Power.best.static[s]
  Eff <- DF_Forces$DP.Turbine.Power.best.spinning[s]
  if(Raw == 0) { #blades are not spinning
    DP.Turbine.Power.Effective[s] <- Cons
  } else { #blades are spinning
    DP.Turbine.Power.Effective[s] <- Eff
  }
}

DF_Forces <- mutate(DF_Forces, 
                    DP.Turbine.Power.Effective = DP.Turbine.Power.Effective) 

#combine DP and 'raw' turbine power curves
plot(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.unif, type ='n',
     xlim = c(0,40),
     ylim = c(-8,15),
     xlab = "Wind Speed [m/s]",
     ylab = "Power Output [MW]",
     main = "Power Output of Moored and DP Floating Turbine")
abline(h = 0, col = "gray")
lines(x = DF_Forces$speeds, y = DF_Forces$Turbine.Power, col = 'black',lwd = 2.5)
# lines(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.unif, col = 'blue', lwd = 2)
# lines(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.height, col = 'red', lwd = 2)
# lines(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.best.spinning, col = 'green', lwd = 2.5)
# lines(x = DF_Forces$speeds, y = -DF_Forces$DP.Power.unif, col = 'blue', lty = 2)
lines(x = DF_Forces$speeds, y = -DF_Forces$DP.Power.best.static, col = 'red')
lines(x = DF_Forces$speeds, y = -DF_Forces$DP.Power.best.spinning, col = 'black', lty = 2)
lines(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.Effective, col = 'blue', lwd = 2.5)
# legend("bottomleft", legend = c("Turbine Power Curve (moored turbine)",
#                                 "Turbine with DP Motor Power Curve (uniform wind)",
#                                 "DP Motor Power Consumption (uniform wind)",
#                                 "Turbine with DP Motor Power Curve (variable wind)",
#                                 "DP Motor Power Consumption (variable wind)",
#                                 "Turbine with DP Motor Power Curve (best)",
#                                 "DP Motor Power Consumption (best)"),
#        col = c('black','blue','blue','red','red','green','green'),
#        lty = c(1,1,2,1,2,1,2), cex = 0.7)

legend("bottomleft", legend = c("Turbine Power Curve (moored turbine)",
                                "Turbine Power Curve (DP turbine)",
                                "DP System Consumption, Spinning Blades",
                                "DP System Consumption, Static Blades"),
       col = c('black','blue','red','black'),
       lty = c(1,1,1,2), lwd = c(2.5,2.5,1,1), cex = 0.9)
#####################################################################################
#Power Ratio: XU Paper

Power.Ratio <- (DF_Forces$Turbine.Power - DF_Forces$DP.Turbine.Power.Effective)/DF_Forces$Turbine.Power
DF_Ratios <- as_tibble(data.frame(speeds,Power.Ratio))

par(mar = c(5,4,4,6) + 0.1)
plot(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.unif, type ='n', axes=FALSE,
     xlim = c(0,40),
     ylim = c(-5,15),
     xlab = "",
     ylab = "",
     main = "Power Output of Moored and DP Floating Turbine")
abline(h = 0, col = "gray")
lines(x = DF_Forces$speeds, y = DF_Forces$Turbine.Power, col = 'black',lwd = 2.5)
lines(x = DF_Forces$speeds, y = -DF_Forces$DP.Power.best.static, col = 'red')
lines(x = DF_Forces$speeds, y = -DF_Forces$DP.Power.best.spinning, col = 'black', lty = 2)
lines(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.Effective, col = 'blue', lwd = 2.5)
axis(2, ylim = c(-5,15), col = "black", las = 1)
mtext("Turbine Power Output [MW]", side = 2, line = 2.5)
box()

#secondary y-axis plot for power ratio
par(new=TRUE)

plot(x = DF_Ratios$speeds, y = DF_Ratios$Power.Ratio, pch = 15, xlab = "", ylab = "",
     ylim=c(-0.5,1.5), axes = FALSE, type = 'b', col = 'red')
mtext("Power Ratio", side = 4, col = 'red', line = 3)
axis(4, ylim = c(0,1.2), col = 'red', col.axis = 'red', las = 1)

#x axis specification
axis(1, pretty(DF_Forces$speeds,8))
mtext("Wind Speed [m/s]", side = 1, line = 2.5)

# legend("bottomleft", legend = c("Turbine Power Curve (moored turbine)",
#                                 "Turbine Power Curve (DP turbine)",
#                                 "DP System Consumption, Spinning Blades",
#                                 "DP System Consumption, Static Blades"),
#        col = c('black','blue','red','black'),
#        lty = c(1,1,1,2), lwd = c(2.5,2.5,1,1), cex = 0.9)

#####################################################################################
# Mooring system and DP system cost 
dp.unit.cost <- 1500 #$/hp
# dp.unit.hp <- 1676 #Extreme condition design load with redundancy (https://www.thrustmaster.net/azimuth-thrusters/thru-hull-l-drive-azimuth-thrusters/#techspecs)
# dp.unit.hp2 <- 4693 #Extreme condition design load without redundancy
# dp.unit.hp3 <- 1676 #Expected condition design load with redundancy
# n.engines <- 10
# 
# DP.Cost <- dp.unit.cost*dp.unit.hp*n.engines #30 Million USD
# DP.Cost2 <- dp.unit.cost*dp.unit.hp2*n.engines #21
# DP.Cost3 <- dp.unit.cost*dp.unit.hp3*n.engines
R4.weight <- 558 #kg/m
R4S.weight <- 493 #kg/m
R4.cost.high <- 3.08 #$/kg
R4.cost.low <- 2.31 #$/kg

chain.to.depth.ratio <- 850/200 #from gaertner et al. (2020)

plot(x = lengths, y = costs/1000, type = 'n',
     pch = 19,
     xlim = c(0,2000),
     ylim = c(0,30000),
     cex = 0.8,
     ylab = "PV of Costs (CAPEX and OPEX) [Thousand USD]",
     xlab = "Water Depth [m]",
     main = "Three-Chain Mooring system cost \ncompared to DP system cost")
lines(x = lengths/chain.to.depth.ratio, y = costs/1000, col = 'red',lwd = 2)
curve((3*R4S.weight*R4.cost.low*(x*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000, 
      from = 0, to = 10000, col = 'darkgreen', add = TRUE)
curve((3*R4.weight*R4.cost.high*(x*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000, 
      from = 0, to = 10000, col = 'blue', add = TRUE)
# abline(h = 14070, col = 'blue')
abline(h = 11500 + 3100, col = 'green', lty = 2, lwd = 2)
abline(h = 6600 + 4100, col = 'blue',lty = 2)
# abline(h = 11500 , col = 'green', lty = 2, lwd = 2)
# abline(h = 6600 , col = 'blue',lty = 2)
# abline(v = 299)
# abline(v = 701)
abline(h = 0, col = 'gray')
abline(v = 0, col = 'gray')
# abline(v = high.switchover)
legend('topleft', legend = c("DP System, DLC 6.1",
                             "DP System, DLC 1.3",
                             "Three-Chain Mooring System, high",
                             "Three-Chain Mooring System, baseline",
                             "Three-Chain Mooring System, low"),
       lty = c(2,2,1,1,1), col = c('green','blue','blue','red','darkgreen'))

# abline(h = DP.Cost2/1000, col = 'green')
# abline(h = DP.Cost3/1000, col = 'orange')
# legend("bottomright", 
#        legend = c("Three-Chain Mooring System from Gaertner et al. (2020)", 
#                   "DP System under DLC 6.1",
#                   # "Three-Motor DP System (without redundancy criterion)",
#                   "DP System under DLC 1.3"),
#        col = c('red','green','blue'), lty = 1, cex = 0.9)

#### Intersections
# origin.mooring <- c(0,3*Anchor.cost(MBL)/1000)
# low.mooring <- c(500, (3*R4S.weight*R4.cost.low*(500*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000)
# high.mooring <- c(500, (3*R4.weight*R4.cost.high*(500*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000)

low.switchover <- (12300000 - 3*Anchor.cost(MBL))/(3*R4.weight*R4.cost.high*chain.to.depth.ratio)
high.switchover <- (16700000 - 3*Anchor.cost(MBL))/(3*R4S.weight*R4.cost.low*chain.to.depth.ratio)
# best.switchover <- 

################################################################################
##PLOT WITH DISTRIBUTION ABOUT THE DP CAPEX ESTIMATE
#distribution estimate
Motor.HP <- 335
n.motors <- 24
min.cost <- 550 #[$/HP]
best.cost <- 1500 #[$/HP]
max.cost <- 2000 #[$/HP]

MIN <- n.motors*min.cost*Motor.HP/1000000
MODE <- n.motors*best.cost*Motor.HP/1000000
MAX <- n.motors*max.cost*Motor.HP/1000000

pdf <- data.frame(yval = seq(MIN, MAX, 0.1),
                  xval = 1000*(mc2d::dpert(seq(MIN, MAX, 0.1),min = MIN, mode = MODE, max = MAX)))

#The Plot of CAPEX with distribution of DLC 6.1
par(mar = c(5,5,3,5))
plot(x = lengths, y = costs/1000, type = 'n',
     pch = 19, las = 1, xaxs = 'i', yaxs = 'i',
     xlim = c(0,1500),
     ylim = c(0,35),
     cex.axis = 1.5, cex.lab = 1.7, las = 1, 
     ylab = "Capital Cost [Million USD]",
     xlab = "Water Depth [m]",
     main = "")
polygon(c(p,rev(p)),c(y1,rev(y2)), col = 'lightblue', border = NA)
lines(x = lengths/chain.to.depth.ratio, y = costs/1000000, col = 'red',lwd = 2)
# curve((3*R4S.weight*R4.cost.low*(x*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000000, 
#       from = 0, to = 10000, col = 'darkgreen', add = TRUE)
# curve((3*R4.weight*R4.cost.high*(x*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000000, 
#       from = 0, to = 10000, col = 'blue', add = TRUE)

abline(h = 12.1, col = 'black', lty = 2, lwd = 2)
# 
# abline(h = 0, col = 'gray')
# abline(v = 0, col = 'gray')

with(pdf, lines(xval,yval,lwd = 2)) #the probability distribution surrounding the cost

# legend('topleft', legend = c("DP System, DLC 6.1",
#                              "Distirubtion of CAPEX",
#                              "Three-Chain Mooring System, high",
#                              "Three-Chain Mooring System, baseline",
#                              "Three-Chain Mooring System, low"),
#        lty = c(2,1,1,1,1), col = c('black','black','blue','red','darkgreen'), cex = 0.8)

#The Plot of PV of costs accounting for CAPEX and OPEX
plot(x = lengths, y = costs/1000, type = 'n',
     pch = 19,
     xlim = c(0,1500),
     las = 1,
     ylim = c(0,30), xaxs = 'i', yaxs = 'i',
     cex.axis = 1.7,
     cex.lab = 1.7,
     ylab = "PV of Costs, Million USD",
     xlab = "Water Depth, m",
     main = "")
lines(x = lengths/chain.to.depth.ratio, y = costs/1000000, col = 'red',lwd = 2)
curve((3*R4S.weight*R4.cost.low*(x*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000000, 
      from = 0, to = 10000, col = 'darkgreen', add = TRUE)
curve((3*R4.weight*R4.cost.high*(x*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000000, 
      from = 0, to = 10000, col = 'blue', add = TRUE)

abline(h = 14.6, col = 'black', lty = 2, lwd = 2)
abline(h = 14.5 + sqrt(5), col = 'blue', lty = 2, lwd = 1)
abline(h = 14.5 - sqrt(5), col = 'blue', lty = 2, lwd = 1)

# abline(h = 0, col = 'gray')
# abline(v = 0, col = 'gray')

legend('topleft', legend = c("Median PV of CAPEX and OPEX Costs",
                             "Standard Deviation from the Mean PV of Costs",
                             "Three-Chain Mooring System, high",
                             "Three-Chain Mooring System, baseline",
                             "Three-Chain Mooring System, low"),
       lty = c(2,2,1,1,1), lwd = c(2,1,1,1,1), 
       col = c('black','blue','blue','red','darkgreen'), cex = 0.8)

#The same Plot above with bands to highlight of PV of costs accounting for CAPEX and OPEX
p <- seq(0,1500,1)
y1 <- (3*R4S.weight*R4.cost.low*(p*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000000
y2 <- (3*R4.weight*R4.cost.high*(p*chain.to.depth.ratio) + 3*Anchor.cost(MBL))/1000000
mygreen <- adjustcolor('lightgreen',alpha.f = 0.4)
  
plot(x = lengths, y = costs/1000, type = 'n',
     pch = 19,
     xlim = c(0,1500),
     las = 1,
     ylim = c(0,35), xaxs = 'i', yaxs = 'i',
     cex.axis = 1.7,
     cex.lab = 1.7,
     ylab = "PV of Costs [Million USD]",
     xlab = "Water Depth [m]",
     main = "")
polygon(c(p,rev(p)),c(y1,rev(y2)), col = 'lightblue', border = NA)
lines(x = lengths/chain.to.depth.ratio, y = costs/1000000, col = 'red',lwd = 2)

polygon(c(p, rev(p)),c(rep(14.5+sqrt(5),length.out = length(p)), 
                       rep(14.5-sqrt(5),length.out = length(p))), col = mygreen, border = NA)

abline(h = 14.6, col = 'black', lty = 2, lwd = 2)
abline(h = 14.5 + sqrt(5), col = 'blue', lty = 2, lwd = 1)
abline(h = 14.5 - sqrt(5), col = 'blue', lty = 2, lwd = 1)
################################################################################
# FAKE DEMONSTRATION GRAPH###
#### FAKE ####
plot(x = lengths/4, y = costs/150000, type = 'n',
     pch = 19,
     cex = 0.8,
     ylab = "Lifecycle Cost Contribution from Stabilizing Only [USD/MW]",
     xlab = "Plant Water Depth [m]",
     main = "Mooring system cost and DP system cost with depth")
lines(x = lengths/4, y = costs/150000, col = 'darkblue', lwd = 2)
# abline(h = 0.8*DP.Cost/100000, col = 'blue')
# abline(h = DP.Cost2/1000, col = 'green')
abline(h = 1.45*DP.Cost3/150000, col = 'darkgreen', lwd = 2)
legend("topleft", 
       legend = c("Mooring System Lifecycle Cost", 
                  "DP System Lifecycle Cost"),
       col = c('darkblue','darkgreen'), lty = 1, lwd = 2, cex = 0.75)
# FAKE DEMONSTRATION GRAPH###
#### FAKE ####

################################################################################
# Combine into a nice table

library(kableExtra)

#####################################################################################

#Wind Speed Distribution
shp1 <- 2.3
scl1 <- 10

curve(dweibull(x, shape = shp1, scale = scl1), from = 0, to = 40,
      # xlim = c(0,40),
      # ylim = c(0, 1000),
      xlab = "Wind Speed [m/s]",
      ylab = "Probability Density",
      main = c("Weibull Distribution of a Potential Candidate Site \nShape =" + shp + "Scale ="+ scl+ "m/s"))
abline(h = 0, col = 'gray')

Energy.Penalty <- function(x) {
  shp <- 2
  scl <- 12
  power.req <- as.numeric(DF_Forces[DF_Forces$speeds == x, "DP.Power.best"])
  energy <- 8760*dweibull(x, shape = shp, scale =scl)*power.req #[hours/year]*[probability density of wind speed]*[power(windspeed)] = [energy/year]
  return(energy) #MWh
}

Energy.Production <- function(x) {
  shp = 2 
  scl = 10.5
  power.prod <- as.numeric(DF_Forces[DF_Forces$speeds == x, "Turbine.Power"])
  energy <- 8760*dweibull(x, shape = shp, scale = scl)*power.prod
  return(energy) #MWh
}

Energy.Production.DP <- function(x) {
  shp = 2 
  scl = 10.5
  power.prod <- as.numeric(DF_Forces[DF_Forces$speeds == x, "DP.Turbine.Power.Effective"])
  energy <- 8760*dweibull(x, shape = shp, scale = scl)*power.prod
  return(energy) #MWh
}

energy.pen <- sapply(speeds,Energy.Penalty)

barplot(energy.pen, names.arg = DF_Forces$speeds, width = 0.1, 
        xlim = c(0,40),
        ylim = c(0,1000),
        space = 8,
        cex.axis = 0.8,
        cex.names = 0.8,
        xlab = "Wind Speed [m/s]",
        ylab = "Energy [MWh/year]",
        main = "Energy Penalty at Each Wind Speed \n(Considering Number of Hours per Year at that Speed)")
sum(energy.pen) #12,845 MWh/year of energy as fuel. 9,495 MWh/year with wind profile. 3,409 MWh/year with best wind profile
OPEX <- sum(energy.pen)*65 #65$/MWh

energy.prod <- sapply(speeds,Energy.Production)
energy.prod.DP <- sapply(speeds,Energy.Production.DP)
sum(energy.prod) #82,200 MWh/year. Effective power curve = 78,598.45 MWh/year == 3601.43 MWh/year of energy penalty
sum(energy.prod.DP) #68325.2

Parasitic.Losses <- sum(energy.prod) - sum(energy.prod.DP) #MWh

####################################################################################
##Mis-Aligned. Force as a function of any wind speed
DP.Turbine.Force <- function(wind) {
  theta <- 0 #degrees
  theta.rad <- theta*pi/180
  
  #non-blade components
  F.Force.Current.extr <- Current_Force(speed = 0, f.area.water,Cd_cyl)/1000
  T.Force.best <- Wind_Force_Tower(v.meas = wind)/1000 #in kN
  F.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = floater.diam, u.elev = floater.freeboard)/1000 #in [kN]
  FT.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = tower.diam, u.elev = floater.freeboard)/1000 #in [kN]
  
  #blade spinning component
  B.Force.integral <- coef(mod.blade)[1] + (wind^2)*coef(mod.blade)[2] #in kN
  
  #blade static component
  B.Force.best.static <- Wind_Force_Blade(wind)/1000 #in [kN]
  
  # Spinning airfoil blades, wind speed with height, tower diameter with height
  Tot.Force.best.spinning.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                         (T.Force.best + (3*B.Force.integral) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)
  
  # Static airfoil blades, wind speed with height, tower diameter with height
  # Tot.Force.best.static.extr <- F.Force.Current + T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height
  Tot.Force.best.static.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                       (T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)

  return(c(Tot.Force.best.spinning.extr, Tot.Force.best.static.extr))
}
DP.Turbine.Wind.Spinning <- function(wind) {
  theta <- 0 #degrees
  theta.rad <- theta*pi/180
  
  #non-blade components
  F.Force.Current.extr <- Current_Force(speed = 0, f.area.water,Cd_cyl)/1000
  T.Force.best <- Wind_Force_Tower(v.meas = wind)/1000 #in kN
  F.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = floater.diam, u.elev = floater.freeboard)/1000 #in [kN]
  FT.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = tower.diam, u.elev = floater.freeboard)/1000 #in [kN]
  
  #blade spinning component
  B.Force.integral <- coef(mod.blade)[1] + (wind^2)*coef(mod.blade)[2] #in kN
  
  #blade static component
  B.Force.best.static <- Wind_Force_Blade(wind)/1000 #in [kN]
  
  # Spinning airfoil blades, wind speed with height, tower diameter with height
  Tot.Force.best.spinning.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                         (T.Force.best + (3*B.Force.integral) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)
  
  # Static airfoil blades, wind speed with height, tower diameter with height
  # Tot.Force.best.static.extr <- F.Force.Current + T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height
  Tot.Force.best.static.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                       (T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)
  
  return(Tot.Force.best.spinning.extr)
}

DP.Turbine.Wind.Static <- function(wind) {
  theta <- 0 #degrees
  theta.rad <- theta*pi/180
  
  #non-blade components
  F.Force.Current.extr <- Current_Force(speed = 0, f.area.water,Cd_cyl)/1000
  T.Force.best <- Wind_Force_Tower(v.meas = wind)/1000 #in kN
  F.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = floater.diam, u.elev = floater.freeboard)/1000 #in [kN]
  FT.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = tower.diam, u.elev = floater.freeboard)/1000 #in [kN]
  
  #blade spinning component
  B.Force.integral <- coef(mod.blade)[1] + (wind^2)*coef(mod.blade)[2] #in kN
  
  #blade static component
  B.Force.best.static <- Wind_Force_Blade(wind)/1000 #in [kN]
  
  # Spinning airfoil blades, wind speed with height, tower diameter with height
  Tot.Force.best.spinning.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                         (T.Force.best + (3*B.Force.integral) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)
  
  # Static airfoil blades, wind speed with height, tower diameter with height
  # Tot.Force.best.static.extr <- F.Force.Current + T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height
  Tot.Force.best.static.extr <- sqrt((F.Force.Current.extr*sin(theta.rad))^2 + 
                                       (T.Force.best + (3*B.Force.best.static) + (3*F.Force.height) + FT.Force.height + F.Force.Current.extr*cos(theta.rad))^2)
  
  return(Tot.Force.best.static.extr)
}
#####################################################################################
Parasitic <- function(shp, scl) {
  #Integrate continuous function to get power output of raw turbine
  fun <- function(x) { #3 - 10.59 m/s
    power <- as.numeric(predict(power.mod, newdata = data.frame(x = x)))
    probability <- dweibull(x, shape = shp, scale = scl)
    return(power*probability)
  }
  fun2 <- function(x) { #10.59 - 25 m/s
    power <- 15
    probability <- dweibull(x, shape = shp, scale = scl)
    return(power*probability)
  }
  
  Raw.Output <- 8760*(integrate(fun, lower = 3, upper = 10.59)$value + 
                         integrate(fun2, lower = 10.59, upper = 25)$value)
  
  #Integrate continuous function to get power out put of DP turbine
  DPfun <- function(x) { #0 - 3 m/s
    force <- DP.Turbine.Force(x)[2] #static
    power <- -DP.Power.n(force)
    probability <- dweibull(x, shape = shp, scale = scl)
    return(power*probability)
  }
  DPfun2 <- function(x) { #3 - 10.59 m/s
    force <- DP.Turbine.Force(x)[1] #spinning
    power <- as.numeric(predict(power.mod, newdata = data.frame(x = x))) - DP.Power.n(force)
    probability <- dweibull(x, shape = shp, scale = scl)
    return(power*probability)
  }
  DPfun3 <- function(x) { #10.59 - 25 m/s
    force <- DP.Turbine.Force(x)[1] #spinning
    power <- 15 - DP.Power.n(force)
    probability <- dweibull(x, shape = shp, scale = scl)
    return(power*probability)
  }
  DPfun4 <- function(x) { #25 m/s +
    force <- DP.Turbine.Force(x)[2] #static
    power <- -DP.Power.n(force)
    probability <- dweibull(x, shape = shp, scale = scl)
    return(power*probability)
  }
  
  DP.Output <- 8760*(integrate(DPfun, lower = 0, upper = 3)$value + 
                       integrate(DPfun2, lower = 3, upper = 10.59)$value +
                       integrate(DPfun3, lower = 10.59, upper = 25)$value +
                       integrate(DPfun4, lower = 25, upper = 38)$value)
  
  Parasitic <- Raw.Output - DP.Output
  percent <- DP.Output/Raw.Output
  return(c(Parasitic,percent)) # Gives Parasitic Losses in MWh/year

}

parasitic.scale <- c()
parasitic.shape <- c()
parasitic.scale.pct <- c()
parasitic.shape.pct <- c()

shape.v <- seq(from = 1.5, to = 9, by = 0.1)
shape.baseline <- 2.56

scale.v <- seq(from = 2, to = 25, by = 0.1)
scale.baseline <- 10.24

parasitic.baseline <- Parasitic(shp = shape.baseline, scl = scale.baseline)[1]

for(i in 1:length(scale.v)) {
  parasitic.scale[i] <- Parasitic(shp = shape.baseline, scl = scale.v[i])[1]
  parasitic.scale.pct[i] <- Parasitic(shp = shape.baseline, scl = scale.v[i])[2]
}

for(i in 1:length(shape.v)) {
  parasitic.shape[i] <- Parasitic(shp = shape.v[i], scl = scale.baseline)[1]
  parasitic.shape.pct[i] <- Parasitic(shp = shape.v[i], scl = scale.baseline)[2]
  
}

#PLOT: Weibull curve characteristics (with turbine curves underneath)
par(mar = c(5, 5, 3, 5))
curve(dweibull(x, shape = shape.baseline, scale = scale.baseline), from = 0, to = 40, lwd = 3,
      xlab = "Wind Speed [m/s]",
      ylab = "Probability Density",
      main = "Approximated Weibull Wind Distribution and Turbine Power Curves",
      ylim = c(-0.04, 0.12))
# curve(dweibull(x, shape = shape.baseline*0.9, scale = scale.baseline), from = 0, to = 40,
#       col = 'red',
#       lwd = 2,
#       add = TRUE)
abline(h = 0, col = 'gray')
abline(v = 0, col = 'gray')
par(new = TRUE)
plot(x = DF_Forces$speeds, y = DF_Forces$Turbine.Power, type = 'l', xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", 
     col = 'black', 
     ylim = c(-5, 15), lwd = 0.5,lty = 2)
axis(side = 4)
mtext("Turbine Power Output [MW]", side = 4, line = 3)
lines(x = DF_Forces$speeds, y = DF_Forces$DP.Turbine.Power.Effective, col = 'blue', 
      lwd = 0.5,lty = 2)
legend('bottomleft', legend = c("Probability Density (left axis)", 
                                # "Probability Density of 0.9*baseline shape (left axis)",
                                "Raw Turbine Power Curve (right axis)",
                                "DP Turbine Power Curve (right axis)"),
       col = c('black','black','blue'), lty = c(1,2,2), lwd = c(3,0.5,0.5), cex = 0.8)

loss <- Parasitic(shp = shape.baseline, scl = scale.baseline)
# loss <- Parasitic(shp = 2, scl = 10.5)

#PLOT: Parasitic losses versus change in Weibull parameters (percent from baseline)
plot(x = 100*(shape.v/shape.baseline - 1), y = 100*(parasitic.shape/parasitic.baseline - 1), 
     type = 'n', 
     xlim = c(-100, 300),
     ylim = c(-100, 500),
     xlab = "Percent Change in Weibull Parameter from Baseline [%]",
     ylab = "Percent Change in Parasitic Losses from Baseline [%]",
     main = "Wind Profile Parameters vs. Parasitic Losses")
lines(x = 100*(shape.v/shape.baseline - 1), y = 100*(parasitic.shape/parasitic.baseline - 1))
lines(x = 100*(scale.v/scale.baseline - 1), y = 100*(parasitic.scale/parasitic.baseline - 1), col = 'red')
abline(h = 0, col = 'gray', lty = 2)
abline(v = 0, col = 'gray', lty = 2)
legend('topright', legend = c("Scale Parameter", "Shape Parameter"), col = c('red','black'), lty = 1)


#PLOT: Parasitic losses versus change in Weibull parameters (percent from raw turbine)
plot(x = 100*(shape.v/shape.baseline - 1), y = 100*parasitic.shape.pct, 
     type = 'n', 
     xlim = c(-50, 300),
     ylim = c(80, 100),
     xlab = "Percent Change in Weibull Parameter from Baseline [%]",
     ylab = "DP Turbine Output as a Percent Percent of Raw Turbine Output [%]",
     main = "Wind Profile Parameters vs. DP Turbine Output as a % of Raw Turbine Output")
lines(x = 100*(shape.v/shape.baseline - 1), y = 100*parasitic.shape.pct)
lines(x = 100*(scale.v/scale.baseline - 1), y = 100*parasitic.scale.pct, col = 'red')
abline(h = 100, col = 'blue', lty = 1)
abline(h = 0, col = 'gray', lty = 2)
abline(v = 0, col = 'gray', lty = 2)
legend('bottomright', legend = c("Scale Parameter", "Shape Parameter", "Raw Turbine Output"), col = c('red','black','blue'), lty = 1)
