# Created by Rudolph Santarromana
# June 2021
# Blade Thrust Forces

library(tidyverse)
library(dplyr)


# Recreate 3-1.b in 15 MW Reference turbine document (Gaertner et al., 2020)
#REGION 1: 3 - 10.59
X.R1 <- c(3,5,10.59) #m/s of windspeed
Y.R1 <- c(609.4,937.5,2789) #kN of thrust

X.R1.2 <- X.R1^2

mod.R1 <- lm(Y.R1 ~ X.R1 + X.R1.2)

#REGION 2: 10.59 - 25
X.R2 <- c(10.59,15,20,25) #m/s of windspeed
Y.R2 <- c(2789,1617,1312.5,1171.9) #kN of thrust

X.R2.2 <- X.R2^2
X.R2.3 <- X.R2^3

mod.R2 <- lm(Y.R2 ~ X.R2 + X.R2.2)
# mod.R2 <- lm(Y.R2 ~ log(X.R2))


#plot of how model fits the data
plot(X.R1,Y.R1, pch = 19, col = 'red', xlim = c(3,25),
     xlab = "Wind Speed [m/s]", ylab = "Swept Area Thrust Force [kN]")
curve(mod.R1$coefficients[1] + mod.R1$coefficients[2]*x + mod.R1$coefficients[3]*x^2, 
      from = 3, to = 10.59, add = TRUE)
points(X.R2,Y.R2, pch = 19, col = 'red')
curve(mod.R2$coefficients[1] + mod.R2$coefficients[2]*x + mod.R2$coefficients[3]*x^2, #+ mod.R2$coefficients[4]*x^3,
      from = 10.59, to = 25, add = TRUE)
# curve(mod.R2$coefficients[1] + mod.R2$coefficients[2]*log(x),
#       from = 10.59, to = 25, add = TRUE)

Wind_Blade_Force_spinning <- function(wind) { #windspeed in m/s as input only defined on [3,25] interval
  if(wind < 3 | wind > 25) force <- 0
  if(wind >= 3 && wind <= 10.59) force <- as.numeric(mod.R1$coefficients[1] + mod.R1$coefficients[2]*wind + mod.R1$coefficients[3]*wind^2)
  if(wind >10.59 && wind <= 25) force <- as.numeric(mod.R2$coefficients[1] + mod.R2$coefficients[2]*wind + mod.R2$coefficients[3]*wind^2)
  return(force) #returns a value in kN
}

Wind_Blade_Force_spinning(10.59)

DP.Turbine.Wind.Force <- function(wind) { #functions in this function are contained in "Dynamic Positioning Forces 20210309 - Debugging"
  theta <- 0 #degrees
  theta.rad <- theta*pi/180
  
  #non-blade components
  T.Force.best <- Wind_Force_Tower(v.meas = wind)/1000 #in kN
  F.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = floater.diam, u.elev = floater.freeboard)/1000 #in [kN]
  FT.Force.height <- Wind_Force_height2(wind, Cd = Cd_cyl, D = tower.diam, u.elev = floater.freeboard)/1000 #in [kN]
  
  #blade spinning component
  B.Force.best.spinning <- Wind_Blade_Force_spinning(wind) #in kN. returns 0 when blades are static
  
  #blade static component
  if(wind < 3 | wind > 25) B.Force.best.static <- Wind_Force_Blade(wind)/1000  else B.Force.best.static <- 0 #in [kN]. returns 0 when blades are spinning
  
  #Blade Total Forces vs. windspeed
  Tot.Wind.Force <- T.Force.best + 
    (3*B.Force.best.static) + 
    B.Force.best.spinning + 
    (3*F.Force.height) + 
    FT.Force.height
  
  return(Tot.Wind.Force)
}

DP.Turbine.Current.Force <- function(current) {
  F.Force.Current.extr <- Current_Force(speed = current, f.area.water,Cd_cyl)/1000 #in kN
  return(Tot.Current.Force)
}

DP.Turbine.Wind.Force(10.59) #in kN
####### Plots

winds <- seq(0,40, 0.1)
wind.forces.eff <- mapply(DP.Turbine.Wind.Force, wind = winds)
wind.forces.stat <- mapply(Wind_Force_Blade, wind = winds)
wind.forces.spin <- mapply(Wind_Blade_Force_spinning, wind = winds)
 
plot(winds, wind.forces.eff, type = 'l', lwd = 1,
     xlab = "Wind Speed [m/s]", ylab = "Wind Force [kN]",
     ylim = c(0,3000),
     xaxs = 'i', yaxs = 'i', las = 1,)
# lines(winds, wind.forces.stat, lty = 2, col = 'blue', lwd = 2)
# lines(winds, wind.forces.spin, lty = 2, col = 'green', lwd = 2)


#########################################
# Check integration of image 3.3 in Gaertner et al. (2020)

x.points <- c(0,0.75,0.82,0.88,0.97,1,0)*blade.length #I Originally did this without multiplying by blade length
y.points <- c(0,10000,11000,11000,8714,0,0)

#Unit square check of area function
# Xs <- c(0,0,1,1)
# Ys <- c(0,1,1,0)

plot(x.points, y.points, pch = 19, type = 'b', ylab = 'Normal Force [N/m]', xlab = "Blade Dist from Root [m]")

one.blade.spinning <- pracma::polyarea(x = x.points, y = y.points)/1000 #kN
one.blade.spinning*3
