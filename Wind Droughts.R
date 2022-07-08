library(tidyverse)
library(dplyr)


#15 MW Turbine
Effective.15 <- function(windspeed) { #gives the effective power output in MW
  force <- DLC1.3_15.fun(windspeed) #calculates the environmental forces in kn
  gen <- Power.Curve.15(windspeed) 
  cons <- DP.Power.n(force, motor.diam = 4.1, n = 26) #minimum LCOE configuration. WSC 08
  return(gen-cons)
}

Effective.10 <- function(windspeed) {
  force <- DLC1.3_10.fun(windspeed)
  gen <- Power.Curve.15(windspeed)
  cons <- DP.Power.n(force,motor.diam = 4.1, n = 10)
  return(gen-cons)
}

Effective.8 <- function(windspeed) {
  force <- DLC1.3_8.fun(windspeed) 
  gen <- Power.Curve.8(windspeed)
  cons <- DP.Power.n(force,motor.diam = 4.1, n = 7)
  return(gen-cons)
}

Effective.5 <- function(windspeed) {
  force <- DLC1.3_5.fun(windspeed) 
  gen <- Power.Curve.5(windspeed)
  cons <- DP.Power.n(force,motor.diam = 4.1, n = 7)
  return(gen-cons)
}

Effective.3.3 <- function(windspeed) {
  force <- DLC1.3_3.3.fun(windspeed) 
  gen <- Power.Curve.3.3(windspeed)
  cons <- DP.Power.n(force,motor.diam = 4.1, n = 5)
  return(gen-cons)
}


#Humboldt Call Area
CAH <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Wind Speed Data/Humboldt Call Area 40.952 -124.659/Humboldt_10yrs.csv",
               colClasses = c("wind_speed" = "double")) %>%
  transmute(time,wind_speed) %>%
  mutate(time = as.POSIXct(time, format = "%d-%m-%y %H:%M")) %>% 
  mutate(drought = NA) %>%
  mutate(hours = NA) 

hist(CAH$wind_speed)
  
#Morro Bay Call Area
CAM <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Wind Speed Data/Morro Bay Call Area 35.562 -121.829/Morro Bay_10yrs.csv",
                colClasses = c("wind_speed" = "double")) %>%
  rename(time = ï..time) %>%
  transmute(time,wind_speed) %>%
  mutate(time = as.POSIXct(time, format = "%d-%m-%y %H:%M")) %>% 
  mutate(drought = NA) %>%
  mutate(hours = NA) 

#Windfloat Atlantic Location
POR <- read.csv("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Wind Speed Data/Viana do Castelo 41.695 -9.0686/Viana do Castelo_10yrs.csv",
                colClasses = c("wind_speed" = "double")) %>%
  transmute(time,wind_speed) %>%
  mutate(time = as.POSIXct(time, format = "%d-%m-%y %H:%M")) %>% 
  mutate(drought = NA) %>%
  mutate(hours = NA) 

hist(POR$wind_speed)


Wind.Drought <- function(DF, cut.in, cut.out){
  DF$drought <- as.numeric(DF$wind_speed < cut.in | DF$wind_speed > cut.out)
  DF$hours[1] <- DF$drought[1] #set the first hour value to the drought value
  for(row in 2:dim(DF)[1]) { #for rows 2 to end
    DF$hours[row] <- (DF$hours[row-1] + DF$drought[row])*DF$drought[row] #sums all previous hours, unless the drought value is zero, then it goes to zero for this hour
  }
  return(DF)
}

Wind.Drought.Energy <- function(DF) {
  # DF$eff_3.3 <- c()
  # DF$eff_5 <-c()
  # DF$eff_8 <- c()
  # DF$eff_10 <- c()
  DF$eff_15 <- c()
  
  for(i in 1:dim(DF)[1]){
    DF$eff_15[i] <- Effective.15(DF$wind_speed[i])*DF$drought
  }
}

All.Droughts <- function(DF) {
  tot.droughts <- DF$hours
  list.droughts <- c()
  for(i in 1:length(tot.droughts)){
    this.hour <- tot.droughts[i]
    next.hour <- tot.droughts[i+1]
    if(this.hour == 0) { #if this hour equals zero, we are not in a drought
      next
    } else if(is.na(next.hour)){ #if the next hour is NA, we are at the end of the list
      break
    } else if(next.hour != 0){ #if the next hour is a number, the next hour continues the drought
      next
    } else {
      list.droughts <- c(list.droughts,this.hour) #if the next hour is a zero, we are at the end of this drought
    }
  }
  return(list.droughts)
}

All.Droughts.Energy <- function(DF.V) { #input DF$eff.#
  list.energy <- c()
  cummul <- 0 #the count starts at zero
  for(i in 1:length(DF.V)){
    this.hour <- DF.V[i]
    next.hour <- DF.V[i+1]
    if(this.hour == 0) { #if this hour equals zero, we are not in a drought
      cummul <- 0 #resent the count
      next
    } else if(is.na(next.hour)){ #if the next hour is NA, we are at the end of the list
      list.energy <- c(list.energy,cummul)
      break
    } else { #if this hour is a number, increase the count
      cummul <- cummul + this.hour #increase the count by the amount of energy
    }
    if(next.hour == 0) {#if this is the last hour of the drought, this is the max energy. add it to the list
      list.energy <- c(list.energy,cummul) 
    }
  }
  return(list.energy)
}

CAH.15 <- Wind.Drought(CAH,3,25) #wind droughts for the 15,5,and 3.3 MW turbines
CAM.15 <- Wind.Drought(CAM,3,25) #wind droughts for the 15,5,and 3.3 MW turbines
POR.15 <- Wind.Drought(POR,3,25)
CAH.10 <- Wind.Drought(CAH,4,25) #wind droughts for the 10 and 8 MW turbines
CAM.10 <- Wind.Drought(CAM,4,25) #wind droughts for the 10 and 8 MW turbines
POR.10 <- Wind.Drought(POR,4,25)

A <- All.Droughts(CAH.15) #length of each drought, doesn't count the sub-lengths leading up to the ultimate final length
B <- All.Droughts(CAM.15)
C <- All.Droughts(POR.15)
D <- All.Droughts(CAH.10)
E <- All.Droughts(CAM.10)
G <- All.Droughts(POR.10)

summary(A)
summary(B)
summary(C)

summary(D)
summary(E)
summary(G)

setEPS()
postscript("Figure S5a.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
hist(A, breaks = "FD", xaxs = 'i', yaxs = 'i', 
     xlab = "Length of Wind Drought [hours]",
     ylab = "Frequency",
     main = "")
segments(x0 = mean(A), x1=mean(A), y0 = 0, y1 = 290, col = 'red', lty = 2, lwd = 2)
text(x = mean(A), y = 300, "Mean", col = 'red')
dev.off()

setEPS()
postscript("Figure S5b.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
hist(B, breaks = "FD", xaxs = 'i', yaxs = 'i', 
     xlab = "Length of Wind Drought [hours]",
     ylab = "Frequency",
     main = "")
segments(x0 = mean(B), x1=mean(B), y0 = 0, y1 = 240, col = 'red', lty = 2, lwd = 2)
text(x = mean(B), y = 250, "Mean", col = 'red')
dev.off()


setEPS()
postscript("Figure S5c.eps", width = 7.5, height = 5)
par(mar = c(5,7,2,2))
hist(C, breaks = 50,xaxs = 'i', yaxs = 'i', 
     xlab = "Length of Wind Drought [hours]",
     ylab = "Frequency",
     main = "")
segments(x0 = mean(C), x1=mean(C), y0 = 0, y1 = 275, col = 'red', lty = 2, lwd = 2)
text(x = mean(C), y = 280, "Mean", col = 'red')
dev.off()

plot(x = CAH.15$time, y = CAH.15$hours, pch = 19, xaxs = 'i', yaxs = 'i',
     xlab = "", ylab = "Hours")
abline(h = mean(A), col = 'red', lwd = 2)

plot(x = CAM.15$time, y = CAM.15$hours, pch = 19, xaxs = 'i', yaxs = 'i',
     xlab = "", ylab = "Hours")
abline(h = mean(B), col = 'red', lwd = 2)

plot(x = POR.15$time, y = POR.15$hours, pch = 19, xaxs = 'i', yaxs = 'i',
     xlab = "", ylab = "Hours")
abline(h = mean(C), col = 'red', lwd = 2)

CAH.15$eff.15 <- NA
for(i in 1:dim(CAH.15)[1]) {
  wind_speed <- CAH.15$wind_speed[i]
  drought <- CAH.15$drought[i]
  CAH.15$eff.15[i] <- Effective.15(wind_speed)*drought
}

CAH.15$eff.5 <- NA
for(i in 1:dim(CAH.15)[1]) {
  wind_speed <- CAH.15$wind_speed[i]
  drought <- CAH.15$drought[i]
  CAH.15$eff.5[i] <- Effective.5(wind_speed)*drought
}

CAH.15$eff.3.3 <- NA
for(i in 1:dim(CAH.15)[1]) {
  wind_speed <- CAH.15$wind_speed[i]
  drought <- CAH.15$drought[i]
  CAH.15$eff.3.3[i] <- Effective.3.3(wind_speed)*drought
}



CAM.15$eff.15 <- NA
for(i in 1:dim(CAM.15)[1]) {
  wind_speed <- CAM.15$wind_speed[i]
  drought <- CAM.15$drought[i]
  CAM.15$eff.15[i] <- Effective.15(wind_speed)*drought
}

CAH.10$eff.8 <- NA
for(i in 1:dim(CAH.10)[1]) {
  wind_speed <- CAH.10$wind_speed[i]
  drought <- CAH.10$drought[i]
  CAH.10$eff.8[i] <- Effective.8(wind_speed)*drought
}

CAH.10$eff.10 <- NA
for(i in 1:dim(CAH.10)[1]) {
  wind_speed <- CAH.10$wind_speed[i]
  drought <- CAH.10$drought[i]
  CAH.10$eff.10[i] <- Effective.10(wind_speed)*drought
}
 
E <- All.Droughts.Energy(CAH.15$eff.15)
G <- All.Droughts.Energy(CAM.15$eff.15)
H <- All.Droughts.Energy(CAH.15$eff.5)
Z <- All.Droughts.Energy(CAH.15$eff.3.3)

min(H) #1MWh
min(Z) #1 MWh
min(E)

J <- All.Droughts.Energy(CAH.10$eff.10)
L <- All.Droughts.Energy(CAH.10$eff.8)

min(J)
min(L)

head(CAH.15,100)






ann.energy.PT <- sum(PT$effective_15[PT$effective_15 < 0]) #2000 MWh of net negative energy
power.PT <- min(PT$effective_15) #1.06 MW of power needed at most
ann.energy.CA <- sum(CA$effective_15[CA$effective_15 < 0]) #1664 MWh of net negative energy
power.CA <- min(CA$effective_15) #1.06 MW of power needed at most

PT[PT$effective_15 == min(PT$effective_15),]
PT[PT$wind_speed == max(PT$wind_speed),] #NOTE, the wind never reaches cut-out wind speed

CA[CA$effective_15 == min(CA$effective_15),]
CA[CA$wind_speed == max(CA$wind_speed),] #NOTE, the wind never reaches cut-out wind speed

#total energy consumed during longest wind drought in portugal
if(PT$drought.15[1] == 1) PT$hours.15[1] <- 1 else PT$hours.15[1] <- 0
if(PT$drought.15[1] == 1) PT$cumulative.15[1] <- PT$effective_15[1] else PT$cumulative.15[1] <- 0
for(i in 2:dim(PT)[1]) {
  if(PT$drought.15[i] == 1) {
    PT$hours.15[i] <- PT$hours.15[i-1] + 1
    PT$cumulative.15[i] <- PT$cumulative.15[i-1] + PT$effective_15[i]
  } else {
    PT$hours.15[i] <- 0
    PT$cumulative.15[i] <- 0
  }
}
max(PT$hours.15) #64 hours was longest drought
min(PT$cumulative.15) #wind droughts in a row equal 56 MWh


summary(PT$cumulative.15)

#total energy consumed during longest wind drought in california
if(CA$drought.15[1] == 1) CA$hours.15[1] <- 1 else CA$hours.15[1] <- 0
if(CA$drought.15[1] == 1) CA$cumulative.15[1] <- CA$effective_15[1] else CA$cumulative.15[1] <- 0
for(i in 2:dim(CA)[1]) {
  if(CA$drought.15[i] == 1) {
    CA$hours.15[i] <- CA$hours.15[i-1] + 1
    CA$cumulative.15[i] <- CA$cumulative.15[i-1] + CA$effective_15[i]
  } else {
    CA$hours.15[i] <- 0
    CA$cumulative.15[i] <- 0
  }
}
max(CA$hours.15) #46 hours was longest length of wind drought
min(CA$cumulative.15) #wind droughts in a row equal 38 MWh


