# Trial using functions from "20210930 Parasitic Losses v Turbines.R"

Cp_computed <- mapply(Cp_Ct, Ct = Ct_T1)

diff <- Cp_T1 - Cp_computed

plot(winds_T1, diff)

Ct_computed <- mapply(Ct_Cp, Cp = Cp_T1)
diff2 <- Ct_T1 - Ct_computed

plot(winds_T1, diff2)


gamma(4.1)


####### Testing Thrustmaster Thrusters
## Given Data
RPM <- c(0,100,150,250,300,350,400,450,500,550,600,650,700,750)
Input_PWR_MW <- c(0,11,38,178,308,489,729,1038,1425,1896,2462,3130,3909,4800)/1000
Deliv_Thrust_kN <- c(0,16,35,98,141,192,251,318,392,475,565,663,769,881)
Diam_m <- 3.8
Nom_PWR_MW <- 4800/1000
Nom_Thrust_kN <- 881

#Function (From ABS)
DP.Power <- function(force.kn, n = n) { #power of one motor
  power <- ((1000*(force.kn/n)/K)^(1.5))/motor.diam
  return(power/1000) #in MW
}

DP.Power.n <- function(force.kn, motor.diam, n) { #power of n motors system
  power <- (n/motor.diam)*(1000*force.kn/(K*n))^1.5
  return(power/1000) #in MW
}


forces <- seq(0,5000, by = 100)
powers.1 <- DP.Power.n(forces,motor.diam = Diam_m, n = 1)
powers.3 <- DP.Power.n(forces,motor.diam = Diam_m, n = 3)
powers.5 <- DP.Power.n(forces,motor.diam = Diam_m, n = 5)

DF_Forces_T <- data.frame(forces,powers.1,powers.3,powers.5)
  
#Plot 2
plot(x = forces, y = powers.1, type = 'l',
     xaxs = 'i',yaxs = 'i', las = 1,
     cex = 1.5, cex.lab = 1.5, cex.axis = 1.5,
     xlim = c(0,Nom_Thrust_kN*2),
     ylim = c(0,Nom_PWR_MW*2),
     ylab = "Thruster System Power Requirement [MW]",
     xlab = "Reaction Force [kN]",
     main = "")
lines(forces, powers.3, col = 'blue')
lines(forces,powers.5,col = 'green')
points(x = Deliv_Thrust_kN, y = Input_PWR_MW, pch = 19)
abline(v = Nom_Thrust_kN, col = 'red')
abline(h = Nom_PWR_MW, col = 'red')

legend("topleft", legend = c("Given Data of Input Power vs Deliv. Thrust",
                             "Functional Representation of Input Power vs Deliv. Thrust (Function from ABS)",
                             "Input Power vs. Deliv. Thrust for 3-Thruster System",
                             "Input Power vs. Deliv. Thrust for 5-Thruster System"),
       col = c('black','black','blue','green'), lty = c(NA,1,1,1), lwd = c(NA,1,2,2),pch = c(19,NA,NA,NA),cex = 1.1)

###############################################################################
###### Sensitivity Analysis. 15 MW turbine.
#Uses codes from "20210622 15 MW Turbine and Functions.R"

# parasitic.power15 <- DP.Power.n(DLC1.3_15, motor.diam = 3.91, n = 6) #baseline
parasitic.power15 <- DP.Power.n(DLC1.3_15, motor.diam = 3.91, n = 16)

## WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale = 10.82 #10.82

## Turn effective power curve into several linear models
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


###############################################################################
###### Sensitivity Analysis. 8 MW turbine.
#Uses codes from "20210714 8 MW Turbine DP.R"

parasitic.power8 <- DP.Power.n(DLC1.3_8, motor.diam = 3.35, n = 15)

#WEIBULL PARAMETERS
P.shape = 2.3 #2.3
P.scale = 10.82#10
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
(gen8)
(parasitic.loss8)

###############################################################################
###### Sensitivity Analysis. 5 MW turbine.
#Uses codes from "20210618 5 MW Turbine DP.R"

######################## Turn effective power curve into several linear models
parasitic.power5 <- DP.Power.n(DLC1.3_5, motor.diam = 3.91, n = 2)

#WEIBULL PARAMETERS
P.shape = 2 #2.3
P.scale = 10.82 #7.95,10,10.82
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

###############################################################################
###### Sensitivity Analysis. 2.5 MW turbine.
#Uses codes from "20210930 Parasitic Losses v Turbines.R"

#Weibull Parameters
P.shape = 2 #2.3
P.scale = 10.82 #7.95,10,10.82

############ Calculate Parasitic losses. What % goes toward thrusters?
parasitic.powerT2 <- DP.Power.n(DP.Vector_T2, motor.diam = 2.18, n = 25)

DF_T2 <- data.frame(WIND,Parasitic_T2 = parasitic.powerT2) %>%
  merge(., DF_power_T2, by.x = "WIND", by.y = "Wind.speed", all = TRUE) %>%
  rename(Parasitic = Parasitic_T2,
         Power = Power_MW) %>%
  transmute(WIND,Parasitic,Ct,Power)

DF_T2[is.na(DF_T2)] <- 0
DF_T2$Effective <- DF_T2$Power - DF_T2$Parasitic
mutate(DF_T2, Effective = Power - Parasitic)

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

(parasitic.loss_T2/gen_T2) 
(gen_T2)
(parasitic.loss_T2)

###############################################################################
###### Sensitivity Analysis. 3.45 MW turbine.
#Uses codes from "20210930 Parasitic Losses v Turbines.R"

#Weibull Parameters
P.shape = 2 #2.3
P.scale = 10.82 #7.95,10,10.82

############ Calculate Parasitic losses. What % goes toward thrusters?
parasitic.powerT1 <- DP.Power.n(DP.Vector_T1, motor.diam = 1.98, n = 16)

DF_T1 <- data.frame(WIND,Parasitic_T1 = parasitic.powerT1) %>%
  merge(., DF_power_T1, by.x = "WIND", by.y = "winds_T1", all = TRUE) %>%
  rename(Parasitic = Parasitic_T1,
         Power = power_T1,
         Cp = Cp_T1,
         Ct = Ct_T1) 

DF_T1[is.na(DF_T1)] <- 0
DF_T1$Effective <- DF_T1$Power - DF_T1$Parasitic
mutate(DF_T1, Effective = Power - Parasitic)

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

(parasitic.loss_T1/gen_T1)
(gen_T1)
(parasitic.loss_T1)

###############################################################################
###### Sensitivity Analysis. 3.3 MW turbine.
#Uses codes from "20210930 Parasitic Losses v Turbines.R"

#Weibull Parameters
P.shape = 2 #2.3
P.scale = 10.82 #7.95,10,10.82

############ Calculate Parasitic losses. What % goes toward thrusters?
parasitic.powerT3 <- DP.Power.n(DP.Vector_T3, motor.diam = 1.98, n = 16) #lower one

DF_T3 <- data.frame(WIND,Parasitic_T3 = parasitic.powerT3) %>%
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

(parasitic.loss_T3/gen_T3)
(gen_T3)
(parasitic.loss_T3)

###############################################################################
###### Sensitivity Analysis. 3.4 MW turbine.
#Uses codes from "20210930 Parasitic Losses v Turbines.R"

#Weibull Parameters
P.shape = 2 #2.3
P.scale = 10.82 #7.95,10,10.82

############ Calculate Parasitic losses. What % goes toward thrusters?
parasitic.powerT4 <- DP.Power.n(DP.Vector_T4, motor.diam = 1.98, n = 16)

DF_T4 <- data.frame(WIND,Parasitic_T4 = parasitic.powerT4) %>%
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

(parasitic.loss_T4/gen_T4)
(gen_T4)
(parasitic.loss_T4)