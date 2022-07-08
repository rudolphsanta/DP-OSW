# Created by Rudolph Santarromana
# May 2022

setwd("C:/Users/Rudolph/Desktop/Rudolph/0 - CMU - Portugal PhD Files/0 - Research/Wind Energy/Dynamic Positioning")
library("xlsx")
### BEGIN BY RUNNING THE FOLLOWING R CODES:
#20220523 Parasitic Losses v Turbines.R
#20220523 5 MW Turbine DP.R
#20220523 8 MW Turbine DP.R
#20220523 10 MW Turbine DP.R
#20220523 15 MW Turbine DP.R
#Wave Drift Forces.R

#Turbine Types: 
##A=2.5MW; B=3.3MW; C=3.4MW; D=3.45MW; E=5MW; F=8MW; G=10MW; H=15MW
#Wind Speed Classes: 
##C08: shape = 2.3, scale = 10.82
##C14: shape = 2.3, scale = 7.95
##CPT: shape = 2.3, scale = 10

FOM.High08 <- 88 #[$/kW-yr]
FOM.Low08 <- 80
FOM.Mid08 <- mean(c(FOM.High08, FOM.Low08))
FOM.High14 <- 98
FOM.Low14 <- 90
FOM.Mid14 <- mean(c(FOM.High14, FOM.Low14))


OCC.High08 <- 3969 #[$/kW]
OCC.Low08 <- 3630
OCC.Mid08 <- mean(c(OCC.High08, OCC.Low08))
OCC.High14 <- 4717
OCC.Low14 <- 4314
OCC.Mid14 <- mean(c(OCC.High14, OCC.Low14))


A.C08 <- sensitivity_T2(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
A.C14 <- sensitivity_T2(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
A.CPT <- sensitivity_T2(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
write.xlsx(A.C08,file = "Sensitivity Analysis.xlsx", sheetName = "2.5MW - Class 08", append = FALSE)
write.xlsx(A.C14,file = "Sensitivity Analysis.xlsx", sheetName = "2.5MW - Class 14", append = TRUE)
write.xlsx(A.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "2.5MW - Class PT", append = TRUE)

B.C08 <- sensitivity_T3(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
B.C14 <- sensitivity_T3(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
B.CPT <- sensitivity_T3(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
write.xlsx(B.C08,file = "Sensitivity Analysis.xlsx", sheetName = "3.3MW - Class 08", append = TRUE)
write.xlsx(B.C14,file = "Sensitivity Analysis.xlsx", sheetName = "3.3MW - Class 14", append = TRUE)
write.xlsx(B.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "3.3MW - Class PT", append = TRUE)

C.C08 <- sensitivity_T4(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
C.C14 <- sensitivity_T4(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
C.CPT <- sensitivity_T4(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
write.xlsx(C.C08,file = "Sensitivity Analysis.xlsx", sheetName = "3.4MW - Class 08", append = TRUE)
write.xlsx(C.C14,file = "Sensitivity Analysis.xlsx", sheetName = "3.4MW - Class 14", append = TRUE)
write.xlsx(C.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "3.4MW - Class PT", append = TRUE)

D.C08 <- sensitivity_T1(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
D.C14 <- sensitivity_T1(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
D.CPT <- sensitivity_T1(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
write.xlsx(D.C08,file = "Sensitivity Analysis.xlsx", sheetName = "3.45MW - Class 08", append = TRUE)
write.xlsx(D.C14,file = "Sensitivity Analysis.xlsx", sheetName = "3.45MW - Class 14", append = TRUE)
write.xlsx(D.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "3.45MW - Class PT", append = TRUE)

E.C08 <- sensitivity_5(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
E.C14 <- sensitivity_5(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
E.CPT <- sensitivity_5(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
xlsx::write.xlsx(E.C08,file = "Sensitivity Analysis.xlsx", sheetName = "5MW - Class 08", append = TRUE)
xlsx::write.xlsx(E.C14,file = "Sensitivity Analysis.xlsx", sheetName = "5MW - Class 14", append = TRUE)
xlsx::write.xlsx(E.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "5MW - Class PT", append = TRUE)

F.C08 <- sensitivity_8(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
F.C14 <- sensitivity_8(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
F.CPT <- sensitivity_8(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
write.xlsx(F.C08,file = "Sensitivity Analysis.xlsx", sheetName = "8MW - Class 08", append = TRUE)
write.xlsx(F.C14,file = "Sensitivity Analysis.xlsx", sheetName = "8MW - Class 14", append = TRUE)
write.xlsx(F.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "8MW - Class PT", append = TRUE)

G.C08 <- sensitivity_10(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
G.C14 <- sensitivity_10(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
G.CPT <- sensitivity_10(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
write.xlsx(G.C08,file = "Sensitivity Analysis.xlsx", sheetName = "10MW - Class 08", append = TRUE)
write.xlsx(G.C14,file = "Sensitivity Analysis.xlsx", sheetName = "10MW - Class 14", append = TRUE)
write.xlsx(G.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "10MW - Class PT", append = TRUE)


H.C08 <- sensitivity_15(shape = 2.3, scale = 10.82, OCC.High = 3969, OCC.Low = 3630, FOM.High = 88, FOM.Low = 80, disc.r = 0.052, years = 30)
H.C14 <- sensitivity_15(shape = 2.3, scale = 7.95, OCC.High = 4717, OCC.Low = 4314, FOM.High = 98, FOM.Low = 90, disc.r = 0.052, years = 30)
H.CPT <- sensitivity_15(shape = 2.3, scale = 10, OCC.High = OCC.Mid14, OCC.Low = OCC.Mid14, FOM.High = FOM.Mid14, FOM.Low = FOM.Mid14, disc.r = 0.052, years = 30)
write.xlsx(H.C08,file = "Sensitivity Analysis.xlsx", sheetName = "15MW - Class 08", append = TRUE)
write.xlsx(H.C14,file = "Sensitivity Analysis.xlsx", sheetName = "15MW - Class 14", append = TRUE)
write.xlsx(H.CPT,file = "Sensitivity Analysis.xlsx", sheetName = "15MW - Class PT", append = TRUE)
