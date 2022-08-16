#MERGE ALL LONG TERM DATA TOGETHER AS DAILY

#PACKAGES REQUIRED
library(tidyverse)


#INPUT FILES
A <- read.csv("Data/Weather_2004_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
A$Date <- as.Date(A$Date,"%d/%m/%Y")
str(A)

B <- read.csv("Data/rivers_discharge_2004_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
B$Date <- as.Date(B$Date,"%d/%m/%Y")
str(B)

C <- read.csv("Data/wtr_ss_2004_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
C$Date <- as.Date(C$Date,"%d/%m/%Y")
str(C)

D <- read.csv("Data/GR_TN_2009_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
D$Date <- as.Date(D$Date,"%d/%m/%Y")
str(D)

E <- read.csv("Data/BR_TN_2009_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
E$Date <- as.Date(E$Date,"%d/%m/%Y")
str(E)

G <- read.csv("Data/F_TN_2009_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
G$Date <- as.Date(G$Date,"%d/%m/%Y")
str(G)

H <- read.csv("Data/GR_TP_2009_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
H$Date <- as.Date(H$Date,"%d/%m/%Y")
str(H)

I <- read.csv("Data/BR_TP_2009_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
I$Date <- as.Date(I$Date,"%d/%m/%Y")
str(I)

J <- read.csv("Data/F_TP_2009_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
J$Date <- as.Date(J$Date,"%d/%m/%Y")
str(J)

K <- read.csv("Data/GR_Colour_2010_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
K$Date <- as.Date(K$Date,"%d/%m/%Y")
str(K)

L <- read.csv("Data/BR_Colour_2010_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
L$Date <- as.Date(L$Date,"%d/%m/%Y")
str(L)

M <- read.csv("Data/F_Colour_2009_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
M$Date <- as.Date(M$Date,"%d/%m/%Y")
str(M)

N <- read.csv("Data/Chla_2007_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
N$Date <- as.Date(N$Date,"%d/%m/%Y")
str(N)

O <- read.csv("Data/phyto_2012_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
O$Date <- as.Date(O$Date,"%d/%m/%Y")
str(O)

P <- read.csv("Data/zoo_2004_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
P$Date <- as.Date(P$Date,"%d/%m/%Y")
str(P)

Q <- read.csv("Data/secchi_depth_2004_2020.csv", header = TRUE, sep = ",", stringsAsFactors = T)
Q$Date <- as.Date(Q$Date,"%d/%m/%Y")
str(Q)

fishcounts <- read.csv("Data/newport_daily_fish_counts_2004_2020.csv", header = TRUE, sep = ";", stringsAsFactors = T)
fishcounts$Date <- as.Date(fishcounts$Date,"%d/%m/%Y")
str(fishcounts)

#join DATASETS by DATE = metadatafile
join1 <- full_join(A, B, by = "Date")
join2 <- full_join(join1, C, by = "Date")
join3 <- full_join(join2, D, by = "Date")
join4 <- full_join(join3, E, by = "Date")
join5 <- full_join(join4, G, by = "Date")
join6 <- full_join(join5, H, by = "Date")
join7 <- full_join(join6, I, by = "Date")
join8 <- full_join(join7, J, by = "Date") 
join9 <- full_join(join8, K, by = "Date")
join10 <- full_join(join9, L, by = "Date")
join11 <- full_join(join10, M, by = "Date") 
join12 <- full_join(join11, N, by = "Date")
join13 <- full_join(join12, O, by = "Date")
join14 <- full_join(join13, P, by = "Date") 
join15 <- full_join(join14, Q, by = "Date")
join16 <- full_join(join15, fishcounts, by = "Date")

#ALL = 6210 obs 
names(join16)

#Calculate new variables
#Colour loadings:
#river Q m3/s * 1000 L/m3 * 86400 s/day * mgPt/L / 1000000 = KgPt/day
join16$BR.COLload <- join16$BR_Q*1000*86400*join16$BR.Colour/1000000
join16$GR.COLload <- join16$GR_Q*1000*86400*join16$GR.Colour/1000000
#TN loadings:
#river Q m3/s * 1000 L/m3 * 86400 s/day * mgN/L / 1000000 = KgN/day
join16$BR.TNload <- join16$BR_Q*1000*86400*join16$BR.TN/1000000
join16$GR.TNload <- join16$GR_Q*1000*86400*join16$GR.TN/1000000
#TP loadings:
#river Q m3/s * 1000 L/m3 * 86400 s/day * ugP/L / 1000000000 = KgN/day
join16$BR.TPload <- join16$BR_Q*1000*86400*join16$BR.TP/1000000000
join16$GR.TPload <- join16$GR_Q*1000*86400*join16$GR.TP/1000000000
#Convert Kg/day of nutrients to mol/day to carry out molar ratios
#N = 14g/mol
join16$BR.TNloadmolar <- join16$BR.TNload/0.014
join16$GR.TNloadmolar <- join16$GR.TNload/0.014
#P = 31g/mol
join16$BR.TPloadmolar <- join16$BR.TPload/0.031
join16$GR.TPloadmolar <- join16$GR.TPload/0.031
#sum molar total nutrients inflow
join16$COLload <- join16$BR.COLload+join16$GR.COLload #kg/day
join16$TNload <- join16$BR.TNload+join16$GR.TNload #kg/day
join16$TPload <- join16$BR.TPload+join16$GR.TPload #kg/day
join16$TNloadmolar <- join16$BR.TNloadmolar+join16$GR.TNloadmolar #mol/day
join16$TPloadmolar <- join16$BR.TPloadmolar+join16$GR.TPloadmolar #mol/day
join16$TN.TPload <- log(join16$TNloadmolar/join16$TPloadmolar) #log() = default ln()
#Feeagh TN:TP
join16$F.TNmolar <- join16$F.TN*1000/14 #umol/l
join16$F.TPmolar <- join16$F.TP/31 #umol/l
join16$F_TN.TP <- log(join16$F.TNmolar/join16$F.TPmolar) #log() = default ln()
#Zoo:chla
join16$Zoo.Chla <- log(join16$Zoo_TB+1/(join16$Chl.a+1)*66) #log() = default ln()
#copepodsB:cladoceransB
join16$copB.claB <- log((join16$Cala_TB+join16$Cyc_TB+join16$Nau_TB)/(join16$Dap_TB+join16$Diaph_TB+join16$Cerio_TB+join16$Bosm_TB))
#copepodsA:cladoceransA
join16$copA.claA <- log((join16$Cala_TA+join16$Cyc_TA+join16$Nau_TA)/(join16$Dap_TA+join16$Diaph_TA+join16$Cerio_TA+join16$Bosm_TA))

#add year, month and DOY column from Date
join16[, "Year"] <- format(join16[,"Date"], "%Y")
join16[, "Month"] <- format(join16[,"Date"], "%b")
#join14[, "DOY"] <- (format(join14[,"Date"], "%d"))
join16[, "DOY"] <- as.numeric(yday(join16$Date))

names(join16) #now 137 variables
metadata.summary <- summary(join16)


#save and export values to excel file
write.table(metadata.summary, file="Data/metadataSummary.csv",sep=",",row.names=F)
write.table(join16, file="Data/longterm_metadata.csv", sep=",",row.names=F)





