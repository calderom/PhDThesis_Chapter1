library(tidyverse)

##set your working directory
setwd("C:/Users/calderom/OneDrive - Dundalk Institute of Technology/LongtermData/2004_2020_datasets")

#INPUT FILES
A <- read.csv("Weather_2004to2020.csv", stringsAsFactors = T)
A$Date <- as.Date(A$Date,"%d/%m/%Y")
str(A)

B <- read.csv("RiversDischarge_2004to2020.csv", stringsAsFactors = T)
B$Date <- as.Date(B$Date,"%d/%m/%Y")
str(B)

C <- read.csv("WTR&SS_2004to2019.csv", stringsAsFactors = T)
C$Date <- as.Date(C$Date,"%d/%m/%Y")
str(C)

D <- read.csv("GR_TN_2009to2020.csv", stringsAsFactors = T)
D$Date <- as.Date(D$Date,"%d/%m/%Y")
str(D)

E <- read.csv("BR_TN_2009to2020.csv", stringsAsFactors = T)
E$Date <- as.Date(E$Date,"%d/%m/%Y")
str(E)

G <- read.csv("F_TN_2009to2020.csv", stringsAsFactors = T)
G$Date <- as.Date(G$Date,"%d/%m/%Y")
str(G)

H <- read.csv("GR_TP_2009to2020.csv", stringsAsFactors = T)
H$Date <- as.Date(H$Date,"%d/%m/%Y")
str(H)

I <- read.csv("BR_TP_2009to2020.csv", stringsAsFactors = T)
I$Date <- as.Date(I$Date,"%d/%m/%Y")
str(I)

J <- read.csv("F_TP_2009to2020.csv", stringsAsFactors = T)
J$Date <- as.Date(J$Date,"%d/%m/%Y")
str(J)

K <- read.csv("GR_Colour_2010to2020.csv", stringsAsFactors = T)
K$Date <- as.Date(K$Date,"%d/%m/%Y")
str(K)

L <- read.csv("BR_Colour_2010to2020.csv", stringsAsFactors = T)
L$Date <- as.Date(L$Date,"%d/%m/%Y")
str(L)

M <- read.csv("F_Colour_2009to2020.csv", stringsAsFactors = T)
M$Date <- as.Date(M$Date,"%d/%m/%Y")
str(M)

N <- read.csv("Chla_2007to2020.csv", stringsAsFactors = T)
N$Date <- as.Date(N$Date,"%d/%m/%Y")
str(N)

O <- read.csv("Phyto_2008to2020.csv", stringsAsFactors = T)
O$Date <- as.Date(O$Date,"%d/%m/%Y")
str(O)

P <- read.csv("Zoo_2004to2020.csv", stringsAsFactors = T)
P$Date <- as.Date(P$Date,"%d/%m/%Y")
str(P)

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
join14 <- full_join(join13, P, by = "Date") #ALL = 6210 obs + 104 variables
names(join14)

#Calculate new variables
#Colour loadings:
#river Q m3/s * 1000 L/m3 * 86400 s/day * mgPt/L / 1000000 = KgPt/day
join14$BR.COLload <- join14$BR_Q*1000*86400*join14$BR.Colour/1000000
join14$GR.COLload <- join14$GR_Q*1000*86400*join14$GR.Colour/1000000
#TN loadings:
#river Q m3/s * 1000 L/m3 * 86400 s/day * mgN/L / 1000000 = KgN/day
join14$BR.TNload <- join14$BR_Q*1000*86400*join14$BR.TN/1000000
join14$GR.TNload <- join14$GR_Q*1000*86400*join14$GR.TN/1000000
#TP loadings:
#river Q m3/s * 1000 L/m3 * 86400 s/day * ugP/L / 1000000000 = KgN/day
join14$BR.TPload <- join14$BR_Q*1000*86400*join14$BR.TP/1000000000
join14$GR.TPload <- join14$GR_Q*1000*86400*join14$GR.TP/1000000000
#Convert Kg/day of nutrients to mol/day to carry out molar ratios
#N = 14g/mol
join14$BR.TNloadmolar <- join14$BR.TNload/0.014
join14$GR.TNloadmolar <- join14$GR.TNload/0.014
#P = 31g/mol
join14$BR.TPloadmolar <- join14$BR.TPload/0.031
join14$GR.TPloadmolar <- join14$GR.TPload/0.031
#sum molar total nutrients inflow
join14$COLload <- join14$BR.COLload+join14$GR.COLload #kg/day
join14$TNload <- join14$BR.TNload+join14$GR.TNload #kg/day
join14$TPload <- join14$BR.TPload+join14$GR.TPload #kg/day
join14$TNloadmolar <- join14$BR.TNloadmolar+join14$GR.TNloadmolar #mol/day
join14$TPloadmolar <- join14$BR.TPloadmolar+join14$GR.TPloadmolar #mol/day
join14$TN.TPload <- log(join14$TNloadmolar/join14$TPloadmolar) #log() = default ln()
#Feeagh TN:TP
join14$F.TNmolar <- join14$F.TN*1000/14 #umol/l
join14$F.TPmolar <- join14$F.TP/31 #umol/l
join14$F_TN.TP <- log(join14$F.TNmolar/join14$F.TPmolar) #log() = default ln()
#Zoo:chla
join14$Zoo.Chla <- log(join14$Zoo_TB+1/(join14$Chl.a+1)*66) #log() = default ln()
#cyanoB:phytoB
join14$cyanoB.phytoB <- log(join14$Cyano_TB+1/join14$Phyto_TB+1) #log() = default ln()
#cyamoA:phytoA
join14$cyanoA.phytoA <- log(join14$Cyano_TA+1/join14$Phyto_TA+1) #log() = default ln()
#copepodsB:cladoceransB
join14$copB.claB <- log((join14$Cala_TB+join14$Cyc_TB+join14$Nau_TB)/(join14$Dap_TB+join14$Diaph_TB+join14$Cerio_TB+join14$Bosm_TB))
#copepodsA:cladoceransA
join14$copA.claA <- log((join14$Cala_TA+join14$Cyc_TA+join14$Nau_TA)/(join14$Dap_TA+join14$Diaph_TA+join14$Cerio_TA+join14$Bosm_TA))

#add year, month and DOY column from Date
join14[, "Year"] <- format(join14[,"Date"], "%Y")
join14[, "Month"] <- format(join14[,"Date"], "%b")
#join14[, "DOY"] <- (format(join14[,"Date"], "%d"))
join14[, "DOY"] <- as.numeric(yday(join14$Date))

names(join14)
metadata.summary <- summary(join14)

write.table(metadata.summary, file="metadataSummary.csv",sep=",",row.names=F)

#Export to .csv
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-10000",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(join14) #named/saved as dailymetadata_2004to2020.csv
write.excel(meanwtr) #from longterm_wtr&SS script

