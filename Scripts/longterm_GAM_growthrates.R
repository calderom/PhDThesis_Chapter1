library(mgcv) #gam() function
library(gratia) #gam outputs linked with ggplot environment --> nicer default
library(tidyverse) #manipulate datasets

#input file
metadata <- read.csv("Data/longterm_metadata.csv", stringsAsFactors = T)
metadata$Date <- as.Date(metadata$Date,"%d/%m/%Y")
names(metadata)

#...................Zoo taxa GR.............................................#
#select data
GR <- metadata %>% 
  select(DOY, Year, Nau_GR, Cala_GR, Cyl_GR, Dap_GR, Diaph_GR, Cerio_GR, Bosm_GR)
#remove NA values
GR.na <- GR[complete.cases(GR),] 
names(GR.na) #9 variables --> ok
str(GR.na) #188 obs --> ok

#Nauplii
mod.NauGR <- gamm(Nau_GR ~
                    s(DOY, k = 12, bs = "cc"),
                  correlation = corAR1(form = ~ 1 | Year),
                  data = GR.na, method = "REML", family = gaussian()) 


#GAM output 
summary(mod.NauGR) #R-sq(adj) = 10.4% s(DOY)***
draw(mod.NauGR) #2 peaks
AIC(mod.NauGR) #-910.9012
#GAMM output 
summary(mod.NauGR$gam) #R-sq(adj) = 10.3% s(DOY)***
draw(mod.NauGR$gam) #2 peaks
AIC(mod.NauGR$lme) #-912.488
#check assumptions
appraise(mod.NauGR) #perfect
appraise(mod.NauGR$gam) #perfect
#GAM:
E1<-residuals(mod.NauGR, type="response")
I1<-!is.na(GR.na$Nau_GR)
Efull<-vector(length = length(GR.na$Nau_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=12, na.action=na.pass) #slightly temporal correlation at lag 1
#GAMM:
E1<-residuals(mod.NauGR$lme, type="normalized")
I1<-!is.na(GR.na$Nau_GR)
Efull<-vector(length = length(GR.na$Nau_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,  lag.max=12, na.action=na.pass) #NO temporal correlation!
#plot
plot(mod.NauGR$gam, shade=F, col="yellow3", lwd=2, xlab="DOY", ylab="Nauplii Growth Rate")


#Calanoids
mod.CalGR <- gamm(Cala_GR ~
                    s(DOY, k = 12, bs = "cc"),
                  correlation = corARMA(form = ~ 1 | Year, p = 2),
                  data = GR.na, method = "REML", family = gaussian()) 


#GAM output 
summary(mod.CalGR) #R-sq(adj) = 4.22% s(DOY)**
draw(mod.CalGR) #1 peaks
AIC(mod.CalGR) #-902.88
#GAMM output 
summary(mod.CalGR$gam) #R-sq(adj) = 4.21% s(DOY)***
draw(mod.CalGR$gam) #1 peaks
AIC(mod.CalGR$lme) #-898.96 --> -905.9486
#check assumptions
appraise(mod.CalGR) #good
appraise(mod.CalGR$gam) #good
#GAM:
E1<-residuals(mod.CalGR, type="response")
I1<-!is.na(GR.na$Cala_GR)
Efull<-vector(length = length(GR.na$Cala_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=12, na.action=na.pass) #slightly temporal correlation at lag 1 and lag 2
#GAMM:
E1<-residuals(mod.CalGR$lme, type="normalized")
I1<-!is.na(GR.na$Cala_GR)
Efull<-vector(length = length(GR.na$Cala_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,  lag.max=12, na.action=na.pass) #NO temporal correlation!
#plot
plot(mod.CalGR$gam, shade=F, col="green3", lwd=2, xlab="DOY", ylab="Calanoid Growth Rate")



#Cyclopoids
mod.CycGR <- gamm(Cyl_GR ~
                    s(DOY, k = 12, bs = "cc"),
                  correlation = corARMA(form = ~ 1 | Year, p = 2),
                  data = GR.na, method = "REML", family = gaussian()) 


#GAM output 
summary(mod.CycGR) #R-sq(adj) = na s(DOY) ns
draw(mod.CycGR) #flat
AIC(mod.CycGR) #-1055.219
#GAMM output 
summary(mod.CycGR$gam) #R-sq(adj) = 1.24% s(DOY)*
draw(mod.CycGR$gam) #1 peaks
AIC(mod.CycGR$lme) #-1055.418 --> -1058.953
#check assumptions
appraise(mod.CycGR) #good
appraise(mod.CycGR$gam) #good
#GAM:
E1<-residuals(mod.CycGR, type="response")
I1<-!is.na(GR.na$Cyl_GR)
Efull<-vector(length = length(GR.na$Cyl_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=12, na.action=na.pass) #slightly temporal correlation at lag 1 and lag 2
#GAMM:
E1<-residuals(mod.CycGR$lme, type="normalized")
I1<-!is.na(GR.na$Cyl_GR)
Efull<-vector(length = length(GR.na$Cyl_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,  lag.max=12, na.action=na.pass) #NO temporal correlation!
#plot
plot(mod.CycGR$gam, shade=F, col="yellow4", lwd=2, xlab="DOY", ylab="Cyclopoid Growth Rate")


#Daphnia
mod.DapGR <- gamm(Dap_GR ~
                     s(DOY, k = 12, bs = "cc"),
                    correlation = corAR1(form = ~ 1 | Year),
                   data = GR.na, method = "REML", family = gaussian()) 
                   

#GAM output 
summary(mod.DapGR) #R-sq(adj) = 17% s(DOY)***
draw(mod.DapGR) #2 peaks
AIC(mod.DapGR) #-1095.531
#GAMM output 
summary(mod.DapGR$gam) #R-sq(adj) = 17.1% s(DOY)***
draw(mod.DapGR$gam) #2 peaks
AIC(mod.DapGR$lme) #-1082.604
#check assumptions
appraise(mod.DapGR) #perfect
appraise(mod.DapGR$gam) #perfect
#GAM:
E1<-residuals(mod.DapGR, type="response")
I1<-!is.na(GR.na$Dap_GR)
Efull<-vector(length = length(GR.na$Dap_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=12, na.action=na.pass) #slightly temporal correlation at lag 1
#GAMM:
E1<-residuals(mod.DapGR$lme, type="normalized")
I1<-!is.na(GR.na$Dap_GR)
Efull<-vector(length = length(GR.na$Dap_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,  lag.max=12, na.action=na.pass) #NO temporal correlation!
#plot
plot(mod.DapGR$gam, shade=F, col="blue", lwd=2, xlab="DOY", ylab="Daphnia Growth Rate")

#Diaphanosoma
mod.DiapGR <- gamm(Diaph_GR ~
                    s(DOY, k = 12, bs = "cc"),
                  correlation = corARMA(form = ~ 1 | Year, p=3),
                  data = GR.na, method = "REML", family = gaussian()) 


#GAM output 
summary(mod.DiapGR) #R-sq(adj) = 42.8% s(DOY)***
draw(mod.DiapGR) #2 peaks
AIC(mod.DiapGR) #-957.2055
#GAMM output 
summary(mod.DiapGR$gam) #R-sq(adj) = 42% s(DOY)***
draw(mod.DiapGR$gam) #2 peaks
AIC(mod.DiapGR$lme) #-927.1775
#check assumptions
appraise(mod.DiapGR) #ok --> maybe 1 outlier but...
appraise(mod.DiapGR$gam) #ok --> maybe 1 outlier but...
#GAM:
E1<-residuals(mod.DiapGR, type="response")
I1<-!is.na(GR.na$Diaph_GR)
Efull<-vector(length = length(GR.na$Diaph_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=12, na.action=na.pass) #slightly temporal correlation at lag 1 and 2
#GAMM:
E1<-residuals(mod.DiapGR$lme, type="normalized")
I1<-!is.na(GR.na$Diaph_GR)
Efull<-vector(length = length(GR.na$Diaph_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,  lag.max=12, na.action=na.pass) #NO temporal correlation!
#plot
plot(mod.DiapGR$gam, shade=F, col="turquoise", lwd=2, xlab="DOY", ylab="Diaphanosoma Growth Rate")

#Ceriodaphnia --> not working... around 4 "outliers" a lot of zeros and temporal correlation at lag 4 that doesn't go...
#will have to remove outliers: https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
install.packages("ggstatsplot")
library(ggstatsplot)
boxplot(GR.na$Cerio_GR)$out

Q <- quantile(GR.na$Cerio_GR, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(GR.na$Cerio_GR)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
Cerio.eliminated <- subset(GR.na, GR.na$Cerio_GR > (Q[1] - 1.5*iqr) & GR.na$Cerio_GR < (Q[2]+1.5*iqr))
names(Cerio.eliminated)
str(Cerio.eliminated) # 152 obs



mod.CerioGR <- gam(Cerio_GR ~
                     s(DOY, k = 12, bs = "cc"),
                   #correlation = corARMA(form = ~ 1 | Year, p=6),
                   data = Cerio.eliminated, method = "REML", family = gaussian()) 


#GAM output 
summary(mod.CerioGR) #R-sq(adj) = 8.59% s(DOY)***
draw(mod.CerioGR) #2 peaks
AIC(mod.CerioGR) #-1316.233
#GAMM output 
summary(mod.CerioGR$gam) #
draw(mod.CerioGR$gam) #
AIC(mod.CerioGR$lme) #
#check assumptions
appraise(mod.CerioGR) #ok
appraise(mod.CerioGR$gam) #
#GAM:
E1<-residuals(mod.CerioGR, type="response")
I1<-!is.na(Cerio.eliminated$Cerio_GR)
Efull<-vector(length = length(Cerio.eliminated$Cerio_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=12, na.action=na.pass) #GOOD!
#GAMM:
E1<-residuals(mod.CerioGR$lme, type="normalized")
I1<-!is.na(Cerio.eliminated$Cerio_GR)
Efull<-vector(length = length(Cerio.eliminated$Cerio_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,  lag.max=12, na.action=na.pass) #!
#plot
plot(mod.CerioGR, shade=F, col="red", lwd=2, xlab="DOY", ylab="Ceriodaphnia Growth Rate")



#Bosmina
mod.BosGR <- gamm(Bosm_GR ~
                    s(DOY, k = 12, bs = "cc"),
                  correlation = corAR1(form = ~ 1 | Year),
                  data = GR.na, method = "REML", family = gaussian()) 


#GAM output 
summary(mod.BosGR) #R-sq(adj) = 11.5% s(DOY)***
draw(mod.BosGR) #2 peaks
AIC(mod.BosGR) #-1056.905
#GAMM output 
summary(mod.BosGR$gam) #R-sq(adj) = 11.2% s(DOY)***
draw(mod.BosGR$gam) #2 peaks
AIC(mod.BosGR$lme) #-1042.154
#check assumptions
appraise(mod.BosGR) #ok --> maybe 2 outliers
appraise(mod.BosGR$gam) #ok --> maybe 2 outliers
#GAM:
E1<-residuals(mod.BosGR, type="response")
I1<-!is.na(GR.na$Bosm_GR)
Efull<-vector(length = length(GR.na$Bosm_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=12, na.action=na.pass) #slightly temporal correlation at lag 1
#GAMM:
E1<-residuals(mod.BosGR$lme, type="normalized")
I1<-!is.na(GR.na$Bosm_GR)
Efull<-vector(length = length(GR.na$Bosm_GR))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,  lag.max=12, na.action=na.pass) #NO temporal correlation!
#plot
plot(mod.BosGR$gam, shade=F, col="grey30", lwd=2, xlab="DOY", ylab="Bosmina Growth Rate")
