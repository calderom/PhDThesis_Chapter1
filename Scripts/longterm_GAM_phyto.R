library(mgcv) #gam() function
library(gratia) #gam outputs linked with ggplot environment --> nicer default
library(tidyverse) #manipulate datasets

#TEMPORAL CORRELATION STRUCTURES
correlation = corAR1(form = ~ 1|Year) #or with DOY

#https://www.r-bloggers.com/2014/05/modelling-seasonal-data-with-gams/
correlation = corARMA(form = ~ 1|Year, p = 1) #CHANGE p value or with DOY --> p=1 the same as corAR1

#define other possible correlation structures Zuur2009 - Chapter 6
cs1 <- corARMA(c(0.2), p=1, q=0) 
cs2 <- corARMA(c(0.3,-0.3), p=2, q=0) 
cs3 <- corARMA(c(0.2, 0.3, -0.1), p = 1, q = 2)
cs4 <- corARMA(c(0.2), p=0, q=1) 
cs5 <- corARMA(c(0.2, 0.3, -0.1), p = 2, q = 1) 
cs6 <- corARMA(c(0.2, 0.3), p = 1, q = 1) 

#BE AWARE THAT DOESN'T MAKE SENSE TO COMPARE AIC OF DIFFERENT RV or transformed RV --> best model when assumptions are met!
#CAREFUL WITH REML METHOD WITH gam() --> to compare same gam() with different number of smoother use method ML!
#TO COMPARE AIC between gam() and gamm() use METHOD REML!

#input file
metadata <- read.csv("Data/longterm_metadata.csv", stringsAsFactors = T)
metadata$Date <- as.Date(metadata$Date,"%d/%m/%Y")
names(metadata)

#...................Chla.............................................#
#select data 
chla <- metadata %>% 
  select(Year, DOY, Chl.a)
chla.na <- chla[complete.cases(chla),] #remove NA values
names(chla.na)
str(chla.na) #281 obs

chla.na$lnChl.a <- log(chla.na$Chl.a+1) #had to add +1 because 0 values of chla in some cases which gives NA when ln-transformation

#GAM/M models
mod1.chla <- gam(Chl.a ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 8, bs ='cr')+
                      ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                    data=chla.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.chla) #(DOY)***; s(Year)***; ti()*** --> R-sq = 71.9%
draw(mod1.chla) 
AIC(mod1.chla) #232.9479
#check assumptions
appraise(mod1.chla) #4 outliers
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.chla, type="response")
I1<-!is.na(chla.na$Chl.a)
Efull<-vector(length = length(chla.na$Chl.a))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #slightly temporal correlation

mod2.chla <- gam(lnChl.a ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 8, bs ='cr')+
                   ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                 data=chla.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.chla) #(DOY)***; s(Year)***; ti()*** --> R-sq = 76.4%
draw(mod2.chla) 
AIC(mod2.chla) #232.9479 --> -190.1615 (but not comparable)
#check assumptions
appraise(mod2.chla) #4 outliers --> CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.chla, type="response")
I1<-!is.na(chla.na$lnChl.a)
Efull<-vector(length = length(chla.na$lnChl.a))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #slightly temporal correlation AT LAG 1

mod3.chla <- gamm(lnChl.a ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 14, bs ='cr')+
                   ti(DOY,Year, k=c(12,14), bs=c('cc','cr')),
                  correlation = corAR1(form = ~ 1|Year),
                 data=chla.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod3.chla$gam) #(DOY)***; s(Year)***; ti()* --> R-sq = 76.6%
draw(mod3.chla$gam) 
AIC(mod3.chla$lme) #232.9479 --> -190.1615 --> -156.023
#check assumptions
appraise(mod3.chla$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.chla$lme, type="normalized")
I1<-!is.na(chla.na$lnChl.a)
Efull<-vector(length = length(chla.na$lnChl.a))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!


#...................phyto H................................................#
#select data 
phytoH <- metadata %>% 
  select(Year, DOY, Phyto_H)
phytoH.na <- phytoH[complete.cases(phytoH),] #remove NA values
names(phytoH.na)
str(phytoH.na) #94 obs

#GAM/M models
mod1.H <- gam(Phyto_H ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 8, bs ='cr')+
                   ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                 data=phytoH.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.H) #(DOY)***; ti()*** --> R-sq = 55.6%
draw(mod1.H) 
AIC(mod1.H) #50.209
#check assumptions
appraise(mod1.H) #GOOD
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.H, type="response")
I1<-!is.na(phytoH.na$Phyto_H)
Efull<-vector(length = length(phytoH.na$Phyto_H))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #slightly temporal correlation at lag 5

mod2.H <- gamm(Phyto_H ~
                    s(DOY, fx=FALSE, k=12, bs='cc')+
                    s(Year, fx = FALSE, k = 8, bs ='cr')+
                    ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
               correlation = corARMA(form = ~ 1|Year, p = 5), 
                  data=phytoH.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.H$gam) #(DOY)***; ti()*** --> R-sq = 54.4%
draw(mod2.H$gam) 
AIC(mod2.H$lme) #50.209 --> 83.766 (non comparable)
#check assumptions
appraise(mod2.H$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.H$lme, type="normalized")
I1<-!is.na(phytoH.na$Phyto_H)
Efull<-vector(length = length(phytoH.na$Phyto_H))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #slightly temporal correlation at lag 5 #NOT CORRECTED!

mod3.H <- gamm(Phyto_H ~
                 s(DOY, fx=FALSE, k=12, bs='cc'),
               correlation = corARMA(form = ~ 1|Year, p = 5), 
               data=phytoH.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod3.H$gam) #(DOY)**--> R-sq = 21.8%
draw(mod3.H$gam) 
AIC(mod3.H$lme) #50.209 --> 83.766 --> 82.95
#check assumptions
appraise(mod3.H$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.H$lme, type="normalized")
I1<-!is.na(phytoH.na$Phyto_H)
Efull<-vector(length = length(phytoH.na$Phyto_H))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!


#...................cyanobacteria relative biovolume.............................................#
#select data 
cyanoRB <- metadata %>% 
  select(Year, DOY, Cyano_RB, Cyano_RA) 
cyanoRB.na <- cyanoRB[complete.cases(cyanoRB),] #remove NA values
names(cyanoRB.na)
str(cyanoRB.na) #94 obs

cyanoRB.na$lnCyano_RB <- log(cyanoRB.na$Cyano_RB+1)
cyanoRB.na$root4.Cyano_RB <- (cyanoRB.na$Cyano_RB)^1/4
#cyanoRB.na$root4.Cyano_RA <- (cyanoRB.na$Cyano_RA)^1/4
summary(cyanoRB.na)

#GAM/M models
mod1.cyanoRB <- gam(lnCyano_RB ~
                s(DOY, fx=FALSE, k=12, bs='cc')+
                s(Year, fx = FALSE, k = 8, bs ='cr')+
                ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
              data=cyanoRB.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.cyanoRB) #(DOY)* --> R-sq = 13%
draw(mod1.cyanoRB) 
AIC(mod1.cyanoRB) #278.33
#check assumptions
appraise(mod1.cyanoRB) #normality good but heterogeneity bad! --> a lot of zeros! --> try with nb() family
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.cyanoRB, type="response")
I1<-!is.na(cyanoRB.na$lnCyano_RB)
Efull<-vector(length = length(cyanoRB.na$lnCyano_RB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation!

mod2.cyanoRB <- gam(lnCyano_RB ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 8, bs ='cr')+
                      ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                    data=cyanoRB.na, method = 'REML', family = nb())
#GAM output 
summary(mod2.cyanoRB) #(DOY)* --> R-sq = 14.2%
draw(mod2.cyanoRB) 
AIC(mod2.cyanoRB) #278.33 --> 244.29
#check assumptions
appraise(mod2.cyanoRB) #IMPROVED, RIGHT?
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.cyanoRB, type="response")
I1<-!is.na(cyanoRB.na$lnCyano_RB)
Efull<-vector(length = length(cyanoRB.na$lnCyano_RB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #CORRECT!

mod2corr.cyanoRB <- gam(lnCyano_RB ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 8, bs ='cr'),
                    data=cyanoRB.na, method = 'REML', family = nb())
#GAM output 
summary(mod2corr.cyanoRB) #R-sq = 6%
draw(mod2corr.cyanoRB) 
AIC(mod2corr.cyanoRB) #no need to go on
#check assumptions
appraise(mod2corr.cyanoRB) #IMPROVED, RIGHT?
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2corr.cyanoRB, type="response")
I1<-!is.na(cyanoRB.na$lnCyano_RB)
Efull<-vector(length = length(cyanoRB.na$lnCyano_RB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #CORRECT!


mod3.cyanoRB <- gam(Cyano_RB ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 8, bs ='cr')+
                      ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                    data=cyanoRB.na, method = 'REML', family = nb())
#GAM output 
summary(mod3.cyanoRB) #(DOY)***; S(year)***; ti()** --> R-sq = 14.1%
draw(mod3.cyanoRB) 
AIC(mod3.cyanoRB) #449.2949 --> 609.0877 (but not comparable)
#check assumptions
appraise(mod3.cyanoRB) #mmmmmm?? 
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.cyanoRB, type="response")
I1<-!is.na(cyanoRB.na$Cyano_RB)
Efull<-vector(length = length(cyanoRB.na$Cyano_RB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #CORRECT!


mod4.cyanoRB <- gam(lnCyano_RB ~
                      s(DOY, fx=FALSE, k=12, bs='cc'),
                    data=cyanoRB.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod4.cyanoRB) #(DOY)* --> R-sq = 4.9%
draw(mod4.cyanoRB) 
AIC(mod4.cyanoRB) #273.3203
#check assumptions
appraise(mod4.cyanoRB) #BETTER!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod4.cyanoRB, type="response")
I1<-!is.na(cyanoRB.na$Cyano_RB)
Efull<-vector(length = length(cyanoRB.na$Cyano_RB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #CORRECT!

