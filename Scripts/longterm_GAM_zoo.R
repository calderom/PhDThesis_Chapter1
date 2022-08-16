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

#...................Zoo total biomass.............................................#
#select data 
zooTB <- metadata %>% 
  select(Year, DOY, Zoo_TB)
zooTB.na <- zooTB[complete.cases(zooTB),] #remove NA values
names(zooTB.na)
str(zooTB.na) #189 obs

zooTB.na$lnZoo_TB <- log(zooTB.na$Zoo_TB)
summary(zooTB.na)

#GAM/M models
mod1.zooTB <- gam(Zoo_TB ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 8, bs ='cr')+
                   ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                 data=zooTB.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.zooTB) #(DOY)**; s(Year)**; ti()*** --> R-sq = 20.6%
draw(mod1.zooTB) 
AIC(mod1.zooTB) #1713.073
#check assumptions
appraise(mod1.zooTB) #3 outliers
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.zooTB, type="response")
I1<-!is.na(zooTB.na$Zoo_TB)
Efull<-vector(length = length(zooTB.na$Zoo_TB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO TEMPORAL CORRELATION

mod2.zooTB <- gam(lnZoo_TB ~
                    s(DOY, fx=FALSE, k=12, bs='cc')+
                    s(Year, fx = FALSE, k = 8, bs ='cr')+
                    ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                  data=zooTB.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.zooTB) #(DOY)**; s(Year)** --> R-sq = 12.9%
draw(mod2.zooTB) 
AIC(mod2.zooTB) #1713.073 --> 454.4963 (but cant compare)
#check assumptions
appraise(mod2.zooTB) #3 outliers --> CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.zooTB, type="response")
I1<-!is.na(zooTB.na$lnZoo_TB)
Efull<-vector(length = length(zooTB.na$lnZoo_TB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #TEMPORAL CORRELATION!


mod3.zooTB <- gamm(lnZoo_TB ~
                    s(DOY, fx=FALSE, k=12, bs='cc')+
                    s(Year, fx = FALSE, k = 8, bs ='cr')+
                    ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                     correlation = corAR1(form = ~ 1|Year) ,
                  data=zooTB.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod3.zooTB$gam) #(DOY)* --> R-sq = 0.0723%
draw(mod3.zooTB$gam) 
AIC(mod3.zooTB$lme) #1713.073 --> 454.4963 (but cant compare) --> 443.0583
#check assumptions
appraise(mod3.zooTB$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.zooTB$lme, type="normalized")
I1<-!is.na(zooTB.na$lnZoo_TB)
Efull<-vector(length = length(zooTB.na$lnZoo_TB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!

mod4.zooTB <- gamm(lnZoo_TB ~
                     s(DOY, fx=FALSE, k=12, bs='cc'),
                   correlation = corAR1(form = ~ 1|Year) ,
                   data=zooTB.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod4.zooTB$gam) #nothing
draw(mod4.zooTB$gam) 
AIC(mod4.zooTB$lme) #1713.073 --> 454.4963 (but cant compare) --> 443.0583 --> 437.2197
#check assumptions
appraise(mod4.zooTB$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod4.zooTB$lme, type="normalized")
I1<-!is.na(zooTB.na$lnZoo_TB)
Efull<-vector(length = length(zooTB.na$lnZoo_TB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!



#...................Zoo shannon diversity.............................................#
#select data 
zooH <- metadata %>% 
  select(Year, DOY, Zoo_H)
zooH.na <- zooH[complete.cases(zooH),] #remove NA values
names(zooH.na)
str(zooH.na) #189 obs

#GAM/M models
mod1.zooH <- gam(Zoo_H ~
                    s(DOY, fx=FALSE, k=12, bs='cc')+
                    s(Year, fx = FALSE, k = 8, bs ='cr')+
                    ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                  data=zooH.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.zooH) #(DOY)***; s(Year)***; ti()* --> R-sq = 63.8%
draw(mod1.zooH) 
AIC(mod1.zooH) #-85.87511
#check assumptions
appraise(mod1.zooH) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.zooH, type="response")
I1<-!is.na(zooH.na$Zoo_H)
Efull<-vector(length = length(zooH.na$Zoo_H))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #HIGH TEMPORAL CORRELATION


mod2.zooH <- gamm(Zoo_H ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 8, bs ='cr')+
                   ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                  correlation = corAR1(form = ~ 1|Year),
                 data=zooH.na, method = 'REML', family = gaussian())
#GAM output
summary(mod2.zooH$gam) #(DOY)**; s(Year)***--> R-sq = 60.2%
draw(mod2.zooH$gam) 
AIC(mod2.zooH$lme) #-85.87511 --> -104.5229
#check assumptions
appraise(mod2.zooH$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.zooH$lme, type="normalized")
I1<-!is.na(zooH.na$Zoo_H)
Efull<-vector(length = length(zooH.na$Zoo_H))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!

mod3.zooH <- gamm(Zoo_H ~
                    s(DOY, fx=FALSE, k=12, bs='cc')+
                    s(Year, fx = FALSE, k = 17, bs ='cr'),
                  correlation = corAR1(form = ~ 1|Year),
                  data=zooH.na, method = 'REML', family = gaussian())
#GAM output
summary(mod3.zooH$gam) #(DOY)**; s(Year)***--> R-sq = 60.5%
draw(mod3.zooH$gam) 
AIC(mod3.zooH$lme) #-85.87511 --> -104.5229 --> -108.5229
#check assumptions
appraise(mod3.zooH$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.zooH$lme, type="normalized")
I1<-!is.na(zooH.na$Zoo_H)
Efull<-vector(length = length(zooH.na$Zoo_H))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!


#...................Zoo Size diversity.............................................#
#select data 
zooSD <- metadata %>% 
  select(Year, DOY, Zoo_SizeD)
zooSD.na <- zooSD[complete.cases(zooSD),] #remove NA values
names(zooSD.na)
str(zooSD.na) #177 obs


#GAM/M models
mod1.zooSD <- gam(Zoo_SizeD ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 8, bs ='cr')+
                   ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                 data=zooSD.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.zooSD) #(DOY)**; s(Year)***; ti()*** --> R-sq = 39.1%
draw(mod1.zooSD) 
AIC(mod1.zooSD) #182.1626
#check assumptions
appraise(mod1.zooSD) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.zooSD, type="response")
I1<-!is.na(zooSD.na$Zoo_SizeD)
Efull<-vector(length = length(zooSD.na$Zoo_SizeD))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #slightly TEMPORAL CORRELATION

mod2.zooSD <- gamm(Zoo_SizeD ~
                    s(DOY, fx=FALSE, k=12, bs='cc')+
                    s(Year, fx = FALSE, k = 17, bs ='cr')+
                    ti(DOY,Year, k=c(12,17), bs=c('cc','cr')),
                   correlation = corAR1(form = ~ 1|Year),
                  data=zooSD.na, method = 'REML', family = gaussian())
#GAM output
summary(mod2.zooSD$gam) #(DOY)**; s(Year)***; ti()* --> R-sq = 37.4%
draw(mod2.zooSD$gam) 
AIC(mod2.zooSD$lme) #182.1626 --> 190.968
#check assumptions
appraise(mod2.zooSD$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.zooSD$lme, type="normalized")
I1<-!is.na(zooSD.na$Zoo_SizeD)
Efull<-vector(length = length(zooSD.na$Zoo_SizeD))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!
