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

#...................TN LOADINGS (KNOTS=11).............................................#
#select data 
tn.loads <- metadata %>% 
  select(Year, DOY, TNload)
tn.loads.na <- tn.loads[complete.cases(tn.loads),] #remove NA values
names(tn.loads.na)
str(tn.loads.na) #115 obs

tn.loads.na$lnTNload <- log(tn.loads.na$TNload)

#GAM/M models
mod1.tn <- gam(TNload ~
                  s(DOY, fx=FALSE, k=12, bs='cc')+
                  s(Year, fx = FALSE, k = 8, bs ='cr')+
                  ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                data=tn.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.tn) #only s(DOY)* --> R-sq = 6.31%
draw(mod1.tn) 
AIC(mod1.tn) #1532.556
#check assumptions
appraise(mod1.tn) #REALLY BAD!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.tn, type="response")
I1<-!is.na(tn.loads.na$TNload)
Efull<-vector(length = length(tn.loads.na$TNload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


mod2.tn <- gam(lnTNload ~
                 s(DOY, fx=FALSE, k=12, bs='cc')+
                 s(Year, fx = FALSE, k = 8, bs ='cr')+
                 ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
               data=tn.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.tn) #s(DOY)* + s(Year)*--> R-sq = 11.2%
draw(mod2.tn) 
AIC(mod2.tn)#1532.556 --> 367.6133
#check assumptions
appraise(mod2.tn) #CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.tn, type="response")
I1<-!is.na(tn.loads.na$lnTNload)
Efull<-vector(length = length(tn.loads.na$lnTNload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


mod3.tn <- gam(lnTNload ~
                 s(DOY, fx=FALSE, k=12, bs='cc')+
                 s(Year, fx = FALSE, k = 11, bs ='cr'),
               data=tn.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod3.tn) #s(DOY)* + s(Year)*--> R-sq = 11.2%
draw(mod3.tn) 
AIC(mod3.tn)#1532.556 --> 367.6133 --> 367.6109
#check assumptions
appraise(mod3.tn) #CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.tn, type="response")
I1<-!is.na(tn.loads.na$lnTNload)
Efull<-vector(length = length(tn.loads.na$lnTNload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation

#...................TP LOADINGS (knots = 12).............................................#
#select data 
tp.loads <- metadata %>% 
  select(Year, DOY, TPload)
tp.loads.na <- tp.loads[complete.cases(tp.loads),] #remove NA values
names(tp.loads.na)
str(tp.loads.na) #122 obs

tp.loads.na$lnTPload <- log(tp.loads.na$TPload)

#GAM/M models
mod1.tp <- gam(TPload ~
                 s(DOY, fx=FALSE, k=12, bs='cc')+
                 s(Year, fx = FALSE, k = 8, bs ='cr')+
                 ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
               data=tp.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.tp) #non-significant
draw(mod1.tp) 
AIC(mod1.tp) #830.2483
#check assumptions
appraise(mod1.tp) #REALLY BAD!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.tp, type="response")
I1<-!is.na(tp.loads.na$TPload)
Efull<-vector(length = length(tp.loads.na$TPload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


mod2.tp <- gam(lnTPload ~
                 s(DOY, fx=FALSE, k=12, bs='cc')+
                 s(Year, fx = FALSE, k = 8, bs ='cr')+
                 ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
               data=tp.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.tp) #s(DOY)* --> R-sq = 8.3%
draw(mod2.tp) 
AIC(mod2.tp)##830.2483 --> 407.78
#check assumptions
appraise(mod2.tp) #CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.tp, type="response")
I1<-!is.na(tp.loads.na$TPload)
Efull<-vector(length = length(tp.loads.na$TPload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation

mod3.tp <- gam(lnTPload ~
                 s(DOY, fx=FALSE, k=12, bs='cc'),
               data=tp.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod3.tp) #s(DOY)* --> R-sq = 5.56%
draw(mod3.tp) 
AIC(mod3.tp)##830.2483 --> 407.78 --> 405.6819
#check assumptions
appraise(mod3.tp) #CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.tp, type="response")
I1<-!is.na(tp.loads.na$TPload)
Efull<-vector(length = length(tp.loads.na$TPload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


#...................COLOUR LOADINGS (knots = 11).............................................#
#select data 
col.loads <- metadata %>% 
  select(Year, DOY, COLload)
col.loads.na <- col.loads[complete.cases(col.loads),] #remove NA values
names(col.loads.na)
str(col.loads.na) #471 obs

col.loads.na$lnCOLload <- log(col.loads.na$COLload)

#GAM/M models
mod1.col <- gam(COLload ~
                 s(DOY, fx=FALSE, k=12, bs='cc')+
                 s(Year, fx = FALSE, k = 8, bs ='cr')+
                 ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
               data=col.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.col) #s(DOY)*** --> 9.47%
draw(mod1.col) 
AIC(mod1.col) #11654.54
#check assumptions
appraise(mod1.col) #REALLY BAD!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.col, type="response")
I1<-!is.na(col.loads.na$COLload)
Efull<-vector(length = length(col.loads.na$COLload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation

mod2.col <- gam(lnCOLload ~
                  s(DOY, fx=FALSE, k=12, bs='cc')+
                  s(Year, fx = FALSE, k = 8, bs ='cr')+
                  ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                data=col.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.col) #s(DOY)***; s(Year)**; ti()*** --> 27.3%
draw(mod2.col) 
AIC(mod2.col) #11654.54 --> 1548.613
#check assumptions
appraise(mod2.col) #CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.col, type="response")
I1<-!is.na(col.loads.na$lnCOLload)
Efull<-vector(length = length(col.loads.na$lnCOLload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #slighly temporal correlation at lag1


mod4.col <- gamm(lnCOLload ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 8, bs ='cr')+
                   ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                 correlation = corAR1(form = ~ 1 | Year),
                 data=col.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod4.col$gam) #s(DOY)***--> 16.9%
draw(mod4.col$gam) 
AIC(mod4.col$lme) #11654.54 --> 1548.613 --> 1562.481
#check assumptions
appraise(mod4.col$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod4.col$lme, type="normalized")
I1<-!is.na(col.loads.na$lnCOLload)
Efull<-vector(length = length(col.loads.na$lnCOLload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!

mod5.col <- gamm(lnCOLload ~
                   s(DOY, fx=FALSE, k=12, bs='cc')+
                   s(Year, fx = FALSE, k = 8, bs ='cr')+
                   ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                 correlation = corAR1(form = ~ 1 | DOY),
                 data=col.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod5.col$gam) #s(DOY)***; s(Year)**; ti()*** --> 27.2%
draw(mod5.col$gam) 
AIC(mod5.col$lme) #11654.54 --> 1548.613 --> 1562.481 --> 1590.983
#check assumptions
appraise(mod5.col$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod5.col$lme, type="normalized")
I1<-!is.na(col.loads.na$lnCOLload)
Efull<-vector(length = length(col.loads.na$lnCOLload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #NOT CORRECTED!

mod6.col <- gamm(lnCOLload ~
                   s(DOY, fx=FALSE, k=12, bs='cc'),
                 correlation = corAR1(form = ~ 1 | Year),
                 data=col.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod6.col$gam) #s(DOY)***--> 14.2%
draw(mod6.col$gam) 
AIC(mod6.col$lme) #11654.54 --> 1548.613 --> 1562.481 --> 1555.173
#check assumptions
appraise(mod6.col$gam) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod6.col$lme, type="normalized")
I1<-!is.na(col.loads.na$lnCOLload)
Efull<-vector(length = length(col.loads.na$lnCOLload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #CORRECTED!




#..................TN:TP LOADINGS (KNOTS = 10).............................................#
#select data 
rat.loads <- metadata %>% 
  select(Year, DOY, TN.TPload) #ratios already ln-transformed
rat.loads.na <- rat.loads[complete.cases(rat.loads),] #remove NA values
names(rat.loads.na)
str(rat.loads.na) #113 obs

#GAM/M models
mod1.rat <- gam(TN.TPload ~
                  s(DOY, fx=FALSE, k=12, bs='cc')+
                  s(Year, fx = FALSE, k = 10, bs ='cr')+
                  ti(DOY,Year, k=c(12,10), bs=c('cc','cr')),
                data=rat.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.rat) #s(DOY)***; s(Year)***; ti()* --> 41.1%
draw(mod1.rat) 
AIC(mod1.rat) #82.366
#check assumptions
appraise(mod1.rat) #GOOD!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.rat, type="response")
I1<-!is.na(rat.loads.na$TN.TPload)
Efull<-vector(length = length(rat.loads.na$TN.TPload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


mod2.rat <- gam(TN.TPload ~
                  s(DOY, fx=FALSE, k=12, bs='cc')+
                  s(Year, fx = FALSE, k = 8, bs ='cr'),
                data=rat.loads.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.rat) #s(DOY)***; s(Year)** --> 23.6%
draw(mod2.rat) 
AIC(mod2.rat) #82.366 --> 91.53
#check assumptions
appraise(mod2.rat) #GOOD!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.rat, type="response")
I1<-!is.na(rat.loads.na$TN.TPload)
Efull<-vector(length = length(rat.loads.na$TN.TPload))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation
