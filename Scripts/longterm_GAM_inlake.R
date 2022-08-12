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

#...................in-lake TN (knots = 12).............................................#
#select data 
tn <- metadata %>% 
  select(Year, DOY, F.TN)
tn.na <- tn[complete.cases(tn),] #remove NA values
names(tn.na)
str(tn.na) #132 obs


#GAM/M models
mod1.tn.lake <- gam(F.TN ~
                 s(DOY, fx=FALSE, k=12, bs='cc')+
                 s(Year, fx = FALSE, k = 8, bs ='cr')+
                 ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
               data=tn.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.tn.lake) #(DOY)***; s(Year)*** --> R-sq = 78%
draw(mod1.tn.lake) 
AIC(mod1.tn.lake) #-349.716
#check assumptions
appraise(mod1.tn.lake) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.tn.lake, type="response")
I1<-!is.na(tn.na$F.TN)
Efull<-vector(length = length(tn.na$F.TN))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation

mod2.tn.lake <- gam(F.TN ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 12, bs ='cr'),
                    data=tn.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.tn.lake) #(DOY)***; s(Year)*** --> R-sq = 79.4%
draw(mod2.tn.lake) 
AIC(mod2.tn.lake) #-349.716 --> -350.667
#check assumptions
appraise(mod2.tn.lake) #OK
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.tn.lake, type="response")
I1<-!is.na(tn.na$F.TN)
Efull<-vector(length = length(tn.na$F.TN))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation

#...................in-lake TP (knots = 12).............................................#
#select data 
tp <- metadata %>% 
  select(Year, DOY, F.TP)
tp.na <- tp[complete.cases(tp),] #remove NA values
names(tp.na)
str(tp.na) #131 obs

tp.na$lnF.TP <- log(tp.na$F.TP)

#GAM/M models
mod1.tp.lake <- gam(F.TP ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 8, bs ='cr')+
                      ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                    data=tp.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.tp.lake) #(DOY)***; s(Year)*** --> R-sq = 19%
draw(mod1.tp.lake) 
AIC(mod1.tp.lake) #535.768
#check assumptions
appraise(mod1.tp.lake) # 1 OUTLIER!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.tp.lake, type="response")
I1<-!is.na(tp.na$F.TP)
Efull<-vector(length = length(tp.na$F.TP))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


mod2.tp.lake <- gam(lnF.TP ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 11, bs ='cr')+
                      ti(DOY,Year, k=c(12,11), bs=c('cc','cr')),
                    data=tp.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.tp.lake) #(DOY)***; s(Year)*** --> R-sq = 26.4%
draw(mod2.tp.lake) 
AIC(mod2.tp.lake) #535.768 --> 7.25
#check assumptions
appraise(mod2.tp.lake) # 1 OUTLIER! --> CORRECTED
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.tp.lake, type="response")
I1<-!is.na(tp.na$lnF.TP)
Efull<-vector(length = length(tp.na$lnF.TP))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


mod3.tp.lake <- gam(lnF.TP ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 12, bs ='cr'),
                    data=tp.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod3.tp.lake) #(DOY)***; s(Year)*** --> R-sq = 24.6%
draw(mod3.tp.lake) 
AIC(mod3.tp.lake) #535.768 --> 7.25 --> 5.648
#check assumptions
appraise(mod3.tp.lake) # 1 OUTLIER! --> CORRECTED
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.tp.lake, type="response")
I1<-!is.na(tp.na$lnF.TP)
Efull<-vector(length = length(tp.na$lnF.TP))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


#...................in-lake colour (knots = 12).............................................#
#select data 
col.lake <- metadata %>% 
  select(Year, DOY,F.Colour)
col.lake.na <- col.lake[complete.cases(col.lake),] #remove NA values
names(col.lake.na)
str(col.lake.na) #192 obs

col.lake.na$lnF.Colour <- log(col.lake.na$F.Colour)

#GAM/M models
mod1.col.lake <- gam(F.Colour ~
                      s(DOY, fx=FALSE, k=12, bs='cc')+
                      s(Year, fx = FALSE, k = 8, bs ='cr')+
                      ti(DOY,Year, k=c(12,8), bs=c('cc','cr')),
                    data=col.lake.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.col.lake) #(DOY)***; s(Year)***; ti()*** --> R-sq = 49%
draw(mod1.col.lake) 
AIC(mod1.col.lake) #1583.814
#check assumptions
appraise(mod1.col.lake) # 3 outliers
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.col.lake, type="response")
I1<-!is.na(col.lake.na$F.Colour)
Efull<-vector(length = length(col.lake.na$F.Colour))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation

mod2.col.lake <- gam(lnF.Colour ~
                       s(DOY, fx=FALSE, k=12, bs='cc')+
                       s(Year, fx = FALSE, k = 12, bs ='cr')+
                       ti(DOY,Year, k=c(12,12), bs=c('cc','cr')),
                     data=col.lake.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod2.col.lake) #(DOY)***; s(Year)***; ti()* --> R-sq = 52.9%
draw(mod2.col.lake) 
AIC(mod2.col.lake) #1583.814 --> -128.609 (but not comparable)
#check assumptions
appraise(mod2.col.lake) # 3 outliers --> CORRECTED!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.col.lake, type="response")
I1<-!is.na(col.lake.na$lnF.Colour)
Efull<-vector(length = length(col.lake.na$lnF.Colour))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation


#...................in-lake TN:TP (knots = 12 but max 11 because interaction term).............................................#
#select data 
rat.lake <- metadata %>% 
  select(Year, DOY,F_TN.TP) #RATIOS already ln-transformed
rat.lake.na <- rat.lake[complete.cases(rat.lake),] #remove NA values
names(rat.lake.na)
str(rat.lake.na) #130 obs


#GAM/M models
mod1.rat.lake <- gam(F_TN.TP ~
                       s(DOY, fx=FALSE, k=12, bs='cc')+
                       s(Year, fx = FALSE, k = 11, bs ='cr')+
                       ti(DOY,Year, k=c(12,11), bs=c('cc','cr')),
                     data=rat.lake.na, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.rat.lake) #(DOY)*; s(Year)***; ti()** --> R-sq = 78.8%
draw(mod1.rat.lake) 
AIC(mod1.rat.lake) #63.203
#check assumptions
appraise(mod1.rat.lake) #GOOD!
#taking into account NA values for correct plot of lags!
E1<-residuals(mod1.rat.lake, type="response")
I1<-!is.na(rat.lake.na$F_TN.TP)
Efull<-vector(length = length(rat.lake.na$F_TN.TP))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1, lag.max=20, na.action=na.pass) #NO temporal correlation

