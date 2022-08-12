library(caret) #train function method = cforest --> from party package
library(party)
library(PerformanceAnalytics) #correlation
library(tidyverse)
library(car) #to calculate the vif for each predictor
library(mgcv) #gam() function
library(gratia) #gam outputs linked with ggplot environment --> nicer default
library(ggplot2) #nice plots
#install.packages("pdp")
install.packages("ggRandomForests")
install.packages("randomForest")
devtools::install_github("zmjones/edarf", subdir = "pkg")
library(pdp) #partial dependency plots --> not working with caret train cforest method!
library(ggRandomForests)
library(randomForest)
library(edarf)

##set your working directroy tow hat you are using 
setwd("C:/Users/calderom/OneDrive - Dundalk Institute of Technology/LongtermData/2004_2020_datasets")

phytoRF <- read.csv("phyto_RFinput.csv", stringsAsFactors = T)
str(phytoRF)
names(phytoRF)

#CIF.1
chla.airT<- phytoRF %>% 
  select(Chl.a, AirTmax, SolRad, F.Colour, F.TN, F.TP, Zoo_TB)
chla.airT.NA <- chla.airT[complete.cases(chla.airT),] #78obs + 6predictors
str(chla.airT.NA)

set.seed(143) 
#use seed = ... inside trainControl() 
#other trainControl() method could be method = 'oob'
fitControl1 <- trainControl(method = 'cv', number=10) #Resampling: Cross-Validated (10 fold) 
dataControl1 <- cforest_control(trace=F, ntree = 1000, mincriterion = 0.95) #I tried ntree = 500/1000/2000 --> all stable and consistent results
grid1 <- expand.grid(mtry =seq(2,10,1))
RF1 <- train(Chl.a ~., data = chla.airT.NA,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)
print(RF1) #mtry/RMSE/R2/MAE --> 4/0.42/0.61/0.32 (ntree = 1000)
plot(RF1) #RMSE against mtry (randomly selected predictors) --> mtry = 4 --> lowest RMSE

#nice cforest results plot - variance of importance
caret::varImp(RF1)
varimp_cforest <- caret::varImp(RF1)
plot(varimp_cforest, main="CIF Variable Importance")

#CIT.1
set.seed(143) 
fitControl1.1 <- trainControl(method = 'cv', number=10)
#dataControl1.1 <- ctree_control(mincriterion = 0.95)
grid1.1 <- expand.grid(maxdepth = 0, mincriterion = 0.95)
CT1 <- train(Chl.a ~., data = chla.airT.NA,
             method = 'ctree2', metric='RMSE',
             trControl = fitControl1.1, tuneGrid = grid1.1)
print(CT1) #RMSE/R2/MAE --> 0.46/0.52/0.36
#plot ctree - caret
plot(CT1$finalModel) #only AirTmax


#CIF.2
chla.wtr<- phytoRF %>% 
  select(Chl.a, wtr_mean, SolRad, F.Colour, F.TN, F.TP, Zoo_TB)
chla.wtr.NA <- chla.wtr[complete.cases(chla.wtr),] #68obs + 6predictors
str(chla.wtr.NA)

set.seed(143) 
fitControl1 <- trainControl(method = 'cv', number=10) #Resampling: Cross-Validated (10 fold) 
dataControl1 <- cforest_control(trace=F, ntree = 1000, mincriterion = 0.95) #I tried ntree = 500/1000/2000 --> all stable and consistent results
grid1 <- expand.grid(mtry =seq(2,10,1))
RF2 <- train(Chl.a ~., data = chla.wtr.NA,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)
print(RF2) #mtry/RMSE/R2/MAE --> 9/0.47/0.55/0.35 (ntree = 1000)
plot(RF2) #RMSE against mtry (randomly selected predictors) --> mtry = 9 --> lowest RMSE

#nice cforest results plot - variance of importance
caret::varImp(RF2)
varimp_cforest <- caret::varImp(RF2)
plot(varimp_cforest, main="CIF Variable Importance")


#CIF.3
chla.SS <- phytoRF %>% 
  select(Chl.a, SS, SolRad, F.Colour, F.TN, F.TP, Zoo_TB)
chla.SS.NA <- chla.SS[complete.cases(chla.SS),] #68obs + 6predictors
str(chla.SS.NA)

set.seed(143)
fitControl1 <- trainControl(method = 'cv', number=10) #Resampling: Cross-Validated (10 fold) 
dataControl1 <- cforest_control(trace=F, ntree = 1000, mincriterion = 0.95) #I tried ntree = 500/1000/2000 --> all stable and consistent results
dataControl1 <- cforest_unbiased(trace=F, ntree = 1000) #not sure about the difference with cforest_control
grid1 <- expand.grid(mtry =seq(1,6,1))
RF3 <- train(Chl.a ~., data = chla.SS.NA,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)
print(RF3) #mtry/RMSE/R2/MAE --> 5/0.40/0.71/0.32 (ntree = 1000)
plot(RF3) #RMSE against mtry (randomly selected predictors) --> mtry = 5 --> lowest RMSE
#nice cforest results plot - variance of importance
caret::varImp(RF3) #absolute value of <10% changes a bit every time --> different from cforest() party package, why?
varimp_cforest <- caret::varImp(RF3)
plot(varimp_cforest, main="CIF Variable Importance")

#PARTIAL DEPENDENCE PLOTS:
#gg_variable only works with randomForest() function!
RF3.pdp <- randomForest(Chl.a ~., data = chla.SS.NA, ntree = 1000, mtry=5)
print(RF3.pdp)
pdpRF3 <- gg_variable(RF3.pdp)
str(pdpRF3)

plot(pdpRF3, xvar=c("SS","SolRad","F.Colour","F.TN","F.TP","Zoo_TB"), 
     panel=TRUE) 

#edarf PACKAGE https://www.r-bloggers.com/2019/08/in-search-of-the-perfect-partial-plot/
#cforest with party package: https://rdrr.io/cran/party/man/cforest.html 
#https://stats.stackexchange.com/questions/218300/explanation-of-different-testtype-and-teststats-in-ctree-in-party-package-of-r
set.seed(143)
set.seed(250) #with different seeds the "same" varimp for predictors --> model is stable --> GOOD!
?cforest() #cforest_control() cforest_unbiased after Strobl et al. (2007) and is the default 
mtry  = ceiling(sqrt(6)) #TRULS OPTION

RF3.cforest <- cforest(Chl.a ~., data = chla.SS.NA, controls = cforest_unbiased(ntree = 1000, mtry=5))
RF3.cforest <- cforest(Chl.a ~., data = chla.SS.NA, controls = cforest_control(trace=F, mtry = 5, ntree = 1000, mincriterion = 0.95))
RF3.cforest <- cforest(Chl.a ~., data = chla.SS.NA, controls = cforest_control(trace=F, mtry = mtry, ntree = 1000, mincriterion = 0.95))

caret::varImp(RF3.cforest)
cforestStats(RF3.cforest)
?varimp()
?cforestStats()

predictor.varimp <- varimp(RF3.cforest)
predictor.varimp[which(predictor.varimp<0)] <- 0
predictor.varimp <- round(predictor.varimp/sum(predictor.varimp)*100,2)
print(predictor.varimp)

#only the ones that importance >10%

varnames  = colnames(chla.SS.NA[2:7])
pd_df <- partial_dependence(fit = RF3.cforest,
                            vars = varnames,
                            data = chla.SS.NA)
plot_pd(pd_df) #default

perfectPartialPlot <- function(df, x, y){
    # Need string for aes_string()
  centile <- "centile"
    # Save marginal probabilities as separate data frame
  vote_prop <- df %>% 
    select(y) %>% 
    mutate(row = row_number())
    # Gather predictor centiles into a single column and join vote_prop
  pd_tidy <- df %>% 
    select(x) %>% 
    gather(x, key = "predictor", value = "centile") %>% 
    na.omit() %>% 
    mutate(row = row_number()) %>% 
    left_join(vote_prop, by = "row")
  # Create the perfect partial plot
  ggplot(pd_tidy, aes_string(x = centile, y = y)) +
    geom_line(lwd = 1, colour = "black") +
    geom_point(shape = 21, fill = "grey50", colour = "black", size = 2)+
    #geom_smooth(method="loess", se = TRUE, colour = "black")+
    labs(title = "Partial Dependence Plots",
         x = "Predictors",
         y = paste("Prediction for", y)) +
    facet_wrap(~predictor, scale="free_x") + #, scale = "free_x" 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme(text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 12, face="bold.italic"),
          panel.background = element_rect(fill = 'white', colour = 'black'))
  }
perfectPartialPlot(df = pd_df, x = varnames, y = "Chl.a")


#check correlation between variables (do not include co-variates in the analyses)
#Variance inflation factor (VIF) is used to detect the severity of 
#multicollinearity in the ordinary least square (OLS) regression analysis.
#VIF < 1 = no multicollinearity
# 1 < VIF < 5 = moderete collinearity but ok to include both variables
#VIF > 5 = SEVERE correlation between variables

model <- lm(Chl.a ~ SS + SolRad + F.Colour  + 
              F.TN + F.TP + Zoo_TB, data = chla.SS.NA)

vif(model)
vif_values <- vif(model)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
#All predictors with VIF < 5 (GOOD)

#GAMs with %importance predictors > 5% --> CIF.3
mod1.chla.SS <- gam(Chl.a~
                     s(SS, fx=FALSE, bs='cr')+
                     s(SolRad, fx=FALSE, bs='cr'),
                   data=chla.SS.NA, method = 'REML', family = gaussian())
mod2.chla.SS <- gam(Chl.a~
                      s(SS, fx=FALSE, bs='cr'),
                    data=chla.SS.NA, method = 'REML', family = gaussian())

#GAM output 
summary(mod1.chla.SS) #66.2% --> SolRad not significant
summary(mod2.chla.SS) #66.1%
draw(mod2.chla.SS) 
AIC(mod2.chla.SS) #66.55
#check assumptions
appraise(mod2.chla.SS) #ok
#taking into account NA values for correct plot of lags!
E1<-residuals(mod2.chla.SS, type="response")
I1<-!is.na(chla.SS.NA$Chl.a)
Efull<-vector(length = length(chla.SS.NA$Chl.a))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #ok 

#par(mfrow=c(1,2))
plot(mod2.chla.SS, shade=F, col="green", lwd=4, select = 1, xlab="Schmidt stability", ylab="Chl-a")

P.chla.ss <- ggplot(chla.SS.NA, aes(x = SS, y = Chl.a)) +
  geom_smooth(method = "loess", se=TRUE, colour = "green", size = 2)+
  geom_point(size = 3, shape = 21, fill="green") +
  ylab("Chl-a")+
  xlab("Schmidt stability")+
  scale_x_continuous(limits = c(-5, 600), breaks = seq(0, 600, by = 100))+
  theme(text = element_text(size = 12),
        plot.title = element_text(color="black",size = 12, face="bold.italic"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")
P.chla.ss

#CIF.4
cyanoB.phytoB.airT<- phytoRF %>% 
  select(cyanoB.phytoB, AirTmax, SolRad, F.Colour, F.TN, F.TP, Zoo_TB)
cyanoB.phytoB.airT.NA<- cyanoB.phytoB.airT[complete.cases(cyanoB.phytoB.airT),] #74obs + 6predictors
str(cyanoB.phytoB.airT.NA)

set.seed(143) 
fitControl1 <- trainControl(method = 'cv', number=10) #Resampling: Cross-Validated (10 fold) 
dataControl1 <- cforest_control(trace=F, ntree = 1000, mincriterion = 0.95) #I tried ntree = 500/1000/2000 --> all stable and consistent results
grid1 <- expand.grid(mtry =seq(2,10,1))
RF4 <- train(cyanoB.phytoB ~., data = cyanoB.phytoB.airT.NA,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)
print(RF4) #mtry/RMSE/R2/MAE --> 6/0.88/0.29/0.71 (ntree = 1000)
plot(RF4) #RMSE against mtry (randomly selected predictors) --> mtry = 4 --> lowest RMSE

#nice cforest results plot - variance of importance
caret::varImp(RF4)
varimp_cforest <- caret::varImp(RF4)
plot(varimp_cforest, main="CIF Variable Importance")

#CIF.5
cyanoRB.wtr<- phytoRF %>% 
  select(Cyano_RB, wtr_mean, SolRad, F.Colour, F.TN, F.TP, Zoo_TB)
cyanoRB.wtr.NA<- cyanoRB.wtr[complete.cases(cyanoRB.wtr),] #64obs + 6predictors
str(cyanoRB.wtr.NA)

cyanoRB.wtr.NA$lnCyano_RB <- log(cyanoRB.wtr.NA$Cyano_RB+1)
cyanoRB.wtr.NA <- cyanoRB.wtr.NA %>% select(lnCyano_RB, wtr_mean, SolRad, F.Colour, F.TN, F.TP, Zoo_TB)

set.seed(143) 
fitControl1 <- trainControl(method = 'cv', number=10) #Resampling: Cross-Validated (10 fold) 
dataControl1 <- cforest_control(trace=F, ntree = 1000, mincriterion = 0.95) #I tried ntree = 500/1000/2000 --> all stable and consistent results
grid1 <- expand.grid(mtry =seq(1,6,1)) #ask exactly...maximum has to be number of predictors, right?
RF5 <- train(lnCyano_RB ~., data = cyanoRB.wtr.NA,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)
print(RF5) #mtry/RMSE/R2/MAE --> 2/0.91/0.38/0.77 (ntree = 1000)
plot(RF5) #RMSE against mtry (randomly selected predictors) --> mtry = 4 --> lowest RMSE

#nice cforest results plot - variance of importance
caret::varImp(RF5)
varimp_cforest <- caret::varImp(RF5)
plot(varimp_cforest, main="CIF Variable Importance")

set.seed(143) 
RF5.cforest <- cforest(lnCyano_RB ~., data = cyanoRB.wtr.NA, controls = cforest_control(trace=F, mtry = 2, ntree = 1000, mincriterion = 0.95))
cforestStats(RF5.cforest)

predictor.varimp <- varimp(RF5.cforest)
predictor.varimp[which(predictor.varimp<0)] <- 0
predictor.varimp <- round(predictor.varimp/sum(predictor.varimp)*100,2)
print(predictor.varimp)

varnames  = colnames(cyanoRB.wtr.NA[2:7])
#varnames <- as.character(ordered(varnames, levels=c("wtr_mean", "F.TN", "F.Colour","F.TP", "Zoo_TB","SolRad")))
pd_df <- partial_dependence(fit = RF5.cforest,
                            vars = varnames,
                            data = cyanoRB.wtr.NA)

perfectPartialPlot(df = pd_df, x = varnames, y = "lnCyano_RB") #I cannot order the predictors in the function above

?gather()
#CIF.6
cyanoB.phytoB.SS <- phytoRF %>% 
  select(cyanoB.phytoB, SS, SolRad, F.Colour, F.TN, F.TP, Zoo_TB)
cyanoB.phytoB.SS.NA<- cyanoB.phytoB.SS[complete.cases(cyanoB.phytoB.SS),] #64obs + 6predictors
str(cyanoB.phytoB.SS.NA)

set.seed(143) 
fitControl1 <- trainControl(method = 'cv', number=10) #Resampling: Cross-Validated (10 fold) 
dataControl1 <- cforest_control(trace=F, ntree = 1000, mincriterion = 0.95) #I tried ntree = 500/1000/2000 --> all stable and consistent results
grid1 <- expand.grid(mtry =seq(2,10,1))
RF6 <- train(cyanoB.phytoB ~., data = cyanoB.phytoB.SS.NA,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)
print(RF6) #mtry/RMSE/R2/MAE --> 9/0.89/0.29/0.72 (ntree = 1000)
plot(RF6) #RMSE against mtry (randomly selected predictors) --> mtry = 4 --> lowest RMSE

#nice cforest results plot - variance of importance
caret::varImp(RF6)
varimp_cforest <- caret::varImp(RF6)
plot(varimp_cforest, main="CIF Variable Importance")

#GAMs with %importance predictors > 5% --> CIF.5
mod1.cyano.wtr <- gam(cyanoB.phytoB~
                      s(wtr_mean, fx=FALSE, bs='cr')+
                      s(F.TN, fx=FALSE, bs='cr'),
                    data=cyanoB.phytoB.wtr.NA, method = 'REML', family = gaussian())
mod2.cyano.wtr <- gam(cyanoB.phytoB~
                      s(wtr_mean, fx=FALSE, bs='cr')+
                      s(F.TN, fx=FALSE, bs='cr')+
                      ti(wtr_mean,F.TN, bs=c('cr','cr')),
                    data=cyanoB.phytoB.wtr.NA, method = 'REML', family = gaussian())
mod3.cyano.wtr <- gam(cyanoB.phytoB~
                        s(wtr_mean, fx=FALSE, bs='cr'),
                      data=cyanoB.phytoB.wtr.NA, method = 'REML', family = gaussian())
#GAM output 
summary(mod1.cyano.wtr) #16.3% --> TN not significant
summary(mod2.cyano.wtr) #15.9% --> TN and ti not significant
summary(mod3.cyano.wtr) #15.2%
draw(mod3.cyano.wtr)
AIC(mod3.cyano.wtr) #173.29
#check assumptions
appraise(mod3.cyano.wtr) #ok
#taking into account NA values for correct plot of lags!
E1<-residuals(mod3.cyano.wtr, type="response")
I1<-!is.na(cyanoB.phytoB.wtr.NA$cyanoB.phytoB)
Efull<-vector(length = length(cyanoB.phytoB.wtr.NA$cyanoB.phytoB))
Efull<-NA
Efull[I1]<-E1
E1<-Efull
AFC1<-acf(E1,lag.max=20, na.action=na.pass) #ok 

par(mfrow=c(1,1))
plot(mod3.cyano.wtr, shade=F, col="deepskyblue", lwd=4, select = 1, xlab="mean WTR", ylab="CyanoB:TotalB")


P.cyano.wtr <- ggplot(cyanoB.phytoB.wtr.NA, aes(x = wtr_mean, y = cyanoB.phytoB)) +
  geom_smooth(method = "loess", se=TRUE, colour = "deepskyblue", size = 2)+
  geom_point(size = 3, shape = 21, fill="deepskyblue") +
  ylab("ln(cyanobacteria_RB)")+
  xlab("mean WTR")+
  scale_x_continuous(limits = c(3, 16), breaks = seq(3, 15, by = 3))+
  theme(text = element_text(size = 12),
        plot.title = element_text(color="black",size = 12, face="bold.italic"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")
P.cyano.wtr
