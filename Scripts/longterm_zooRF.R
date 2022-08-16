library(caret)
library(party)
library(PerformanceAnalytics) #correlation
library(tidyverse)
library(car) #to calculate the vif for each predictor
library(mgcv) #gam() function
library(gratia) #gam outputs linked with ggplot environment --> nicer default
library(ggplot2) #nice plots
library(pdp) #partial dependency plots --> not working with caret train cforest method!
library(edarf) #partial dependency plots --> not working with caret train cforest method!

#input file
metadata <- read.csv("Data/longterm_metadata.csv", stringsAsFactors = T)
metadata$Date <- as.Date(metadata$Date,"%d/%m/%Y")
names(metadata)

zooRF <- metadata %>% 
  select(Date, Dap_TB, Diaph_TB, Cala_TB, Nau_TB, Cyc_TB, Cerio_TB, Bosm_TB, 
         SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Chl.a, Phyto_H)

zooRF.NA <- zooRF[complete.cases(zooRF),]
str(zooRF.NA) #54obs 
names(zooRF.NA)

#CHECK FOR COVARIATES WITH Pearson coefficient
zooTB.NA <- zooRF.NA %>% 
  select(SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Chl.a, Phyto_H) #8 PREDICTORS
chart.Correlation(zooTB.NA, histogram = TRUE, method = "pearson", pch = 20)
#predictors with r>0.7 --> SS and Chla (try CIF with one or another)

#CIF

DaphTB.ss<- zooRF.NA %>% 
  select(Dap_TB, SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#2/5.247994/0.4383922/4.194996
DaphTB.chla<- zooRF.NA %>% 
  select(Dap_TB, SolRad, wtr_mean, Chl.a, F.TN, F.TP, F.Colour, Phyto_H)
#mtry/RMSE/R2/MAE (ntree = 1000) 
#2/5.286725/0.4267453/4.302426

DiaphTB.ss<- zooRF.NA %>% 
  select(Diaph_TB, SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#13/5.040007/0.5802778/3.665255
DiaphTB.chla<- zooRF.NA %>% 
  select(Diaph_TB, SolRad, wtr_mean, Chl.a, F.TN, F.TP, F.Colour, Phyto_H)
#mtry/RMSE/R2/MAE (ntree = 1000) 
#5/5.082933/0.5532797/3.728773

CerioTB.ss<- zooRF.NA %>% 
  select(Cerio_TB, SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Phyto_H)
#mtry/RMSE/R2/MAE (ntree = 1000) 
#1/0.7747133/0.2040629/0.5296265
CerioTB.chla<- zooRF.NA %>% 
  select(Cerio_TB, SolRad, wtr_mean, Chl.a, F.TN, F.TP, F.Colour, Phyto_H)
#mtry/RMSE/R2/MAE (ntree = 1000) 
#1/0.7824080/0.1491556/0.5592090

BosmTB.ss<- zooRF.NA %>% 
  select(Bosm_TB, SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#1/1.458343/0.3036271/1.111075
BosmTB.chla<- zooRF.NA %>% 
  select(Bosm_TB, SolRad, wtr_mean, Chl.a, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#1/1.427105/ 0.3222565/1.105967

CalaTB.ss<- zooRF.NA %>% 
  select(Cala_TB, SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#1/5.908798/0.2290724/4.466196
CalaTB.chla<- zooRF.NA %>% 
  select(Cala_TB, SolRad, wtr_mean, Chl.a, F.TN, F.TP, F.Colour, Phyto_H)
#mtry/RMSE/R2/MAE (ntree = 1000) 
#1/5.653925/0.2441269/4.520216

CycTB.ss<- zooRF.NA %>% 
  select(Cyc_TB, SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#13/8.409767/0.3187552/5.120123
CycTB.chla<- zooRF.NA %>% 
  select(Cyc_TB, SolRad, wtr_mean, Chl.a, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#13/8.516103/0.3105646/5.221999

NauTB.ss<- zooRF.NA %>% 
  select(Nau_TB, SolRad, wtr_mean, ss, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#2/0.7678258/0.3503480/0.6212429
NauTB.chla<- zooRF.NA %>% 
  select(Nau_TB, SolRad, wtr_mean, Chl.a, F.TN, F.TP, F.Colour, Phyto_H) 
#mtry/RMSE/R2/MAE (ntree = 1000) 
#13/0.7996312/0.2660172/0.5970021


#CIF trials with caret package
set.seed(143) 
fitControl1 <- trainControl(method = 'cv', number=10) #Resampling: Cross-Validated (10 fold) 
dataControl1 <- cforest_control(trace=F, ntree = 1000, mincriterion = 0.95) #I tried ntree = 500/1000/2000/5000 --> all stable and consistent results
grid1 <- expand.grid(mtry =seq(1,14,1))
#Manually change it and write down model outputs;
RF1 <- train(Nau_TB ~., data = NauTB.ss,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)
RF2 <- train(Nau_TB ~., data = NauTB.chla,
             method = 'cforest', metric='RMSE',
             trControl = fitControl1, controls = dataControl1,
             tuneGrid = grid1)

print(RF1) 
  #mtry/RMSE/R2/MAE (ntree = 1000) 
  #...
print(RF2) 
  #mtry/RMSE/R2/MAE (ntree = 1000) 
  #...

#select RF1 or RF2 based on higher r2!

#nice cforest results plot - variance of importance
caret::varImp(RF1)
varimp_cforest <- caret::varImp(RF1)
plot(varimp_cforest, main="... Biomass (% predictor importance)") 

#final CIF with party package
set.seed(143)
#change everytime mtry and name of data and RV
RF1.cforest <- cforest(Nau_TB ~., data = NauTB.ss, controls = cforest_control(trace=F, mtry = 13, ntree = 1000, mincriterion = 0.95))

cforestStats(RF1.cforest) 
caret::varImp(RF1.cforest) #the same as varim()

predictor.varimp <- varimp(RF1.cforest)
predictor.varimp[which(predictor.varimp<0)] <- 0
predictor.varimp <- round(predictor.varimp/sum(predictor.varimp)*100,2) #convert to 100%
print(predictor.varimp)

#PARTIAL DEPENDENCE PLOTS
#function
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


varnames  = colnames(NauTB.ss[2:8]) #change everytime name dataset
pd_df <- partial_dependence(fit = RF1.cforest,
                            vars = varnames,
                            data = NauTB.ss)
perfectPartialPlot(df = pd_df, x = varnames, y = "Nau_TB") #change everytime name y "..."
