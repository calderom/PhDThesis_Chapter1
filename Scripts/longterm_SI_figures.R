library(vegan) #NMDS + diversity
library(dplyr) #manipulate datasets
library(ggplot2) #nice plots
library (TimeSeries) #for timeseries
library(lubridate) #for timeseries
library(scales) #labels and axis
library(tidyverse) #manipulate datasets
library(cowplot) #nice figures
library(gridExtra) #to plot more than 1 plot in the same figure
library(zoo) #for timeseries
library(gridGraphics) #to plot more than 1 plot in the same figure
library(PerformanceAnalytics) #correlation
library(mgcv) #gam() function
library(gratia) #gam outputs linked with ggplot environment --> nicer default
#density plots
library(hrbrthemes) #exporting
library(viridis) #colour palete
library(MARSS) #DFA trials


#input file
metadata <- read.csv("Data/longterm_metadata.csv", stringsAsFactors = T)
metadata$Date <- as.Date(metadata$Date,"%Y-%m-%d") #careful here with format of Date!
str(metadata)
names(metadata)

#RIVER DISCHARGES
Q <- read.csv("Data/rivers_discharge_2004_2020.csv", stringsAsFactors = T)
Q$Date <- as.Date(Q$Date,"%d/%m/%Y")
str(Q)

RiversQ <- ggplot() +
  geom_line(data = Q, aes(x=Date, y = BR_Q), colour ="black", size = 0.5) +
  geom_line(data = Q, aes(x=Date, y =  GR_Q), colour ="red", size = 0.5) +
  ylab(~paste("Rivers daily discharge ", "(m"^3, "·s"^-1, ")")) +
  ylim(0,100)+
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank())
RiversQ

ggsave(path = "Figures", filename ="FigureS2.jpeg", width = 15, height = 8, units = "cm")


#Phytoplankton community structure

#PHYTO GROUPS COLOURS
#Ciliates = deeppink
#Cyano = turquoise1
#Diatoms = grey50
#Chloro = chartreuse
#Crypto = chocolate4
#Chryso = khaki3
#Hapto = red
#Dino = darkorchid3

#barplots for each year of absolute ABUNDANCES (Figure S4) and BIOVOLUMES (Figure S5)

#pivot dataset for taxa specific total ABUNDANCE............................................................
phyto_RA <- metadata %>% 
  select(Date, Year, Ciliates_TA, Cyano_TA, Diatoms_TA, Chloro_TA, Crypto_TA, Chryso_TA, Hapto_TA, Dino_TA)
str(phyto_RA)

#create a dataframe for each year and pivot to plot
#2012
phyto_RA_2012 <-phyto_RA[phyto_RA$Year %in% "2012",]
phyto_RA_2012 <- phyto_RA_2012 %>% 
  select(-Year)
pivot_phyto_RA_2012 <- phyto_RA_2012 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2012$value <- as.numeric(pivot_phyto_RA_2012$value)
pivot_phyto_RA_2012$variable <- ordered(pivot_phyto_RA_2012$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))


#2013
phyto_RA_2013 <-phyto_RA[phyto_RA$Year %in% "2013",]
phyto_RA_2013 <- phyto_RA_2013 %>% 
  select(-Year)
pivot_phyto_RA_2013 <- phyto_RA_2013 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2013$value <- as.numeric(pivot_phyto_RA_2013$value)
pivot_phyto_RA_2013$variable <- ordered(pivot_phyto_RA_2013$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))


#2014
phyto_RA_2014 <-phyto_RA[phyto_RA$Year %in% "2014",]
phyto_RA_2014 <- phyto_RA_2014 %>% 
  select(-Year)
pivot_phyto_RA_2014 <- phyto_RA_2014 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2014$value <- as.numeric(pivot_phyto_RA_2014$value)
pivot_phyto_RA_2014$variable <- ordered(pivot_phyto_RA_2014$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))


#2015
phyto_RA_2015 <-phyto_RA[phyto_RA$Year %in% "2015",]
phyto_RA_2015 <- phyto_RA_2015 %>% 
  select(-Year)
pivot_phyto_RA_2015 <- phyto_RA_2015 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2015$value <- as.numeric(pivot_phyto_RA_2015$value)
pivot_phyto_RA_2015$variable <- ordered(pivot_phyto_RA_2015$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))


#2016
phyto_RA_2016 <-phyto_RA[phyto_RA$Year %in% "2016",]
phyto_RA_2016 <- phyto_RA_2016 %>% 
  select(-Year)
pivot_phyto_RA_2016 <- phyto_RA_2016 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2016$value <- as.numeric(pivot_phyto_RA_2016$value)
pivot_phyto_RA_2016$variable <- ordered(pivot_phyto_RA_2016$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))


#2017
phyto_RA_2017 <-phyto_RA[phyto_RA$Year %in% "2017",]
phyto_RA_2017 <- phyto_RA_2017 %>% 
  select(-Year)
pivot_phyto_RA_2017 <- phyto_RA_2017 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2017$value <- as.numeric(pivot_phyto_RA_2017$value)
pivot_phyto_RA_2017$variable <- ordered(pivot_phyto_RA_2017$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))

#2018
phyto_RA_2018 <-phyto_RA[phyto_RA$Year %in% "2018",]
phyto_RA_2018 <- phyto_RA_2018 %>% 
  select(-Year)
pivot_phyto_RA_2018 <- phyto_RA_2018 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2018$value <- as.numeric(pivot_phyto_RA_2018$value)
pivot_phyto_RA_2018$variable <- ordered(pivot_phyto_RA_2018$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))

#2019
phyto_RA_2019 <-phyto_RA[phyto_RA$Year %in% "2019",]
phyto_RA_2019 <- phyto_RA_2019 %>% 
  select(-Year)
pivot_phyto_RA_2019 <- phyto_RA_2019 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2019$value <- as.numeric(pivot_phyto_RA_2019$value)
pivot_phyto_RA_2019$variable <- ordered(pivot_phyto_RA_2019$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))


#2020
phyto_RA_2020 <-phyto_RA[phyto_RA$Year %in% "2020",]
phyto_RA_2020 <- phyto_RA_2020 %>% 
  select(-Year)
pivot_phyto_RA_2020 <- phyto_RA_2020 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RA_2020$value <- as.numeric(pivot_phyto_RA_2020$value)
pivot_phyto_RA_2020$variable <- ordered(pivot_phyto_RA_2020$variable, 
                                        levels=c("Ciliates_TA", "Cyano_TA","Chryso_TA", "Crypto_TA","Hapto_TA", "Dino_TA", "Chloro_TA", "Diatoms_TA"))


#plot 
#2012 
a <- 
  ggplot(pivot_phyto_RA_2012, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2012-01-01","2012-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2012")


#2013
b <- 
  ggplot(pivot_phyto_RA_2013, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2013-01-01","2013-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2013")


#2014
c <- 
  ggplot(pivot_phyto_RA_2014, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2014-01-01","2014-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2014")

#2015
d <- 
  ggplot(pivot_phyto_RA_2015, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2015-01-01","2015-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2015")


#2016
e <- 
  ggplot(pivot_phyto_RA_2016, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2016-01-01","2016-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2016")

#2017
f <- 
  ggplot(pivot_phyto_RA_2017, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2017-01-01","2017-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2017")

#2018
g <- 
  ggplot(pivot_phyto_RA_2018, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2018-01-01","2018-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2018")

#2019
h <- 
  ggplot(pivot_phyto_RA_2019, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2019-01-01","2019-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2019")

#2020
i <- 
  ggplot(pivot_phyto_RA_2020, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2020-01-01","2020-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2020")


#final plot
phytoPLOT <- plot_grid(a, d, g, NULL,
                       b, e, h, NULL,
                       c, f, i, NULL,
                       align = "h", ncol = 4)

title <- ggdraw() + 
  draw_label(
    ~paste("Phytoplankton and ciliates abundances ", "(cells", "·mL"^-1, ")"),
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

FigureS4 <- plot_grid(title, phytoPLOT, ncol=1, rel_heights = c(0.1, 1))

ggsave(path = "Figures", filename ="FigureS4.jpeg", width = 32, height = 20, units = "cm")



#pivot dataset for taxa specific total BIOVOLUME...........................................................
phyto_RB <- metadata %>% 
  select(Date, Year, Ciliates_TB, Cyano_TB, Diatoms_TB, Chloro_TB, Crypto_TB, Chryso_TB, Hapto_TB, Dino_TB)
str(phyto_RB)

#create a dataframe for each year and pivot to plot
#2012
phyto_RB_2012 <-phyto_RB[phyto_RB$Year %in% "2012",]
phyto_RB_2012 <- phyto_RB_2012 %>% 
  select(-Year)
pivot_phyto_RB_2012 <- phyto_RB_2012 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2012$value <- as.numeric(pivot_phyto_RB_2012$value)
pivot_phyto_RB_2012$variable <- ordered(pivot_phyto_RB_2012$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))


#2013
phyto_RB_2013 <-phyto_RB[phyto_RB$Year %in% "2013",]
phyto_RB_2013 <- phyto_RB_2013 %>% 
  select(-Year)
pivot_phyto_RB_2013 <- phyto_RB_2013 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2013$value <- as.numeric(pivot_phyto_RB_2013$value)
pivot_phyto_RB_2013$variable <- ordered(pivot_phyto_RB_2013$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))

#2014
phyto_RB_2014 <-phyto_RB[phyto_RB$Year %in% "2014",]
phyto_RB_2014 <- phyto_RB_2014 %>% 
  select(-Year)
pivot_phyto_RB_2014 <- phyto_RB_2014 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2014$value <- as.numeric(pivot_phyto_RB_2014$value)
pivot_phyto_RB_2014$variable <- ordered(pivot_phyto_RB_2014$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))

#2015
phyto_RB_2015 <-phyto_RB[phyto_RB$Year %in% "2015",]
phyto_RB_2015 <- phyto_RB_2015 %>% 
  select(-Year)
pivot_phyto_RB_2015 <- phyto_RB_2015 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2015$value <- as.numeric(pivot_phyto_RB_2015$value)
pivot_phyto_RB_2015$variable <- ordered(pivot_phyto_RB_2015$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))

#2016
phyto_RB_2016 <-phyto_RB[phyto_RB$Year %in% "2016",]
phyto_RB_2016 <- phyto_RB_2016 %>% 
  select(-Year)
pivot_phyto_RB_2016 <- phyto_RB_2016 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2016$value <- as.numeric(pivot_phyto_RB_2016$value)
pivot_phyto_RB_2016$variable <- ordered(pivot_phyto_RB_2016$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))

#2017
phyto_RB_2017 <-phyto_RB[phyto_RB$Year %in% "2017",]
phyto_RB_2017 <- phyto_RB_2017 %>% 
  select(-Year)
pivot_phyto_RB_2017 <- phyto_RB_2017 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2017$value <- as.numeric(pivot_phyto_RB_2017$value)
pivot_phyto_RB_2017$variable <- ordered(pivot_phyto_RB_2017$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))

#2018
phyto_RB_2018 <-phyto_RB[phyto_RB$Year %in% "2018",]
phyto_RB_2018 <- phyto_RB_2018 %>% 
  select(-Year)
pivot_phyto_RB_2018 <- phyto_RB_2018 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2018$value <- as.numeric(pivot_phyto_RB_2018$value)
pivot_phyto_RB_2018$variable <- ordered(pivot_phyto_RB_2018$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))

#2019
phyto_RB_2019 <-phyto_RB[phyto_RB$Year %in% "2019",]
phyto_RB_2019 <- phyto_RB_2019 %>% 
  select(-Year)
pivot_phyto_RB_2019 <- phyto_RB_2019 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2019$value <- as.numeric(pivot_phyto_RB_2019$value)
pivot_phyto_RB_2019$variable <- ordered(pivot_phyto_RB_2019$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))
#2020
phyto_RB_2020 <-phyto_RB[phyto_RB$Year %in% "2020",]
phyto_RB_2020 <- phyto_RB_2020 %>% 
  select(-Year)
pivot_phyto_RB_2020 <- phyto_RB_2020 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_phyto_RB_2020$value <- as.numeric(pivot_phyto_RB_2020$value)
pivot_phyto_RB_2020$variable <- ordered(pivot_phyto_RB_2020$variable, 
                                        levels=c("Ciliates_TB", "Cyano_TB","Chryso_TB", "Crypto_TB","Hapto_TB", "Dino_TB", "Chloro_TB", "Diatoms_TB"))


#plot 
#2012 
a <- 
  ggplot(pivot_phyto_RB_2012, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2012-01-01","2012-12-31"))) +
 theme(text = element_text(size = 10),
       axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2012")


#2013
b <- 
  ggplot(pivot_phyto_RB_2013, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2013-01-01","2013-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2013")
 

#2014
c <- 
  ggplot(pivot_phyto_RB_2014, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2014-01-01","2014-12-31"))) +
  theme(text = element_text(size = 10),
       axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2014")

#2015
d <- 
  ggplot(pivot_phyto_RB_2015, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2015-01-01","2015-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2015")


#2016
e <- 
  ggplot(pivot_phyto_RB_2016, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2016-01-01","2016-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2016")

#2017
f <- 
  ggplot(pivot_phyto_RB_2017, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2017-01-01","2017-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2017")

#2018
g <- 
  ggplot(pivot_phyto_RB_2018, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2018-01-01","2018-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2018")

#2019
h <- 
  ggplot(pivot_phyto_RB_2019, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2019-01-01","2019-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2019")

#2020
i <- 
  ggplot(pivot_phyto_RB_2020, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values= c("deeppink", "turquoise1", "khaki2", "chocolate4","red", "darkorchid3", "chartreuse", "grey50"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2020-01-01","2020-12-31"))) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2020")


#final plot
phytoPLOT <- plot_grid(a, d, g, NULL,
                       b, e, h, NULL,
                       c, f, i, NULL,
          align = "h", ncol = 4)

title <- ggdraw() + 
  draw_label(
    ~paste("Phytoplankton and ciliates biovolume ", "(mm"^3, "·m"^-3, ")"),
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

FigureS5 <- plot_grid(title, phytoPLOT, ncol=1, rel_heights = c(0.1, 1))

ggsave(path = "Figures", filename ="FigureS5.jpeg", width = 32, height = 20, units = "cm")


#Shifts in zooplankton community structure
#ZOOPLANKTON TAXA COLOURS
#"Dap" = "blue", 
#"Diaph" = "turquoise1",
#"Cerio"="red", 
#"Bosm"="grey",
#"Nau" = "yellow",
#"Cyc"="yellow4",
#"Cala"="green3"

#pivot dataset for taxa specific total ABUNDANCE...........................................................
zoo_TA <- metadata %>% 
  select(Date, Year, Dap_TA, Diaph_TA, Cala_TA, Nau_TA, Cyc_TA, Cerio_TA, Bosm_TA)
str(zoo_TA)

#create a dataframe for each year and pivot to plot
#2004
zoo_TA_2004 <-zoo_TA[zoo_TA$Year %in% "2004",]
zoo_TA_2004 <- zoo_TA_2004 %>% 
  select(-Year)
pivot_zoo_TA_2004 <- zoo_TA_2004 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2004$value <- as.numeric(pivot_zoo_TA_2004$value)
pivot_zoo_TA_2004$variable <- ordered(pivot_zoo_TA_2004$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2005
zoo_TA_2005 <-zoo_TA[zoo_TA$Year %in% "2005",]
zoo_TA_2005 <- zoo_TA_2005 %>% 
  select(-Year)
pivot_zoo_TA_2005 <- zoo_TA_2005 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2005$value <- as.numeric(pivot_zoo_TA_2005$value)
pivot_zoo_TA_2005$variable <- ordered(pivot_zoo_TA_2005$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))

#2006
zoo_TA_2006 <-zoo_TA[zoo_TA$Year %in% "2006",]
zoo_TA_2006 <- zoo_TA_2006 %>% 
  select(-Year)
pivot_zoo_TA_2006 <- zoo_TA_2006 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2006$value <- as.numeric(pivot_zoo_TA_2006$value)
pivot_zoo_TA_2006$variable <- ordered(pivot_zoo_TA_2006$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2007
zoo_TA_2007 <-zoo_TA[zoo_TA$Year %in% "2007",]
zoo_TA_2007 <- zoo_TA_2007 %>% 
  select(-Year)
pivot_zoo_TA_2007 <- zoo_TA_2007 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2007$value <- as.numeric(pivot_zoo_TA_2007$value)
pivot_zoo_TA_2007$variable <- ordered(pivot_zoo_TA_2007$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))

#2008
zoo_TA_2008 <-zoo_TA[zoo_TA$Year %in% "2008",]
zoo_TA_2008 <- zoo_TA_2008 %>% 
  select(-Year)
pivot_zoo_TA_2008 <- zoo_TA_2008 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2008$value <- as.numeric(pivot_zoo_TA_2008$value)
pivot_zoo_TA_2008$variable <- ordered(pivot_zoo_TA_2008$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2009
zoo_TA_2009 <-zoo_TA[zoo_TA$Year %in% "2009",]
zoo_TA_2009 <- zoo_TA_2009 %>% 
  select(-Year)
pivot_zoo_TA_2009 <- zoo_TA_2009 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2009$value <- as.numeric(pivot_zoo_TA_2009$value)
pivot_zoo_TA_2009$variable <- ordered(pivot_zoo_TA_2009$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))

#2010
zoo_TA_2010 <-zoo_TA[zoo_TA$Year %in% "2010",]
zoo_TA_2010 <- zoo_TA_2010 %>% 
  select(-Year)
pivot_zoo_TA_2010 <- zoo_TA_2010 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2010$value <- as.numeric(pivot_zoo_TA_2010$value)
pivot_zoo_TA_2010$variable <- ordered(pivot_zoo_TA_2010$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))

#2011
zoo_TA_2011 <-zoo_TA[zoo_TA$Year %in% "2011",]
zoo_TA_2011 <- zoo_TA_2011 %>% 
  select(-Year)
pivot_zoo_TA_2011 <- zoo_TA_2011 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2011$value <- as.numeric(pivot_zoo_TA_2011$value)
pivot_zoo_TA_2011$variable <- ordered(pivot_zoo_TA_2011$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2012
zoo_TA_2012 <-zoo_TA[zoo_TA$Year %in% "2012",]
zoo_TA_2012 <- zoo_TA_2012 %>% 
  select(-Year)
pivot_zoo_TA_2012 <- zoo_TA_2012 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2012$value <- as.numeric(pivot_zoo_TA_2012$value)
pivot_zoo_TA_2012$variable <- ordered(pivot_zoo_TA_2012$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2013
zoo_TA_2013 <-zoo_TA[zoo_TA$Year %in% "2013",]
zoo_TA_2013 <- zoo_TA_2013 %>% 
  select(-Year)
pivot_zoo_TA_2013 <- zoo_TA_2013 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2013$value <- as.numeric(pivot_zoo_TA_2013$value)
pivot_zoo_TA_2013$variable <- ordered(pivot_zoo_TA_2013$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2014
zoo_TA_2014 <-zoo_TA[zoo_TA$Year %in% "2014",]
zoo_TA_2014 <- zoo_TA_2014 %>% 
  select(-Year)
pivot_zoo_TA_2014 <- zoo_TA_2014 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2014$value <- as.numeric(pivot_zoo_TA_2014$value)
pivot_zoo_TA_2014$variable <- ordered(pivot_zoo_TA_2014$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2015
zoo_TA_2015 <-zoo_TA[zoo_TA$Year %in% "2015",]
zoo_TA_2015 <- zoo_TA_2015 %>% 
  select(-Year)
pivot_zoo_TA_2015 <- zoo_TA_2015 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2015$value <- as.numeric(pivot_zoo_TA_2015$value)
pivot_zoo_TA_2015$variable <- ordered(pivot_zoo_TA_2015$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2016
zoo_TA_2016 <-zoo_TA[zoo_TA$Year %in% "2016",]
zoo_TA_2016 <- zoo_TA_2016 %>% 
  select(-Year)
pivot_zoo_TA_2016 <- zoo_TA_2016 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2016$value <- as.numeric(pivot_zoo_TA_2016$value)
pivot_zoo_TA_2016$variable <- ordered(pivot_zoo_TA_2016$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2017
zoo_TA_2017 <-zoo_TA[zoo_TA$Year %in% "2017",]
zoo_TA_2017 <- zoo_TA_2017 %>% 
  select(-Year)
pivot_zoo_TA_2017 <- zoo_TA_2017 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2017$value <- as.numeric(pivot_zoo_TA_2017$value)
pivot_zoo_TA_2017$variable <- ordered(pivot_zoo_TA_2017$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))

#2018
zoo_TA_2018 <-zoo_TA[zoo_TA$Year %in% "2018",]
zoo_TA_2018 <- zoo_TA_2018 %>% 
  select(-Year)
pivot_zoo_TA_2018 <- zoo_TA_2018 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2018$value <- as.numeric(pivot_zoo_TA_2018$value)
pivot_zoo_TA_2018$variable <- ordered(pivot_zoo_TA_2018$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))
#2019
zoo_TA_2019 <-zoo_TA[zoo_TA$Year %in% "2019",]
zoo_TA_2019 <- zoo_TA_2019 %>% 
  select(-Year)
pivot_zoo_TA_2019 <- zoo_TA_2019 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2019$value <- as.numeric(pivot_zoo_TA_2019$value)
pivot_zoo_TA_2019$variable <- ordered(pivot_zoo_TA_2019$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))

#2020
zoo_TA_2020 <-zoo_TA[zoo_TA$Year %in% "2020",]
zoo_TA_2020 <- zoo_TA_2020 %>% 
  select(-Year)
pivot_zoo_TA_2020 <- zoo_TA_2020 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TA_2020$value <- as.numeric(pivot_zoo_TA_2020$value)
pivot_zoo_TA_2020$variable <- ordered(pivot_zoo_TA_2020$variable, 
                                      levels=c("Dap_TA", "Diaph_TA","Cerio_TA", "Bosm_TA","Nau_TA", "Cyc_TA", "Cala_TA"))



#plot
#A = 2004
A <- 
  ggplot(pivot_zoo_TA_2004, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2004-01-01","2004-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2004")
A
#B = 2005
B <- 
  ggplot(pivot_zoo_TA_2005, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2005-01-01","2005-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2005")
B
#C = 2006
C <- 
  ggplot(pivot_zoo_TA_2006, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2006-01-01","2006-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2006")
C
#D = 2007
D <- 
  ggplot(pivot_zoo_TA_2007, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2007-01-01","2007-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2007")
D
#E = 2008
E <- 
  ggplot(pivot_zoo_TA_2008, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2008-01-01","2008-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2008")
E
#F = 2009
Fp <- 
  ggplot(pivot_zoo_TA_2009, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2009-01-01","2009-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2009")
Fp
#G = 2010
G <- 
  ggplot(pivot_zoo_TA_2010, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2010-01-01","2010-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2010")
G
#H = 2011
H <- 
  ggplot(pivot_zoo_TA_2011, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2011-01-01","2011-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2011")
H
#I = 2012
I <- 
  ggplot(pivot_zoo_TA_2012, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2012-01-01","2012-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2012")
I
#J = 2013
J <- 
  ggplot(pivot_zoo_TA_2013, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2013-01-01","2013-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2013")
J
#K = 2014
K <- 
  ggplot(pivot_zoo_TA_2014, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2014-01-01","2014-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2014")
K
#L = 2015
L <- 
  ggplot(pivot_zoo_TA_2015, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2015-01-01","2015-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2015")
L
#M = 2016
M <- 
  ggplot(pivot_zoo_TA_2016, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2016-01-01","2016-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2016")
M
#N = 2017
N <- 
  ggplot(pivot_zoo_TA_2017, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2017-01-01","2017-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2017")
N
#O = 2018
O <- 
  ggplot(pivot_zoo_TA_2018, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2018-01-01","2018-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2018")
O
#P = 2019
P <- 
  ggplot(pivot_zoo_TA_2019, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2019-01-01","2019-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2019")
P
#Q = 2020
Q <- 
  ggplot(pivot_zoo_TA_2020, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TA" = "blue", 
                               "Diaph_TA" = "turquoise1",
                               "Cerio_TA"="red", 
                               "Bosm_TA"="grey",
                               "Nau_TA" = "yellow",
                               "Cyc_TA"="yellow4",
                               "Cala_TA"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2020-01-01","2020-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2020")
Q

#final plot
zooPLOT <- plot_grid(A, Fp, K, P,
                     B, G, L, Q,
                     C, H, M, NULL,
                     D, I, N, NULL,
                     E, J, O, NULL,
                     align = "h", ncol = 4)
zooPLOT

title <- ggdraw() + 
  draw_label(
    ~paste("Crustaceans zooplankton abundance ", "(ind", "·L"^-1, ")"),
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = (margin = ggplot2::margin(0, 0, 0, 7)))


FigureS6 <- plot_grid(title, zooPLOT, ncol=1, rel_heights = c(0.1, 1))

FigureS6 #manually exported!



#pivot dataset for taxa specific total BIOVOLUME...........................................................
zoo_TB <- metadata %>% 
  select(Date, Year, Dap_TB, Diaph_TB, Cala_TB, Nau_TB, Cyc_TB, Cerio_TB, Bosm_TB)
str(zoo_TB)

#create a dataframe for each year and pivot to plot
#2004
zoo_TB_2004 <-zoo_TB[zoo_TB$Year %in% "2004",]
zoo_TB_2004 <- zoo_TB_2004 %>% 
  select(-Year)
pivot_zoo_TB_2004 <- zoo_TB_2004 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2004$value <- as.numeric(pivot_zoo_TB_2004$value)
pivot_zoo_TB_2004$variable <- ordered(pivot_zoo_TB_2004$variable, 
                                        levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2005
zoo_TB_2005 <-zoo_TB[zoo_TB$Year %in% "2005",]
zoo_TB_2005 <- zoo_TB_2005 %>% 
  select(-Year)
pivot_zoo_TB_2005 <- zoo_TB_2005 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2005$value <- as.numeric(pivot_zoo_TB_2005$value)
pivot_zoo_TB_2005$variable <- ordered(pivot_zoo_TB_2005$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))

#2006
zoo_TB_2006 <-zoo_TB[zoo_TB$Year %in% "2006",]
zoo_TB_2006 <- zoo_TB_2006 %>% 
  select(-Year)
pivot_zoo_TB_2006 <- zoo_TB_2006 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2006$value <- as.numeric(pivot_zoo_TB_2006$value)
pivot_zoo_TB_2006$variable <- ordered(pivot_zoo_TB_2006$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2007
zoo_TB_2007 <-zoo_TB[zoo_TB$Year %in% "2007",]
zoo_TB_2007 <- zoo_TB_2007 %>% 
  select(-Year)
pivot_zoo_TB_2007 <- zoo_TB_2007 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2007$value <- as.numeric(pivot_zoo_TB_2007$value)
pivot_zoo_TB_2007$variable <- ordered(pivot_zoo_TB_2007$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))

#2008
zoo_TB_2008 <-zoo_TB[zoo_TB$Year %in% "2008",]
zoo_TB_2008 <- zoo_TB_2008 %>% 
  select(-Year)
pivot_zoo_TB_2008 <- zoo_TB_2008 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2008$value <- as.numeric(pivot_zoo_TB_2008$value)
pivot_zoo_TB_2008$variable <- ordered(pivot_zoo_TB_2008$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2009
zoo_TB_2009 <-zoo_TB[zoo_TB$Year %in% "2009",]
zoo_TB_2009 <- zoo_TB_2009 %>% 
  select(-Year)
pivot_zoo_TB_2009 <- zoo_TB_2009 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2009$value <- as.numeric(pivot_zoo_TB_2009$value)
pivot_zoo_TB_2009$variable <- ordered(pivot_zoo_TB_2009$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))

#2010
zoo_TB_2010 <-zoo_TB[zoo_TB$Year %in% "2010",]
zoo_TB_2010 <- zoo_TB_2010 %>% 
  select(-Year)
pivot_zoo_TB_2010 <- zoo_TB_2010 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2010$value <- as.numeric(pivot_zoo_TB_2010$value)
pivot_zoo_TB_2010$variable <- ordered(pivot_zoo_TB_2010$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))

#2011
zoo_TB_2011 <-zoo_TB[zoo_TB$Year %in% "2011",]
zoo_TB_2011 <- zoo_TB_2011 %>% 
  select(-Year)
pivot_zoo_TB_2011 <- zoo_TB_2011 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2011$value <- as.numeric(pivot_zoo_TB_2011$value)
pivot_zoo_TB_2011$variable <- ordered(pivot_zoo_TB_2011$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2012
zoo_TB_2012 <-zoo_TB[zoo_TB$Year %in% "2012",]
zoo_TB_2012 <- zoo_TB_2012 %>% 
  select(-Year)
pivot_zoo_TB_2012 <- zoo_TB_2012 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2012$value <- as.numeric(pivot_zoo_TB_2012$value)
pivot_zoo_TB_2012$variable <- ordered(pivot_zoo_TB_2012$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2013
zoo_TB_2013 <-zoo_TB[zoo_TB$Year %in% "2013",]
zoo_TB_2013 <- zoo_TB_2013 %>% 
  select(-Year)
pivot_zoo_TB_2013 <- zoo_TB_2013 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2013$value <- as.numeric(pivot_zoo_TB_2013$value)
pivot_zoo_TB_2013$variable <- ordered(pivot_zoo_TB_2013$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2014
zoo_TB_2014 <-zoo_TB[zoo_TB$Year %in% "2014",]
zoo_TB_2014 <- zoo_TB_2014 %>% 
  select(-Year)
pivot_zoo_TB_2014 <- zoo_TB_2014 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2014$value <- as.numeric(pivot_zoo_TB_2014$value)
pivot_zoo_TB_2014$variable <- ordered(pivot_zoo_TB_2014$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2015
zoo_TB_2015 <-zoo_TB[zoo_TB$Year %in% "2015",]
zoo_TB_2015 <- zoo_TB_2015 %>% 
  select(-Year)
pivot_zoo_TB_2015 <- zoo_TB_2015 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2015$value <- as.numeric(pivot_zoo_TB_2015$value)
pivot_zoo_TB_2015$variable <- ordered(pivot_zoo_TB_2015$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2016
zoo_TB_2016 <-zoo_TB[zoo_TB$Year %in% "2016",]
zoo_TB_2016 <- zoo_TB_2016 %>% 
  select(-Year)
pivot_zoo_TB_2016 <- zoo_TB_2016 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2016$value <- as.numeric(pivot_zoo_TB_2016$value)
pivot_zoo_TB_2016$variable <- ordered(pivot_zoo_TB_2016$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2017
zoo_TB_2017 <-zoo_TB[zoo_TB$Year %in% "2017",]
zoo_TB_2017 <- zoo_TB_2017 %>% 
  select(-Year)
pivot_zoo_TB_2017 <- zoo_TB_2017 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2017$value <- as.numeric(pivot_zoo_TB_2017$value)
pivot_zoo_TB_2017$variable <- ordered(pivot_zoo_TB_2017$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))

#2018
zoo_TB_2018 <-zoo_TB[zoo_TB$Year %in% "2018",]
zoo_TB_2018 <- zoo_TB_2018 %>% 
  select(-Year)
pivot_zoo_TB_2018 <- zoo_TB_2018 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2018$value <- as.numeric(pivot_zoo_TB_2018$value)
pivot_zoo_TB_2018$variable <- ordered(pivot_zoo_TB_2018$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))
#2019
zoo_TB_2019 <-zoo_TB[zoo_TB$Year %in% "2019",]
zoo_TB_2019 <- zoo_TB_2019 %>% 
  select(-Year)
pivot_zoo_TB_2019 <- zoo_TB_2019 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2019$value <- as.numeric(pivot_zoo_TB_2019$value)
pivot_zoo_TB_2019$variable <- ordered(pivot_zoo_TB_2019$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))

#2020
zoo_TB_2020 <-zoo_TB[zoo_TB$Year %in% "2020",]
zoo_TB_2020 <- zoo_TB_2020 %>% 
  select(-Year)
pivot_zoo_TB_2020 <- zoo_TB_2020 %>% 
  pivot_longer(-Date, 
               names_to = "variable", 
               values_to = "value")
pivot_zoo_TB_2020$value <- as.numeric(pivot_zoo_TB_2020$value)
pivot_zoo_TB_2020$variable <- ordered(pivot_zoo_TB_2020$variable, 
                                      levels=c("Dap_TB", "Diaph_TB","Cerio_TB", "Bosm_TB","Nau_TB", "Cyc_TB", "Cala_TB"))



#plot
#A = 2004
A <- 
  ggplot(pivot_zoo_TB_2004, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
    scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2004-01-01","2004-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2004")
A
#B = 2005
B <- 
  ggplot(pivot_zoo_TB_2005, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2005-01-01","2005-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2005")
B
#C = 2006
C <- 
  ggplot(pivot_zoo_TB_2006, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2006-01-01","2006-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2006")
C
#D = 2007
D <- 
  ggplot(pivot_zoo_TB_2007, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2007-01-01","2007-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2007")
D
#E = 2008
E <- 
  ggplot(pivot_zoo_TB_2008, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2008-01-01","2008-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2008")
E
#F = 2009
Fp <- 
  ggplot(pivot_zoo_TB_2009, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2009-01-01","2009-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2009")
Fp
#G = 2010
G <- 
  ggplot(pivot_zoo_TB_2010, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2010-01-01","2010-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2010")
G
#H = 2011
H <- 
  ggplot(pivot_zoo_TB_2011, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2011-01-01","2011-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2011")
H
#I = 2012
I <- 
  ggplot(pivot_zoo_TB_2012, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2012-01-01","2012-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2012")
I
#J = 2013
J <- 
  ggplot(pivot_zoo_TB_2013, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2013-01-01","2013-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2013")
J
#K = 2014
K <- 
  ggplot(pivot_zoo_TB_2014, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2014-01-01","2014-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2014")
K
#L = 2015
L <- 
  ggplot(pivot_zoo_TB_2015, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2015-01-01","2015-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2015")
L
#M = 2016
M <- 
  ggplot(pivot_zoo_TB_2016, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2016-01-01","2016-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2016")
M
#N = 2017
N <- 
  ggplot(pivot_zoo_TB_2017, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2017-01-01","2017-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2017")
N
#O = 2018
O <- 
  ggplot(pivot_zoo_TB_2018, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2018-01-01","2018-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2018")
O
#P = 2019
P <- 
  ggplot(pivot_zoo_TB_2019, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2019-01-01","2019-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2019")
P
#Q = 2020
Q <- 
  ggplot(pivot_zoo_TB_2020, aes(x = Date, y =  value, fill = variable))+ 
  geom_bar(stat = "identity", color = "black", width = 7)+ 
  scale_fill_manual(values = c("Dap_TB" = "blue", 
                               "Diaph_TB" = "turquoise1",
                               "Cerio_TB"="red", 
                               "Bosm_TB"="grey",
                               "Nau_TB" = "yellow",
                               "Cyc_TB"="yellow4",
                               "Cala_TB"="green3"))+
  scale_x_date(breaks=date_breaks("1 months"),labels=date_format("%b"),
               limits = as.Date(c("2020-01-01","2020-12-31"))) +
  theme(text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  ggtitle("2020")
Q

#final plot
zooPLOT <- plot_grid(A, Fp, K, P,
                       B, G, L, Q,
                       C, H, M, NULL,
                       D, I, N, NULL,
                       E, J, O, NULL,
                       align = "h", ncol = 4)
zooPLOT

title <- ggdraw() + 
  draw_label(
    ~paste("Crustaceans zooplankton biomass ", "(ugDW", "·L"^-1, ")"),
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = (margin = ggplot2::margin(0, 0, 0, 7)))


FigureS7 <- plot_grid(title, zooPLOT, ncol=1, rel_heights = c(0.1, 1))

FigureS7 #manually exported!

#Correlation between diversity metrics
div_I <- zoop %>% 
  select(Zoo_SizeD, Zoo_H, Zoo_SizeMean)
str(div_I)

chart.Correlation(div_I, histogram = TRUE, method = "pearson", pch = 20)
chart.Correlation(div_I, histogram = TRUE, method = "spearman", pch = 20)


#Zoo:Chla ratio (already ln transformed)
#ln(zoo/chla)
lnzoophyto_df <- metadata %>% 
  select (Date, Zoo.Chla)
#create month column
months <- as.numeric(format(as.Date(lnzoophyto_df$Date, '%d/%m/%Y'), '%m'))
indx <- setNames( rep(c('winter', 'spring', 'summer',
                        'fall'),each=3), c(12,1:11))
lnzoophyto_df$Season <- unname(indx[as.character(months)])
str(lnzoophyto_df) #it worked!
season.colours <- c("winter"="cyan", 
                    "spring"="chartreuse",
                    "summer"="yellow",
                    "fall"="red")

lnZP <- ggplot(data = lnzoophyto_df, aes(x=Date, y = Zoo.Chla)) +
  geom_line(size = 1, data=lnzoophyto_df[!is.na(lnzoophyto_df$Zoo.Chla),])+
  geom_point(aes(color=Season), size = 4) +
  scale_color_manual(values=c("winter"="cyan", 
                              "spring"="chartreuse",
                              "summer"="yellow",
                              "fall"="red"))+
  ylab("ln(Zoo:Chla) (DW)") +
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y"),
               limits = as.Date(c("2007-01-01","2020-12-31"))) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank()) +
  geom_hline(yintercept=4.5, linetype=2, size=0.5, colour="black") +
  geom_hline(yintercept=4, linetype=2, size=0.5, colour="black") +
  geom_hline(yintercept=3.5, linetype=2, size=0.5, colour="black") 
lnZP


#ln(cladocerans/copepods) 

zoop <- metadata %>% 
  select (Date, copA.claA, copB.claB)


#create month column
months <- as.numeric(format(as.Date(zoop$Date, '%d/%m/%Y'), '%m'))
indx <- setNames( rep(c('winter', 'spring', 'summer',
                        'fall'),each=3), c(12,1:11))
zoop$Season <- unname(indx[as.character(months)])

lnClaCopA <- ggplot(data = zoop, aes(x=Date, y = copA.claA)) +
  geom_line(size = 1, data=zoop[!is.na(zoop$copA.claA),])+
  geom_point(aes(color=Season), size = 4) +
  scale_color_manual(values=c("winter"="cyan", 
                              "spring"="chartreuse",
                              "summer"="yellow",
                              "fall"="red"))+
  ylab("ln(cladocerans:copepods) (abundances)") +
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank()) +
  geom_hline(yintercept=0, linetype=2, size=0.5, colour="black")
lnClaCopA

lnClaCopB <- ggplot(data = zoop, aes(x=Date, y = copB.claB)) +
  geom_line(size = 1, data=zoop[!is.na(zoop$copB.claB),])+
  geom_point(aes(color=Season), size = 4) +
  scale_color_manual(values=c("winter"="cyan", 
                              "spring"="chartreuse",
                              "summer"="yellow",
                              "fall"="red"))+
  ylab("ln(cladocerans:copepods) (biomass)") +
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank())+
  geom_hline(yintercept=0, linetype=2, size=0.5, colour="black")
lnClaCopB

#final plot
zoopPLOT <- plot_grid(lnClaCopA, lnClaCopB,
                     align = "h", ncol = 1, labels = c("a","b"))
zoopPLOT  #manually exported!



#Smolt run vs zoo size diversity --> plot in excel!






