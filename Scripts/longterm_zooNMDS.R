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
library(viridis) #colour palete


#input file
bioA <- read.csv("Data/Zoo_NMDSbio_transRA_2004_2020.csv", head = TRUE, check.names = FALSE, row.names = 1)
str(bioA)
factorA <- subset(bioA, select=Season)
factorB <- subset(bioA, select=Year)

#remove Group column
bioA1 <- subset(bioA, select=-c(Season,Year))
str(bioA1)

#NMDS run
set.seed(200)
fm <- metaMDS(bioA1, autotransform = FALSE, trymax = 100)
fm #0.199
#DEFAULT:
#distance = Bray-curtis
#max 100 tries
#Scaling = centring, PC rotation, halfchange scaling
#Scpecies = expanded scores based on data

#ANOSIM
#anomin test to differentiate between season --> use "factorA" 
bio.dist <- vegdist(bioA1, method="bray") 
attach(factorA)
bio.ano <- anosim(bio.dist, Season)
summary(bio.ano) #R = 0.37 significance 0.001
plot(bio.ano)

#anomin test to differentiate between years --> use "factorB" 
bio.dist <- vegdist(bioA1, method="bray") 
attach(factorB)
bio.ano <- anosim(bio.dist, Year)
summary(bio.ano) #R = 0.16 significance 0.001
plot(bio.ano)


#Extract scores from metaMDS function to plot with ggplot2

#data.scores
str(bioA)
data.scores <- as.data.frame(scores(fm,"sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$Date <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Season <- bioA$Season  #  add the grp variable created earlier
data.scores$Year <- bioA$Year  #  add the grp variable created earlier
head(data.scores)  #look at the data


#species.scores
species.scores <- as.data.frame(scores(fm, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$Species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


#colour by season
data.scores$Season <- ordered(data.scores$Season, 
                              levels=c("Winter", "Spring","Summer", 
                                       "Autumn"))

a <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,fill=Season),size=5, alpha = 1, shape=21) + # add the point markers
  scale_fill_manual(values=c("Winter" = "cyan", "Spring" = "green", "Summer"="yellow", "Autumn"="red")) + #manual colours for date points
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=Species),alpha=1, size=5, colour = "black") +  # add the species labels
  coord_equal() + #important for the dimension of the NMDS
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  ylim(-0.45,0.6)+
  xlim(-0.5,0.5)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))# remove y-axis labels)
a


#colour by Year
b <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,fill=Year),size=5, alpha = 1, shape=21) + # add the point markers
  scale_fill_viridis_c() + #manual colours for date points
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=Species),alpha=1, size=5, colour = "black") +  # add the species labels
  coord_equal() + #important for the dimension of the NMDS
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  ylim(-0.45,0.6)+
  xlim(-0.5,0.5)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))# remove y-axis labels)
b


zooNMDS <- plot_grid(a, b,
                       align="hv", axis="tblr", ncol = 1, labels =c("a","b"))
title <- ggdraw() + 
  draw_label("Zooplankton - community composition (2004-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0,
             size = 12
  ) +
  theme(plot.margin = (margin = ggplot2::margin(0, 0, 0, 7)))

Figure21 <- plot_grid(title, zooNMDS, ncol=1, rel_heights = c(0.1, 1))
Figure21

ggsave(path = "Figures", filename ="Figure21.jpeg", width = 18, height = 24, units = "cm")


