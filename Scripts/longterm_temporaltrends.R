library(ggplot2) #nice plots
library (TimeSeries) #for timeseries
library(lubridate) #for timeseries
library(scales) #labels and axis
library(cowplot) #nice figures
library(gridExtra) #to plot more than 1 plot in the same figure
library(zoo) #for timeseries
library(gridGraphics) #to plot more than 1 plot in the same figure
library(tidyverse) #manipulate datasets

#ridgeline plots
#install.packages("ggridges")
library(ggridges)
library(viridis)
library(hrbrthemes)

#https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/ 
#https://www.datanovia.com/en/blog/elegant-visualization-of-density-distribution-in-r-using-ridgeline/

#input file
metadata <- read.csv("Data/longterm_metadata.csv", stringsAsFactors = T)
metadata$Date <- as.Date(metadata$Date,"%d/%m/%Y")
names(metadata)


#......................................NUTRIENTS - LOADINGS............................................................................#
#FIGURES 13 - THESIS CHAPTER 1

#select data 
loads <- metadata %>% 
  select(Date, Year, Month, TNload, TPload, TN.TPload)
loads$Year <- as.factor(loads$Year)
names(loads)
str(loads)
loads$Month <- ordered(loads$Month, 
                       levels=c("Jan", "Feb","Mar", 
                                "Apr","May", "Jun", 
                                "Jul","Aug","Sep",
                                "Oct","Nov", "Dec"))
cols <- c("Jan"="turquoise1","Feb"="turquoise2","Mar"="chartreuse",
          "Apr"="chartreuse1","May"="chartreuse2","Jun"="yellow",
          "Jul"="yellow1","Aug"="yellow2","Sep"="coral",
          "Oct"="coral1","Nov"="coral2","Dec"="turquoise")


a <- ggplot(loads, aes(x = TNload, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("TN ", "(kgN", "·day"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


b <- ggplot(loads, aes(x = TNload, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                              jittered_points = TRUE, 
                               position=position_points_jitter(width=0.05, height = 0),
                               point_shape = '|', point_size = 2, point_alpha = 1,
                               point_color = "black") +
  xlab(~paste("TN ", "(kgN", "·day"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

c <- ggplot(loads, aes(x = TPload, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("TP ", "(kgP", "·day"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

d <- ggplot(loads, aes(x = TPload, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("TP ", "(kgP", "·day"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

e <- ggplot(loads, aes(x = TN.TPload, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("ln(TN:TP) (molar)") +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

f <- ggplot(loads, aes(x = TN.TPload, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("ln(TN:TP) (molar)") +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


loadsPlot <- plot_grid(a, c, e, b, d, f,
                       align="hv", axis="tblr", ncol = 3, labels =c("a","c","e","b","d","f"))
title <- ggdraw() + 
  draw_label("Nutrient loadings - temporal dynamics (2009-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

Figure13 <- plot_grid(title, loadsPlot, ncol=1, rel_heights = c(0.1, 1))

ggsave(path = "Figures", filename ="Figure13.jpeg", width = 15, height = 20, units = "cm")

#......................................NUTRIENTS - LAKE............................................................................#
#select data 
inlake <- metadata %>% 
  select(Date, Year, Month, F.TN, F.TP, F_TN.TP)
inlake$Year <- as.factor(inlake$Year)
names(inlake)
str(inlake)

inlake$Month <- ordered(inlake$Month, 
                       levels=c("Jan", "Feb","Mar", 
                                "Apr","May", "Jun", 
                                "Jul","Aug","Sep",
                                "Oct","Nov", "Dec"))
cols <- c("Jan"="turquoise1","Feb"="turquoise2","Mar"="chartreuse",
          "Apr"="chartreuse1","May"="chartreuse2","Jun"="yellow",
          "Jul"="yellow1","Aug"="yellow2","Sep"="coral",
          "Oct"="coral1","Nov"="coral2","Dec"="turquoise")

a <- ggplot(inlake, aes(x = F.TN, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("TN ", "(mgN", "·L"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


b <- ggplot(inlake, aes(x = F.TN, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("TN ", "(mgN", "·L"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

c <- ggplot(inlake, aes(x = F.TP, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("TP ", "(ugP", "·L"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

d <- ggplot(inlake, aes(x = F.TP, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("TP ", "(ugP", "·L"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

e <- ggplot(inlake, aes(x = F_TN.TP, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("ln(TN:TP) (molar)") +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

f <- ggplot(inlake, aes(x = F_TN.TP, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("ln(TN:TP) (molar)") +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


inlakePlot <- plot_grid(a, c, e, b, d, f,
                       align="hv", axis="tblr", ncol = 3, labels =c("a","c","e","b","d","f"))
title <- ggdraw() + 
  draw_label("In-lake nutrient concentrations - temporal dynamics (2009-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

Figure14 <- plot_grid(title, inlakePlot, ncol=1, rel_heights = c(0.1, 1))

ggsave(path = "Figures", filename ="Figure14.jpeg", width = 15, height = 20, units = "cm")


#......................................COLOUR............................................................................#
#select data 
col.load.conc <- metadata %>% 
  select(Date, Year, Month, COLload, F.Colour)
col.load.conc$Year <- as.factor(col.load.conc$Year)
names(col.load.conc)
str(col.load.conc)

col.load.conc$Month <- ordered(col.load.conc$Month, 
                        levels=c("Jan", "Feb","Mar", 
                                 "Apr","May", "Jun", 
                                 "Jul","Aug","Sep",
                                 "Oct","Nov", "Dec"))
cols <- c("Jan"="turquoise1","Feb"="turquoise2","Mar"="chartreuse",
          "Apr"="chartreuse1","May"="chartreuse2","Jun"="yellow",
          "Jul"="yellow1","Aug"="yellow2","Sep"="coral",
          "Oct"="coral1","Nov"="coral2","Dec"="turquoise")

a <- ggplot(col.load.conc, aes(x = COLload, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("Colour ", "(kgPtCo", "·day"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

b <- ggplot(col.load.conc, aes(x = COLload, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("Colour ", "(kgPtCo", "·day"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

c <- ggplot(col.load.conc, aes(x = F.Colour, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("Colour ", "(mgPtCo", "·L"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

d <- ggplot(col.load.conc, aes(x = F.Colour, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("Colour ", "(mgPtCo", "·L"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

col.load.concPlot <- plot_grid(a, c, b, d,
                        align="hv", axis="tblr", ncol = 2, labels =c("a","c","b","d"))
title <- ggdraw() + 
  draw_label("Colour loadings & concentrations - temporal dynamics (2009-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0,
             size = 12,
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

Figure15 <- plot_grid(title, col.load.concPlot, ncol=1, rel_heights = c(0.1, 1))

Figure15

ggsave(path = "Figures", filename ="Figure15.jpeg", width = 15, height = 18, units = "cm")


#......................................PHYTOPLANKTON............................................................................#

#select data 
Phyto <- metadata %>% 
  select(Date, Year, Month, Chl.a, Phyto_H, Cyano_RB)
Phyto$Year <- as.factor(Phyto$Year)
names(Phyto)
str(Phyto)
Phyto$Month <- ordered(Phyto$Month, 
                     levels=c("Jan", "Feb","Mar", 
                              "Apr","May", "Jun", 
                              "Jul","Aug","Sep",
                              "Oct","Nov", "Dec"))
cols <- c("Jan"="turquoise1","Feb"="turquoise2","Mar"="chartreuse",
          "Apr"="chartreuse1","May"="chartreuse2","Jun"="yellow",
          "Jul"="yellow1","Aug"="yellow2","Sep"="coral",
          "Oct"="coral1","Nov"="coral2","Dec"="turquoise")

#Chlorophyll-a temporal dynamics = Figure 16 - Chapter 1

chl.seas <- ggplot(Phyto, aes(x = Chl.a, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlim(-0.5,4)+
  xlab(~paste("Chlorophyll-a ", "(ugChl-a", "·L"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

chl.annual <- ggplot(Phyto, aes(x = Chl.a, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
 xlim(-0.5,4)+
  xlab(~paste("Chlorophyll-a ", "(ugChl-a", "·L"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


ChloroPlot <- plot_grid(chl.seas, chl.annual,
                       align="hv", axis="tblr", ncol = 2, labels =c("a","b"))
title <- ggdraw() + 
  draw_label("Chlorophyll-a - temporal dynamics (2007-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

Figure16 <- plot_grid(title, ChloroPlot, ncol=1, rel_heights = c(0.1, 1))

ggsave(path = "Figures", filename ="Figure16.jpeg", width = 18, height = 12, units = "cm")


#Phyto H and cyano RB temporal dynamics = Figure 18 - Chapter 1

a <- ggplot(Phyto, aes(x =Phyto_H, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Species diversity") +
  scale_fill_manual(values=cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

b <- ggplot(Phyto, aes(x = Phyto_H, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Species diversity") +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

c <- ggplot(Phyto, aes(x =Cyano_RB, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Cyanobacteria_RB (%)") +
  scale_fill_manual(values=cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

d <- ggplot(Phyto, aes(x = Cyano_RB, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Cyanobacteria_RB (%)") +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


phytoPlot <- plot_grid(a, c,
                       b, d,
                     align="hv", axis="tblr", ncol = 2, labels =c("a","c","b","d"))
title <- ggdraw() + 
  draw_label("Phytoplankton H & cyanobacteria relative biovolumes - temporal dynamics (2012-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0,
             size = 12
  ) +
  theme(plot.margin = (margin = ggplot2::margin(0, 0, 0, 7)))

Figure18 <- plot_grid(title, phytoPlot, ncol=1, rel_heights = c(0.1, 1))
Figure18

ggsave(path = "Figures", filename ="Figure18.jpeg", width = 18, height = 24, units = "cm")





#......................................ZOOPLANKTON............................................................................#

#select data 
Zoo <- metadata %>% 
  select(Date, Year, Month, Zoo_TB, Zoo_H, Zoo_SizeD, copB.claB)
Zoo$Year <- as.factor(Zoo$Year)
names(Zoo)
str(Zoo)
Zoo$Month <- ordered(Zoo$Month, 
          levels=c("Jan", "Feb","Mar", 
                   "Apr","May", "Jun", 
                   "Jul","Aug","Sep",
                   "Oct","Nov","Dec"))
cols <- c("Jan"="turquoise1","Feb"="turquoise2","Mar"="chartreuse",
          "Apr"="chartreuse1","May"="chartreuse2","Jun"="yellow",
          "Jul"="yellow1","Aug"="yellow2","Sep"="coral",
          "Oct"="coral1","Nov"="coral2","Dec"="turquoise")

#Zooplankton total biomass temporal dynamics = Figure 20 - Chapter 1

a <- ggplot(Zoo, aes(x = Zoo_TB, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("Zooplankton_TB ", "(ugDW", "·L"^-1, ")")) +
  scale_fill_manual(values=cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


b <- ggplot(Zoo, aes(x = Zoo_TB, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab(~paste("Zooplankton_TB ", "(ugDW", "·L"^-1, ")")) +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


ZooPlot <- plot_grid(a, b,
                        align="hv", axis="tblr", ncol = 2, labels =c("a","b"))

title <- ggdraw() + 
  draw_label("Zooplankton biomass - temporal dynamics (2004-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = (margin = ggplot2::margin(0, 0, 0, 7)))

Figure20 <- plot_grid(title, ZooPlot , ncol=1, rel_heights = c(0.1, 1))
Figure20

ggsave(path = "Figures", filename ="Figure20.jpeg", width = 18, height = 12, units = "cm")


#Zooplankton size and species diversity temporal dynamics = Figure 23 - Chapter 1
a <- ggplot(Zoo, aes(x = Zoo_SizeD, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Size Diversity") +
  scale_fill_manual(values=cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

b <- ggplot(Zoo, aes(x = Zoo_SizeD, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Size Diversity") +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

c <- ggplot(Zoo, aes(x = Zoo_H, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month)),alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Shannon Diversity") +
  scale_fill_manual(values=cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")

d <- ggplot(Zoo, aes(x = Zoo_H, y = Year, fill = stat(y))) +
  geom_density_ridges(alpha = 0.7,
                      jittered_points = TRUE, 
                      position=position_points_jitter(width=0.05, height = 0),
                      point_shape = '|', point_size = 2, point_alpha = 1,
                      point_color = "black") +
  xlab("Shannon Diversity") +
  scale_fill_gradient(low = "white", high = "grey10")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")


zooPlot <- plot_grid(a, c,
                       b, d,
                       align="hv", axis="tblr", ncol = 2, labels =c("a","c","b","d"))
title <- ggdraw() + 
  draw_label("Zooplankton size & species diversity - temporal dynamics (20040-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0,
             size = 12
  ) +
  theme(plot.margin = (margin = ggplot2::margin(0, 0, 0, 7)))

Figure23 <- plot_grid(title, zooPlot, ncol=1, rel_heights = c(0.1, 1))
Figure23

ggsave(path = "Figures", filename ="Figure23.jpeg", width = 18, height = 24, units = "cm")



#......................................ZOOPLANKTON GROWTH RATES...........................................................................#
#select data 
GR <- metadata %>% 
  select(Date, Dap_GR, Diaph_GR, Cala_GR, Nau_GR, Cyl_GR, Cerio_GR, Bosm_GR)
GR.pivot <- GR %>% 
  pivot_longer(-Date,
               names_to = "Taxa", 
               values_to = "GR")
GR.pivot$Taxa <- ordered(GR.pivot$Taxa, 
                         levels=c("Nau_GR","Cala_GR", "Cyl_GR",
                                  "Dap_GR","Diaph_GR","Cerio_GR", "Bosm_GR"))
taxa.cols <- c("Nau_GR"="yellow","Cala_GR"="green3","Cyl_GR"="yellow4",
               "Dap_GR"="blue","Diaph_GR"="turquoise1","Cerio_GR"="red",
               "Bosm_GR"="grey30")
a <- ggplot(GR.pivot, aes(x = GR, y = Taxa, fill = stat(y))) +
  geom_density_ridges_gradient(aes(fill=as.factor(Taxa))) +
  xlab(~paste("Growth Rate ", "(day"^-1, ")")) +
  scale_fill_manual(values=taxa.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")
a


#select data 
GRseason <- metadata %>% 
  select(Date, Month, Dap_GR, Diaph_GR, Cala_GR, Nau_GR, Cyl_GR, Cerio_GR, Bosm_GR)

GRseason$Month <- ordered(GRseason$Month, 
                          levels=c("Jan", "Feb","Mar", 
                                   "Apr","May", "Jun", 
                                   "Jul","Aug","Sep",
                                   "Oct","Nov", "Dec"))
month.cols <- c("Jan"="turquoise1","Feb"="turquoise2","Mar"="chartreuse",
                "Apr"="chartreuse1","May"="chartreuse2","Jun"="yellow",
                "Jul"="yellow1","Aug"="yellow2","Sep"="coral",
                "Oct"="coral1","Nov"="coral2","Dec"="turquoise")
b.cols <- c("Jan"="grey80","Feb"="yellow","Mar"="grey80",
            "Apr"="grey80","May"="grey80","Jun"="grey80",
            "Jul"="grey80","Aug"="yellow","Sep"="grey80",
            "Oct"="grey80","Nov"="grey80","Dec"="grey80")


b <- ggplot(GRseason, aes(x = Nau_GR, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month))) +
  xlim(-0.14,0.1)+
  xlab(~paste("Nauplii GR ", "(day"^-1, ")")) +
  scale_fill_manual(values=b.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")
b

c.cols <- c("Jan"="grey80","Feb"="grey80","Mar"="grey80",
            "Apr"="green3","May"="green3","Jun"="grey80",
            "Jul"="grey80","Aug"="grey80","Sep"="grey80",
            "Oct"="grey80","Nov"="grey80","Dec"="grey80")
c <- ggplot(GRseason, aes(x = Cala_GR, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month))) +
  xlab(~paste("Calanoids GR ", "(day"^-1, ")")) +
  scale_fill_manual(values=c.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")
c

d.cols <- c("Jan"="grey80","Feb"="grey80","Mar"="grey80",
            "Apr"="yellow4","May"="yellow4","Jun"="grey80",
            "Jul"="grey80","Aug"="grey80","Sep"="grey80",
            "Oct"="grey80","Nov"="grey80","Dec"="grey80")

d <- ggplot(GRseason, aes(x = Cyl_GR, y = Month, fill = stat(y))) +
  geom_density_ridges(aes(fill=as.factor(Month))) +
  xlab(~paste("Cyclopoids GR ", "(day"^-1, ")")) +
  scale_fill_manual(values=d.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")

d

e.cols <- c("Jan"="grey80","Feb"="grey80","Mar"="blue",
            "Apr"="grey80","May"="grey80","Jun"="grey80",
            "Jul"="grey80","Aug"="grey80","Sep"="grey80",
            "Oct"="grey80","Nov"="blue","Dec"="grey80")
e <- ggplot(GRseason, aes(x = Dap_GR, y = Month, fill = stat(y))) +
  geom_density_ridges_gradient(aes(fill=as.factor(Month))) +
  xlab(~paste("Daphnia GR ", "(day"^-1, ")")) +
  scale_fill_manual(values=e.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")
e

f.cols <- c("Jan"="grey80","Feb"="grey80","Mar"="grey80",
            "Apr"="grey80","May"="turquoise1","Jun"="turquoise1",
            "Jul"="grey80","Aug"="turquoise1","Sep"="grey80",
            "Oct"="grey80","Nov"="grey80","Dec"="grey80")
f <- ggplot(GRseason, aes(x = Diaph_GR, y = Month, fill = stat(y))) +
  geom_density_ridges_gradient(aes(fill=as.factor(Month))) +
  xlab(~paste("Diaphanosoma GR ", "(day"^-1, ")")) +
  scale_fill_manual(values=f.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")
f

g.cols <- c("Jan"="grey80","Feb"="grey80","Mar"="grey80",
            "Apr"="grey80","May"="grey80","Jun"="grey80",
            "Jul"="grey80","Aug"="grey80","Sep"="grey80",
            "Oct"="red","Nov"="red","Dec"="grey80")
g <- ggplot(GRseason, aes(x = Cerio_GR, y = Month, fill = stat(y))) +
  geom_density_ridges_gradient(aes(fill=as.factor(Month))) +
  xlab(~paste("Ceriodaphnia GR ", "(day"^-1, ")")) +
  scale_fill_manual(values=g.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")
g

h.cols <- c("Jan"="grey80","Feb"="grey80","Mar"="grey80",
            "Apr"="grey30","May"="grey80","Jun"="grey80",
            "Jul"="grey80","Aug"="grey80","Sep"="grey80",
            "Oct"="grey30","Nov"="grey80","Dec"="grey80")
h <- ggplot(GRseason, aes(x = Bosm_GR, y = Month, fill = stat(y))) +
  geom_density_ridges_gradient(aes(fill=as.factor(Month))) +
  xlab(~paste("Bosmina GR ", "(day"^-1, ")")) +
  scale_fill_manual(values=h.cols)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_blank(), legend.position = "none")+
  geom_vline(xintercept=0, linetype=1, size=0.5, colour="black")
h


GRPlot <- plot_grid(a, b,
                    c, d,
                    e, f,
                    g, h,
                    align="hv", axis="tblr", ncol = 4, labels =c("a","b","c","d","e","f", "g", "h"))
title <- ggdraw() + 
  draw_label("Seasonal zooplankton taxa growth rates (2004-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = (margin = ggplot2::margin(0, 0, 0, 7)))


Figure22 <- plot_grid(title, GRPlot, ncol=1, rel_heights = c(0.1, 1))
Figure22

ggsave(path = "Figures", filename ="Figure22.jpeg", width = 28, height = 20, units = "cm")

