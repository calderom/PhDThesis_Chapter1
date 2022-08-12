library(ggplot2) #nice plots
library (TimeSeries) #for timeseries
library(lubridate) #for timeseries
library(scales) #labels and axis
library(cowplot) #nice figures
library(gridExtra) #to plot more than 1 plot in the same figure
library(zoo) #for timeseries
library(gridGraphics) #to plot more than 1 plot in the same figure
library(corrplot) #correlograms
library(tidyverse) #manipulate datasets

#input file
metadata <- read.csv("Data/longterm_metadata.csv", stringsAsFactors = T)
metadata$Date <- as.Date(metadata$Date,"%d/%m/%Y")
names(metadata)


#FIGURE S1 - THESIS CHAPTER 1

#select weather data
weather <- metadata %>% 
  select(Date, AirTmax, AirTmin, Rain, WindSpeed, SolRad, Year, Month, DOY)

#corrplot between weather variables
dat_corrplot <- weather %>% 
  select(-Date, -Year, -Month, -DOY)
dat_corrplot <- dat_corrplot[complete.cases(dat_corrplot),] #remove NA rows
M<-cor(dat_corrplot)
head(round(M,2))
corrplot(M, method="number", tl.col="black", type="lower")
corrplot(M, method="color", tl.col="black", type="lower")
corrplot.mixed(M,upper = "circle", tl.col="black")

#weather data plot
AirT <- ggplot() +
  geom_line(data = weather, aes(x=Date, y = AirTmax), colour ="grey10", size = 0.5) +
  geom_line(data = weather, aes(x=Date, y = AirTmin), colour ="grey60", size = 0.5) +
  ylab("Air temperature (ºC)") +
  #ylim(0,25)+
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank())+
  geom_hline(yintercept=0, linetype=2, size=0.5, colour="black")
A <- AirT + annotate("rect",
                     xmin = as.Date("2006-06-01"), xmax = as.Date("2006-08-31"), 
                     ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2008-06-01"), xmax = as.Date("2008-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2009-07-02")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2009-11-01"), xmax = as.Date("2009-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2009-12-01"), xmax = as.Date("2010-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2010-11-01"), xmax = as.Date("2010-12-31"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-07-01"), xmax = as.Date("2013-07-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-12-01"), xmax = as.Date("2014-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5)+
  annotate("rect",
           xmin = as.Date("2015-11-01"), xmax = as.Date("2015-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2017-09-16")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2018-02-28"), xmax = as.Date("2018-03-04"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2018-06-01"), xmax = as.Date("2018-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5)+
  geom_vline(xintercept=as.numeric(as.Date("2018-06-14")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2020-04-01"), xmax = as.Date("2020-06-30"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) 

RainPlot <- ggplot(data = weather, aes(x=Date, y = Rain)) +
  geom_bar(stat = "identity", color = "grey10", width=2) +
  ylab("Precipitation (mm)") +
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank())
B <- RainPlot + annotate("rect",
                         xmin = as.Date("2006-06-01"), xmax = as.Date("2006-08-31"), 
                         ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2008-06-01"), xmax = as.Date("2008-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2009-07-02")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2009-11-01"), xmax = as.Date("2009-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2009-12-01"), xmax = as.Date("2010-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2010-11-01"), xmax = as.Date("2010-12-31"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-07-01"), xmax = as.Date("2013-07-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-12-01"), xmax = as.Date("2014-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5)+
  annotate("rect",
           xmin = as.Date("2015-11-01"), xmax = as.Date("2015-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2017-09-16")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2018-02-28"), xmax = as.Date("2018-03-04"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2018-06-01"), xmax = as.Date("2018-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5)+
  geom_vline(xintercept=as.numeric(as.Date("2018-06-14")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2020-04-01"), xmax = as.Date("2020-06-30"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5)
B

weather$m.av.wind<-rollmean(weather$WindSpeed, 30,fill = list(NA, NULL, NA))
str(weather)
wind <- ggplot() +
  geom_line(data = weather, aes(x=Date, y = WindSpeed), colour ="grey50", size = 0.5) +
  geom_line(data = weather, aes(x=Date, y = m.av.wind), colour ="black", size = 1) +
  ylab(~paste("Wind speed ", "(m", "·s"^-1, ")")) +
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank())
C <- wind + annotate("rect",
                     xmin = as.Date("2006-06-01"), xmax = as.Date("2006-08-31"), 
                     ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2008-06-01"), xmax = as.Date("2008-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2009-07-02")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2009-11-01"), xmax = as.Date("2009-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2009-12-01"), xmax = as.Date("2010-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2010-11-01"), xmax = as.Date("2010-12-31"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-07-01"), xmax = as.Date("2013-07-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-12-01"), xmax = as.Date("2014-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5)+
  annotate("rect",
           xmin = as.Date("2015-11-01"), xmax = as.Date("2015-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2017-09-16")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2018-02-28"), xmax = as.Date("2018-03-04"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2018-06-01"), xmax = as.Date("2018-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5)+
  geom_vline(xintercept=as.numeric(as.Date("2018-06-14")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2020-04-01"), xmax = as.Date("2020-06-30"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5)


weather$m.av.solar<-rollmean(weather$SolRad, 30,fill = list(NA, NULL, NA))
str(weather)
solarR <- ggplot() +
  geom_line(data = weather, aes(x=Date, y = SolRad), colour ="grey50", size = 0.5) +
  geom_line(data = weather, aes(x=Date, y = m.av.solar), colour ="black", size = 1) +
  ylab(~paste("Solar radiation ", "(W", "·m"^-2, ")")) +
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(text = element_text(size = 10), panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank())
D <- solarR + annotate("rect",
                       xmin = as.Date("2006-06-01"), xmax = as.Date("2006-08-31"), 
                       ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2008-06-01"), xmax = as.Date("2008-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2009-07-02")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2009-11-01"), xmax = as.Date("2009-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2009-12-01"), xmax = as.Date("2010-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2010-11-01"), xmax = as.Date("2010-12-31"), 
           ymin = -Inf, ymax = Inf,  fill = "cyan", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-07-01"), xmax = as.Date("2013-07-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-12-01"), xmax = as.Date("2014-02-28"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5)+
  annotate("rect",
           xmin = as.Date("2015-11-01"), xmax = as.Date("2015-11-30"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  geom_vline(xintercept=as.numeric(as.Date("2017-09-16")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2018-02-28"), xmax = as.Date("2018-03-04"), 
           ymin = -Inf, ymax = Inf,  fill = "blue", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2018-06-01"), xmax = as.Date("2018-08-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5)+
  geom_vline(xintercept=as.numeric(as.Date("2018-06-14")), linetype=4, size=1, colour="blue")+
  annotate("rect",
           xmin = as.Date("2020-04-01"), xmax = as.Date("2020-06-30"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5)

#final plot
weatherPLOT <- plot_grid(A, 
                         B, 
                         C, 
                         D, 
                         align = "v", ncol = 1, labels =c("a","b","c", "d"))
weatherPLOT

title <- ggdraw() + 
  draw_label("Weather data (2004-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))
plot_grid(title, weatherPLOT, ncol=1, rel_heights = c(0.1, 1))

#save figure as image 
ggsave("Figures/FigureS1.jpeg", width = 28, height = 18, units = "cm")


