#CALCULATE LONG-TERM PHYTOPLANKTON SPECIES DIVERSITY

#PACKAGES REQUIRED
library(vegan)
library(tidyverse)


#input data
H <- read.csv('Data/phyto_diversity_input.csv', header = TRUE, sep = ";", stringsAsFactors = T)

#Shannon diversity
phyto_diversity_output <- diversity(H)


#save and export values to excel file
write.table(phyto_diversity_output, file="Data/phyto_diversity_output.csv", sep=";",row.names=F)

