##Mean Density of focspp. by Depth Zone 
##Created Oct 4, 2024 

# Require packages
library("dplyr")
library("plyr")
library("tidyr")
library("ggplot2")
library("lubridate")

setwd("/Users/alyssaplayer/Desktop/AP VRG PVR 2024")

#### Species Info ####
# Read data set that contains means between replicates
data_PV <- read.csv("PV_Stars_Urchins_2024-09-27.csv", check.names = F)
colnames(data_PV)[colnames(data_PV)=="BenthicReefSpecies"] <- "Species"


# Create a value of Mean Density / Year (100m2)
data_PV <- data_PV %>%
  mutate(SampleYear = dmy(SampleYear),
         #Year = year(SampleYear),
         Density_100m2 = 100*Density_m2) %>%
  ungroup() %>% droplevels() %>%
  complete(nesting(Site, SampleYear), Species, fill = list(Density_m2=0, Density_100m2=0)) #Looks for grouped data of the nesting variables and if there is N/A, it inputs 0 for both density variables

# Focal species list
foc_spp <- c("Mesocentrotus franciscanus",
             "Strongylocentrotus purpuratus",
             "Patiria miniata", 
             "Pisaster ochraceus", 
             "Pisaster giganteus", 
             "Apostichopus parvimensis",
             "Apostichopus californicus")


data_PV <- data_PV %>%
  filter(Species %in% foc_spp, Year >= 2008)

# Create 'Before' and 'After' Dates for Construction of PVR and Wasting Disease
data_PV <- data_PV %>%
  mutate(Period = if_else(Year < 2020, "Before","After"),
         Wasting = if_else(Year < 2013, "Pre-Wasting", "Wasting"))