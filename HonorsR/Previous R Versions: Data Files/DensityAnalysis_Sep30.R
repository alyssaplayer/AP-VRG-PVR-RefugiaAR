#Density analysis between sites MPA, PVR, and NR
#SEPTEMBER 30, 2023
# Set working directory
setwd("/Users/alyssaplayer/Desktop/Honors/HonorsR")

# Require packages
library("dplyr")
library("plyr")
library("tidyr")
library("ggplot2")
library("lubridate")


#### Species Info ####
# Read data set that contains means between replicates
data_PV <- read.csv("PV_Stars_Urchins2023-09-20.csv", check.names = F)
colnames(data_PV)[colnames(data_PV)=="BenthicReefSpecies"] <- "Species"


# Add year and density per 100m2
data_PV <- data_PV %>%
  dplyr::mutate(SampleDate = dmy(SampleDate),
         Year = year(SampleDate),
         Density_100m2 = 100*Density_m2) %>%
  ungroup() %>% droplevels() %>%
  complete(nesting(Site, Year, SampleDate), Species, fill = list(Density_m2=0, Density_100m2=0)) #Looks for grouped data of the nesting variables and if there is N/A, it inputs 0 for both density variables

# Focal species list
foc_spp <- c("Mesocentrotus franciscanus",
             "Strongylocentrotus purpuratus",
             "Patiria miniata", 
             "Pisaster ochraceus", 
             "Pisaster giganteus")

pvr_impact_sites <- c("KOU Rock",
                      "Old 18th",
                      "Burial Grounds",
                      "Cape Point",
                      "3 Palms West",
                      "3 Palms East",
                      "Bunker Point")

MPA_impact_sites <- c("120 Reef",
                      "Abalone Cove Kelp West",
                      "Long Point East",
                      "Long Point West",
                      "Old Marineland",                 
                      "Point Vicente West",
                      "Portuguese Point")

control_sites <- c("Hawthorne Reef",
                   "Honeymoon Cove",
                   "Lunada Bay",
                   "Resort Point",  
                   "Rocky Point South",
                   "Rocky Point North",
                   "Ridges North")

data_PV <- data_PV %>%
  dplyr::filter(Species %in% foc_spp, Year >= 2008) 

# Create 'Before' and 'After' Dates for Construction of PVR and Wasting Disease
data_PV <- data_PV %>%
  mutate(Period = if_else(Year < 2020, "Before","After"),
         Wasting = if_else(Year < 2013, "Pre-Wasting", "Wasting"))

#Create categories for sites and filter the data set 
data_PV <- data_PV %>%
  mutate(
    Category = case_when(
      Site %in% pvr_impact_sites ~ "PVR",
      Site %in% MPA_impact_sites ~ "MPA",
      Site %in% control_sites ~ "NR",
    )
  )

cat_colours <- c("lightgreen", "lightblue", "bisque4")


# Create a plot for Species density by year before and after SSWS
#ggsave(file="SpeciesDensity_Year.png", units = "in", width = 7, height = 4, res = 300)
ggplot(data_PV, aes(x = Year, y = Density_100m2, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "darkseagreen", alpha = 0.8) + #Adds dashed line indicating completed construction of PVR
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "chocolate1")

#Adds rectangle highlighting wasting event
#dev.off()

#Used for poster figures 
pisgig <- data_PV %>%
  filter(Species=="Pisaster giganteus", Year >= 2008)


#ggsave(file="MESFRA.png", units = "in", width = 7, height = 4, res = 300)
ggplot(pisgig, aes(x = Year, y = Density_100m2, color = Category)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "palegreen4", alpha = 0.8) + #Adds dashed line indicating completed construction of PVR
  theme_gray()+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .3,fill = "chocolate1") #Adds rectangle highlighting wasting event
#dev.off()
