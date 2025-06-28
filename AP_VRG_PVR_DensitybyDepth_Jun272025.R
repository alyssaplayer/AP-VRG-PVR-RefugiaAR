##Mean Density of focspp. by Depth Zone 
##Created Oct 4, 2024 

# Require packages
library("dplyr")
library("plyr")
library("tidyr")
library("ggplot2")
library("lubridate")

#setwd("/Users/alyssaplayer/Desktop/AP VRG PVR 2024")

#### Species Info ####
# Read data set that contains means between replicates
data_PV <- read.csv("PV_Stars_Urchins_2024-10-11.csv", check.names = F)
colnames(data_PV)[colnames(data_PV)=="BenthicReefSpecies"] <- "Species"
colnames(data_PV)[colnames(data_PV)=="SampleYear"] <- "Year"

data_PV <- data_PV %>%
  mutate(Density_100m2=100*Density_m2) #Create a column that is Density per 100m2
  complete(nesting(Site, Year), Species, fill = list(Density_m2=0, Density_100m2=0)) 
  
# Focal species list
foc_spp <- c("Mesocentrotus franciscanus",
              "Strongylocentrotus purpuratus",
              "Patiria miniata", 
              "Pisaster ochraceus", 
              "Pisaster giganteus", 
              "Apostichopus parvimensis",
              "Apostichopus californicus")
  
data_PV <- data_PV %>%
  filter(Species %in% foc_spp, Year >= 2008) %>%
  filter(DepthZone !="Outer Middle")

data_PV <- data_PV %>%
  mutate(Period = if_else(Year < 2020, "Before", "After"),
         Wasting = if_else(Year < 2013, "Pre-Wasting", "Wasting"),
         Era = case_when(
           Year < 2014 ~ "Pre-Wasting",
           Year >= 2014 & Year <= 2016 ~ "Wasting Event",
           TRUE ~ "Post Wasting Recovery"))
  mutate(
    Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE),
    DepthZone = factor(DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"), ordered = TRUE) 
  )
  
pvr_control_sites <- c(#"Hawthorne Reef",
  "Honeymoon Cove",
  "Lunada Bay",
  #"Resort Point",  
  "Rocky Point South",
  "Rocky Point North",
  "Ridges North")


pvr_impact_sites <- c("KOU Rock",
                      "Old 18th",
                      "Burial Grounds",
                      "Cape Point",
                      "3 Palms West",
                      "3 Palms East",
                      "Bunker Point")

MPA_control_sites <- c(#"Hawthorne Reef",
  "Marguerite Central",
  "Albondigas",
  "Marguerite East",  
  "Golden Cove",
  #"Resort Point",
  "Portuguese Bend")

MPA_impact_sites <- c("Long Point East",
                      "120 Reef",
                      "Abalone Cove Kelp West",
                      "Long Point West",
                      "Old Marineland",                 
                      "Point Vicente West",
                      "Portuguese Point")

both_control_sites <- c("Hawthorne Reef",
                        "Resort Point") 
###THIS IS FROM HONORS THESIS
# data_PV <- data_PV %>%
#   mutate(SiteType = case_when(
#     Site %in% pvr_control_sites ~ 'Naturally Occuring Palos Verdes Sites',
#     Site %in% pvr_impact_sites ~ 'Sites Adjacent to Palos Verdes Reef',
#     Site %in% MPA_control_sites ~ 'Naturally Occuring MPA Sites',
#     Site %in% MPA_impact_sites ~ 'Sites Within the Marine Protected Area',
#     TRUE ~ 'Other'  # This will capture any site that doesn't fall into the above categories
#   ))

densitybydepth <- data_PV %>%
  group_by(DepthZone, Year, Species, Site, Era) %>%
  dplyr::summarise(DZ_Density_100m2=mean(Density_100m2))

densitybydepthplot <- ggplot(densitybydepth, aes(x = DepthZone, y = log(DZ_Density_100m2), color = Era)) +
  geom_boxplot(outlier.shape = NA) + # Set outlier.shape inside geom_boxplot()
  geom_point(aes(group = Era),
             #position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.5),
             size = 0.7)+
  # labs(x = "Depth Zone",
  #      y = expression(paste("Mean Density (100m ^{2}, ", " /yr)"))) +
  # scale_x_discrete(
  #   labels = c(
  #     "Inner" = "I",
  #     "Middle" = "M",
  #     "Outer" = "O",
  #     "Deep" = "D"
  #   )) +
  facet_grid(vars(Species, DepthZone)) +
  geom_crossbar(
    data = drop_na(as_tibble(data_PV)),
    aes(
      y = Density_100m2,
      ymin = 0,
      ymax = 500,
      group = Era,
      fill = Era,
      alpha = 0.4
    ),
  width = 0.4,
  position = position_dodge(width = 0.5),
  color = "black"
  )
  

print(densitybydepthplot)

