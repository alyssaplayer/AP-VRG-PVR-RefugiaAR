##Species Density by Site/Era
##Created Oct 18, 2024 

# Require packages
library("dplyr")
library("plyr")
library("tidyr")
library("ggplot2")
library("lubridate")
library("ggh4x")

#### Dataset Filtering - Keep the Same for Each Response ####
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
  filter(Species %in% foc_spp, Year >= 2008)

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

data_PV <- data_PV %>%
  mutate(SiteType = case_when(
    Site %in% pvr_control_sites ~ 'Palos Verdes Reference',
    Site %in% pvr_impact_sites ~ 'Palos Verdes Adjacent',
    Site %in% MPA_control_sites ~ 'MPA Reference',
    Site %in% MPA_impact_sites ~ 'MPA',
    TRUE ~ 'Other'  # This will capture any site that doesn't fall into the above categories
  ))



data_PV <- data_PV %>%
  mutate(
    Period = if_else(Year < 2020, "Before", "After"),
    Wasting = if_else(Year < 2013, "Pre-Wasting", "Wasting"),
    Era = case_when(
      Year < 2014 ~ "Pre-Wasting",
      Year >= 2014 & Year <= 2016 ~ "Wasting Event",
      TRUE ~ "Post-Wasting Recovery"),
    Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"))  # Set factor levels here
  )

data_PV <- data_PV %>%
  mutate(
    Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE),
    DepthZone = factor(DepthZone, levels = c("Inner", "Middle", "Outer Middle", "Deep", "ARM"), ordered = TRUE) 
  )

data_PV <- data_PV %>%
  mutate(
    FunctionalGroup = case_when(
      Species %in% c("Patiria miniata", "Pisaster ochraceus", "Pisaster giganteus") ~ "Stars",
      Species %in% c("Mesocentrotus franciscanus", "Strongylocentrotus purpuratus") ~ "Urchins",
      Species %in% c("Apostichopus californicus", "Apostichopus parvimensis") ~ "Cucumbers",
      TRUE ~ "Other"  # To catch any species not listed
    ),
    FunctionalGroup = factor(FunctionalGroup, levels = c("Stars", "Urchins", "Cucumbers"))
  )

data_PV <- data_PV %>%
  filter(!is.na(DepthZone))

# stacked bar plot of species production by site, era, depth zone

#GOT SPECIES TO BE FACET WRAPPED
ggplot(data_PV, aes(x = Era, y = Density_100m2, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~DepthZone) +
  facet_wrap(~ FunctionalGroup, nrow = 1, ncol = 3) +  # Adjust based on your functional groups
  scale_fill_brewer() +
  guides(fill = guide_legend(
    position = "top",
    theme = theme(
      legend.text = element_text(size = 9, face = "italic", margin = margin(l = 0)),
      legend.key.spacing.x = unit(2, "pt"),
      legend.key.spacing.y = unit(0, "pt")
    ))) +
  labs(x = "Era", y = bquote("Mean Density / 100 m"^2)) +
  facet_nested(~ SiteType + DepthZone,scales = "free_x",  space='free') + 
  theme_classic() +
  scale_x_discrete(labels = c("Palos Verdes Reference" = 'PV Ref', "MPA" = 'MPA', "MPA Control Sites" = "MPA Con", "Palos Verdes Adjacent" = "PVR Adj")) +
  theme(panel.spacing.x = unit(0.1, "lines"),
  axis.text.x = element_text(angle = 45, hjust = 1)) 


