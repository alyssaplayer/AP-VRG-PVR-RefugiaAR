##Mean Density of Species (as a whole over time)
##Created July 13, 2024 

# Require packages
library("dplyr")
library("plyr")
library("tidyr")
library("ggplot2")
library("lubridate")
library("ggh4x")
library("broom")
library("AICcmodavg")
library("car")

#setwd("/Users/alyssaplayer/Desktop/AP VRG PVR 2024")

#### Species Info ####
# Read data set that contains means between replicates
data_PV <- read.csv("PV_Stars_Urchins2025-07-09.csv", check.names = F)
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
  filter(DepthZone !="Outer Middle") #Removes outermiddle as a zone

data_PV <- data_PV %>%
  mutate(Era = case_when(
    Year < 2014 ~ "Pre-Wasting",
    Year >= 2014 & Year <= 2016 ~ "Wasting Event",
    Year > 2016 ~ "Post-Wasting Recovery"),
    #Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE),#I added this change to the densitybydepth function and now it's correctly being ordered in the plot
    DepthZone = factor(DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"), ordered = TRUE)
  )


data_PV <- data_PV %>%
  mutate(
    FunctionalGroup = case_when(
      Species %in% c("Patiria miniata", "Pisaster ochraceus", "Pisaster giganteus") ~ "Stars",
      Species %in% c("Mesocentrotus franciscanus", "Strongylocentrotus purpuratus") ~ "Urchins",
      Species %in% c("Apostichopus californicus", "Apostichopus parvimensis") ~ "Cucumbers",
      TRUE ~ "Other"  # To catch any species not listed
    ),
    FunctionalGroup = factor(FunctionalGroup, levels = c("Stars", "Urchins", "Cucumbers")) #defining the functional groups 
  )

# #Creating the site groups (More necessary for the BACIPS analysis, but keeping in for good measure 7/12/25)
# pvr_control_sites <- c(#"Hawthorne Reef",
#   "Honeymoon Cove",
#   "Lunada Bay",
#   #"Resort Point",  
#   "Rocky Point South",
#   "Rocky Point North",
#   "Ridges North")
# 
# 
# pvr_impact_sites <- c("KOU Rock",
#                       "Old 18th",
#                       "Burial Grounds",
#                       "Cape Point",
#                       "3 Palms West",
#                       "3 Palms East",
#                       "Bunker Point")
# 
# MPA_control_sites <- c(#"Hawthorne Reef",
#   "Marguerite Central",
#   "Albondigas",
#   "Marguerite East",  
#   "Golden Cove",
#   #"Resort Point",
#   "Portuguese Bend")
# 
# MPA_impact_sites <- c("Long Point East",
#                       "120 Reef",
#                       "Abalone Cove Kelp West",
#                       "Long Point West",
#                       "Old Marineland",                 
#                       "Point Vicente West",
#                       "Portuguese Point")
# 
# 
# both_control_sites <- c("Hawthorne Reef",
#                         "Resort Point") 

###THIS IS FROM HONORS THESIS, unnecessary for this plot
# data_PV <- data_PV %>%
#   mutate(SiteType = case_when(
#     Site %in% pvr_control_sites ~ 'Naturally Occuring Palos Verdes Sites',
#     Site %in% pvr_impact_sites ~ 'Sites Adjacent to Palos Verdes Reef',
#     Site %in% MPA_control_sites ~ 'Naturally Occuring MPA Sites',
#     Site %in% MPA_impact_sites ~ 'Sites Within the Marine Protected Area',
#     TRUE ~ 'Other'  # This will capture any site that doesn't fall into the above categories
#   ))

#Statistical Tests:
##STASTICAL TEST
leveneTest(Density_100m2 ~ DepthZone * Species, data = data_PV)

# 2. Fit the two-way ANOVA model
anova_model <- aov(Density_100m2 ~ DepthZone * Species, data = data_PV)

# Summary of ANOVA results
summary(anova_model)

# 3. Check normality of residuals (useful for assumptions)
# Plotting residuals to inspect
par(mfrow = c(1, 2))
plot(anova_model, which = 1)  # R


speciesdensityplot <- ggplot(data_PV, aes(x = Era, y = log(Density_100m2), fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~DepthZone) +
  facet_wrap(~ FunctionalGroup, nrow = 1, ncol = 3) +  # Adjust based on your functional groups
  #scale_fill_brewer() +
  guides(fill = guide_legend(
    position = "top",
    theme = theme(
      legend.text = element_text(size = 9, face = "italic", margin = margin(l = 0)),
      legend.key.spacing.x = unit(2, "pt"),
      legend.key.spacing.y = unit(0, "pt")
    ))) +
  labs(x = "Era", y = bquote("Mean Density / 100 m"^2)) +
  # facet_nested(~ SiteType + DepthZone,scales = "free_x",  space='free') + 
  theme_classic() +
  #scale_x_discrete(labels = c("Palos Verdes Reference" = 'PV Ref', "MPA" = 'MPA', "MPA Control Sites" = "MPA Con", "Palos Verdes Adjacent" = "PVR Adj")) +
  theme(panel.spacing.x = unit(0.1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


print(speciesdensityplot)
