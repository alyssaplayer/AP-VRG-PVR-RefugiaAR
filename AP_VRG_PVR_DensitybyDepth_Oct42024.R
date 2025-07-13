##Mean Density of focspp. by Depth Zone 
##Created Oct 4, 2024 
##updated may 20, 2024

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
    Site %in% pvr_control_sites ~ 'PVR Ref',
    Site %in% pvr_impact_sites ~ 'PVR Adj',
    Site %in% MPA_control_sites ~ 'MPA Ref',
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

##STASTICAL TEST
leveneTest(Density_100m2 ~ DepthZone * Species, data = data_PV)

# 2. Fit the two-way ANOVA model
anova_model <- aov(Density_100m2 ~ DepthZone * Species, data = data_PV)

# Summary of ANOVA results
summary(anova_model)

# 3. Check normality of residuals (useful for assumptions)
# Plotting residuals to inspect
par(mfrow = c(1, 2))
plot(anova_model, which = 1)  # Residuals vs Fitted
plot(anova_model, which = 2)

densitybydepth <- ggplot(data_PV, aes(x = DepthZone, y = Density_100m2, color = Era)) +
  geom_boxplot(outlier.shape = NA) +  # Set outlier.shape inside geom_boxplot()
  geom_point(aes(group = Era),
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.5),
             size = 0.7)+
  labs(x = "Depth Zone",
       y = bquote("Mean Density / 100 m"^2)) +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange", 
               position = position_dodge(width = 0.5), 
               aes(group = Era), 
               color = "black", 
               size = 0.3) +
  scale_x_discrete(
    labels = c(
      "Inner" = "I",
      "Middle" = "M",
      "Outer" = "O",
      "Deep" = "D",
      "ARM" = "A",
      "Outer Middle"= "OM")) + 
  #Following facet_wraps are three variations of organising the plot
 facet_grid(rows = vars(FunctionalGroup), cols = vars(SiteType), scales = "free_y", space = 'free') + #Too squished on the y axis 
 #facet_grid(rows = vars(SiteType), cols = vars(FunctionalGroup), scales = "free_y", space = 'free') + #This could be the best, has the Sitetype on the left and then in columns grouped by species 
 #facet_wrap(~ SiteType + FunctionalGroup, scales = "free_y", ncol = 3) + #This one has the SiteType and the Group stacked on each other in columns
 scale_color_brewer(palette="Blues")

print(densitybydepth)

#print(densitybyspecies)

