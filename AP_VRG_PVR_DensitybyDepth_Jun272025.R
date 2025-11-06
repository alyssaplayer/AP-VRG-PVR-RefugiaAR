##Mean Density of focspp. by Depth Zone 
##Created Oct 4, 2024 

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
data_PVR <- read.csv("PV_Stars_Urchins2025-07-09.csv", check.names = F)
crane_data <-read.csv("PV_Stars_Urchins2025-11-05.csv", check.names = F)
data_PVR <- data_PVR %>%
  filter(DepthZone == "ARM")
data_PV <- rbind(data_PVR, crane_data)
  
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
              "Pisaster giganteus")
  
#used to have a. parvimensis, a. californicus but removed bc not relevant to findings


data_PV <- data_PV %>%
  filter(Species %in% foc_spp, Year >= 2011) %>%
  filter(DepthZone %in% c("Outer", "Deep", "ARM"))

data_PV %>%
  pull(Site) %>%
  unique()

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
        TRUE ~ "Other"  # To catch any species not listed
        ),
      FunctionalGroup = factor(FunctionalGroup, levels = c("Stars", "Urchins")) #defining the functional groups 
    )

#Creating the site groups 
data_PV <- data_PV %>%
  mutate(Site_Category = case_when(
    Site %in% c("Long Point East", 
                "120 Reef", 
                "Abalone Cove Kelp West",
                "Long Point West", 
                "Old Marineland", 
                "Point Vicente West",
                "Portuguese Point") ~ "MPA",
    DepthZone == "ARM" ~ "PVR",
    TRUE ~ "Non-MPA"
  ))


densitybydepth <- data_PV %>%
  group_by(DepthZone, Year, Species, Site, Era, Site_Category) %>%
  dplyr::summarise(DZ_Density_100m2=mean(Density_100m2)) %>%
  mutate(Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE))

density_stars <- densitybydepth %>%
  filter(Species %in% c("Patiria miniata", "Pisaster ochraceus", "Pisaster giganteus"))

densitybydepthplot_stars <- ggplot(density_stars, aes(x = Era, y = log(DZ_Density_100m2), color = Era)) +
  geom_boxplot(outlier.shape = NA, aes(fill=Era), alpha = 0.4) + # Set outlier.shape inside geom_boxplot(), alpha makes transparency so the data points are visible
  geom_point(aes(color = Era),
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.5),
             size = 0.7)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Site Type", 
       y = expression("log of Mean Density (100" * m^2 * " / yr)"))+
  facet_grid(rows = vars(Species), cols = vars(Site_Category), scales = "free_y", space = 'free') +
  scale_color_brewer(palette="Blues")+
  scale_fill_brewer(palette="Blues")


print(densitybydepthplot_stars)

density_urchins <- densitybydepth %>%
  filter(Species %in% c("Mesocentrotus franciscanus", "Strongylocentrotus purpuratus"))
         
densitybydepthplot_urchins <- ggplot(density_urchins, aes(x = Era, y = log(DZ_Density_100m2), color = Era)) +
  geom_boxplot(outlier.shape = NA, aes(fill=Era), alpha = 0.4) + # Set outlier.shape inside geom_boxplot(), alpha makes transparency so the data points are visible
  geom_point(aes(color = Era),
                      position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.5),
                      size = 0.7)+
           theme_classic()+
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           labs(x = "Site Type", 
                y = expression("Log of Mean Density (100" * m^2 * " / yr)"))+
           facet_grid(rows = vars(Species), cols = vars(Site_Category), scales = "free_y", space = 'free') +
           scale_color_brewer(palette="Oranges") +
           scale_fill_brewer(palette="Oranges")
         

print(densitybydepthplot_urchins)


#### STATISTICS ####
#goal is test if the difference in means is significant between MPA, non-MPA and PVR post-wasting 

postwaste_groups <- data_PV %>%
  filter(Era == c("Post-Wasting Recovery"), Species == c("Mesocentrotus franciscanus")) %>%
  group_by(Year, Species, Site, Site_Category) %>%
  dplyr::summarise(DZ_Density_100m2=mean(Density_100m2))
#do a for loop or pipe with group-by / mutate


leveneTest(DZ_Density_100m2 ~ Site_Category, data = postwaste_groups)
#if the variance is equal between Era by DZ by Species

# 2. Fit the two-way ANOVA model
anova_model_era <- aov(DZ_Density_100m2 ~ Site_Category, data = postwaste_groups)

# Summary of ANOVA results
summary(anova_model_era)


# 3. Check normality of residuals (useful for assumptions)
# Plotting residuals to inspect
par(mfrow = c(1, 2))
plot(anova_model_era, which = 1)  # R

#non-parametric of one-way ANOVA
kruskal.test(DZ_Density_100m2 ~ Site_Category, data = postwaste_groups)

#residuals will show any unexplained variance
#try a qqplot - plotting quantiles against each other 

# 
# #new stats test: 
# TukeyHSD(anova_model_era, which = "Era")

# this function can run a one-way ANOVA with a Welch correction if variances not equal (like corrected version of 2 sample t-test)
oneway.test(DZ_Density_100m2 ~ Site_Category, data = postwaste_groups)

#try my stats test
