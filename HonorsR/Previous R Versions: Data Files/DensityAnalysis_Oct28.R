#Density analysis between sites MPA, PVR, and NR
#OCT28, 2023
# Set working directory
setwd("/Users/alyssaplayer/Desktop/Honors/HonorsR")

# Require packages
library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lubridate")


#### Species Info ####
# Read data set that contains means between replicates
data_PV <- read.csv("PV_Stars_Urchins2023-10-26.csv", check.names = F)
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
             "Pisaster giganteus", 
             "Apostichopus parvimensis",
             "Apostichopus californicus")

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
  ) %>%
  na.omit()

cat_colours <- c("darkseagreen", "bisque4", "cadetblue3")


# Create a plot for Species density by year before and after SSWS
# ggsave(file="SpeciesDensity_Year.png", units = "in", width = 7, height = 3.5, res = 450)
# ggplot(data_PV, aes(x = Year, y = Density_100m2, color = Category)) +
#   geom_line(position = position_dodge(width = 0.5)) +
#   scale_color_manual(values = cat_colours)+
#   geom_vline(xintercept = 2020, linetype = "dashed", color = "darkseagreen", alpha = 0.8) + #Adds dashed line indicating completed construction of PVR
#   theme_gray()+
#   theme(axis.line = element_line(color = "black"),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank())+
#   xlab("Year")+
#   ylab("Density (per 100 m^2)")+
#   facet_wrap(~ Species, scales = "free_y")+
#   annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
#            alpha = .1,fill = "sandybrown")

#Adds rectangle highlighting wasting event
#dev.off()

#Used for poster figures 
# pisgig <- data_PV %>%
#   filter(Species=="Pisaster giganteus", Year >= 2008)

#################STARS
#SPECIES: PISASTER GIGANTEUS
avg_pisgig <- data_PV %>%
  filter(Species == "Pisaster giganteus", Year >= 2008) %>%
  group_by(Species, Year, Category) %>%
  dplyr::summarise(avg_density_year_site = mean(Density_100m2))

ggplot(avg_pisgig, aes(x = Year, y = avg_density_year_site, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "cadetblue3", alpha = 0.8)+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "darkseagreen", alpha = 0.8)+#Adds dashed line indicating completed construction of PVR
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Avg Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "sandybrown")
ggsave(file="PisGigSpeciesDensity_Year.png", units = "in", width = 4, height = 3.5, dpi = 450)
dev.off()

#SPECIES: PISASTER OCHRACEOUS
avg_pisoch <- data_PV %>%
  filter(Species == "Pisaster ochraceus", Year >= 2008) %>%
  group_by(Species, Year, Category) %>%
  dplyr::summarise(avg_density_year_site = mean(Density_100m2))

ggplot(avg_pisoch, aes(x = Year, y = avg_density_year_site, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "cadetblue3", alpha = 0.8)+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "darkseagreen", alpha = 0.8)+ #Adds dashed line indicating completed construction of PVR
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Avg Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "sandybrown")
ggsave(file="PisOchSpeciesDensity_Year.png", units = "in", width = 4, height = 3.5, dpi = 450)
dev.off()

#SPECIES: PATIRIA MINIATA
avg_patmin <- data_PV %>%
  filter(Species == "Patiria miniata", Year >= 2008) %>%
  group_by(Species, Year, Category) %>%
  dplyr::summarise(avg_density_year_site = mean(Density_100m2))

ggplot(avg_patmin, aes(x = Year, y = avg_density_year_site, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "cadetblue3", alpha = 0.8)+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "darkseagreen", alpha = 0.8)+ #Adds dashed line indicating completed construction of PVR
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Avg Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "sandybrown")
ggsave(file="PatMinSpeciesDensity_Year.png", units = "in", width = 4, height = 3.5, dpi = 450)
dev.off()

##################URCHINS

#SPECIES: STRONGLYOCENTROTUS PURPURATUS
avg_strpur <- data_PV %>%
  filter(Species == "Strongylocentrotus purpuratus", Year >= 2008) %>%
  group_by(Species, Year, Category) %>%
  dplyr::summarise(avg_density_year_site = mean(Density_100m2))

ggplot(avg_strpur, aes(x = Year, y = avg_density_year_site, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "cadetblue3", alpha = 0.8)+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "darkseagreen", alpha = 0.8)+
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Avg Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "sandybrown")

ggsave(file="StrPurSpeciesDensity_Year.png", units = "in", width = 4, height = 3.5, dpi = 450)
dev.off()

#SPECIES: MESOCENTROTUS FRANCISANUS
avg_mesfra <- data_PV %>%
  filter(Species == "Mesocentrotus franciscanus", Year >= 2008) %>%
  group_by(Species, Year, Category) %>%
  dplyr::summarise(avg_density_year_site = mean(Density_100m2))

ggplot(avg_mesfra, aes(x = Year, y = avg_density_year_site, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "cadetblue3", alpha = 0.8)+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "darkseagreen", alpha = 0.8)+
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Avg Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "sandybrown")
ggsave(file="MesFraSpeciesDensity_Year.png", units = "in", width = 4, height = 3.5, dpi = 450)
dev.off()

##################CUCUMBERS

#SPECIES: APOSTICHOPUS PARVIMENSIS
avg_apopar <- data_PV %>%
  filter(Species == "Apostichopus parvimensis", Year >= 2008) %>%
  group_by(Species, Year, Category) %>%
  dplyr::summarise(avg_density_year_site = mean(Density_100m2))

ggplot(avg_apopar, aes(x = Year, y = avg_density_year_site, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "cadetblue3", alpha = 0.8)+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "darkseagreen", alpha = 0.8)+
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Avg Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "sandybrown")
ggsave(file="ApoParSpeciesDensity_Year.png", units = "in", width = 4, height = 3.5, dpi = 450)
dev.off()

#SPECIES: APOSTICHOPUS CALIFORNICUS
avg_apocal <- data_PV %>%
  filter(Species == "Apostichopus californicus", Year >= 2008) %>%
  group_by(Species, Year, Category) %>%
  dplyr::summarise(avg_density_year_site = mean(Density_100m2))

ggplot(avg_apocal, aes(x = Year, y = avg_density_year_site, color = Category)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = cat_colours)+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "cadetblue3", alpha = 0.8)+
  geom_vline(xintercept = 2012, linetype = "dashed", color = "darkseagreen", alpha = 0.8)+
  theme_gray()+
  theme(axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Year")+
  ylab("Avg Density (per 100 m^2)")+
  facet_wrap(~ Species, scales = "free_y")+
  annotate("rect", xmin = 2013, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1,fill = "sandybrown")
ggsave(file="ApoCalSpeciesDensity_Year.png", units = "in", width =4, height = 3.5, dpi = 450)
dev.off()

