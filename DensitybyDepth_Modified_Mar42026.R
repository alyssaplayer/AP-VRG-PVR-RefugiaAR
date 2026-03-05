##Mean Density of focspp. by Depth Zone 
##Created Oct 4, 2024 
#Modified March 4, 2026

# Require packages
library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lubridate")
library("ggh4x")
library("broom")
library("AICcmodavg")
library("car")
library("ggpubr")
library("rstatix")


#setwd("/Users/alyssaplayer/Desktop/AP VRG PVR 2024")

####DATA MANAGEMENT AND FILTERING####
#### Species Info ####
# Read data set that contains means between replicates
data_PVR <- read.csv("PV_Stars_Urchins2025-07-09.csv", check.names = F)
crane_data <- read.csv("PV_Stars_Urchins2025-11-05.csv", check.names = F)
data_PV <- rbind(data_PVR, crane_data)
write.csv(data_PV, "data_PV.csv", row.names = FALSE)

colnames(data_PV)[colnames(data_PV) == "BenthicReefSpecies"] <- "Species"
colnames(data_PV)[colnames(data_PV) == "SampleYear"] <- "Year"

data_PV <- data_PV %>%
  mutate(Density_100m2 = 100 * Density_m2) %>%
  complete(nesting(Site, Year), Species, fill = list(Density_m2 = 0, Density_100m2 = 0))


####Focal Species List####
foc_spp <- c("Mesocentrotus franciscanus",
             "Strongylocentrotus purpuratus",
             "Patiria miniata",
             "Pisaster ochraceus",
             "Pisaster giganteus")

data_PV <- data_PV %>%
  filter(Species %in% foc_spp, Year >= 2011) %>%
  filter(DepthZone %in% c("Outer", "Deep", "ARM"))

#Creating the Eras
data_PV <- data_PV %>%
  mutate(
    Era = case_when(
      Year < 2014 ~ "Pre-Wasting",
      Year >= 2014 & Year <= 2016 ~ "Wasting Event",
      Year > 2016 ~ "Post-Wasting Recovery"
    ),
    Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE),
    DepthZone = factor(DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"), ordered = TRUE)
  )

# Functional groups for the species
data_PV <- data_PV %>%
  mutate(
    FunctionalGroup = case_when(
      Species %in% c("Patiria miniata", "Pisaster ochraceus", "Pisaster giganteus") ~ "Stars",
      Species %in% c("Mesocentrotus franciscanus", "Strongylocentrotus purpuratus") ~ "Urchins",
      TRUE ~ "Other"
    ),
    FunctionalGroup = factor(FunctionalGroup, levels = c("Stars", "Urchins"))
  )

# Site categories (PVR Modules and PVR-Adj combined)
data_PV <- data_PV %>%
  mutate(Site_Category = case_when(
    Site %in% c("Long Point East", 
                "120 Reef",
                "Abalone Cove Kelp West",
                "Long Point West", 
                "Old Marineland", 
                "Point Vicente West",
                "Portuguese Point") ~ "MPA",
    Site %in% c("Ridges North", 
                "Honeymoon Cove", 
                "Resort Point",
                "Rocky Point North", 
                "Rocky Point South", 
                "Hawthorne Reef",
                "Lunada Bay") ~ "PVR-Control",
    Site %in% c("3 Palms East", 
                "3 Palms West", 
                "Bunker Point",
                "Burial Grounds", 
                "Cape Point", 
                "KOU Rock",
                "Old 18th") ~ "PVR&Adj",
    DepthZone == "ARM" ~ "PVR&Adj",
    TRUE ~ "Non-MPA"
  ))


####CREATING THE DENSITY PLOTS####
densitybydepth <- data_PV %>%
  group_by(DepthZone, Year, Species, Site, Era, Site_Category) %>%
  dplyr::summarise(DZ_Density_100m2 = mean(Density_100m2), .groups = "drop") %>%
  mutate(Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE))

density_stars <- densitybydepth %>%
  filter(Species %in% c("Patiria miniata", "Pisaster ochraceus", "Pisaster giganteus"))

densitybydepthplot_stars <- ggplot(density_stars, aes(x = Era, y = log(DZ_Density_100m2), color = Era)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Era), alpha = 0.4) +
  geom_point(aes(color = Era),
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.5),
             size = 0.7) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Site Type",
       y = expression("log of Mean Density (100" * m^2 * " / yr)")) +
  facet_grid(rows = vars(Species), cols = vars(Site_Category), scales = "free_y", space = "free") +
  scale_color_brewer(palette = "Blues") +
  scale_fill_brewer(palette = "Blues")

print(densitybydepthplot_stars)

density_urchins <- densitybydepth %>%
  filter(Species %in% c("Mesocentrotus franciscanus", "Strongylocentrotus purpuratus"))

densitybydepthplot_urchins <- ggplot(density_urchins, aes(x = Era, y = log(DZ_Density_100m2), color = Era)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Era), alpha = 0.4) +
  geom_point(aes(color = Era),
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.5),
             size = 0.7) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Site Type",
       y = expression("Log of Mean Density (100" * m^2 * " / yr)")) +
  facet_grid(rows = vars(Species), cols = vars(Site_Category), scales = "free_y", space = "free") +
  scale_color_brewer(palette = "Oranges") +
  scale_fill_brewer(palette = "Oranges")

print(densitybydepthplot_urchins)


###PROPORTION CALCULATIONS####

# Per-site proportion of post-wasting vs pre-wasting (used for plotting points)
proportion <- data_PV %>%
  group_by(Site_Category, Species, Era, Site) %>%
  mutate(Site_Mean = mean(Density_100m2), stdev = sd(Density_100m2)) %>%
  select(Site_Category, Species, Era, Site_Mean, stdev) %>%
  distinct() %>%
  pivot_wider(
    names_from = Era,
    values_from = c(Site_Mean, stdev),
    names_sep = "_"
  ) %>%
  mutate(prop_baseline = `Site_Mean_Post-Wasting Recovery` / `Site_Mean_Pre-Wasting`) %>%
  filter(!is.na(prop_baseline) & !is.infinite(prop_baseline))

# Error bars: mean ± SD of prop_baseline across sites per Site_Category x Species
error_bars <- proportion %>%
  filter(!is.infinite(prop_baseline) & !is.na(prop_baseline)) %>%
  dplyr::group_by(Site_Category, Species) %>%
  dplyr::summarise(
    error_mean = mean(prop_baseline),
    stdev = sd(prop_baseline),
    .groups = "drop"
  )


# Bar chart version with explicit error bars
proportion_bar_plot <- ggplot() +
  # Bars for group means
  geom_col(data = error_bars,
           aes(x = Site_Category, y = error_mean, fill = Site_Category),
           alpha = 0.6, width = 0.6) +
  # Error bars (mean ± 1 SD)
  geom_errorbar(data = error_bars,
                aes(x = Site_Category, ymin = error_mean - stdev, ymax = error_mean + stdev),
                width = 0.2, linewidth = 0.5) +
  # Jittered individual site points overlaid
  geom_jitter(data = proportion,
              aes(x = Site_Category, y = prop_baseline, color = Site_Category),
              width = 0.15, size = 1.8, alpha = 0.6) +
  # Reference line at 1 (= no change from baseline)
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.6) +
  facet_wrap(~ Species, ncol = 2, scales = "free_y") +
  labs(
    title = "Post-Wasting Recovery as Proportion of Pre-Wasting",
    x = "Site Category",
    y = "Proportion of Baseline",
    fill = "Site Category",
    color = "Site Category"
  ) +
  #ylim(-5, 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "italic", size = 10),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

show(proportion_bar_plot)

differences <- data_PV %>%
  group_by(Site_Category, Species, Era, Site) %>%
  mutate(Site_Mean = mean(Density_100m2), stdev = sd(Density_100m2)) %>%
  select(Site_Category, Species, Era, stdev, Site_Mean) %>%
  distinct() %>%
  pivot_wider(names_from = Era, values_from = c(stdev, Site_Mean)) %>%
  group_by(Site_Category, Species) %>%
  mutate(stdev_Difference = `stdev_Post-Wasting Recovery` - `stdev_Pre-Wasting`) %>%
  mutate(mean_Difference = `Site_Mean_Post-Wasting Recovery` - `Site_Mean_Pre-Wasting`)

--------------------------------
  # 
  # # Proportion plot: boxplot with jittered points
  # proportion_plot <- ggplot(proportion, aes(x = Site_Category, y = prop_baseline,
  #                                           color = Site_Category, fill = Site_Category)) +
  #   geom_boxplot(alpha = 0.4, linewidth = 0.8, outlier.size = 1.5) +
  #   geom_jitter(width = 0.1, size = 2, alpha = 0.6) +
  #   geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.6) + # reference line at baseline
  #   facet_wrap(~ Species, ncol = 2) +
  #   labs(
  #     title = "Post-Wasting Recovery as Proportion of Pre-Wasting",
  #     x = "Site Category",
  #     y = "Proportion of Baseline",
  #     color = "Site Category",
  #     fill = "Site Category"
  #   ) +
  #   ylim(0, 6) +
  #   theme_minimal() +
  #   theme(
  #     axis.text.x = element_text(angle = 45, hjust = 1),
  #     strip.text = element_text(face = "italic", size = 10),
  #     panel.grid.major.x = element_blank()
  #   ) +
  #   scale_color_brewer(palette = "Set2") +
  #   scale_fill_brewer(palette = "Set2")
  # 
  # show(proportion_plot)