##Mean Density of focspp. by Depth Zone 
##Created Oct 4, 2024 
#Modified March 4, 2026

###LOAD PACKAGES####
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

#For Habitat Metric + GLMMM
library("GGally")
library("DataExplorer")
library("purrr")     ## for iterative tasks
library("dplyr")     ## for data wrangling
library("patchwork")
library("stringr")
library("prismatic")
library("ggforce")
library("glmmTMB")
library("ggeffects")
library("DHARMa")
library(tidyverse)
library(mgcv)

#For the Random Forest / GBT 
library(ranger)       # install.packages("ranger")
library(gbm)          # install.packages("gbm")
library(vip)          # install.packages("vip")




####Load Dataset / Feature Engineering####

# Read the PVR dataset (inclusive of ARM, and merge to the habitat data set)
data_PV_raw <- read.csv("PV_Stars_Urchins2026-06-03 (2).csv", check.names = F)
habitat_data_raw <- read.csv("all_env_lat_lon.csv", check.names = F)

#Rename Columns 
colnames(data_PV_raw)[colnames(data_PV_raw) == "BenthicReefSpecies"] <- "Species"
colnames(data_PV_raw)[colnames(data_PV_raw) == "SampleYear"] <- "Year"

#Merge
data_PV <- data_PV_raw %>%
  group_by(Year, DepthZone, Site, Species) %>%
  summarise(mean_density_100m2 = mean(Density_m2, na.rm = TRUE) * 100) %>%
  ungroup() %>% 
  left_join(habitat_data_raw, by = c("Site", "DepthZone"))# %>%
  #select(-giantkelp_density_m2, -giantkelp_stipe_density_m2)

#Focal Species List#
foc_spp <- c("Mesocentrotus franciscanus",
             "Strongylocentrotus purpuratus",
             "Patiria miniata",
             "Pisaster ochraceus",
             "Pisaster giganteus")

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


#Creating the Eras 
data_PV <- data_PV %>%
  mutate(
    Era = case_when(
      Year < 2014 ~ "Pre-Wasting",
      Year >= 2014 & Year <= 2019 ~ "Wasting Event",
      Year >= 2020 ~ "Post-Wasting Recovery"
    ),
    Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE),
    DepthZone = factor(DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"), ordered = TRUE)
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
                "Portuguese Point",
                "Hawthorne Reef") ~ "MPA&Adj",
    Site %in% c("3 Palms East", 
                "3 Palms West", 
                "Bunker Point",
                "Burial Grounds", 
                "Cape Point", 
                "KOU Rock",
                "Old 18th") ~ "PVR&Adj",
    DepthZone == "ARM" ~ "PVR&Adj",
    TRUE ~ "Reference" #all natural sites
  ))

#Filtering for only Outer / Deep for this project 
data_PV <- data_PV %>%
  filter(Species %in% foc_spp, Year >= 2011) %>%
  filter(DepthZone %in% c("Outer", "Deep", "ARM"))


#print list of unique sites
#unique_sites <- data_PV %>% distinct(Site)

#unhash to save the file
#write_csv(data_PV, "data_PV_**replace with date***.csv")


###EDA / Feature Importance 

####FEATURE IMPORTANCE — RF + GBT PRE-SCREENING####

# Full habitat features
habitat_features <- c(
  "Relief_index", "Relief_SD", "Relief_simpson",
  "Substrate_index", "Substrate_SD", "Substrate_simpson",
  "dist_200m_bath",
  "mean_chl_mg_m3", "max_chl_mg_m3", "min_chl_mg_m3",
  "mean_sst_C", "max_sst_C", "min_sst_C"
)

# Fit RF + GBT per species to extract feature importance 
importance_results <- map(foc_spp, function(sp) {
  
  sp_data <- data_PV %>%
    filter(Species == sp) %>%
    select(mean_density_100m2, all_of(habitat_features)) %>%
    drop_na() %>%
    mutate(log_density = log1p(mean_density_100m2)) %>%
    select(-mean_density_100m2)
  
  message("Fitting models for: ", sp, " (n = ", nrow(sp_data), ")")
  
  # Random Forest — permutation importance
  rf <- ranger(
    log_density ~ .,
    data        = sp_data,
    num.trees   = 500,
    importance  = "permutation",
    seed        = 42
  )
  
  rf_imp <- tibble(
    feature    = names(rf$variable.importance),
    rf_perm    = rf$variable.importance,
    species    = sp
  )
  
  # Gradient Boosted Trees — impurity importance via gbm
  gbt <- gbm(
    log_density ~ .,
    data         = sp_data,
    distribution = "gaussian",
    n.trees      = 300,
    interaction.depth = 3,
    shrinkage    = 0.05,
    cv.folds     = 5,
    verbose      = FALSE
  )
  
  gbt_imp <- summary(gbt, plotit = FALSE) %>%
    as_tibble() %>%
    rename(feature = var, gbt_impurity = rel.inf)
  
  # Join both
  left_join(rf_imp, gbt_imp, by = "feature") %>%
    mutate(
      species = sp,
      rf_r2   = rf$r.squared
    )
  
}) %>%
  set_names(foc_spp)

# Join results into table
all_importance <- bind_rows(importance_results)

# Print ranked table per species
walk(foc_spp, function(sp) {
  cat("\n", strrep("─", 55), "\n")
  cat(" Species:", sp, "\n")
  cat(strrep("─", 55), "\n")
  all_importance %>%
    filter(species == sp) %>%
    arrange(desc(rf_perm)) %>%
    select(feature, rf_perm, gbt_impurity) %>%
    print(n = Inf)
})

# Bar chart with feature importance per model
all_importance %>%
  group_by(species) %>%
  mutate(feature = reorder(feature, rf_perm)) %>%
  ungroup() %>%
  ggplot(aes(x = feature, y = rf_perm)) +
  geom_col(aes(fill = rf_perm > 0), alpha = 0.8, show.legend = FALSE) +
  geom_point(aes(y = gbt_impurity / max(gbt_impurity) * max(rf_perm)),
             color = "black", size = 1.8, shape = 16) +
  coord_flip() +
  facet_wrap(~ species, scales = "free_x", ncol = 2,
             labeller = labeller(species = label_wrap_gen(25))) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70")) +
  labs(
    title    = "RF Permutation Importance + GBT Impurity by Species",
    x        = NULL,
    y        = "RF Permutation Importance"
  ) +
  theme_minimal() +
  theme(
    strip.text       = element_text(face = "italic", size = 9),
    axis.text.y      = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

####CREATING THE DENSITY PLOTS####
densitybydepth <- data_PV %>%
  group_by(DepthZone, Year, Species, Site, Era, Site_Category) %>%
  mutate(Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE))

density_stars <- densitybydepth %>%
  filter(Species %in% c("Patiria miniata", "Pisaster ochraceus", "Pisaster giganteus"))

densitybydepthplot_stars <- ggplot(density_stars, aes(x = Era, y = log(mean_density_100m2), color = Era)) +
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

densitybydepthplot_urchins <- ggplot(density_urchins, aes(x = Era, y = log(mean_density_100m2), color = Era)) +
  geom_boxplot(outlier.shape = NA, aes(fill = Era), alpha = 0.4) +
  geom_point(aes(color = Era),
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.5),
             size = 0.7) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Site Type",
       y = expression("log of Mean Density (100" * m^2 * " / yr)")) +
  facet_grid(rows = vars(Species), cols = vars(Site_Category), scales = "free_y", space = "free") +
  scale_color_brewer(palette = "Oranges") +
  scale_fill_brewer(palette = "Oranges")

print(densitybydepthplot_urchins)


###PROPORTION CALCULATIONS####


###LOG RESPONSE RATIO (LRR) CALCULATIONS####
# LRR = log(Post-Wasting Recovery mean / Pre-Wasting mean) per site x species
# LRR = 0  → no change from pre-wasting baseline
# LRR > 0  → density increased post-wasting
# LRR < 0  → density decreased post-wasting
# Symmetric: doubling (+0.69) and halving (-0.69) are equidistant from 0

# Canonical site-level summary used by all downstream calculations
# PVR module sites are collapsed to parent sites before averaging
site_summary <- data_PV %>%
  mutate(Site = case_when(
    Site %in% c("PVR 2A", "PVR 2B", "PVR 2C") ~ "Burial Grounds",
    Site %in% c("PVR 4A", "PVR 4B", "PVR 4C",
                "PVR 5A", "PVR 5B", "PVR 5C",
                "PVR 6B", "PVR 6C", "PVR 6D") ~ "Old 18th",
    Site %in% c("PVR 7A", "PVR 7B", "PVR 7C",
                "PVR 8A", "PVR 8B", "PVR 8C") ~ "Cape Point",
    .default = as.character(Site)
  )) %>%
  group_by(Site_Category, Species, Era, Site) %>%
  dplyr::summarise(
    Site_Mean = mean(mean_density_100m2, na.rm = TRUE), 
    stdev     = sd(mean_density_100m2, na.rm = TRUE),   
    .groups   = "drop"
  )

# Differences table (raw mean and SD change, pre → post)
differences <- site_summary %>%
  pivot_wider(
    names_from  = Era,
    values_from = c(Site_Mean, stdev),
    names_sep   = "_"
  ) %>%
  group_by(Site_Category, Species) %>%
  mutate(
    stdev_Difference = `stdev_Post-Wasting Recovery` - `stdev_Pre-Wasting`,
    mean_Difference  = `Site_Mean_Post-Wasting Recovery` - `Site_Mean_Pre-Wasting`
  ) %>%
  ungroup()

#write.csv(differences, "differences_mar10.csv", row.names = FALSE)

# LRR table — only sites with Pre-Wasting mean > 0 are valid (avoids log(0) and Inf)
proportion <- site_summary %>%
  pivot_wider(
    names_from  = Era,
    values_from = c(Site_Mean, stdev),
    names_sep   = "_"
  ) %>%
  filter(
    !is.na(`Site_Mean_Pre-Wasting`),
    !is.na(`Site_Mean_Post-Wasting Recovery`),
    `Site_Mean_Pre-Wasting` > 0   # exclude true pre-wasting absences
  ) %>%
  mutate(
    LRR = log(`Site_Mean_Post-Wasting Recovery` / `Site_Mean_Pre-Wasting`)
  )

#write.csv(proportion, "proportion_LRR.csv", row.names = FALSE)

# Summary: mean LRR ± 95% CI across sites per Site_Category x Species
# Used for error bars on the plot
error_bars <- proportion %>%
  dplyr::group_by(Site_Category, Species) %>%
  dplyr::summarise(
    LRR_mean = mean(LRR),
    LRR_se   = sd(LRR) / sqrt(n()),
    LRR_ci   = qt(0.975, df = n() - 1) * LRR_se,  # 95% CI
    .groups  = "drop"
  )

# LRR plot: boxplot + jittered site points + mean 95% CI + reference line at 0
proportion_bar_plot <- ggplot() +
  geom_boxplot(
    data  = proportion,
    aes(x = Site_Category, y = LRR, fill = Site_Category),
    alpha = 0.6, width = 0.6, outlier.shape = NA
  ) +
  # Mean ± 95% CI across sites
  geom_errorbar(
    data = error_bars,
    aes(x = Site_Category, ymin = LRR_mean - LRR_ci, ymax = LRR_mean + LRR_ci),
    width = 0.2, linewidth = 0.7, color = "grey30"
  ) +
  geom_point(
    data = error_bars,
    aes(x = Site_Category, y = LRR_mean),
    size = 2.5, shape = 18, color = "grey30"
  ) +
  # Jittered individual site points
  geom_jitter(
    data  = proportion,
    aes(x = Site_Category, y = LRR, color = Site_Category),
    width = 0.15, size = 1.8, alpha = 0.6
  ) +
  # Reference line at 0 = no change from pre-wasting baseline
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  facet_wrap(~ Species, ncol = 2) +
  labs(
    title   = "Post-Wasting Recovery: Log Response Ratio vs. Pre-Wasting Baseline",
    caption = "LRR = 0: no change  |  LRR > 0: increased density  |  LRR < 0: decreased density\nDiamond = mean; error bars = 95% CI; sites with zero pre-wasting density excluded",
    x       = "Site Category",
    y       = "Log Response Ratio (LRR)",
    fill    = "Site Category",
    color   = "Site Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    strip.text     = element_text(face = "italic", size = 10),
    panel.grid.major.x = element_blank(),
    plot.caption   = element_text(size = 8, color = "grey40", hjust = 0)
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

show(proportion_bar_plot)

###HABITAT METRICS

#Density vs Relief SD 
density_vs_relief <- data_PV %>%
  filter(!is.na(Relief_SD)) %>%
  ggplot(aes(x = Relief_SD, y = log1p(mean_density_100m2))) + 
  geom_point(aes(color = Site_Category), size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 0.7, color = "black") +
  facet_grid(rows = vars(Species), cols = vars(Era), scales = "free_y") +
  labs(
    x = "Relief SD",
    y = "Log Mean Density + 1 (per 100m²)",
    color = "Site Category",
    title = "Density vs. Relief by Species and Era"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_brewer(palette = "Set2")

show(density_vs_relief)

#Density vs Substrate Index
density_vs_substrate <- data_PV %>%
  filter(!is.na(Substrate_index)) %>%
  ggplot(aes(x = Substrate_index, y = log(mean_density_100m2), color = Site_Category)) +
  geom_point(aes(color = Site_Category), size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 0.7, color = "black")+
  facet_grid(rows = vars(Species), cols = vars(Era), scales = "free_y") +
  labs(
    x = "Substrate Index (Hard Rock)",
    y = "Log Mean Density (per 100m²)",
    color = "Site Category",
    title = "Density vs. Hard Substrate by Species and Era"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_brewer(palette = "Set2")

show(density_vs_substrate)

# Habitat by Era
habitat_prewasting <- data_PV %>%
  filter(Era == 'Pre-Wasting') %>%
  mutate(mean_density_100m2 = log1p(mean_density_100m2)) # log-transform density

habitat_postwasting <- data_PV %>%
  filter(Era == 'Post-Wasting Recovery') %>%
  mutate(mean_density_100m2 = log1p(mean_density_100m2)) # log-transform density

#Variables of Interest
variables <- c("mean_density_100m2",
               "dist_200m_bath",
               "giantkelp_stipe_density_m2",
               "giantkelp_density_m2",
               "Relief_index",
               "Relief_SD",
               "Relief_simpson",
               "Substrate_index",
              "Substrate_SD",
              "Substrate_simpson")



predictor_vars <- setdiff(variables, "mean_density_100m2")

#combined Pre and Post by species 
for (sp in foc_spp) {
  
  combined <- bind_rows(
    habitat_prewasting  %>% mutate(era = "Pre-Wasting"),
    habitat_postwasting %>% mutate(era = "Post-Wasting")
  ) %>%
    mutate(
      mean_density_100m2 = log1p(mean_density_100m2),
      era = factor(era, levels = c("Pre-Wasting", "Post-Wasting"))  # fix order
    ) %>%
    filter(Species == sp) %>%
    select(mean_density_100m2, era, all_of(predictor_vars)) %>%
    pivot_longer(cols = all_of(predictor_vars),
                 names_to  = "variable",
                 values_to = "value")
  
  p <- combined %>%
    ggplot(aes(x = value, y = mean_density_100m2, color = era, fill = era)) +
    geom_point(size = 1.2, alpha = 0.4) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.6, alpha = 0.2) +
    scale_color_manual(values = c("Pre-Wasting" = "darkorchid4", "Post-Wasting" = "steelblue")) +
    scale_fill_manual(values  = c("Pre-Wasting" = "darkorchid",  "Post-Wasting" = "steelblue")) +
    facet_grid(era ~ variable, scales = "free_x") +
    labs(
      title = sp,
      y     = "log(Density + 1)",
      x     = NULL
    ) +
    theme_minimal(base_size = 9) +
    theme(
      strip.text        = element_text(size = 7),
      plot.title        = element_text(face = "italic", hjust = 0.5),
      panel.grid.minor  = element_blank(),
      legend.position   = "none"  # era is already shown in row strip labels
    )
  
  print(p)
}


#COLOR BY ERA
era_colors <- c(
  "Pre-Wasting"  = "darkseagreen4",
  "Wasting Event" = "chocolate",
  "Post-Wasting Recovery" = "cadetblue4"
)


depth_colors <- c(
  "Outer"  = "white",
  "Deep" = "white",
  "ARM" = "black"
)



#highlight ARM as DepthZone
for (sp in foc_spp) {
  
  plot_data <- data_PV %>%
    mutate(mean_density_100m2 = log(mean_density_100m2)) %>%
    filter(Species == sp) %>%
    select(mean_density_100m2, Era, all_of(predictor_vars), DepthZone) %>%
    pivot_longer(cols = all_of(predictor_vars),
                 names_to  = "variable",
                 values_to = "value")
  
  p <- ggplot(plot_data, aes(x = value, y = mean_density_100m2)) +
    # All points (muted)
    geom_point(aes(color = Era),
               size = 1.2, alpha = 0.2) +
    # ARM points on top (highlighted)
    geom_point(data = filter(plot_data, DepthZone == "ARM"),
               aes(color = Era),
               size = 1.8, alpha = 0.9, shape = 17) +  # triangle shape
    # Smooth lines across all data
    geom_smooth(aes(color = Era, fill = Era),
                method = "lm", se = TRUE, linewidth = 0.6, alpha = 0.15) +
    scale_color_manual(values = era_colors) +
    scale_fill_manual(values  = era_colors) +
    # ARM annotation per facet
    geom_rug(data = filter(plot_data, DepthZone == "ARM"),
             aes(color = Era),
             sides = "b", linewidth = 0.5, alpha = 0.7) +
    facet_wrap(~ variable, scales = "free_x", ncol = 3) +
    labs(
      title    = sp,
      y        = "log(Density)",
      x        = NULL,
      caption  = "▲ ARM depth zone"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      strip.text       = element_text(size = 7),
      plot.title       = element_text(face = "italic", hjust = 0.5),
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 7, hjust = 0)
    )
  
  print(p)
}


#### Multivariate Generalised Linear Model ####
habitat_pisgig <- data_PV %>%
  filter(Species == "Pisaster giganteus") %>%
  select(-c(mean_chl_mg_m3,
            max_chl_mg_m3,
            min_chl_mg_m3,
            mean_sst_C,
            max_sst_C,
            min_sst_C,
            dist_200m_bath)) %>%
  drop_na()

colnames(habitat_pisgig)

#https://bedeffinianrowedavies.com/statisticstutorials/multivariateglms
#Zero-Inflated Tweedie Generalized Linear Mixed Model (ZI-Tweedie GLMM) is a powerful statistical approach for modeling continuous, strictly positive data that also contains an excessive number of zeros
#https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
#page 4 -> add ziformula=~1
#did not work 

#mean_density_100m2 ~ Era + habitat_metrics + (1 | Site)

glmm0 <- glmmTMB(mean_density_100m2 ~ Era + (1 | Site),
                 #ziformula = ~1,
                 data   = habitat_pisgig,
                 family = tweedie(link = "log"))

glmm1 <- glmmTMB(mean_density_100m2 ~
                   Era +
                   Substrate_index +
                   Relief_index +
                   #giantkelp_stipe_density_m2 +
                   (1 | Site),          # Site as random intercept
                 #ziformula = ~1,
                 data   = habitat_pisgig,  
                 family = tweedie(link = "log"))

# Interaction model to test if habitat effects differ by Era
glmm2 <- glmmTMB(mean_density_100m2 ~
                   Era * Substrate_index +
                   Era * Relief_index +
                  # Era * giantkelp_stipe_density_m2 +
                   (1 | Site),
                 #ziformula = ~1,
                 data   = habitat_pisgig,
                 family = tweedie(link = "log"))


# Interaction model to test if habitat effects differ by Era and Site Category
glmm3 <- glmmTMB(mean_density_100m2 ~
                   Site_Category * Era * Substrate_index +
                   Site_Category * Era * Relief_index +
                   #Site_Category * Era * giantkelp_stipe_density_m2 +
                   (1 | Site),
                 #ziformula = ~1,
                 data   = habitat_pisgig,
                 family = tweedie(link = "log"))


AIC(glmm0, glmm1, glmm2, glmm3)
summary(glmm0)
summary(glmm1)
summary(glmm2)
summary(glmm3)

ggeffects::ggpredict(glmm2, terms = c("Substrate_index", "Era")) 

pred <- ggpredict(glmm2, terms = c("Substrate_index", "Era")) 
plot(pred)

ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) + 
  geom_line(linewidth = 1) + 
  geom_ribbon(aes(ymin =conf.low, ymax = conf.high), alpha = 0.2, color = NA) + 
  scale_color_manual( values = c("Pre-Wasting"  ="darkseagreen4", "Wasting Event"  = "chocolate", "Post-Wasting Recovery" = "cadetblue4"), labels = c("Pre-Wasting","Wasting Event", "Post-Wasting Recovery") ) + 
  scale_fill_manual( values = c("Pre-Wasting"  = "darkseagreen4", "Wasting Event"  = "chocolate", "Post-Wasting Recovery" = "cadetblue4") ) + 
  labs( title  = expression(italic("Pisaster giganteus") ~"density vs. Substrate Index by Era"), x  = "Substrate Index (Hard Rock)", y  = "Predicted Density (per 100m²)", color  ="Era", fill  = "Era", caption = "Shaded bands = 95% CI; predictions averaged over random Site effect" ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Extract residuals #u want the points to fall along the red diagonal
res <- residuals(glmm2, type = "pearson")  # pearson is standard for GLMMs 
qqnorm(res, main = "QQ Plot - glmm2 Pearson Residuals") 
qqline(res, col = "red", lwd = 2)

#DHARMa handles non-normal distribution
library(DHARMa) 
sim_res <- simulateResiduals(fittedModel = glmm2, n = 1000) # QQ
plotQQunif(sim_res)

####PURR MAPPING ####
#This will run all GLMM models on each species and output the best model for each

# Build the models
model_formulas <- list(
  glmm0 = mean_density_100m2 ~ Era + (1 | Site),
  
  glmm1 = mean_density_100m2 ~ Era +
    Substrate_index +
    Relief_index +
    (1 | Site),
  
  glmm2 = mean_density_100m2 ~ Era * Substrate_index +
    Era * Relief_index +
    (1 | Site),
  
  glmm3 = mean_density_100m2 ~ Site_Category * Era * Substrate_index +
    Site_Category * Era * Relief_index +
    (1 | Site)
)

#  Fit all models for one species (helper function)
fit_species_models <- function(sp, data, formulas) {
  
  sp_data <- data |>
    filter(Species == .env$sp) |>   # ← Species, not foc_spp
    drop_na()
  
  message("\n── Fitting models for: ", sp, " (n = ", nrow(sp_data), ") ──")
  
  # Fit each model, catching errors so the loop doesn't break
  models <- imap(formulas, \(formula, name) {
    tryCatch(
      glmmTMB(formula, data = sp_data, family = tweedie(link = "log")),
      error = \(e) { message("  ", name, " failed: ", e$message); NULL }
    )
  })
  
  # Drop any failed models
  models <- compact(models)
  
  # Built AIC comparison tables 
  aic_tbl <- map_dfr(models, \(m) tibble(AIC = AIC(m)), .id = "model") |>
    arrange(AIC) |>
    mutate(
      delta_AIC  = AIC - min(AIC),
      species    = sp
    )
  
  best_name  <- aic_tbl$model[1]
  best_model <- models[[best_name]]
  
  message("  Best model: ", best_name, " (AIC = ", round(aic_tbl$AIC[1], 2), ")")
  
  list(
    species    = sp,
    models     = models,
    aic_table  = aic_tbl,
    best_name  = best_name,
    best_model = best_model
  )
}

# Run across all species 
results <- map(
  foc_spp,
  \(sp) fit_species_models(sp, data = data_PV, formulas = model_formulas)
) |>
  set_names(foc_spp)

# Print summary table: best model per species 
best_model_summary <- map_dfr(results, \(r) {
  r$aic_table |> filter(model == r$best_name)
}) |>
  select(species, best_model = model, AIC, delta_AIC) |>
  arrange(species)

print(best_model_summary)

# Full AIC tables for all species
all_aic <- map_dfr(results, "aic_table") |>
  select(species, model, AIC, delta_AIC) |>
  arrange(species, delta_AIC)

print(all_aic)

# Summaries of best model per species
walk(results, \(r) {
  cat("\n", strrep("═", 60), "\n")
  cat(" Species:", r$species, "| Best model:", r$best_name, "\n")
  cat(strrep("═", 60), "\n")
  print(summary(r$best_model))
})

# DHARMa residual diagnostics for best model per species 
walk(results, \(r) {
  cat("\nDHARMa diagnostics —", r$species, "(", r$best_name, ")\n")
  sim_res <- simulateResiduals(fittedModel = r$best_model, n = 1000)
  plotQQunif(sim_res, main = paste(r$species, "-", r$best_name))
})

# ggeffects predictions for best model per species
era_colors <- c(
  "Pre-Wasting"          = "darkseagreen4",
  "Wasting Event"        = "chocolate",
  "Post-Wasting Recovery"= "cadetblue4"
)

prediction_plots <- imap(results, \(r, sp) {
  
  model_vars <- all.vars(formula(r$best_model, component = "cond"))
  
  plots <- list()
  
  # Plot Substrate_index if in model
  if ("Substrate_index" %in% model_vars) {
    pred_sub <- ggpredict(r$best_model, terms = c("Substrate_index", "Era"))
    plots[["Substrate"]] <- ggplot(pred_sub, aes(x, predicted, color = group, fill = group)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = era_colors) +
      scale_fill_manual(values  = era_colors) +
      labs(
        title   = bquote(italic(.(sp)) ~ "— Substrate Index" ~ "(best:" ~ .(r$best_name) * ")"),
        x       = "Substrate Index (Hard Rock)",
        y       = "Predicted Density (per 100 m²)",
        color = "Era", fill = "Era",
        caption = "Shaded bands = 95% CI; predictions averaged over random Site effect"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  # Plot Relief_index if in model
  if ("Relief_index" %in% model_vars) {
    pred_rel <- ggpredict(r$best_model, terms = c("Relief_index", "Era"))
    plots[["Relief"]] <- ggplot(pred_rel, aes(x, predicted, color = group, fill = group)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = era_colors) +
      scale_fill_manual(values  = era_colors) +
      labs(
        title   = bquote(italic(.(sp)) ~ "— Relief Index" ~ "(best:" ~ .(r$best_name) * ")"),
        x       = "Relief Index",
        y       = "Predicted Density (per 100 m²)",
        color = "Era", fill = "Era",
        caption = "Shaded bands = 95% CI; predictions averaged over random Site effect"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  # Combine side by side with patchwork if both exist
  if (length(plots) == 2) {
    plots[["Substrate"]] + plots[["Relief"]] +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  } else if (length(plots) == 1) {
    plots[[1]]
  } else {
    message("Skipping plots for ", sp, ": neither Substrate nor Relief in ", r$best_name)
    NULL
  }
})

walk(compact(prediction_plots), print)
testDispersion(simulationOutput)




#just testing the residuals -> red line bad, black line good 
#https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
#plot residuals against predictors 

habitat_strpur <- data_PV %>%
  filter(Species == "Strongylocentrotus purpuratus") %>%
  select(-c(mean_chl_mg_m3,
            max_chl_mg_m3,
            min_chl_mg_m3,
            mean_sst_C,
            max_sst_C,
            min_sst_C,
            dist_200m_bath)) %>%
  drop_na()

colnames(habitat_strpur)

#https://bedeffinianrowedavies.com/statisticstutorials/multivariateglms
#Zero-Inflated Tweedie Generalized Linear Mixed Model (ZI-Tweedie GLMM) is a powerful statistical approach for modeling continuous, strictly positive data that also contains an excessive number of zeros
#https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
#page 4 -> add ziformula=~1
#did not work 

#mean_density_100m2 ~ Era + habitat_metrics + (1 | Site)

glmm0_sp <- glmmTMB(mean_density_100m2 ~ Era + (1 | Site),
                 ziformula = ~predictor_vars,
                 data   = habitat_strpur,
                 family = tweedie(link = "log"))

glmm1_sp <- glmmTMB(mean_density_100m2 ~
                   Substrate_index +
                   Relief_index +
                   Era +
                   #giantkelp_stipe_density_m2 +
                   (1 | Site),          # Site as random intercept
                 ziformula = ~ Era,
                 data   = habitat_strpur,  
                 family = tweedie(link = "log"))

# Interaction model to test if habitat effects differ by Era
glmm2_sp <- glmmTMB(mean_density_100m2 ~
                   Era * Substrate_index +
                   Era * Relief_index +
                   # Era * giantkelp_stipe_density_m2 +
                   (1 | Site),
                 ziformula = ~1,
                 data   = habitat_strpur,
                 family = tweedie(link = "log"))

# Interaction model to test if habitat effects differ by Era and Site Category
glmm3_sp <- glmmTMB(mean_density_100m2 ~
                   Site_Category * Era * Substrate_index +
                   Site_Category * Era * Relief_index +
                   #Site_Category * Era * giantkelp_stipe_density_m2 +
                   (1 | Site),
                 ziformula = ~1,
                 data   = habitat_strpur,
                 family = tweedie(link = "log"))

AIC(glmm0_sp, glmm1_sp, glmm2_sp, glmm3_sp)
summary(glmm0_sp)
summary(glmm1_sp)
summary(glmm2_sp)
summary(glmm3_sp)


#DHARMa handles non-normal distribution
library(DHARMa) 
sim_res <- simulateResiduals(fittedModel = glmm3_sp, refit = T) # QQ
plotQQunif(sim_res)



testDispersion(simulationOutput)


###BACIPS####
#####PVR ANALYSIS#####
############################################################################################################
#Object: Perform a Progressive-Change BACIPS analysis using step, linear, asymptotic and sigmoid models	#
#Authors: L. Thiault, L. Kernal??guen, C.W. Osenberg & J. Claudet												#
##### PVR BACIPS #######################################################################################################

# Load packages
require(minpack.lm) # Fitting non-linear models
require(nls2) # Fitting non-linear models
require(AICcmodavg) # calculate second order AIC (AICc)

### The function requires 4 inputs of class integer with the same length (STEP 1) :
# control: includes response variable measured in the Control site at each sampling time.model;
# impact: includes response variable measured in the Impact site at each sampling time.model;
# time.true: time corresponding to each sample;
# time.model: surveys from the Before period are assigned time.model=0, and surveys from the After period are assigned time.models that represent the time since the intervention started.

pvr_control_sites <- c("Hawthorne Reef",
                       "Honeymoon Cove",
                       "Lunada Bay")

pvr_impact_sites <- c("Old 18th",
                      "Burial Grounds",
                      "Cape Point") #updated 6/9/26 

bacip_sites <- c("Hawthorne Reef",
                  "Honeymoon Cove",
                  "Lunada Bay",
                 "Old 18th",
                 "Burial Grounds",
                 "Cape Point")

time.true <- c(2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025) #Sample Years
time.model <- c(0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5) # 0 = pre-impact, 1 = 2020 onwards (recovery period = impact)


subsetyears <- data_PV %>%
  filter(Year %in% time.true) %>%
  mutate(Site = case_when(
    Site %in% c("PVR 2A", "PVR 2B", "PVR 2C") ~ "Burial Grounds",
    Site %in% c("PVR 4A", "PVR 4B", "PVR 4C",
                "PVR 5A", "PVR 5B", "PVR 5C",
                "PVR 6B", "PVR 6C", "PVR 6D") ~ "Old 18th",
    Site %in% c("PVR 7A", "PVR 7B", "PVR 7C",
                "PVR 8A", "PVR 8B", "PVR 8C") ~ "Cape Point",
    .default = Site
  ))

years <- subsetyears %>%
  filter(Site %in% bacip_sites) %>%
  mutate(
    bacip_cat = case_when(
      Site %in% pvr_control_sites ~ "Control",
      Site %in% pvr_impact_sites ~ "Impact")) %>%
  group_by(bacip_cat) %>%
  distinct(Year) 

#Set control and impact and bind to time.true and time.model
bacips_data <- map(foc_spp, function(sp) {

  ctrl <- subsetyears %>%
    filter(Site %in% pvr_control_sites, Species == sp) %>%
    group_by(Year) %>%
    dplyr::summarise(control = mean(mean_density_100m2, na.rm = TRUE), .groups = "drop") %>%
    arrange(Year)

  imp <- subsetyears %>%
    filter(Site %in% pvr_impact_sites, Species == sp) %>%
    group_by(Year) %>%
    dplyr::summarise(impact = mean(mean_density_100m2, na.rm = TRUE), .groups = "drop") %>%
    arrange(Year)

  list(
    control = ctrl$control,
    impact  = imp$impact
  )
}) %>%
  set_names(foc_spp)


### Create the ProgressiveChangeBACIPS function
ProgressiveChangeBACIPS <- function(control, impact, time.true, time.model) 
{
  ### STEP 2 - Calculate the delta at each sampling date
  delta <- impact - control
  
  #Plot delta against time.true
  dev.new(width=10, height=5)
  png(file = paste(i, "_plot_PVR.png", sep = ""), width=10, height=4, unit="in", res = 500)
  par(mfrow=c(1,2))
  plot(delta~time.true, type="n")
  time.model.of.impact=max(which(time.model==0))
  abline(v = 2020, col = "cadetblue3", lty = 5)
  rect(2013, min(delta)-100, 2016, max(delta)+100, col = "sandybrown", border = NULL)
  points(delta~time.true, pch=24, bg="white", cex=2)
  
  png(file = paste(i, "_plot_PVR.png", sep = ""), width=11, height=4, unit="in", res = 500)
  par(mfrow=c(1,2))
  plot(delta ~ time.true, type="n", xaxt="n")  # Setting x-axis to none initially
  time.model.of.impact <- max(which(time.model == 0))
  abline(v = 2020, col = "cadetblue3", lty = 5)
  #rect(2013, min(delta)-100, 2016, max(delta)+100, col = ("sandybrown" alpha = 0.1), border = NULL)
  rect(2013, min(delta)-100, 2016, max(delta)+100, col = adjustcolor("sandybrown", alpha.f = 0.5), border = "white")
  points(delta ~ time.true, pch=24, bg="white", cex=2)
  axis(side=1, at=seq(2008, 2023, by=2))
  
  ### STEP 3 - Fit and compete models
  ## Create a 'period' variable
  period <- ifelse(time.model==0, "Before","After")
  
  ## Fit a step model
  ## Step Model: The difference arises instantly and is constant through time
  step.Model<-aov(delta ~ period)
  
  ## Fit a linear model
  ## Linear Model: The difference increases through time at a constant rate
  linear.Model<-lm(delta ~ time.model)
  
  ## Fit an asymptotic model
  ## Asymptotic Model: The rate of change decreases monotonically to zero as time passes
  # Create an asymptotic function
  myASYfun<-function(delta, time.model)
  {
    funAsy<-function(parS, time.model)	(parS$M * time.model) / (parS$L + time.model) + parS$B
    residFun<-function(p, observed, time.model) observed + funAsy(p,time.model)
    parStart <- list(M=mean(delta[time.model.of.impact:length(time.true)]), B=mean(delta[1:time.model.of.impact]), L=1)
    nls_ASY_out <- nls.lm(par=parStart, fn= residFun, observed=delta, time.model=time.model, control = nls.lm.control(maxfev = integer(), maxiter = 1000))
    foAsy<-delta~(M * time.model) / (L + time.model) + B
    startPar<-c(-coef(nls_ASY_out)[1], coef(nls_ASY_out)[2], coef(nls_ASY_out)[3])
    asyFit<-nls2(foAsy, start=startPar, algorithm="brute-force") # nls2 enables to calculate AICc
    asyFit
  }
  # Fit the asymptotic model
  asymptotic.Model<-myASYfun(delta=delta,time.model=time.model)
  
  
  ## Fit a sigmoid model
  ## Sigmoid Model: The rate of change initially increases with time, but eventually decreases to zero.
  ## Create a sigmoid function
  mySIGfun<-function(delta, time.model)
  {
    funSIG<-function(parS, time.model)	(parS$M * (time.model/parS$L)^parS$K) / (1 + (time.model/parS$L) ^ parS$K) + parS$B
    residFun<-function(p, observed, time.model) observed + funSIG(p,time.model)
    parStart <- list(M=mean(delta[time.model.of.impact:length(time.true)]), B=mean(delta[1:time.model.of.impact]), L=mean(time.model), K=5)
    nls_SIG_out <- nls.lm(par=parStart, fn= residFun, observed=delta, time.model=time.model, control = nls.lm.control(maxfev = integer(), maxiter = 1000))
    foSIG<-delta~(M * (time.model/L) ^ K) / (1 + (time.model/L) ^ K) + B
    startPar<-c(-coef(nls_SIG_out)[1],-coef(nls_SIG_out)[2],coef(nls_SIG_out)[3],coef(nls_SIG_out)[4])
    sigFit<-nls2(foSIG, start=startPar, algorithm="brute-force") # nls2 enables to calculate AICc
    sigFit
  }
  # Fit the sigmoid model
  sigmoid.Model<-mySIGfun(delta=delta,time.model=time.model)
  
  
  ## Compete models
  # Perform AIC tests
  #The Akaike Information Criterion is a measure used for model selection and comparison.
  #It provides a balance between the goodness of fit of a model and its complexity, penalizing models with a larger number of parameters. 
  #The lower the AIC value, the better the model is considered to fit the data.
  AIC.test<-AIC(step.Model, linear.Model, asymptotic.Model, sigmoid.Model)
  AICc.test<-as.data.frame(cbind(AIC.test[,1], c(AICc(step.Model), AICc(linear.Model), AICc(asymptotic.Model), AICc(sigmoid.Model))))
  rownames(AICc.test)<-rownames(AIC.test)
  names(AICc.test)<-names(AIC.test)
  
  # Calculate AICc weight and selected the best model
  for(i in 1:dim(AICc.test)[1])
  {
    AICc.test$diff[i]<-AICc.test$AIC[i]-min(AICc.test$AIC)
  }
  AICc.test$RL<-exp(-0.5* AICc.test$diff)
  RL_sum<-sum(AICc.test$RL)
  AICc.test$aicWeights<-(AICc.test$RL/RL_sum)*100
  w<-AICc.test$aicWeights
  names(w)<-rownames(AICc.test)
  
  # Display AICc weights
  print(w)
  # barplot(w, col="white", ylab="Relative likelihood (%)", cex.names = 0.9, names.arg =c("Step","Linear","Asymptotic","Sigmoid"))
  best.Model<-which(w==max(w))
  
  ### STEP 4 - Derive inference based on the best model (i.e., with the higher AICc weight)
  if(best.Model==1) {writeLines(paste("\n\nSTEP MODEL SELECTED - Likelihood = ", round(w[1],1), "%\n\n", sep=""))
    print(summary(step.Model))}
  if(best.Model==2) {writeLines(paste("\n\nLINEAR MODEL SELECTED - Likelihood = ", round(w[2],1), "%\n\n", sep=""))
    print(summary(linear.Model))}
  if(best.Model==3) {writeLines(paste("\n\nASYMPTOTIC MODEL SELECTED - Likelihood = ", round(w[3],1), "%\n\n", sep=""))
    print(asymptotic.Model)}
  if(best.Model==4) {writeLines(paste("\n\nSIGMOID MODEL SELECTED - Likelihood = ", round(w[4],1), "%\n\n", sep=""))
    print(sigmoid.Model)}
  
  dev.off()
}

ProgressiveChangeBACIPS(control, impact, time.true, time.model)
delta <- impact - control

#Create a for loop to have s
for (i in foc_spp) {
  control <- bacips_data[[i]]$control
  impact  <- bacips_data[[i]]$impact
  delta   <- impact - control
  
  print(paste0("Species = ", i))
  print(ProgressiveChangeBACIPS(control, impact, time.true, time.model))
  
  assign(paste("Delta_PVR", i, sep = "_"),
         data.frame(i, time.true, time.model, impact, control, delta))
}

Delta_PVR_all <- rbind(`Delta_PVR_Mesocentrotus franciscanus`,
                       `Delta_PVR_Patiria miniata`, 
                       `Delta_PVR_Pisaster giganteus`,
                       `Delta_PVR_Pisaster ochraceus`,
                       `Delta_PVR_Strongylocentrotus purpuratus`)

Delta_PVR_all <- Delta_PVR_all %>%
  dplyr::rename(., "Species" = "i") %>%
  dplyr::rename(., "PVR_delta" = "delta")


###Archived Plots-------------------------------
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



# differences <- data_PV %>%
#   group_by(Site_Category, Species, Era, Site) %>%
#   mutate(Site_Mean = mean(Density_100m2), stdev = sd(Density_100m2)) %>%
#   select(Site_Category, Species, Site, Era, stdev, Site_Mean) %>%
#   distinct() %>%
#   pivot_wider(names_from = Era, values_from = c(stdev, Site_Mean), names_sep = "_") %>%
#   group_by(Site_Category, Species) %>%
#   mutate(stdev_Difference = `stdev_Post-Wasting Recovery` - `stdev_Pre-Wasting`) %>%
#   mutate(mean_Difference = `Site_Mean_Post-Wasting Recovery` - `Site_Mean_Pre-Wasting`)%>% 
#   ungroup()
#write.csv(differences, "differences_mar10.csv", row.names = FALSE)


#variants of the habitat plots: 

# #Relief Index vs prop_baseline
# ggplot(habitat_proportion, aes(x = Relief_index, y = prop_baseline, color = Site_Category)) +
#   geom_point(size = 3, alpha = 0.7) +
#   geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") + #linear regression model line
#   facet_wrap(~ Species, scales = "free_y") +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") + #points above = recovery, points below line = decline
#   labs(
#     x = "Relief Index",
#     y = "Proportion of Baseline (Post / Pre-Wasting)",
#     color = "Site Category",
#     title = "Relief index"
#   ) +
#   theme_minimal() +
#   theme(strip.text = element_text(face = "italic")) +
#   scale_color_brewer(palette = "Set2")

# ggplot(habitat_proportion, aes(x = Substrate_index, y = prop_baseline, color = Site_Category)) +
#   geom_point(size = 3, alpha = 0.7) +
#   #geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") + #linear regression model line
#   facet_wrap(~ Species, scales = "free_y") +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") + #points above = recovery, points below line = decline
#   labs(
#     x = "Substrate Index",
#     y = "Proportion of Baseline (Post / Pre-Wasting)",
#     color = "Site Category",
#     title = "Substrate index"
#   ) +
#   theme_minimal() +
#   theme(strip.text = element_text(face = "italic")) +
#   scale_color_brewer(palette = "Set2")

# #- make scatter plots with habitat data
# - should go by species, color by site category, group by era, and seperate by species
# - 1 plot per substrate type

# Relief Index
# habitat_data %>%
#   filter(!is.na(Relief_index)) %>%
#   ggplot(aes(x = Relief_index, y = log(Density_100m2))) +
#   geom_point(aes(color = Site_Category), size = 2.5, alpha = 0.7) +
#   geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 0.7, color = "black")+
#   facet_grid(rows = vars(Species), cols = vars(Era), scales = "free_y") +
#   labs(
#     x = "Relief Index",
#     y = "Log Mean Density (per 100m²)",
#     color = "Site Category",
#     title = "Density vs. Relief by Species and Era"
#   ) +
#   theme_minimal() +
#   theme(
#     strip.text = element_text(face = "italic"),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   scale_color_brewer(palette = "Set2")

# #All Habitat Variables vs Density (All Species, pre-wasting)
# ggplot(habitat_prewasting, aes(x = .panel_x, y = .panel_y, color = Species, fill = Species)) +
#   geom_point(size = 1.2, alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
#   ggforce::geom_autodensity(alpha = 0.4) +
#   scale_color_brewer(palette = "Set2", name = NULL) +
#   scale_fill_brewer(palette = "Set2", name = NULL) +
#   ggforce::facet_matrix(
#     vars(all_of(variables)),
#     layer.lower = 1,   # geom_point
#     layer.diag  = 3,   # geom_autodensity
#     layer.upper = 2    # geom_smooth
#   ) +
#   theme_minimal(base_size = 9) +
#   theme(
#     strip.text = element_text(size = 6),
#     legend.position = "bottom",
#     panel.grid.minor = element_blank()
#   )
# 
# 
# #All Habitat Variables vs Density (All Species, post-wasting)
# ggplot(habitat_postwasting, aes(x = .panel_x, y = .panel_y, color = Species, fill = Species)) +
#   geom_point(size = 1.2, alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
#   ggforce::geom_autodensity(alpha = 0.4) +
#   scale_color_brewer(palette = "Set2", name = NULL) +
#   scale_fill_brewer(palette = "Set2", name = NULL) +
#   ggforce::facet_matrix(
#     vars(all_of(variables)),
#     layer.lower = 1,   # geom_point
#     layer.diag  = 3,   # geom_autodensity
#     layer.upper = 2    # geom_smooth
#   ) +
#   theme_minimal(base_size = 9) +
#   theme(
#     strip.text = element_text(size = 6),
#     legend.position = "bottom",
#     panel.grid.minor = element_blank()
#   )

#ggsave("scatter_matrix.png", width = 20, height = 20)


##SCATTER MATRICES
# #Scatter Matrix of Habitat Variables separated for each Species (pre-wasting)
# for (sp in foc_spp) {
#   p <- habitat_prewasting %>%
#     mutate(DZ_Density_100m2 = log1p(DZ_Density_100m2)) %>%
#     filter(Species == sp) %>%
#     ggplot(aes(x = .panel_x, y = .panel_y)) +
#     geom_point(size = 1.2, alpha = 0.5, color = "darkorchid4") +
#     geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, color = "darkorchid4") +
#     ggforce::geom_autodensity(fill = "darkorchid4", alpha = 0.4) +
#     ggforce::facet_matrix(
#       vars(all_of(variables)),
#       layer.lower = 1,
#       layer.diag  = 3,
#       layer.upper = 2
#     ) +
#     labs(title = sp) +
#     theme_minimal(base_size = 9) +
#     theme(
#       strip.text = element_text(size = 6),
#       plot.title = element_text(face = "italic", hjust = 0.5),
#       panel.grid.minor = element_blank()
#     )
#   print(p)
# }
# #Scatter Matrix of Habitat Variables separated for each Species (post-wasting)
# for (sp in foc_spp) {
#   
#   p <- habitat_postwasting %>%
#     mutate(DZ_Density_100m2 = log1p(DZ_Density_100m2)) %>%
#     filter(Species == sp) %>%
#     ggplot(aes(x = .panel_x, y = .panel_y)) +
#     geom_point(size = 1.2, alpha = 0.5, color = "steelblue") +
#     geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, color = "steelblue") +
#     ggforce::geom_autodensity(fill = "steelblue", alpha = 0.4) +
#     ggforce::facet_matrix(
#       vars(all_of(variables)),
#       layer.lower = 1,
#       layer.diag  = 3,
#       layer.upper = 2
#     ) +
#     labs(title = sp) +
#     theme_minimal(base_size = 9) +
#     theme(
#       strip.text = element_text(size = 6),
#       plot.title = element_text(face = "italic", hjust = 0.5),
#       panel.grid.minor = element_blank()
#     )
#   
#   print(p)
# }
# ##Density / Categories by Era, sep by Species (pre-Wasting only)
# for (sp in foc_spp) {
#   p <- habitat_prewasting %>%
#     mutate(DZ_Density_100m2 = log1p(DZ_Density_100m2)) %>%
#     filter(Species == sp) %>%
#     select(DZ_Density_100m2, all_of(variables)) %>%
#     pivot_longer(cols = all_of(variables),
#                  names_to = "variable",
#                  values_to = "value") %>%
#     ggplot(aes(x = value, y = DZ_Density_100m2)) +
#     geom_point(size = 1.2, alpha = 0.4, color = "darkorchid4") +
#     geom_smooth(method = "lm", se = TRUE, linewidth = 0.6,
#                 color = "darkorchid", fill = "darkorchid4", alpha = 0.2) +
#     facet_wrap(~ variable, scales = "free_x", ncol = 3) +
#     labs(
#       title = paste0(sp, "- Pre-Wasting"),
#       y = "log(Density + 1)",
#       x = NULL
#     ) +
#     theme_minimal(base_size = 9) +
#     theme(
#       strip.text = element_text(size = 7),
#       plot.title = element_text(face = "italic", hjust = 0.5),
#       panel.grid.minor = element_blank()
#     )
#   print(p)
# }
# #Density / Categories by Era, sep by Species (post-Wasting only)  
# for (sp in foc_spp) {
#   p <- habitat_postwasting %>%
#     mutate(DZ_Density_100m2 = log1p(DZ_Density_100m2)) %>%
#     filter(Species == sp) %>%
#     select(DZ_Density_100m2, all_of(variables)) %>%
#     pivot_longer(cols = all_of(variables),
#                  names_to = "variable",
#                  values_to = "value") %>%
#     ggplot(aes(x = value, y = DZ_Density_100m2)) +
#     geom_point(size = 1.2, alpha = 0.4, color = "steelblue") +
#     geom_smooth(method = "lm", se = TRUE, linewidth = 0.6,
#                 color = "steelblue4", fill = "steelblue", alpha = 0.2) +
#     facet_wrap(~ variable, scales = "free_x", ncol = 3) +
#     labs(
#       title = sp,
#       y = "log(Density + 1)",
#       x = NULL
#     ) +
#     theme_minimal(base_size = 9) +
#     theme(
#       strip.text = element_text(size = 7),
#       plot.title = element_text(face = "italic", hjust = 0.5),
#       panel.grid.minor = element_blank()
#     )
#   print(p)
# }

# for (sp in foc_spp) {
#   p <- habitat_data %>%
#     mutate(DZ_Density_100m2 = log(DZ_Density_100m2)) %>%
#     filter(Species == sp) %>%
#     select(DZ_Density_100m2, Era, all_of(variables), DepthZone) %>%  
#     pivot_longer(cols = all_of(predictor_vars),
#                  names_to  = "variable",
#                  values_to = "value") %>%
#     ggplot(aes(x = value, y = DZ_Density_100m2, color = Era, fill = Era)) +  #original both Era
#     geom_point(size = 1.2, alpha = 0.4) +
#     geom_smooth(method = "lm", se = TRUE, linewidth = 0.6, alpha = 0.5) +
#     scale_color_manual(values = era_colors) +   
#     scale_fill_manual(values  = era_colors) +   
#     facet_wrap(~ variable, scales = "free_x", ncol = 3) +
#     labs(
#       title = sp,
#       y     = "log(Density)",
#       x     = NULL
#     ) +
#     theme_minimal(base_size = 9) +
#     theme(
#       strip.text       = element_text(size = 7),
#       plot.title       = element_text(face = "italic", hjust = 0.5),
#       panel.grid.minor = element_blank()
#     )
#   
#   print(p)
# }
