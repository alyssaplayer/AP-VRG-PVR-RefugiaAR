##Mean Density of focspp. by Depth Zone 
##Created Oct 4, 2024 
#Modified March 4, 2026

#installing the packages
# Core data wrangling / stats / plotting
pkgs_core <- c("plyr", "dplyr", "tidyr", "ggplot2", "lubridate", "ggh4x",
               "broom", "AICcmodavg", "car", "ggpubr", "rstatix")
# Habitat metrics + GLMM
pkgs_glmm <- c("GGally", "DataExplorer", "purrr", "patchwork",
               "stringr", "prismatic", "ggforce", "glmmTMB", "ggeffects",
               "DHARMa", "tidyverse", "mgcv")
# Random Forest / GBT
pkgs_rf <- c("ranger", "gbm")
# Non-linear model fitting (BACIPS progressive-change models)
pkgs_nls <- c("minpack.lm", "nls2")
install.packages(pkgs_core)
install.packages(pkgs_glmm)
install.packages(pkgs_rf)
install.packages(pkgs_nls)

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

#https://bedeffinianrowedavies.com/statisticstutorials/multivariateglms
#Zero-Inflated Tweedie Generalized Linear Mixed Model (ZI-Tweedie GLMM) is a powerful statistical approach for modeling continuous, strictly positive data that also contains an excessive number of zeros
#https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
#page 4 -> add ziformula=~1
#did not work 

#mean_density_100m2 ~ Era + habitat_metrics + (1 | Site)

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
      glmmTMB(formula, data = sp_data, family = tweedie(link = "log"),
              ziformula = ~1),
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
  
  cat("\n-- p-values (", r$species, ", ", r$best_name, ") --\n", sep = "")
  coef_tbl <- as.data.frame(summary(r$best_model)$coefficients$cond)
  print(coef_tbl["Pr(>|z|)"])
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


#DHARMa handles non-normal distribution
library(DHARMa) 
sim_res <- simulateResiduals(fittedModel = glmm3_sp, refit = T) # QQ
plotQQunif(sim_res)


testDispersion(sim_res) 

#convergence test: 
# Convergence / trustworthiness check for each species' best model
# Run this after `results` has been built — doesn't touch fit_species_models()

convergence_check <- map_dfr(results, \(r) {
  if (is.null(r$best_model)) {
    return(tibble(
      species        = r$species,
      best_model     = NA_character_,
      pdHess         = NA,
      singular       = NA,
      converged      = NA
    ))
  }
  
  pdHess   <- isTRUE(r$best_model$sdr$pdHess)
  singular <- performance::check_singularity(r$best_model)
  
  tibble(
    species    = r$species,
    best_model = r$best_name,
    pdHess     = pdHess,      # TRUE = Hessian positive-definite (good sign)
    singular   = singular,    # TRUE = singular fit (bad sign)
    converged  = pdHess & !singular
  )
})

print(convergence_check, n = Inf)



#https://bedeffinianrowedavies.com/statisticstutorials/multivariateglms
#Zero-Inflated Tweedie Generalized Linear Mixed Model (ZI-Tweedie GLMM) is a powerful statistical approach for modeling continuous, strictly positive data that also contains an excessive number of zeros
#https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
#page 4 -> add ziformula=~1
#did not work 

#mean_density_100m2 ~ Era + habitat_metrics + (1 | Site)

###pisoch only
habitat_pisoch <- data_PV %>%
  filter(Species == "Pisaster ochraceus") %>%
  select(-c(mean_chl_mg_m3,
            max_chl_mg_m3,
            min_chl_mg_m3,
            mean_sst_C,
            max_sst_C,
            min_sst_C,
            dist_200m_bath)) %>%
  drop_na()

colnames(habitat_pisoch)

glmm0 <- glmmTMB(mean_density_100m2 ~ Era + (1 | Site),
                 ziformula = ~1,
                 data   = habitat_pisoch,
                 family = tweedie(link = "log"))

glmm1 <- glmmTMB(mean_density_100m2 ~
                   Era +
                   Substrate_index +
                   Relief_index +
                   #giantkelp_stipe_density_m2 +
                   (1 | Site),          # Site as random intercept
                 ziformula = ~1,
                 data   = habitat_pisoch,  
                 family = tweedie(link = "log"))

# Interaction model to test if habitat effects differ by Era
glmm2 <- glmmTMB(mean_density_100m2 ~
                   Era * Substrate_index +
                   Era * Relief_index +
                   # Era * giantkelp_stipe_density_m2 +
                   (1 | Site),
                 ziformula = ~1,
                 data   = habitat_pisoch,
                 family = tweedie(link = "log"))


# Interaction model to test if habitat effects differ by Era and Site Category
glmm3 <- glmmTMB(mean_density_100m2 ~
                   Site_Category * Era * Substrate_index +
                   Site_Category * Era * Relief_index +
                   #Site_Category * Era * giantkelp_stipe_density_m2 +
                   (1 | Site),
                 ziformula = ~1,
                 data   = habitat_pisoch,
                 family = tweedie(link = "log"))


AIC(glmm0, glmm1, glmm2, glmm3)
summary(glmm0)
summary(glmm1)
summary(glmm2)
summary(glmm3)




#adjusted code: 7/19/26
###BACIPS####
#####PVR ANALYSIS#####
############################################################################################################
#Object: Perform a Progressive-Change BACIPS analysis using step, linear, asymptotic and sigmoid models	#
#Authors: L. Thiault, L. Kernaléguen, C.W. Osenberg & J. Claudet												#
##### PVR BACIPS #######################################################################################################

# Load packages
require(minpack.lm) # Fitting non-linear models
require(nls2)       # Fitting non-linear models
require(AICcmodavg) # calculate second order AIC (AICc)

### The function requires 4 inputs of class integer with the same length (STEP 1):
# control:    response variable measured at Control sites at each sampling time
# impact:     response variable measured at Impact sites at each sampling time
# time.true:  calendar year of each sample
# time.model: Before period = 0, After period = time since intervention started
# species_name: passed in explicitly so png filename works inside the function

pvr_control_sites <- c("Hawthorne Reef",
                       "Honeymoon Cove",
                       "Lunada Bay")

pvr_impact_sites <- c("Old 18th",
                      "Burial Grounds",
                      "Cape Point") # updated 6/9/26

bacip_sites <- c("Hawthorne Reef",
                 "Honeymoon Cove",
                 "Lunada Bay",
                 "Old 18th",
                 "Burial Grounds",
                 "Cape Point")

time.true  <- c(2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
time.model <- c(0,    0,    0,    0,    0,    0,    1,    2,    3,    4,    5,   6) #2020 is the first year of impact because data was
# 0 = pre-impact; 1+ = years since recovery/impact period began (2021 onwards)
impact_year <- 2020  # calendar year corresponding to time.model == 0/1 boundary

# ---------------------------------------------------------------------------
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
  mutate(bacip_cat = case_when(
    Site %in% pvr_control_sites ~ "Control",
    Site %in% pvr_impact_sites  ~ "Impact")) %>%
  group_by(bacip_cat) %>%
  distinct(Year)

# ---------------------------------------------------------------------------
# Build per-species control/impact lists
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

# ---------------------------------------------------------------------------
# Helper: get predictions from whichever model won, given a time.model grid
get_predictions <- function(model_type, models, tm_grid) {
  switch(model_type,
         "step" = {
           per <- ifelse(tm_grid == 0, "Before", "After")
           predict(models$step, newdata = data.frame(period = per))
         },
         "linear" = {
           predict(models$linear, newdata = data.frame(time.model = tm_grid))
         },
         "asymptotic" = {
           predict(models$asymptotic, newdata = data.frame(time.model = tm_grid))
         },
         "sigmoid" = {
           predict(models$sigmoid, newdata = data.frame(time.model = tm_grid))
         }
  )
}

ProgressiveChangeBACIPS <- function(control, impact, time.true, time.model,
                                    species_name) {
  
  ### STEP 2 — Calculate delta and define impact breakpoint
  delta <- impact - control
  
  time.model.of.impact <- max(which(time.model == 0))
  
  png(file   = paste0(species_name, "_plot_PVR.png"),
      width  = 11,
      height = 4,
      units  = "in",
      res    = 500)
  
  par(mfrow = c(1, 2))
  
  # --- Panel 1: raw delta vs calendar year ---
  plot(delta ~ time.true, type = "n", xaxt = "n",
       xlab = "Year", ylab = "Delta (Impact - Control)",
       main = species_name)
  abline(v = 2020, col = "cadetblue3", lty = 5)
  rect(2013,
       min(delta) - 100,
       2016,
       max(delta) + 100,
       col    = adjustcolor("sandybrown", alpha.f = 0.5),
       border = "white")
  points(delta ~ time.true, pch = 24, bg = "white", cex = 2)
  axis(side = 1, at = seq(2008, 2025, by = 2))
  
  ### STEP 3 — Fit and compete models
  period <- ifelse(time.model == 0, "Before", "After")
  
  ## Step model: instant, constant change
  step.Model <- aov(delta ~ period)
  
  ## Linear model: change increases at constant rate
  linear.Model <- lm(delta ~ time.model)
  
  ## Asymptotic model: rate of change decreases to zero over time
  myASYfun <- function(delta, time.model) {
    funAsy    <- function(parS, time.model) (parS$M * time.model) / (parS$L + time.model) + parS$B
    residFun  <- function(p, observed, time.model) observed + funAsy(p, time.model)
    parStart  <- list(
      M = mean(delta[time.model.of.impact:length(time.true)]),
      B = mean(delta[1:time.model.of.impact]),
      L = 1
    )
    nls_ASY_out <- nls.lm(par     = parStart,
                          fn      = residFun,
                          observed = delta,
                          time.model = time.model,
                          control = nls.lm.control(maxfev = integer(), maxiter = 1000))
    foAsy    <- delta ~ (M * time.model) / (L + time.model) + B
    startPar <- c(-coef(nls_ASY_out)[1], coef(nls_ASY_out)[2], coef(nls_ASY_out)[3])
    nls2(foAsy, start = startPar, algorithm = "brute-force")
  }
  asymptotic.Model <- myASYfun(delta = delta, time.model = time.model)
  
  ## Sigmoid model: rate first increases then decreases to zero
  mySIGfun <- function(delta, time.model) {
    funSIG   <- function(parS, time.model) (parS$M * (time.model / parS$L)^parS$K) / (1 + (time.model / parS$L)^parS$K) + parS$B
    residFun <- function(p, observed, time.model) observed + funSIG(p, time.model)
    parStart <- list(
      M = mean(delta[time.model.of.impact:length(time.true)]),
      B = mean(delta[1:time.model.of.impact]),
      L = mean(time.model),
      K = 5
    )
    nls_SIG_out <- nls.lm(par      = parStart,
                          fn       = residFun,
                          observed = delta,
                          time.model = time.model,
                          control  = nls.lm.control(maxfev = integer(), maxiter = 1000))
    foSIG    <- delta ~ (M * (time.model / L)^K) / (1 + (time.model / L)^K) + B
    startPar <- c(-coef(nls_SIG_out)[1], -coef(nls_SIG_out)[2],
                  coef(nls_SIG_out)[3],  coef(nls_SIG_out)[4])
    nls2(foSIG, start = startPar, algorithm = "brute-force")
  }
  sigmoid.Model <- mySIGfun(delta = delta, time.model = time.model)
  
  ## Model competition via AICc
  AIC.test  <- AIC(step.Model, linear.Model, asymptotic.Model, sigmoid.Model)
  AICc.test <- as.data.frame(cbind(
    AIC.test[, 1],
    c(AICc(step.Model), AICc(linear.Model), AICc(asymptotic.Model), AICc(sigmoid.Model))
  ))
  rownames(AICc.test) <- rownames(AIC.test)
  names(AICc.test)    <- names(AIC.test)
  
  for (j in 1:nrow(AICc.test)) {
    AICc.test$diff[j] <- AICc.test$AIC[j] - min(AICc.test$AIC)
  }
  AICc.test$RL         <- exp(-0.5 * AICc.test$diff)
  AICc.test$aicWeights <- (AICc.test$RL / sum(AICc.test$RL)) * 100
  
  w        <- AICc.test$aicWeights
  names(w) <- rownames(AICc.test)
  print(w)
  
  best.Model <- which(w == max(w))
  model_names <- c("step", "linear", "asymptotic", "sigmoid")
  best_name   <- model_names[best.Model]
  
  models_list <- list(step = step.Model, linear = linear.Model,
                      asymptotic = asymptotic.Model, sigmoid = sigmoid.Model)
  
  ### STEP 4 — Inference from best model
  if (best.Model == 1) {
    writeLines(paste0("\n\nSTEP MODEL SELECTED - Likelihood = ", round(w[1], 1), "%\n\n"))
    print(summary(step.Model))
  }
  if (best.Model == 2) {
    writeLines(paste0("\n\nLINEAR MODEL SELECTED - Likelihood = ", round(w[2], 1), "%\n\n"))
    print(summary(linear.Model))
  }
  if (best.Model == 3) {
    writeLines(paste0("\n\nASYMPTOTIC MODEL SELECTED - Likelihood = ", round(w[3], 1), "%\n\n"))
    print(asymptotic.Model)
  }
  if (best.Model == 4) {
    writeLines(paste0("\n\nSIGMOID MODEL SELECTED - Likelihood = ", round(w[4], 1), "%\n\n"))
    print(sigmoid.Model)
  }
  
  ### STEP 5 — Overlay predicted line on Panel 1 (calendar year axis)
  # Before-period: a single flat prediction (all pre-impact years pool to time.model == 0)
  before_pred <- get_predictions(best_name, models_list, 0)
  segments(x0 = min(time.true[time.model == 0]),
           x1 = max(time.true[time.model == 0]),
           y0 = before_pred, y1 = before_pred,
           col = "black", lwd = 2)
  
  # After-period: time.model maps 1:1 onto calendar year via year = impact_year + time.model
  after_tm_grid <- seq(0, max(time.model), length.out = 200)
  after_pred    <- get_predictions(best_name, models_list, after_tm_grid)
  lines(impact_year + after_tm_grid, after_pred, col = "black", lwd = 2)
  
  legend("topleft", legend = paste0(best_name, " fit (", round(max(w), 1), "%)"),
         lty = 1, lwd = 2, bty = "n", cex = 0.8)
  
  ### STEP 6 — Panel 2: full model fit vs time.model
  plot(delta ~ time.model, pch = 24, bg = "white", cex = 2,
       xlab = "Time since intervention", ylab = "Delta (Impact - Control)",
       main = paste0(species_name, " - ", best_name, " fit"))
  tm_grid   <- seq(min(time.model), max(time.model), length.out = 200)
  pred_grid <- get_predictions(best_name, models_list, tm_grid)
  lines(tm_grid, pred_grid, col = "black", lwd = 2)
  
  dev.off()
  
  ### list of summary needs
  list(
    species     = species_name,
    weights     = w,
    best_model  = best_name,
    models      = models_list,
    time.model  = time.model,
    time.true   = time.true,
    delta       = delta
  )
}

# ---------------------------------------------------------------------------

delta_list    <- list()
bacips_results <- list()

for (i in foc_spp) {
  control <- bacips_data[[i]]$control
  impact  <- bacips_data[[i]]$impact
  delta   <- impact - control
  
  print(paste0("Species = ", i))
  
  bacips_results[[i]] <- ProgressiveChangeBACIPS(control      = control,
                                                 impact       = impact,
                                                 time.true    = time.true,
                                                 time.model   = time.model,
                                                 species_name = i)
  
  delta_list[[i]] <- data.frame(
    Species    = i,
    time.true  = time.true,
    time.model = time.model,
    impact     = impact,
    control    = control,
    PVR_delta  = delta
  )
}

Delta_PVR_all <- do.call(rbind, delta_list)
rownames(Delta_PVR_all) <- NULL

# ---------------------------------------------------------------------------
# 4. SUMMARY OF WINNING MODEL PER SPECIES -----------------------------------
# (The prediction lines themselves are already drawn onto each species' PNG
#  inside ProgressiveChangeBACIPS — this just builds a cross-species table.)

model_comparison_all <- do.call(rbind, lapply(names(bacips_results), function(sp) {
  res <- bacips_results[[sp]]
  data.frame(
    Species    = sp,
    BestModel  = res$best_model,
    Likelihood = round(max(res$weights), 1)
  )
}))
rownames(model_comparison_all) <- NULL

print(model_comparison_all)

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