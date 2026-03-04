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
library("ggpubr")
library("rstatix")


#setwd("/Users/alyssaplayer/Desktop/AP VRG PVR 2024")

####DATA MANAGEMENT AND FILTERING####
#### Species Info ####
# Read data set that contains means between replicates
data_PVR <- read.csv("PV_Stars_Urchins2025-07-09.csv", check.names = F)
crane_data <-read.csv("PV_Stars_Urchins2025-11-05.csv", check.names = F)
#data_PVR <- data_PVR %>%
 # filter(DepthZone == "ARM")
data_PV <- rbind(data_PVR, crane_data)
  
colnames(data_PV)[colnames(data_PV)=="BenthicReefSpecies"] <- "Species"
colnames(data_PV)[colnames(data_PV)=="SampleYear"] <- "Year"

data_PV <- data_PV %>%
  mutate(Density_100m2=100*Density_m2) %>% #Create a column that is Density per 100m2
  complete(nesting(Site, Year), Species, fill = list(Density_m2=0, Density_100m2=0)) 


####Focal Species List####
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
        DepthZone = factor(DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"), ordered = TRUE)
  )

  
#Creating the functional groups for the species
data_PV <- data_PV %>%
    mutate(
      FunctionalGroup = case_when(
        Species %in% c("Patiria miniata", "Pisaster ochraceus", "Pisaster giganteus") ~ "Stars",
        Species %in% c("Mesocentrotus franciscanus", "Strongylocentrotus purpuratus") ~ "Urchins",
        TRUE ~ "Other"  # To catch any species not listed
        ),
      FunctionalGroup = factor(FunctionalGroup, levels = c("Stars", "Urchins")) #defining the functional groups 
    )

#Creating the site groups #all separated
# data_PV <- data_PV %>%
#   mutate(Site_Category = case_when(
#     Site %in% c("Long Point East",
#                 "120 Reef",
#                 "Abalone Cove Kelp West",
#                 "Long Point West",
#                 "Old Marineland",
#                 "Point Vicente West",
#                 "Portuguese Point") ~ "MPA",
#     Site %in% c("Ridges North",
#                  "Honeymoon Cove",
#                  "Resort Point",
#                  "Rocky Point North",
#                  "Rocky Point South",
#                  "Hawthorne Reef",
#                  "Lunada Bay") ~ "PVR-Control",
#     Site %in%  c("3 Palms East",
#                 "3 Palms West",
#                 "Bunker Point",
#                 "Burial Grounds",
#                 "Cape Point",
#                 "KOU Rock",
#                 "Old 18th") ~ "PVR-Adj",
#     DepthZone == "ARM" ~ "PVR",
#     TRUE ~ "Non-MPA"
#   ))


#Creating the site groups  - #with PVR and pVR-Adj together
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
    Site %in%  c("3 Palms East",
                 "3 Palms West",
                 "Bunker Point",
                 "Burial Grounds",
                 "Cape Point",
                 "KOU Rock",
                 "Old 18th") ~ "PVR-Adj",
    DepthZone == "ARM" ~ "PVR-Adj",
    TRUE ~ "Non-MPA"
  ))

###writes a complete site list 
# complete_site_list <- data_PV %>%
#   select(Site, Site_Category)%>%
#   distinct()
#write.csv(complete_site_list, "Complete Site List.csv", row.names = FALSE)

#site categories, MPA, non-MPA, PVR, PVR-Control, PVR-Adj

####CREATING THE DENSITY PLOTS####
densitybydepth <- data_PV %>%
  group_by(DepthZone, Year, Species, Site, Era, Site_Category) %>%
  dplyr::summarise(DZ_Density_100m2=(mean(Density_100m2))) %>%
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
# ggsave(
#   filename = "density_by_depth_stars.png", 
#   plot = densitybydepthplot_stars,
#   width = 10, 
#   height = 8, 
#   units = "in", 
#   dpi = 300
# )

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

#changing my strategy: MPA vs PVR at post-wasting, non-MPA vs PVR at post-wasting 
# #storage variable
# era_comparisons <- list()
# 
# for (spp in foc_spp) {
#   
# #Filter Species + Era
#   era_data <- densitybydepth %>%
#     dplyr::filter(Species == spp, Era == "Post-Wasting Recovery") %>%
#     #checks for replicates
#     group_by(Site, Site_Category) %>%
#     dplyr::summarise(
#       Mean_Density = mean(DZ_Density_100m2, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   # 2 categories needed for this test 
#   if (n_distinct(era_data$Site_Category) < 2) next
#   
#   try({
#     #Test (Kruskal-Wallis) 
#     kw_res <- era_data %>% 
#       rstatix::kruskal_test(Mean_Density ~ Site_Category)
#     
#     # 4. Pairwise Tests (Wilcoxon / Mann-Whitney U)
#     # Compares: MPA vs non-MPA | MPA vs PVR | non-MPA vs PVR
#     pwc_res <- era_data %>% 
#       rstatix::wilcox_test(Mean_Density ~ Site_Category, paired = FALSE, p.adjust.method = "bonferroni") %>%
#       dplyr::mutate(
#         Species = spp,
#         Kruskal_P = kw_res$p  # value showing whether the global kruskal wallis is significant
#       )
#     
#     era_comparisons[[spp]] <- pwc_res
#     
#   }, silent = TRUE)
# }
# 
# 
# era_comparisons_output <- dplyr::bind_rows(era_comparisons)
# #write.csv(final_era_comparisons, "Full_Site_Comparisons_Era.csv", row.names = FALSE)

###PROPORTION CALCULATIONS####
#difference of means: 
#take the mean of MPA pre-wasting and post-wasting (ignore wasting)
#carrying capacity of the habitat type might vary 
differences <- data_PV %>%
  group_by(Site_Category, Species, Era) %>%
  mutate(Site_Mean = mean(Density_100m2), stdev = sd(Density_100m2)) %>%
  select(Site_Category, Species, Era, stdev, Site_Mean) %>%
  distinct() %>%
  pivot_wider(names_from = Era, values_from = c(stdev, Site_Mean)) %>%
  group_by(Site_Category, Species) %>%
  mutate(stdev_Difference = `stdev_Post-Wasting Recovery` - `stdev_Pre-Wasting`) %>%
  mutate(mean_Difference = `Site_Mean_Post-Wasting Recovery` - `Site_Mean_Pre-Wasting`)
#write.csv(differences, "PostWasting_PreWasting_Differences_20260117.csv", row.names = FALSE)

#the data going into the plot
#needs to remove site - PLOT
proportion <- data_PV %>%
  group_by(Site_Category, Species, Era, Site) %>% 
  mutate(Site_Mean = mean(Density_100m2), stdev = sd(Density_100m2)) %>%
  select(Site_Category, Species, Era, Site_Mean, stdev) %>%
  #filter(!(Site_Category == 'Non-MPA' & Species == 'Strongylocentrotus purpuratus' & Site =='Portuguese Bend')) %>%
  distinct() %>%
  pivot_wider(
    names_from = Era, 
    values_from = c(Site_Mean, stdev),
    names_sep = "_" 
  ) %>%
  mutate(prop_baseline = `Site_Mean_Post-Wasting Recovery` / `Site_Mean_Pre-Wasting`)
   
#error bars needs site - BARS  
proportion_forerror <- data_PV %>%
  group_by(Site_Category, Species, Era, Site) %>% 
  mutate(Site_Mean = mean(Density_100m2),stdev = sd(Density_100m2)) %>%
  select(Site_Category, Species, Era, Site_Mean, stdev, Site) %>%
  filter(!(Site_Category == 'Non-MPA' & Species == 'Strongylocentrotus purpuratus' & Site =='Portuguese Bend')) %>%
  distinct() %>%
  pivot_wider(
    names_from = Era, 
    values_from = c(Site_Mean, stdev),
    names_sep = "_" 
  ) %>%
  mutate(prop_baseline = `Site_Mean_Post-Wasting Recovery` / `Site_Mean_Pre-Wasting`)

#calculate error bars 
error_bars <- proportion_forerror %>%
  filter(!is.infinite(prop_baseline)) %>%
  group_by(Site_Category, Species) %>%
  summarise(
    error_mean = mean(prop_baseline),stdev = sd(prop_baseline), .groups = "drop") #.group drops all grouping layers

##keep this version
proportion_plot <- ggplot(proportion(Site_Category, prop_baseline, color = Site_Category)) +
  geom_bar() +  
  facet_wrap(~ Species, ncol = 2) +
  labs(
    title = "Post-Wasting Recovery as Proportion of Pre-Wasting",
    x = "Site Category",
    y = "Proportion of Baseline",
    color = "Site Category",
    fill = "Site Category"
  ) +
  ylim(-5, 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "italic", size = 10),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_brewer(palette = "Set2") +   
  scale_fill_brewer(palette = "Set2") 
show(proportion_plot)

####BOX PLOT VERSION? 
# proportion_plot <- ggplot(proportion, aes(x = Site_Category, y = prop_baseline, 
#                                           color = Site_Category, fill = Site_Category)) +
#   geom_boxplot(alpha = 0.4, linewidth = 0.8, outlier.size = 1.5) +
#   geom_jitter(width = 0.1, size = 2, alpha = 0.6) +# fill adds color inside boxes
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
#   scale_color_brewer(palette = "Set2") +   # Set2 is more saturated than Pastel1
#   scale_fill_brewer(palette = "Set2") 

proportion_plot <- ggplot(proportion, aes(x = Site_Category, color = Site_Category)) +
  geom_jitter(aes(y = prop_baseline), width = 0.1, size = 1.5, alpha = 0.4) +  
  geom_point(aes(y = avg), size = 4) +                                          
  geom_errorbar(aes(ymin = avg - error_sd, ymax = avg + error_sd),              
                width = 0.2, linewidth = 0.8) +
  facet_wrap(~ Species, ncol = 2) +
  labs(
    title = "Post-Wasting Recovery as Proportion of Pre-Wasting",
    x = "Site Category",
    y = "Proportion of Baseline",
    color = "Site Category"
  ) +
  ylim(-5, 5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "italic", size = 10),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_brewer(palette = "Set2")
show(proportion_plot)

show(proportion_plot)


#write.csv(differences, "PostWasting_PreWasting_Differences_20260117.csv", row.names = FALSE)


###OLD CODE BEFORE THINGS WENT SO WRONG 
proportion <- data_PV %>%
  group_by(Site_Category, Species, Era) %>% #CHELSEA THIS WAS OUR ISSUE......, adding site_cat makes them unique
  mutate(Site_Mean = mean(Density_100m2), stdev = sd(Density_100m2))  %>%  
  select(Site_Category, Species, Era, Site_Mean) %>%
  distinct() %>%
  pivot_wider(names_from = Era, values_from = c(Site_Mean)) %>%
  #group_by(Species) %>%
  #mutate(stdev_Difference = `stdev_Post-Wasting Recovery` - `stdev_Pre-Wasting`) %>%
  pivot_wider(names_from = Era, values_from = Site_Mean) %>%
  mutate(prop_baseline = `Post-Wasting Recovery` / `Pre-Wasting`) %>%
  filter(!is.infinite(prop_baseline)) 

proportion_plot <- ggplot(proportion, aes(x = Site_Category, y = prop_baseline, fill = Site_Category)) +
  geom_col() +
  facet_wrap(~ Species, ncol = 2) +
  labs(
    title = "Post-Wasting Recovery as Proportion of Pre-Wasting",
    x = "Site Category",
    y = "Proportion of Baseline",
    fill = "Site Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "italic", size = 10),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Pastel1")
#still need to facet wrap, change colour palette? don't love it 
show(proportion_plot)


show(proportion_plot)
#write.csv(differences, "PostWasting_PreWasting_Differences_20260117.csv", row.names = FALSE)


