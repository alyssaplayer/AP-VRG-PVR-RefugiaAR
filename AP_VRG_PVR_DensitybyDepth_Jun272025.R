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
data_PVR <- data_PVR %>%
  filter(DepthZone == "ARM")
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
        #Era = factor(Era, levels = c("Pre-Wasting", "Wasting Event", "Post-Wasting Recovery"), ordered = TRUE),#I added this change to the densitybydepth function and now it's correctly being ordered in the plot
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
ggsave(
  filename = "density_by_depth_stars.png", 
  plot = densitybydepthplot_stars,
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300
)

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

# postwaste_groups <- data_PV %>%
#   filter(Era == c("Post-Wasting Recovery"), Species == c("Mesocentrotus franciscanus")) %>%
#   group_by(Year, Species, Site, Site_Category) %>%
#   dplyr::summarise(DZ_Density_100m2=mean(Density_100m2))
# #do a for loop or pipe with group-by / mutate

dat <- densitybydepth %>%
  group_by(Era, Site_Category) %>% 
  summarise (
    median_count = median(DZ_Density_100m2, na.rm = TRUE), 
    mean_count = mean(DZ_Density_100m2, na.rm = TRUE), 
    sd_count = sd(DZ_Density_100m2, na.rm = TRUE)
  )


# run levene's test of equal variances among groups using leveneTest function in car package
library(car) # this will generate an error if "car" has not been installed
leveneTest(y=dat$DZ_Density_100m2, group=dat$Site_Category)

#########
# display means
tapply(densitybydepth$DZ_Density_100m2, densitybydepth$Era, mean)
# summary function will show parameter estiamtes (group means in one-way ANOVA context)
summary(lm_count_spray)
# note estimates are relative to intercept value (arbitrarily the first level of categorical variable in data)
# BUT, each test is typically not something of interest (i.e., do not use those results for one-way ANOVA)


# Individual value plots + 95% CI for each group
ggplot(dat, aes(y=count, x=spray, col = spray)) +
  stat_summary(fun.data="mean_cl_normal", mapping = aes(group = spray), geom = "crossbar", width = 0.2, col="black", fill = "gray") +
  geom_jitter(width=0.2, size = 2) +
  theme_bw()


#test 11/17/2025
postwaste_filtered <- densitybydepth %>%
  filter(Era == "Post-Wasting Recovery") %>%
  filter(Species == "Mesocentrotus franciscanus")



#https://www.datanovia.com/en/lessons/friedman-test-in-r/
####Friedman Test####
# .... (Ensure you ran the fixed data_PV code above first) ....

#### Friedman Test Loop ####

all_friedman_results <- list()
all_pairwise_results <- list()

# 2. Start the Loop
for (spp in foc_spp) {
  
  # Filter and summarize
  friedman_data <- densitybydepth %>%
    dplyr::filter(Species == spp) %>% 
    group_by(Site, Era, Site_Category) %>% 
    dplyr::summarise(
      Mean_Density = mean(DZ_Density_100m2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Clean: Keep only sites that have data for ALL 3 Eras
  friedman_data_clean <- friedman_data %>%
    group_by(Site) %>%
    dplyr::filter(n() == 3) %>% 
    ungroup()
  
  # Skip if not enough data
  if(nrow(friedman_data_clean) == 0) next 
  
  # Run Tests (Wrapped in try to prevent crashing on zeros/errors)
  try({
    # --- Friedman Test ---
    ft_res <- friedman_data_clean %>%
      group_by(Site_Category) %>%
      rstatix::friedman_test(Mean_Density ~ Era | Site) %>%
      mutate(Species = spp) # Add species identifier
    
    all_friedman_results[[spp]] <- ft_res
    
    # --- Pairwise Comparisons (Wilcoxon) ---
    pwc <- friedman_data_clean %>%
      group_by(Site_Category) %>%
      rstatix::wilcox_test(Mean_Density ~ Era, p.adjust.method = "bonferroni") %>%
      mutate(Species = spp) # Add species identifier
    
    all_pairwise_results[[spp]] <- pwc
    
  }, silent = TRUE)
}
final_friedman_df <- dplyr::bind_rows(all_friedman_results)
final_pairwise_df <- dplyr::bind_rows(all_pairwise_results)

write.csv(final_friedman_df, "Friedman_Test_Results.csv", row.names = FALSE)
write.csv(final_pairwise_df, "Pairwise_Wilcoxon_Results.csv", row.names = FALSE)


#changing my strategy: MPA vs PVR at post-wasting, non-MPA vs PVR at post-wasting 
#storage variable
era_comparisons <- list()

for (spp in foc_spp) {
  
#Filter Species + Era
  era_data <- densitybydepth %>%
    dplyr::filter(Species == spp, Era == "Post-Wasting Recovery") %>%
    #checks for recplicates
    group_by(Site, Site_Category) %>%
    dplyr::summarise(
      Mean_Density = mean(DZ_Density_100m2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 2 categories needed for this test 
  if (n_distinct(era_data$Site_Category) < 2) next
  
  try({
    #Test (Kruskal-Wallis) 
    kw_res <- era_data %>% 
      rstatix::kruskal_test(Mean_Density ~ Site_Category)
    
    # 4. Pairwise Tests (Wilcoxon / Mann-Whitney U)
    # Compares: MPA vs non-MPA | MPA vs PVR | non-MPA vs PVR
    pwc_res <- era_data %>% 
      rstatix::wilcox_test(Mean_Density ~ Site_Category, paired = FALSE, p.adjust.method = "bonferroni") %>%
      dplyr::mutate(
        Species = spp,
        Kruskal_P = kw_res$p  # value showing whether the global kruskal wallis is significant
      )
    
    era_comparisons[[spp]] <- pwc_res
    
  }, silent = TRUE)
}

final_era_comparisons <- dplyr::bind_rows(era_comparisons)
write.csv(final_era_comparisons, "Full_Site_Comparisons_Post-Wasting.csv", row.names = FALSE)

