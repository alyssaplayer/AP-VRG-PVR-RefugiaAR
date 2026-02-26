
#the data going into the plot
#needs to remove site - PLOT
proportion <- data_PV %>%
  group_by(Site_Category, Species, Era) %>% 
  mutate(Site_Mean = mean(Density_100m2), stdev = sd(Density_100m2)) %>%
  select(Site_Category, Species, Era, Site_Mean, stdev) %>%
  filter(!(Site_Category == 'Non-MPA' & Species == 'Strongylocentrotus purpuratus' & Site =='Portuguese Bend')) %>%
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


#!is.infinite() - remove infinite values from prop_baseline and run the error bars, use the pro

#calculate error bars 
# error_bars <- proportion_forerror %>%
#   group_by(Site_Category, Species) %>% 
#   mutate(stdev = sd(prop_baseline)) %>%
#   filter(!is.infinite(stdev)) %>%  # <--- Filter added here
#   select(Site_Category, Species, stdev) %>%
#   distinct()

#inclined to keep this version
error_bars <- proportion_forerror %>%
  filter(!is.infinite(prop_baseline)) %>%   
  group_by(Site_Category, Species) %>% 
  summarise(
    avg = mean(prop_baseline, na.rm = TRUE), 
    stdev = sd(prop_baseline, na.rm = TRUE), 
    .groups = "drop"
  )


proportion_plot <- ggplot(proportion, aes(x = Site_Category, y = prop_baseline, color = Site_Category, error_bars)) + #aes = error_bars
  geom_jitter() +
  facet_wrap(~ Species, ncol = 2) +
  labs(
    title = "Post-Wasting Recovery as Proportion of Pre-Wasting",
    x = "Site Category",
    y = "Proportion of Baseline",
    fill = "Site Category" ) +
  ylim(0, 3) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "italic", size = 10),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_brewer(palette = "Pastel1")+
  show(proportion_plot)
