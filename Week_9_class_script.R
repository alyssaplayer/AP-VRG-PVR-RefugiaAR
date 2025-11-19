# CREATION DATE 12 Nov 2016
# MODIFIED DATE 22 Nov 2016

# AUTHOR: Chelsea Williams, cmunoz@oxy.edu

# PURPOSE: 
# Two-way ANOVA Examples (2)
# (1) Visualize data (individaul value plots with 95% CIs and interaction plots)
# (2) To save time in class we'll assume models meet assumptions - but you'll need to assess for assignments/real analyses
# (3) run a Two-way ANOVA and interpret ANOVA table
# (4) multiple comparison tests


##########
## Two-Way ANOVA Examples
##########

#!!!!! To save time in class, we will assume data meets two-way ANOVA assumptions !!!!
# But for assignments and with your own data you must check assumptions
# And if you finish early in class, try to assess model asumptions


# Example 1

#The Effect of Vitamin C on Tooth Growth in Guinea Pigs
# The variables are "len", which is tooth length, the response variable, 
# the way in which the supplement was administered, "supp", 
# of which the levels are "OJ" (as orange juice) and "VC" (as ascorbic acid), 
# and "dose" of the supplement in milligrams (0.5, 1.0, and 2.0).

# see more info on data set in help file
?ToothGrowth 
# assign built in data set to a new object name so it can be modified as needed
dat_TG<-ToothGrowth
head(dat_TG)
str(dat_TG)

# dose read in by default as numeric
class(dat_TG$dose)
# R will interpret this as a quantitative variable if left as class numeric and 
# if used in a lm linear model, R will run it as a regression (or ANCOVA), not a two-way ANOVA

# to-do !!! 
# convert dose variable to class factor (function as.factor)
dat_TG$dose <- as.factor(dat_TG$dose)

# look at structure of data to confirm dose was converted to class factor
str(dat_TG)

# use table function to see sample size in each group
table(dat_TG$supp, dat_TG$dose)

####
# !!! to do: Answer these questions (discuss with your group) with output from above:
####
# How many "treatments" are there? 6
# What is the sample size in each Treatment? 10
# Is this study design "balanced"? yes


####
## Plots for Example 1 data & two-way ANOVA interaction plots
####
# load ggplot2 package
require(ggplot2)

# Individual value plots + 95% CI for each group (supp on x-axis, faceted by dose)
ggplot(dat_TG, aes(y=len, x=dose, col = supp)) +
  stat_summary(fun.data="mean_cl_normal", mapping = aes(group = supp), geom = "crossbar", width = 0.2, col="black", fill = "gray") +
  geom_jitter(width=0.2, size = 2) +
  facet_wrap(~supp) +
  theme_bw()

# Individual value plots + 95% CI for each group (dose on x-axis, faceted by supp)
ggplot(dat_TG, aes(y=len, x=supp, col = dose)) +
  stat_summary(fun.data="mean_cl_normal", mapping = aes(group = dose), geom = "crossbar", width = 0.2, col="black", fill = "gray") +
  geom_jitter(width=0.2, size = 2) +
  facet_wrap(~dose) +
  theme_bw()

# interaction plot (dose on x-axis, supp lines)
ggplot(dat_TG, aes(dose, len, col=supp, shape = supp)) +
  stat_summary(aes(group=supp), fun.y = mean, geom="point", size = 3) +
  stat_summary(aes(group=supp), fun.y = mean, geom="line") 

# interaction plot (supp on x-axis, dose lines)
ggplot(dat_TG, aes(supp, len, col=dose, shape = dose)) +
  stat_summary(aes(group=dose), fun.y = mean, geom="point", size = 3) +
  stat_summary(aes(group=dose), fun.y = mean, geom="line") 

# Interaction plots in base R
par(mfrow=c(2,1))
interaction.plot(dat_TG$supp,dat_TG$dose,dat_TG$len)
interaction.plot(dat_TG$dose, dat_TG$supp, dat_TG$len)

###
# !!! to do: Use the plots above to answer the following?
###
# How does sup appear to effect tooth length? Expect significant effect?
# How does dose appear to effect tooth length? Expect significant effect?
# Does there appear to be a significant interaction? If so, describe it.



### Be prepared to discuss above (pause for full class discussion) before running the ANOVA



#####
## Specify linear model and run ANOVA for Example 1


# to-do !!! 
## create a lm model object
lm_supp_dose <- lm(len ~ supp + dose + supp:dose, data = dat_TG)

# to-do !!! 
# produce an ANOVA results table using the anova function with the lm object you created above
anova(lm_supp_dose)


# to-do !!! 
# What is the H0's & Ha's for each test in the two-factor ANOVA?
#
#
#

# interpret and discuss results from ANOVA table with your group:
#    What does each number in the table mean?
#    What do these results tell you? What can't they tell you?



### Pause here for class discussion of ANOVA Table





#############
# Two-way ANOVA Example 2
# Simulate data with a main effect for one variable 
# (but not for 2nd variable and not for interaction)
#############

# simulated data
# 3 species of bacteria (sp1, sp2, sp3) are grown 
# under three experimental treatments (A, B, C)

# simulate data for treatment A
dat_bac<-data.frame(treatment="A", count = round(rnorm(30, mean=100, sd=30),0) )
# add data for treatment B
dat_bac<-rbind(dat_bac, data.frame(treatment="B", count = round(rnorm(30, mean=150, sd=30),0) ))
# add data for treatment C
dat_bac<-rbind(dat_bac, data.frame(treatment="C", count = round(rnorm(30, mean=200, sd=30),0) ))
# add column for second explanitory variable: species
dat_bac$species<-rep(c("sp1", "sp2", "sp3"), 30)
#convert species column to class factor (from class character)
dat_bac$species<-as.factor(dat_bac$species)

# examine structure of data
str(dat_bac)
head(dat_bac)

#
table(dat_bac$treatment, dat_bac$species)


### Plots
require(ggplot2)

# !!! to do (adapt code from Example 1):
# Individual value plots + 95% CI for each group  

# Individual value plots + 95% CI for each group (species on x-axis, faceted by treatment)
ggplot(dat_bac, aes(y=count, x=treatment, col = species)) +
        stat_summary(fun.data="mean_cl_normal", mapping = aes(group = species), geom = "crossbar", width = 0.2, col="black", fill = "gray") +
        geom_jitter(width=0.2, size = 2) +
        facet_wrap(~species) +
        theme_bw()


# !!! to do (adapt code from Example 1):
# Individual value plots + 95% CI for each group (switch which categorical variable is on x-axis) 

# Individual value plots + 95% CI for each group (dose on x-axis, faceted by supp)
ggplot(dat_bac, aes(y=count, x=species, col = treatment)) +
        stat_summary(fun.data="mean_cl_normal", mapping = aes(group = treatment), geom = "crossbar", width = 0.2, col="black", fill = "gray") +
        geom_jitter(width=0.2, size = 2) +
        facet_wrap(~treatment) +
        theme_bw()


# !!! to do (adapt code from Example 1):
# interaction plot

# interaction plot (treatment on x-axis, species lines)
ggplot(dat_bac, aes(treatment, count, col=species, shape = species)) +
        stat_summary(aes(group=species), fun.y = mean, geom="point", size = 3) +
        stat_summary(aes(group=species), fun.y = mean, geom="line") 


# !!! to do (adapt code from Example 1):
# interaction plot (switch which categorical variable is on the x-axis and used for the lines)

# interaction plot (species on x-axis, treatment lines)
ggplot(dat_bac, aes(species, count, col=treatment, shape = treatment)) +
        stat_summary(aes(group=treatment), fun.y = mean, geom="point", size = 3) +
        stat_summary(aes(group=treatment), fun.y = mean, geom="line") 


# !!! to do
# Use plots you made above to answer the following:
# How does treatment appear to affect count? Expect significant effect?
# How does species appear to affect count? Expect significant effect?
# Does there appear to be a significant interaction? If so, describe it.



###
# Pause for full class discussion
###



# !!! to do :
## create linear model object for two-way ANOVA of treatement and species
# to-do !!! 
## create a lm model object
lm_species_treatment <- lm(count ~ species + treatment + species:treatment, data = dat_bac)

# to-do !!! 
# produce an ANOVA results table using the anova function with the lm object you created above
anova(lm_species_treatment)


# to-do !!! 
# What is the H0's & Ha's for each test in the two-factor ANOVA?

# interpret and discuss results from ANOVA table with your group:
#    What does each number in the table mean?
#    What do these results tell you? What can't they tell you?