# CREATION DATE 10 Nov 2016
# MODIFIED DATE 17 Nov 2016

# AUTHOR: full name, e-mail address

# PURPOSE: 
# One-way ANOVA Example
# (1) Visualize data
# (2) Assess model assumptions
# (3) run a One-way ANOVA and interpret ANOVA table
# (4) multiple comparison tests
# (5) Alternatives when assumptions not met 



###################
#  data are from an agricultural experiment in which six different insect sprays were tested in many different fields
dat<-InsectSprays
head(dat)
str(dat)

# use the table function to count the number of data points (rows in your table) 
# for each categorical variable (spray type)
table(dat$spray)

# load ggplot2 package
require(ggplot2)
# individual value plot by group, jittered
ggplot(dat, aes(x=spray, y=count, col=spray)) +
  geom_jitter(width=.25, size=3)

# boxplots by group
ggplot(dat, aes(x=spray, y=count, col=spray)) +
  geom_boxplot()

# histograms faceted by group
ggplot(dat, aes(count)) +
  geom_histogram(binwidth = 3, fill="white", col="black")+
  facet_wrap(~spray, ncol = 1)

# cacluate summary statistics
?tapply
tapply(dat$count, dat$spray, mean)
tapply(dat$count, dat$spray, sd)
tapply(dat$count, dat$spray, summary)

# advanced use dplyr package
require(dplyr)
dat %>% 
  group_by(spray) %>% 
  summarise (median_count=median(count), mean_count = mean(count), sd_count = sd(count))


# run levene's test of equal variances among groups using leveneTest function in car package
library(car) # this will generate an error if "car" has not been installed
leveneTest(y=dat$count, group=dat$spray)


#########
# create linear model object for a one-way anova:
# Does the type of insect spray effect the count of insects?
lm_count_spray<-lm(count ~ spray, data = dat)
lm_count_spray
# use anova function to display ANOVA table results
anova(lm_count_spray)


#########
# display means
tapply(dat$count, dat$spray, mean)
# summary function will show parameter estiamtes (group means in one-way ANOVA context)
summary(lm_count_spray)
# note estimates are relative to intercept value (arbitrarily the first level of categorical variable in data)
# BUT, each test is typically not something of interest (i.e., do not use those results for one-way ANOVA)


# Individual value plots + 95% CI for each group
ggplot(dat, aes(y=count, x=spray, col = spray)) +
  stat_summary(fun.data="mean_cl_normal", mapping = aes(group = spray), geom = "crossbar", width = 0.2, col="black", fill = "gray") +
  geom_jitter(width=0.2, size = 2) +
  theme_bw()
# !!! BUT  - if data have outliers or are heavily skewed, 95%CIs will not be accurate/valid, especially if sample size small.

#########
# Tukey Honestly Significant Difference test
# implemented in the base R as the default post hoc test 

TukeyHSD(aov(lm_count_spray)) 
## !!! compare p adj with p-values in summary results 
#(see how pairwise comparisons with A, p-value adjusted upwards in Tukey HSD)

# TukeyHSD runs all possible pairwise tests by default
# but the more tests, the more conservative each test becomes 
# (i.e., more p-value is adjusted, so the more "diffifcult" it is to find sig difference)
# plot, use boxplot (since it is easy to plot) to compare differences
par(mfrow=c(1,2))
plot(TukeyHSD(aov(lm_count_spray)))
boxplot(dat$count ~ dat$spray)
dev.off() # reset par, so next plot is single plot in plot window

# Additional code I found online to add letters indicating significant pairwise differences to a ggplot
# http://rcompanion.org/rcompanion/d_05.html


# also can look at residules the same way we did for the linear regression model
# can use to assess normality assumption
hist(resid(lm_count_spray))
qqnorm(resid(lm_count_spray))
qqline(resid(lm_count_spray))



## If ANOVA assumptions are not met - here are some options:

#####
# this function can run a one-way ANOVA with a Welch correction if variances not equal (like corrected version of 2 sample t-test)
oneway.test(count ~ spray, data=dat) 
# however output information is limited and cannot run post hoc tests using it

#####
# run nonparametric version of one-way ANOVA:  Kruskal-Wallis Test
kruskal.test(count ~ spray, data = dat)

##############
#sqrt transform count (use sqrt instead of log10 because data has 0's, and very strong transformation is not necessary)
dat$count_sqrt<- sqrt(dat$count)

## plot untransformed and transformed data to compare
# plot untransformed data to look at variability in transformed response variable
ggplot(dat, aes(x=spray, y=count, col=spray)) +
  geom_jitter(width=.25, size=3) + 
  labs(title = "count data") 
# plot data to look at variability in transformed response variable
ggplot(dat, aes(x=spray, y=count_sqrt, col=spray)) +
  geom_jitter(width=.25, size=3) + 
  labs(title = "sqrt transformed count data")

# calculate sd for each group, compare sd for sqrt transformed data
tapply(dat$count_sqrt, dat$spray, sd) # check if largest > 2*smallest?
# run levene's test again with transformed data
leveneTest(y=dat$count_sqrt, group=dat$spray)

# run linear model with tranformed response variable
lm_count_sqrt_spray<-lm(count_sqrt ~ spray, data = dat)
lm_count_sqrt_spray
anova(lm_count_sqrt_spray)
