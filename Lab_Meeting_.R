library(tidyverse)
install.packages("ggforce")
library(ggforce)

ds <- read_csv("data/group_split_metrics_class.csv")

#id in this data refers to participant id and model is the 2 types of models we can look at (split and group)

#Prevalence is how much was that thing observed (e.g., how prevalent was held, prone, sitting, etc.)

# The equal sign is used for many ways; one equal sign is making the value change to that whereas double equal means youre just seeing what is true or a comparison 
#Only keep the models where it says "group" and not include "split" using the double == 
ds <- filter(ds, model == "group")

#mapping is what things are on the X axis, what is on the Y and how to represent data graphically 
#in this example we are looking at prevalence and class aka the different body positions
ggplot(data = ds, mapping = aes(x = Class, y = Prevalence)) +
  geom_boxplot()

#you can add more things like labels for each axis or x and y lims
ggplot(data = ds, mapping = aes(x = Class, y = Kappa)) +
  geom_boxplot() + ylab("Reliability (Kappa)") + xlab("")

#when looking at scatter plots you can use geom_point
geom_point(size = 2, alpha = .5)

ggplot(data = ds, mapping = aes(x = Class, y = Kappa, size = Prevalence)) +
  geom_point(alpha = .5) + ylim(0, 1) + ylab("Reliability (Kappa)") + xlab("")


ggplot(ds, aes(x = Kappa, fill = Class)) + geom_histogram()

ggplot(ds, aes(x = Kappa, fill = Class)) + geom_histogram() + facet_wrap(~ Class)

#stat summary 

#theme_minimal
