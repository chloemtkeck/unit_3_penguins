# Class 02-17-2022
# Intro to ggplot2

library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)

head(penguins)
glimpse(penguins)
summary(penguins)
tail(penguins)

my_data = penguins %>%
  dplyr::select(sex, species) # specifies which package to pull function from  

# scatterplot 
ggplot() + # start with ggplot function then add geometries 
  geom_point(data = penguins, aes(x=flipper_length_mm, y=body_mass_g)) #geom_point adds geometry to scatter plot, aes is aesthetics, what you visualize and how you map it to data

summary(penguins)
penguins %>%
  filter(is.na(flipper_length_mm)) %>%
  summarize(num_nas = n()) #shows NA values and number of NA 

ggplot(data = penguins) + # start with ggplot function then add geometries, add data set so you don't have to repeat
  geom_point(aes(x = flipper_length_mm, 
                 y = body_mass_g, 
                 color = species, # different colors and shapes for each species 
                 shape = species)) +
  geom_smooth(aes(x = flipper_length_mm, y = body_mass_g)) 


ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + # can supply consistent calls in ggplot function so that you do not have to repeat them 
  geom_point(aes(color = species, 
                 shape = species)) +
  geom_smooth(se = F) + # removes shading around SD
  xlab("Flipper length (mm)") +
  ylab("Body magg (g)") +
  ggtitle("penguins are cute :-)") 

# exercise 2.1 

exercise_data = filter(penguins, species == "Adelie")

ggplot(data = exercise_data, aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(color = island))



# line plots !
penguins_ts = penguins %>% #time series data 
  group_by(species, year) %>%
  summarize(count=n())

ggplot(data = penguins_ts) +
  geom_line(aes(x = year, y = count, color = species))


#histogram 
ggplot() +
  geom_histogram(data = penguins, aes(x = flipper_length_mm, color = species))

histogram_flipper = ggplot() +
  geom_histogram(data = penguins, 
                 aes(x = flipper_length_mm, fill = species), 
                 position = "identity", alpha = 0.5) + #alpha makes transparent 
  scale_fill_manual(values = c("palevioletred", "indianred2", "maroon2")) 


#box plots

flipper_box = ggplot(data = penguins) +
  geom_boxplot(aes(y = flipper_length_mm, x = species)) + # makes box plot with three different boxes corresponding to each species 
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), width = 0.2)
ggsave(filename = "figures/flipper_length_histogram.png", plot=histogram_flipper, 
       dpi=300, 
       device="png", 
       width=5, 
       height=5, 
       units="in") #png are "lossless" format meaning figures will stay crispy 


# bar plots 

ggplot() +
  geom_bar(aes(x = sex, fill = species), data = penguins) +
  facet_wrap(~species, nrow = 3) # facet wrap means unique plot for each variable, nrow changes layout 

ggplot() +
  geom_bar(aes(x = island, fill = species), data = penguins) +
  facet_wrap(~species, ncol = 1) +
  coord_flip() +
  theme_bw() #changes theme 

























