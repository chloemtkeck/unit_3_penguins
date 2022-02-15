# Class 02/15/2022

# install tidyverse and palmerpenguins 
install.packages("tidyverse")
install.packages("palmerpenguins")

# load the tidyverse 
library(tidyverse)
tidyverse_packages() # tells you all the packages that are nested in the tidyverse 


head(iris) # looking at preloaded iris plant data, iris data is good to show package examples

library(palmerpenguins)
head(penguins)
summary(penguins)
dim(penguins) # 8 columns, 344 rows (data)

glimpse(penguins) # shows data in row instead of column 


# start playing with dplyr package 
gentoo = penguins[penguins$species == "Gentoo", ] # base r only penguins whose species are gentoo
head(gentoo)

gentoo = filter(penguins, species == "Gentoo") #with dplyr
head(gentoo)

gentoo_ladies = filter(penguins, species == "Gentoo", sex == "female") # only gentoo females 
head(gentoo_ladies)

# pipes make nested functions (i.e., h(g(f(x))) ) more readable and less error prone, takes more space
gentoo_ladies = penguins %>% 
  filter(species == "Gentoo", sex == "female")
head(gentoo_ladies)

gentoo_ladies = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female")

gentoo_ladies = penguins %>% # the correct way to write code, according to erin, makes code most readable 
  filter(species == "Gentoo",
         sex == "female") %>%
  summarize(mean_body_mass_g = mean(body_mass_g),
            sd_body_mass_g = sd(body_mass_g))

# what this would look like in one line of code (super confusing, have to repeat code)
female_mean_mass = mean(unlist(penguins[which(penguins$sex == "female"), "body_mass_g"]))
female_mean_mass = sd(unlist(penguins[which(penguins$sex == "female"), "body_mass_g"]))


# exercise 1.1 Build a data set that contains only Chinstrap penguins. Then build another data 
# set that contains only Chinstrap penguins with a flipper length > 200 mm. What is the sex ratio
# of Chinstrap penguins? How does that compare to the sex ratio of Chinstrap penguins with a 
# flipper length > 200 mm? Use the `summary()` function to examine sex ratios. 
# Given this analysis, what do you think the relationship is between sex and flipper length?


chinstrap = filter(penguins, species == "Chinstrap")

chinstrap_200_plus_flipper = penguins %>%
  filter(species == "Chinstrap", 
         flipper_length_mm > 200)
chinstrap_200_plus_flipper
summary(chinstrap)
summary(chinstrap_200_plus_flipper)

penguins %>%
  group_by(species, sex) %>%
  filter(!is.na(sex)) %>%
  summarize(count = n())

penguins %>%
  group_by(species, sex) %>%
  filter(flipper_length_mm > 200,
         !is.na(sex)) %>%
  summarize(count = n())

penguins_stats_by_sex = penguins %>%
  group_by(species, sex) %>%
  filter(!is.na(sex)) %>%
  summarize(count = n(), 
            mean_body_mass_g = mean(body_mass_g),
            sd_body_mass_g = sd(body_mass_g))

penguins_stats_by_sex

# save as csv 
write_csv(penguins_stats_by_sex, file="data/processed/penguins_stats_by_sex.csv")




penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) # 0.0022 lb/g
penguins_for_america


penguins %>%
  distinct(island) %>%
  arrange(island)

penguins_mass_sort = penguins %>%
  arrange(desc(body_mass_g))

penguins_mass_sort

penguins_no_morph = penguins %>% 
  select (species, sex, island, year)
penguins_no_morph

penguins_no_flipper = penguins %>%
  select(-flipper_length_mm) # removes flipper length 
penguins_no_flipper
glimpse(penguins_no_flipper)

