# 02-22-2022

library(rstatix)
library(palmerpenguins)
library(tidyverse)

summary(penguins)

penguins %>% dplyr::filter(sex=="male")
find("filter")


# explore data before anything 

head(penguins)
glimpse(penguins)

#plot body mass histograms 

ggplot() +
  geom_histogram(data = penguins, aes(x = body_mass_g, fill = species))


gentoo = penguins %>%
  filter(species=="Gentoo", !is.na(body_mass_g))
mean(gentoo$body_mass_g)
sd(gentoo$body_mass_g)


ggplot() + 
  geom_histogram(data = gentoo, aes(x = body_mass_g))


#QQ plot 
ggplot() + 
  stat_qq(aes(sample = body_mass_g), data = gentoo)

gentoo_body_mass_g_lit = 5500 # from paper XXX from et al. 2010
t.test(gentoo$body_mass_g, mu = 5500)

t_test_results = gentoo %>% 
  t_test(body_mass_g ~ 1, mu = 5500)





# compare gentoo and adelie body mass 

data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"), !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()
summary(data_for_t_test)

data_for_t_test %>% 
  group_by(species) %>%
  summarise(mean_body_mass_g = mean(body_mass_g),
            sd_body_mass_g = sd(body_mass_g))

ggplot() +
  geom_histogram(data = data_for_t_test, aes(x = body_mass_g)) +
  facet_wrap(~species, scales = "free")

# QQ plot 

ggplot() +
  stat_qq(data = data_for_t_test, aes(sample = body_mass_g)) +
  facet_wrap(~species, scales = "free")

# t-test are gentoo and adelie body masses significantly different?

data_for_t_test %>% levene_test(body_mass_g ~ species) # if p<0.05 variances are unequal 

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal = T)

t_test_results = data_for_t_test %>%
  t_test(body_mass_g ~ species)


exercise_data = penguins %>%
  filter(species=="Adelie", !is.na(flipper_length_mm), !is.na(sex)) %>%
  select(species, flipper_length_mm, sex) %>%
  droplevels()
head(exercise_data)


ggplot() +
  geom_histogram(data = exercise_data, aes(x = flipper_length_mm, fill = sex)) +
  facet_wrap(~sex, scales = "free")

exercise_t_test = exercise_data %>%
  t_test(flipper_length_mm ~ sex)

