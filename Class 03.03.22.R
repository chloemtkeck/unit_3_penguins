library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)
library(car)
library(broom)

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm), 
         !is.na(bill_length_mm), 
         !is.na(species))
lm_3 = lm(bill_depth_mm ~ bill_depth_mm + species, data = penguins_lm_3)  


lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm * species, data = penguins_lm_3)
summary (lm_3)
summary(lm_4)


AIC(lm_3, lm_4)

best_model = step(lm_4)
summary(best_model)

# visualize innteraction mdoel 

ggPredict(lm_4, se = T, interactive = T)

#visualize using tidyverse 

lm_4_predict = lm_4 %>%
  augment(se_fit = T, interval = "confidence")
head(lm_4_predict)

ggplot(data = lm_4_predict) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(aes(x = bill_length_mm, y = .fitted, color = species)) +
  geom_ribbon(aes(x = bill_length_mm, ymin = .lower, ymax = .upper, fill = species), alpha = 0.3)




# multiple continuous variables 


gentoo = penguins_lm_3 %>%
  filter(species=="Gentoo")
head(gentoo)


lm_1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
lm_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
lm_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)

summary(lm_3)
vif(lm_3) # in car() package, generally vif of 1 means no multicolinearity, higher then more problem with multicolinearity, thresholds vary anywhere from 3-10, luckily 2 falls below, meaning a decent model without stressing multicolinearity 


best_model = step(lm_3) # use step on most complicated model 
summary(best_model)
AIC(lm_1, lm_2, lm_3) # lower AIC the better 


# visualize 

newdata = gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm), 
         body_mass_g = median(gentoo$body_mass_g))
head(newdata)


lm_3_predict = lm_3 %>%
  augment(newdata = newdata, interval = "confidence", se_fit = T)
head(lm_3_predict)

ggplot(data = lm_3_predict) +
  geom_line(aes(x = bill_length_mm, y = .fitted)) +
  geom_ribbon(aes(x = bill_length_mm, ymin = .lower, ymax = .upper), alpha = 0.3) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm), data = gentoo) +
  annotate("text", x = 53, y = 13.7, label = paste0("flipper length =", median(gentoo$flipper_length_mm)))



# exercise 5. 3


exercise_data = gentoo %>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm), 
         body_mass_g = median(gentoo$body_mass_g))
head(exercise_data)


lm_3_predict = lm_3 %>%
  augment(newdata = exercise_data, interval = "confidence", se_fit = T)
head(lm_3_predict)

ggplot(data = lm_3_predict) +
  geom_line(aes(x = flipper_length_mm, y = .fitted)) +
  geom_ribbon(aes(x = flipper_length_mm, ymin = .lower, ymax = .upper), alpha = 0.3) +
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm), data = gentoo) +
  annotate("text", x = 208, y = 17, label = paste0("bill length =", median(gentoo$bill_length_mm))) +
  annotate("text", x = 208, y = 16.5, label = paste0("body mass =", median(gentoo$body_mass_g)))


# ANOVA 

penguin_lm = lm(body_mass_g ~ species + sex, data = penguins) # ANOVA categorical, sex and species, uses lm 
summary(penguin_lm)
anova(penguin_lm)


penguin_aov = aov(body_mass_g ~sex + species, data = penguins) # another way to do anova, analysis of variance, not used on linear model, put formula kdirectly 
summary(penguin_aov)

penguins %>% 
  group_by(sex) %>%
  summarize(mean = mean(body_mass_g))

TukeyHSD(penguin_aov) # have to use aov






