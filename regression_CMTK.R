## class 03-1-22

library(palmerpenguins)
library(tidyverse)

install.packages("broom")
install.packages("ggiraphExtra")
library(broom)
library(ggiraph)
library(ggiraphExtra)
library(ggplot2)


ggplot(data = penguins) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm, color = species), method = "lm") +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm), method = "lm", color = "black")


# multiple regression 

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm), 
         !is.na(bill_length_mm), 
         !is.na(species))

#build model 
lm_3 = lm(bill_depth_mm ~bill_length_mm + species, data = penguins_lm_3)
summary(lm_3)
coef(lm_3)
coef(lm_3)[2]
broom::tidy(lm_3)
lm_3_coefs = tidy(lm_3, conf.int=T) %>%
  mutate_if(is.numeric, round, 2) # if numeric round to 2 decimal 
lm_3_coefs


# visualize 

ggPredict(lm_3, se=T, interactive = T) #interactive allows you to click on each data point, hover over lines, etc 


# visualize mdoel predictions using base r

lm_3_predictions = predict(lm_3, interval = "confidence")


lm_3_predictions = cbind(penguins_lm_3, lm_3_predictions)
head(lm_3_predictions)

ggplot(data = lm_3_predictions) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(aes(x = bill_length_mm, y = fit, color = species)) +
  geom_ribbon(aes(x = bill_length_mm, ymin = lwr, ymax = upr, fill = species), alpha = 0.3) +
  theme_bw()


# give predict() new data 
newdata_bill_length = seq(from = min(penguins_lm_3$bill_length_mm), to = max(penguins_lm_3$bill_length_mm), by = 0.1)
newdata = expand.grid(bill_length_mm = newdata_bill_length, species = unique(penguins_lm_3$species))
head(newdata)
tail(newdata)
summary(newdata)


new_data_predict_lm_3 = cbind(newdata, predict(lm_3, interval = "confidence", newdata = newdata)) #data must be structured correctly for this to work 
head(new_data_predict_lm_3)

# cisualize with newdata 

ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data = penguins_lm_3) +
  geom_line(aes(x = bill_length_mm, y = fit, color = species), data = new_data_predict_lm_3) + 
  geom_ribbon(aes(x = bill_length_mm, ymin = lwr, ymax = upr, fill = species), alpha = 0.3, data = new_data_predict_lm_3)



# generate model predictions using tidyverse 

lm_3_predict_tidy = lm_3 %>%
  broom::augment(penguins_lm_3, se_fit = T, interval = "confidence")
glimpse(lm_3_predict_tidy)                           


ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data = lm_3_predict_tidy) +
  geom_line(aes(x = bill_length_mm, y = .fitted, color = species), data = lm_3_predict_tidy) + 
  geom_ribbon(aes(x = bill_length_mm, ymin = .lower, ymax = .upper, fill = species), alpha = 0.3, data = lm_3_predict_tidy)

# generate and predict new data using tidy verse

library(tidyr)
newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
tail(newdata)

lm_3_predict = lm_3 %>%
  broom::augment(newdata = newdata, se_fit = T, interval = "confidence")
head(lm_3_predict)

ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data = penguins_lm_3) +
  geom_line(aes(x = bill_length_mm, y = .fitted, color = species), data = lm_3_predict) + 
  geom_ribbon(aes(x = bill_length_mm, ymin = .lower, ymax = .upper, fill = species), alpha = 0.3, data = lm_3_predict)

