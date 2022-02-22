#02-22-2022  Correlations 

head(gentoo)


ggplot() + 
  geom_point(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm))

cor(x = gentoo$bill_length_mm, y = gentoo$bill_depth_mm) #anywhere between -1 to 1, 0 meaning no correllation 
cor.test(x = penguins$bill_length_mm, y = penguins$bill_depth_mm)


# correlation matrix 
head(gentoo)
cor(gentoo[,c(3:6)])
install.packages("GGally")
library(GGally)


gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()
