library(tidyverse)

# Read in data
df <- read.csv("data/train.csv", header = T)
head(df)
str(df)
df <- as_tibble(df)
df
names(df)

# Poke around a bit
cor(df$SalePrice, df$LotArea)
df %>% 
  ggplot(aes(x = log(LotArea), y = SalePrice)) +
  geom_point()

df %>% 
  ggplot(aes(Neighborhood, SalePrice)) +
  geom_boxplot(aes(x = reorder(Neighborhood, 
                               SalePrice, 
                               FUN = median))
  ) +
  coord_flip()

df %>% 
  ggplot(aes(Condition1, SalePrice)) +
  geom_boxplot(aes(x = reorder(Condition1,
                               SalePrice,
                               FUN = median))) +
  coord_flip()

df %>% 
  ggplot(aes(HouseStyle, SalePrice)) +
  geom_boxplot(aes(x = reorder(HouseStyle,
                               SalePrice,
                               FUN = median))) +
  coord_flip()
