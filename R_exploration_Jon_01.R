library(tidyverse)

df <- read.csv("data/train.csv", header = T)
head(df)
str(df)
df <- as_tibble(df)
df
names(df)
cor(df$SalePrice, df$LotArea)
cor(df)
df %>% 
  ggplot(aes(x = log(LotArea), y = SalePrice)) +
  geom_point()
df %>% 
  ggplot(aes(Neighborhood, SalePrice)) +
  stat_summary(
    fun.y = median,
    geom = "point"
  ) +
  coord_flip()
df %>% 
  ggplot(aes(Neighborhood, SalePrice)) +
  geom_boxplot(aes(x = reorder(Neighborhood, 
                               SalePrice, 
                               FUN = median))
  ) +
  coord_flip()
