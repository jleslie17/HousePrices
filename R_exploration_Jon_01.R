library(tidyverse)
source("tools.R")

# Read in data
train <- read.csv("data/train.csv", header = T)
train <- as_tibble(train)

# Poke around a bit
cor(train$SalePrice, train$LotArea)
train %>% 
  ggplot(aes(x = log(LotArea), y = SalePrice)) +
  geom_point()

train %>% 
  ggplot(aes(Neighborhood, SalePrice)) +
  geom_boxplot(aes(x = reorder(Neighborhood, 
                               SalePrice, 
                               FUN = median))
  ) +
  coord_flip()

train %>% 
  ggplot(aes(Condition1, SalePrice)) +
  geom_boxplot(aes(x = reorder(Condition1,
                               SalePrice,
                               FUN = median))) +
  coord_flip()

train %>% 
  ggplot(aes(HouseStyle, SalePrice)) +
  geom_boxplot(aes(x = reorder(HouseStyle,
                               SalePrice,
                               FUN = median))) +
  coord_flip()

# Examine the distribution of SalePrice
ggplot(train, aes(x = SalePrice)) +
  geom_histogram()
ggplot(train, aes(x = log(SalePrice))) +
  geom_histogram()
sd(log(train$SalePrice))
summary(log(train$SalePrice))
quantile(log(train$SalePrice))
IQR(log(train$SalePrice))

# Separating factors from numeric/int features
cat_train <- get_cat_cols(train)
num_train <- get_num_cols(train)
# rm(train)

# make sure levels of factors make sense for ordinal data
# Dates? May need to normalise to range of dates in both
# train and test sets
# MoSold, YrSold

names(cat_train)
names(num_train)
# Function to explore numerical data:
make_scatterplot <- function(a, df = num_train) {
  df %>% 
    mutate(x = df[[a]]) %>% 
    select(SalePrice, x) %>% 
    filter(!is.na(x)) %>% 
    ggplot(aes(x, y = SalePrice)) + 
    geom_point() + 
    xlab(a) +
    stat_smooth(method = "lm")
}  

# Find the correlations between price and all other numeric features
num_cors <- cor(num_train, 
                num_train$SalePrice, 
                use = "pairwise.complete.obs") 
# Rank them in descending order:
num_cors <- num_cors[order(-num_cors), , drop = F]
# PLot those that have a correlation greater than 0.5:
for (i in 1:nrow(num_cors)) {
  if (num_cors[i] > 0.5) {
    print(row.names(num_cors)[i])
    print(make_scatterplot(row.names(num_cors)[i]))
  }
}

# This makes a good start, but it needs cleaning up. For example, there are
# items where a 0 value should probably be an NA, like TotalBsmtSF and
# GarageArea...the 0 values are probably hurting the correlation.

cor(num_train, num_train$GrLivArea, use = "pairwise.complete.obs")
