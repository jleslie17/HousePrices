library(dplyr)

get_cat_cols <- function(x) {
  x %>% 
    select(MSSubClass,
           MSZoning,
           Street,
           Alley,
           LotShape,
           LandContour,
           Utilities,
           LotConfig,
           LandSlope,
           Neighborhood,
           Condition1,
           Condition2,
           BldgType,
           HouseStyle,
           OverallQual,
           OverallCond,
           RoofStyle,
           RoofMatl,
           Exterior1st,
           Exterior2nd,
           MasVnrType,
           ExterQual,
           ExterCond,
           Foundation,
           BsmtQual,
           BsmtCond,
           BsmtExposure,
           BsmtFinType1,
           BsmtFinType2,
           Heating,
           HeatingQC,
           CentralAir,
           Electrical,
           KitchenQual,
           Functional,
           FireplaceQu,
           GarageType,
           GarageFinish,
           GarageQual,
           GarageCond,
           PavedDrive,
           PoolQC,
           Fence,
           MiscFeature,
           MoSold,
           SaleType,
           SaleCondition,
           SalePrice) %>% 
    return()
}
get_num_cols <- function(x) {
  x %>% 
    select(-c(MSSubClass,
              MSZoning,
              Street,
              Alley,
              LotShape,
              LandContour,
              Utilities,
              LotConfig,
              LandSlope,
              Neighborhood,
              Condition1,
              Condition2,
              BldgType,
              HouseStyle,
              OverallQual,
              OverallCond,
              RoofStyle,
              RoofMatl,
              Exterior1st,
              Exterior2nd,
              MasVnrType,
              ExterQual,
              ExterCond,
              Foundation,
              BsmtQual,
              BsmtCond,
              BsmtExposure,
              BsmtFinType1,
              BsmtFinType2,
              Heating,
              HeatingQC,
              CentralAir,
              Electrical,
              KitchenQual,
              Functional,
              FireplaceQu,
              GarageType,
              GarageFinish,
              GarageQual,
              GarageCond,
              PavedDrive,
              PoolQC,
              Fence,
              MiscFeature,
              MoSold,
              SaleType,
              SaleCondition)) %>% 
    return()
}