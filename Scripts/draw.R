# create document join information

#libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(readr)


# load data ---------------------------------------------------------------

Products <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/lineitems_.csv")
Orders <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders_.csv")
Transactions <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/trans_.csv")
trans_id <- read.transactions(file = "Data/Raw/trans_id.csv", sep = ",")



# MBA create products -----------------------------------------------------

library(arules)



# add a new labels in the matrix
sku_category <- read_csv2("Data/Raw/products_with_category.csv")

# inspect matrix names
arules::itemInfo(trans)
trans@itemInfo



temp <- trans@itemInfo %>%
  rename(sku = labels) %>% 
  left_join(sku_category)

trans@itemInfo$category <- temp$category

trans@itemInfo
trans_categories <- arules::aggregate(trans, itemInfo(trans)[["category"]])
