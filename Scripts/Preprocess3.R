#------------------------------------------------------------------------------------ 
#Goal: Preprocess
#Description: Apriori with in store data
#Developer: Letícia Marçal
#------------------------------------------------------------------------------------

#libraries
library(arules)
library(arulesViz)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(devtools)
library(StandardizeText)
library(ggplot2)

#upload arquivo
trans <- read.transactions(file = "Data/Raw/trans.csv", sep = ",")
sku_brand <- read_csv2("C:/Users/letic/Documents/UbiqumR/C2_T3_MarketBasket/Data/Raw/products_with_brands.csv")
sku_category <- read_csv2("Data/Raw/products_with_category.csv")
inStore <- read.transactions("Data/Raw/trans_no_shipped.csv", sep = ",")

#in Store data
inStore_trans <- inStore@itemInfo %>%
  left_join(sku_brand, by = c("labels" = "sku")) %>% 
  left_join(sku_category, by = c("labels" = "sku")) %>%
  unite(col = "brand_category", brand, category, sep = "_") %>% 
  distinct() %>% 
  as_tibble()

inStore@itemInfo$brand_category <- inStore_trans$brand_category

inStore_transactions <- arules::aggregate(inStore, inStore@itemInfo$brand_category)

rules3 <- apriori(inStore_transactions, 
                  parameter = list(supp = 0.0003, conf = 0.5),
                  appearance = list(rhs = c("Apple_laptop")))

ruleExplorer(rules3)

#plotings
plot(rules3, jitter = 0)

