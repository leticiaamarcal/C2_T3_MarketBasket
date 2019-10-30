#------------------------------------------------------------------------------------ 
#Goal: Charts
#Description: visualization of the data
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
sku_brand <- read_csv2("C:/Users/letic/Documents/UbiqumR/C2_T3_MarketBasket/Data/Raw/products_with_brands.csv")
sku_category <- read_csv2("Data/Raw/products_with_category.csv")
inStore <- read.transactions("Data/Raw/trans_no_shipped.csv", sep = ",")
Products <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/lineitems_.csv", sep = ";")
Orders <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders_.csv", sep = ";")
Transactions <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/trans.csv", sep = ";")

#get transactions that are completed
orders_completed <- Orders %>% 
  filter(state == "Completed")

# transactions have more than 1 unique product
products_2more <- Products %>% 
  group_by(id_order) %>% 
  summarise(count = n()) %>% 
  filter(count >= 2)

# filter completed orders
final_data <- products_2more %>% 
  filter(id_order %in% orders_completed$id_order)

# join transaction and final data
final_data_orders <- final_data %>% 
  bind_cols(Transactions) %>% 
  left_join(Orders, by = "id_order") 

# total price 
products_total_price <- Products %>% 
  mutate(total_price = product_quantity * unit_price) %>% 
  group_by(id_order) %>% 
  summarise(total_price = sum(total_price))

# join the total price
total_data <- final_data_orders %>% 
  left_join(products_total_price, by = "id_order") %>% 
  mutate(difference = round(total_paid - total_price, 2))

#reorder colums
total_data <- total_data[, c(1, 2, 3, 4, 5, 7, 6, 8)]

#changing names
colnames(total_data)[3] <- 'sku'

#padronizar data
total_data <- total_data %>% 
  mutate(created_date = lubridate::as_datetime(created_date))

total_data %>% 
  mutate(created_date = lubridate::as_datetime(created_date)) %>% 
  group_by(date = lubridate::date(created_date)) %>% 
  summarise(sum = sum(total_paid)) %>% 
  ggplot(aes(x = date, y = sum)) +
  geom_line()
