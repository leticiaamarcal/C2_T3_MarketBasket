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
library(scales)
library(grid)

#upload arquivo
sku_brand <- read_csv2("C:/Users/letic/Documents/UbiqumR/C2_T3_MarketBasket/Data/Raw/products_with_brands.csv")
sku_category <- read_csv2("Data/Raw/products_with_category.csv")
inStore <- read.transactions("Data/Raw/trans_no_shipped.csv", sep = ",")
Products <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/lineitems_.csv", sep = ";")
Orders <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders_.csv", sep = ";")
Transactions <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/trans.csv", sep = ";")
trans_translated <- readxl::read_xlsx("Data/Raw/products_translated.xlsx")

trans_translated$price <- as.numeric(trans_translated$price)

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

#total_paid x date
total_data %>% 
  group_by(date = created_date) %>% 
  summarise(paid = sum(total_paid)) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = paid)) +
  scale_y_continuous(labels = dollar)
  
#creating data frame with brand, category, price to boxplot
brand_cat <- sku_brand %>% 
  left_join(sku_category)

temp <- Products %>% 
  left_join(brand_cat, by = "sku") %>% 
  filter(id_order %in% orders_completed$id_order,
         !is.na(brand),
         !is.na (category))
         
#boxplot category x unit_price
temp %>% 
ggplot(aes(x = category, y = unit_price)) + geom_boxplot()


#changing the file / not good result
trans_translated %>% 
  left_join(brand_cat) %>% 
  filter(!is.na(category)) %>% 
  ggplot() +
  geom_boxplot(aes(x = category, y = price)) +
  scale_y_continuous(labels = dollar)


#creat dataframe
temp2 <- trans_translated %>% 
  left_join(brand_cat) %>% 
  filter(!is.na(category))


#boxplot brand x unit_price
Products %>% 
  left_join(brand_cat, by = "sku") %>% 
  filter(id_order %in% orders_completed$id_order) %>% 
  ggplot(aes(x = category, y = unit_price)) + geom_boxplot()


#######-------
#tentativa 2

#criando dataset que vamos usar
category_DF <- Products %>% 
  left_join(sku_category, by = "sku") %>% 
  left_join(Orders %>% select(state, id_order), by = "id_order") %>% 
  filter(state == "Completed")  %>% 
  mutate(total_price = unit_price * product_quantity) %>% 
  mutate(category = if_else(is.na(category), "unknown",category))

#categorias relevantes
category_relevant <- category_DF %>% 
  group_by(category) %>% 
  summarise(amount = sum(total_price)) %>% 
  arrange(desc(amount))

#classificar os fatores de mais vendas para menos vendas
category_DF$category <- factor(category_DF$category, 
                                     levels = category_relevant$category)

#boxplot - category
category_DF %>% 
  ggplot(aes(x = reorder(category, unit_price), y = unit_price)) + geom_boxplot() +
  coord_flip()

#bar chart / várias tentativas
category_DF %>%  
  ggplot(aes(x = category, y = unit_price)) + geom_col()+
  scale_y_continuous(labels = dollar) +
  coord_flip() 

#esta
category_relevant %>%  
  ggplot(aes(x = reorder(category, amount), y = amount)) + geom_col() + 
  scale_y_continuous(labels = dollar) +
  coord_flip() 

category_DF %>% 
  arrange(desc(unit_price)) %>% 
  ggplot(aes(x = category, y = unit_price)) + geom_col()+
  scale_y_continuous(labels = dollar) +
  coord_flip()
 

#criar dataset brand
brand_DF <- Products %>% 
  left_join(sku_brand, by = "sku") %>% 
  left_join(Orders %>% select(state, id_order), by = "id_order") %>% 
  filter(state == "Completed") %>% 
  mutate(total_price = unit_price * product_quantity) %>% 
  mutate(brand = if_else(is.na(brand), "unknown",brand))

#marcas relevantes
brand_relevant <- brand_DF%>% 
  group_by(brand) %>% 
  summarise(sum = sum(total_price)) %>% 
  arrange(desc(sum))

#classificar os fatores de mais vendas para menos vendas
brand_DF$brand <- factor(brand_DF$brand, 
                            levels = brand_relevant$brand)


important_brands <- brand_relevant %>%  
  head(15)
important_brands %>% 
  ggplot(aes(x = reorder(brand, unit_price), y = unit_price)) +
  geom_col()+
  scale_y_continuous(labels = dollar) +
  coord_flip() 

#boxplot
brand_DF %>% 
  filter(brand %in% important_brands$brand) %>% 
  ggplot(aes(x = reorder(brand, unit_price), y = unit_price)) + 
  geom_boxplot() +
  coord_flip()


  


  
  
  
       
