#------------------------------------------------------------------------------------ 
#Goal: Preprocess
#Description: Limpar os dados e transformar variáveis
#Developer: Letícia Marçal
#------------------------------------------------------------------------------------

#importar data
#o csv e um arquivo separado por virgula e lido como tabela
#neste arquivo, ele esta separado por ; por isso nao esta sendo 
#lido da maneira correta. Quando colocamos sep = ";" avisamos
#o R que ele deve ler ; como separação. pode usar assim:
#Orders <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders.csv", sep = ";")
#mas pode usar read_csv2 tambem como abaixo
#Products <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/lineitems .csv")
#Orders <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders.csv")

#libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(readr)


Products <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/lineitems_.csv", sep = ";")
Orders <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders_.csv", sep = ";")
Transactions <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/trans_.csv")



#understanding columns
Products %>% select(product_id)
Products %>% summarise(mean(Products))

#delete product_id column (its doesnt say anything)
Products <- Products[,-3]

#semi_join to compare what tables have in common. Number of rows change
#pay attetion to it
Matches <- semi_join(Orders, Products) #it takes Orders as reference
Matches2 <- semi_join(Products, Orders)
Matches_ <- anti_join(Products, Orders) #what doesnt match 

#know how many hows we have for completed orders
Matches %>% filter(state == "Completed")
Orders %>% filter(state == "Completed")

#looking for NA's 
sum(is.na(Orders))
sum(is.na(Products))
which(is.na(Orders))

#exclude NAs
Orders <- na.omit(Orders)

#Create a new table with these columns:
#ID order, state, product_quantity, sku, total_price, total_paid

#Join "sku" in one column and kept the ID
skuDF <- aggregate(Products$sku, list(Products$id_order), paste, collapse="; ") 
#use collapse="," to separate the texts by coma

#creat a new column that multiply the product quantity and unit_price
Products$TotalPrice <- Products$product_quantity * Products$unit_price

#group by id_order to be able to sum TotalPrice column with same
#id_order. With summarise, I created a column that sum TotalPrice and
#other that sum product_quantity
Products %>% 
  group_by(id_order)%>% 
  summarise(total_price = sum(TotalPrice), product_quantity = sum(product_quantity)) -> JoinDF

#Join tables 
joinNew <- bind_cols(JoinDF, skuDF)

#join tables
JoinTotal <- inner_join(joinNew, Orders)

#changing the position of the columns
finalDF <- JoinTotal[, c(1, 7, 3, 5, 2, 8, 6, 4)]

#deleting the last column
finalDF <- finalDF[,-8]

#changing names
colnames(finalDF)[4] <- 'sku'

#create a new column shipping
finalDF$shipping <- finalDF$total_paid - finalDF$total_price

#put only two decimal places
finalDF$shipping <- round(finalDF$shipping, digits =2)

#changing the position of the columns
finalDF <- finalDF[,c(1, 2, 3, 4, 5, 6, 8, 7)]

#cross transactions file with the table I created
transactions_completed <- finalDF[finalDF$sku  %in% as.character(Transactions$sku),]
#this table has duplicated values. Now we have to delete them

#deleting duplicates
transactions_complete <- transactions_completed[!duplicated(transactions_completed$sku),]
#da pra usar distinct também, mas tem que ser um tipo específico de tabela

#Filter the shipping rows that are btween -50 and 50
transactions_complete %>% filter(shipping >= -50, shipping <= 50) -> transactions_complete

###-----------

#join id_order and sku (from transctions file)
transactions_id <- Transactions[Transactions$sku %in% transactions_completed$sku,]
#it didnt work



# new code ---------------------------------------------------------------

# get transactions that are completed
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

total_data <- total_data[, c(1, 2, 3, 4, 5, 7, 6, 8)]

#export the dataframe
write.csv(total_data, file = "total_data.csv")
getwd()

#I want to keep just id_order and sku
trans_id <- total_data[, -c(2, 4, 5, 6, 7, 8)]

total_data %>%
  # filter() %>% 
  select(sku) %>% 
  write_csv(path = "trans_id.csv")

#export
write.csv(trans_id, file = "trans_id.csv")

total_data %>% 
  mutate(created_date = lubridate::as_datetime(created_date)) %>% 
  group_by(date = lubridate::date(created_date)) %>% 
  summarise(sum = sum(total_paid)) %>% 
  ggplot(aes(x = date, y = sum)) +
    geom_line()
