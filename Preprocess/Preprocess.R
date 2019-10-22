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
#Orders <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders.csv", sep = ';')
#mas pode usar read_csv2 tambem como abaixo
#Products <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/lineitems .csv")
#Orders <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders.csv")

Transactions <- read.csv("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/transactions.csv")
Products <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/lineitems .csv")
Orders <- read_csv2("~/UbiqumR/C2_T3_MarketBasket/Data/Raw/orders.csv")

#libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(readr)

#change column names
colnames(Orders)[4] <- 'total_paid'
colnames(Products)[7] <- 'unit_price'

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

#Create a new table with these columns:
#ID order, state, product_quantity, sku, total_price, total_paid

#Join "sku" in one column and kept the ID
skuDF <- aggregate(Products$sku, list(Products$id_order), paste, collapse=",") 
#use collapse="," to separate the texts by coma

#transforming as interger
Products$product_quantity <- as.integer(Products$product_quantity)
Products$unit_price <- as.integer(Products$unit_price)

#creat a new column that multiply the product quantity and unit_price
Products$TotalPrice <- Products$product_quantity * Products$unit_price

#group by id_order to be able to sum TotalPrice column with same
#id_order. With summarise, I created a column that sum TotalPrice and
#other that sum product_quantity
Products %>% group_by(id_order)%>% 
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
             
