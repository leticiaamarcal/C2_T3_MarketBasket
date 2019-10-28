#------------------------------------------------------------------------------------ 
#Goal: Preprocess
#Description: Limpar os dados/ arquivos ElectronidexTransactions2017
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

#upload arquivo
trans <- read.transactions(file = "Data/Raw/trans.csv", sep = ",")
sku_brand <- read_csv2("C:/Users/letic/Documents/UbiqumR/C2_T3_MarketBasket/Data/Raw/products_with_brands.csv")
sku_category <- read_csv2("Data/Raw/products_with_category.csv")
sku_pname <- readxl::read_xlsx("Data/Raw/products_byname.xlsx")

#understanding the data
inspect(trans) #You can view the transactions. 
length(trans) #Number of transactions.
size(trans) #Number of items per transaction
LIST(trans) #Lists the transactions by conversion (LIST must be capitalized)
itemLabels(trans) #To see the item labels

#plot
itemFrequencyPlot(trans, type = c("absolute")) 
itemFrequencyPlot(trans, type = c("relative"))

#By using the image() function, you can visualize all of the transactions within 
#your dataset. It will display the 125 possible items as your x-axis 
#(the columns in your sparse matrix) and the number of transactions 
#(the rows in your sparse matrix) as your y-axis. 
#The items that were purchased are blacked out.
image(trans)

#The sample() function will create a sample that contains a random set of transactions.
image(sample(trans, 2000))

#Apriori Algorithm
s1c8 <- apriori (trans, parameter = list(supp = 0.1, conf = 0.8))
s1c8m2 <- apriori (trans, parameter = list(supp = 0.1, conf = 0.8, minlen=2))
s1c1m2 <- apriori (trans, parameter = list(supp = 0.1, conf = 1, minlen=2))
s5c1m2 <- apriori (trans, parameter = list(supp = 0.05, conf = 1, minlen=2))
s4c1m2 <- apriori (trans, parameter = list(supp = 0.04, conf = 1, minlen=2))
          apriori (trans, parameter = list(supp = 0.03, conf = 1, minlen=2))
          apriori (trans, parameter = list(supp = 0.2, conf = 1, minlen=2))
          apriori (trans, parameter = list(supp = 0.003, conf = 0.8, minlen=2))
s3c1m2 <- apriori (trans, parameter = list(supp = 0.0002, conf = 0.5, minlen=2))

#métricas - são ruins, por isso vamos usar clusters (separar por tipo de produto)
inspect(s3c1m2)

#visualize the rules
arulesViz::ruleExplorer(s3c1m2)

# inspect matrix names
arules::itemInfo(trans)
trans@itemInfo

#using the file categories

prod_category <- trans@itemInfo %>%
  rename(sku = labels) %>% 
  left_join(sku_category)

trans@itemInfo$category <- prod_category$category

trans@itemInfo$  #check what is inside

trans_categories <- arules::aggregate(trans, itemInfo(trans)[["category"]])

#run an Apriori Algorithm with the new data
aprioriModel_c <- apriori (trans_categories, parameter = list(supp = 0.0001, conf = 0.3, minlen=1))

#inspect
inspect(aprioriModel_c)
#lhs    rhs           support confidence lift count
#[1] {}  => {accessories} 1       1          1    10454

summary(aprioriModel_c)

##--------------

#now run the model with brand

#using the file brand
prod_brand <- trans@itemInfo %>%
  rename(sku = labels) %>% 
  left_join(sku_brand) %>% 
  distinct()

trans@itemInfo$brand <- prod_brand$brand

trans@itemInfo$

trans_brand <- arules::aggregate(trans, itemInfo(trans)[["brand"]])


#run an Apriori Algorithm with the new data
aprioriModel_b <- apriori (trans_brand, parameter = list(supp = 0.022, conf = 0.7))

#cria a tabelona
ruleExplorer(aprioriModel_b)

#inspect
inspect(aprioriModel_b)

###----

#add brand and category together


prod_brand_cat <- trans@itemInfo %>%
                  left_join(sku_brand, by = c("labels" = "sku")) %>% 
                  left_join(sku_category, by = c("labels" = "sku")) %>%
                  unite(col = "brand_category", brand, category, sep = "_") %>% 
  distinct() %>% 
  as_tibble()

trans@itemInfo$brand_category <- prod_brand_cat$brand_category

trans_bran_category <- arules::aggregate(trans, trans@itemInfo$brand_category)

rules <- apriori(trans_bran_category, 
                 parameter = list(supp = 0.00001, conf = 0.4),
                 appearance = list(rhs = c("Apple_laptop")))


ruleExplorer(rules)

#ver qual rule é mais forte (por lift, support ou confidence)
inspect(sort( rules, by = "lift"))
inspect(sort( rules, by = "support"))
inspect(sort( rules, by = "confidence"))

#To see a certain item's rules
ItemRules <- subset(rules, items %in% "Apple_laptop")
inspect(ItemRules)

#To find out if you have any redundant rules. 

#TRUE means there are redundant rules, and FALSE means there are not.
is.redundant(rules)

#plot
plot(rules)

#additional resource that provides more plotting option
plot(rules[11], method="graph", control=list(type="items"))
plot(rules, engine = "plotly")
inspectDT(rules)

####-------

#take the accessories off

prod_bc_noAc <- trans@itemInfo %>%
  left_join(sku_brand, by = c("labels" = "sku")) %>% 
  left_join(sku_category, by = c("labels" = "sku")) %>%
  filter(category != "accessories") %>% 
  unite(col = "brand_category", brand, category, sep = "_") %>% 
  distinct() %>% 
  as_tibble()

trans@itemInfo$brand_category <- prod_bc_noAc$brand_category
#error

trans_bc_exA <- arules::aggregate(trans, trans@itemInfo$brand_category)

rules_noA <- apriori(trans_bc_exA, 
                     parameter = list(supp = 0.0003, conf = 0.6),
                     appearance = list(rhs = c("Apple_laptop")))

ruleExplorer(rules_noA)


