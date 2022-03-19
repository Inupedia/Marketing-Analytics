library(corrplot)
library(tidyverse)
library(ggmosaic)
library(gridExtra)

# import dataset that has been preprocessed and has merged features
setwd("~/Desktop/Winter2022/DSC424/Final")
df = read.csv('clean_merge_data.csv')

# separate numeric features
df_num <- df[,c('Age','Income','Kidhome', 'Teenhome', 'Recency', 'MntWines', 
                'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 
                'MntGoldProds', 'NumDealsPurchases', 'NumWebPurchases', 
                'NumCatalogPurchases', 'NumStorePurchases', 'NumWebVisitsMonth', 
                'total_children', 'total_amount', 'total_num_purchase')]

# separate categorical features
df_factor <- df[,c('Education', 'Marital_Status', 'AcceptedCmp1', 'AcceptedCmp2',
                   'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'Complain', 
                   'Response', 'Country', 'age_class', 'income_class')]

# correlation plot with all numeric features
cor.df = cor(df_num)
corrplot(cor.df, method="ellipse", order="AOE")

# plot that visualizes total amount of sales by product type
product_performance <- summarise(df, wines_mean = mean(MntWines), 
                                 fruits_mean = mean(MntFruits),
                                 meat_mean = mean(MntMeatProducts), 
                                 fish_mean = mean(MntFishProducts),
                                 sweets_mean = mean(MntSweetProducts), 
                                 gold_mean = mean(MntGoldProds))

product_performance <- data.frame(product = colnames(product_performance),
                                  average_amount = round(as.numeric(product_performance[, 1:6]), 2))

product_performance$product <- factor(product_performance$product, 
                                      levels = c("wines_mean", "fruits_mean", 
                                                 "meat_mean", "fish_mean",
                                                 "sweets_mean", "gold_mean"))

# plot that visualizes total amount of sales by channel
ggplot(product_performance, aes(x = product, y = average_amount)) + geom_col() +
  ggtitle("Total Amount by Product") + labs(y = "Avgerage Amount", x = "Product") + 
  scale_x_discrete(labels = c("wines", "fruits", "meat", "fish", "sweets", "gold"))


channel_performance <- summarise(df, deal_mean = mean(NumDealsPurchases),
                                 web_mean = mean(NumWebPurchases),
                                 catalog_mean = mean(NumCatalogPurchases),
                                 store_mean = mean(NumStorePurchases))

channel_performance <- data.frame(channel = colnames(channel_performance),
                                  avgerage_purchases = round(as.numeric(channel_performance[1,]), 2))

channel_performance$channel <- factor(channel_performance$channel,
                                      levels = c("deal_mean", "web_mean", "catalog_mean", "store_mean"))

ggplot(channel_performance, aes(x = channel, y = avgerage_purchases)) + geom_col() +
  ggtitle("Total Amount by Channel") + labs(y = "Average Amount", x = "Channel") + 
  scale_x_discrete(labels = c("deal", "web", "catalog","store"))

# boxplots that show total amount of sales by categorical features
par(mfrow=c(2,3))  
boxplot(df$total_amount ~ df$income_class, xlab="Income Class", ylab = "Total Amount")
boxplot(df$total_amount ~ df$Marital_Status, xlab="Marital Status", ylab = "Total Amount")
boxplot(df$total_amount ~ df$age_class, xlab="Age Class", ylab = "Total Amount")
boxplot(df$total_amount ~ df$Country, xlab="Country", ylab = "Total Amount")
boxplot(df$total_amount ~ df$Complain, xlab="Complain", ylab = "Total Amount")
boxplot(df$total_amount ~ df$Response, xlab="Response", ylab = "Total Amount")

# mosaic plots that show how categorical features relate to Response feature
par(mfrow=c(2,2)) 
p1 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(age_class, Response), fill=age_class)) + 
  labs(title='Response vs. Age')

p2 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(Marital_Status, Response), fill=Marital_Status)) + 
  labs(title='Response vs. Marital Status')

p3 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(Education, Response), fill=Education)) + 
  labs(title='Response vs. Education')

p4 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(Country, Response), fill=Country)) + 
  labs(title='Response vs. Country')

grid.arrange(p1, p2, p3, p4, ncol = 2)

# show how pairwise numeric features are related to Response feature
df_num_2 <- df[, c('Age', 'Income', 'Recency', 'total_children', 'total_amount', 'total_num_purchase')]
group <- NA
group[df$Response == 0] <- 1
group[df$Response == 1] <- 2

pairs(df_num_2,
      col = c("red", "blue")[group], 
      labels = c('Age', 'Income', 'Recency', 'total_children', 'total_amount',
                 'total_num_purchase'),
      main = "Numeric Features by Response")
