data <- read.csv("C:/Users/isabe/Downloads/clean_merge_data.csv")

datan <- data[,c('Age','Income','Kidhome', 'Teenhome', 'Recency', 'MntWines', 
                 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 
                 'MntGoldProds', 'NumDealsPurchases', 'NumWebPurchases', 
                 'NumCatalogPurchases', 'NumStorePurchases', 'NumWebVisitsMonth', 
                 'total_children', 'total_amount', 'total_num_purchase')]
head(datan)
summary(data)

# linear regression using all numerical variables.
model1 <- lm(Income~., data = datan)
summary(model1)

# linear regression between income and spending products
model2 <- lm(Income~ MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds, data = datan)
summary(model2)

# linear regression between income and spending patterns
model3 <- lm(Income~ NumDealsPurchases+NumWebPurchases+NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth, data = datan)
summary(model3)

model4 <- lm(Income~ Age+Kidhome+Teenhome+Recency, data = datan)
summary(model4)

# linear regression of campaign
model5 <- lm(Response~ AcceptedCmp1+AcceptedCmp2+AcceptedCmp3+AcceptedCmp4+AcceptedCmp5, data= data)
summary(model5)

# linear regression between income and spending prodroucts except the gold.
datan$TotalMnt <- datan$MntWines * datan$MntFruits * datan$MntMeatProducts * datan$MntFishProducts * datan$MntSweetProducts 
model6 <- lm(Income~ TotalMnt, data = datan)
summary(model6)
