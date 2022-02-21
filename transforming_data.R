library(corrplot)
library(psych)
library(stats)
library(forcats)
library(semPlot)
# import clean_merge_data
ds = clean_merge_data
summary(ds)
ds = ds[ ,-c(1:2)]

#### 1. Clear levels on variables ####
# Education and Marital_Status
# Education: Basic -> HighSchool / Graduation -> Bachelor / 2n Cycle -> Master 
# Marital_Status: Absurd & Alone & YOLO -> Other
ds$Education = fct_recode(ds$Education, HighSchool = "Basic", Bachelor = "Graduation", Master = "2n Cycle")
ds$Marital_Status = fct_recode(ds$Marital_Status, Other = "Absurd", Other = "Alone", Other = "YOLO")

#### 2. Examining the data for correlations ####

#### 2-1. Check Multicollinearity ####
# Pull out just the numeric fields, except Age and Income (both variables become categorical variables )
# Compute the correlation matrix and visualize it
xx = c("total_children", "Recency", "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds", "NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth")
dsNumeric = ds[, names(ds) %in% xx] 
cor.mkt = cor(dsNumeric)
corrplot(cor.mkt, method = "ellipse", order="AOE")
dsReduced = dsNumeric[, -1]

#### 2-2. Measures of suitability and fit ####
# Is the dataset suitable for factor analysis?
# Sphericity and Sample Adequacy
bartlett.test(dsReduced)     # Yes, significant correlation
KMO(dsReduced)               # Higher MSA 0.88

####  2-3. Principal Factor Analysis ####
# step1: choose a number of factors
p1 = prcomp(dsReduced, scale=T)
plot(p1)
abline(1, 0, col="red")
summary(p1)

# step2: run "principal"
p2 = principal(dsReduced, nfactors = 5, rotate = "varimax")
print(p2$loadings, cutoff=.4, sort = T)

NewData = as.data.frame(p2$scores)
names(NewData) = c("LightFoodProds", "MeatWinePur", "WinePur", "PurWithChild", "GoldProds")

# step3: put newdata in dataset
dsFactors = cbind(ds, NewData)
head(dsFactors)
dsFactors = dsFactors[, -c(2, 5:7, 10:20, 29:31)]
dsFactors = dsFactors[, c(1,14,2,3,15,13,16:20,12,5,4,6:10,11)]


write.csv(dsFactors,"clean_factor_data.csv", row.names = TRUE)
