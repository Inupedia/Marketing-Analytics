# Load the data
ds = read.csv("clean_data.csv")


# create total_children variable by merging Kidhome and Teenhome
ds$total_children = ds$Kidhome + ds$Teenhome


# create total_amount variable by merging: 
# MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds
ds$total_amount = rowSums(ds[ , c('MntWines',
                                  'MntFruits',
                                  'MntMeatProducts',
                                  'MntFishProducts',
                                  'MntSweetProducts',
                                  'MntGoldProds')])


# create total_num_purchase variable by merging:
# NumWebPurchases, NumCatalogPurchases, NumStorePurchases
ds$total_num_purchase = rowSums(ds[ , c('NumWebPurchases',
                                        'NumCatalogPurchases',
                                        'NumStorePurchases')])

# Create age_class variable by grouping customers based on their age
ds$age_class = ""
for(i in 1:nrow(ds)){
  
  if (ds[i, "Age"] > 58) {
    ds[i, "age_class"] = "Boomers"
  } else if (ds[i, "Age"] > 42) {
    ds[i, "age_class"] = "Gen-X"
  } else if (ds[i, "Age"] > 26) {
    ds[i, "age_class"] = "Millennials"
  } else {
    ds[i, "age_class"] = "Gen-Z"
  }
}

# create income_class by grouping customers based on their income
ds$income_class = ""
for(i in 1:nrow(ds)){
  
  if (ds[i, "Income"] > 300000) {
    ds[i, "income_class"] = "Rich"
  } else if (ds[i, "Income"] > 100000) {
    ds[i, "income_class"] = "Upper-Middle"
  } else if (ds[i, "Income"] > 50000) {
    ds[i, "income_class"] = "Middle"
  } else if (ds[i, "Income"] > 30000) {
    ds[i, "income_class"] = "Lower-Middle"
  } else {
    ds[i, "income_class"] = "Poor"
  }
}

write.csv(ds,"clean_merge_data.csv", row.names = TRUE)

