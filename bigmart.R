#library
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(e1071)
library(caret)
library(dummies)

#importing the dataset
train=fread('Train_data.csv')
test=fread('Test_data.csv')


#creating a new column and combining the dataset
test[,Item_Outlet_Sales := NA]
combi = rbind(train, test) # combining train and test datasets
dim(combi)

#plotting relations
ggplot(data, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")

ggplot(data, aes(Outlet_Identifier, Item_Outlet_Sales)
) + geom_bar(stat = "identity", color = "purple") + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  ggtitle("Outlets vs Total Sales") + theme_bw()

ggplot(data, aes(Item_Type, Item_Outlet_Sales)
) + geom_bar( stat = "identity") + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + 
  xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

ggplot(data, aes(Item_Type, Item_MRP)
) +geom_boxplot() +ggtitle("Box Plot") + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")
  ) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

ggplot(combi) + geom_histogram(aes(Item_Weight), color="black", fill="grey")

ggplot(combi) + geom_histogram(aes(Item_Visibility), color="black", fill="grey")

ggplot(combi) + geom_histogram(aes(Item_MRP), color="grey", fill="grey", binwidth = 0.5)


#filling empty values
train = combi[1:nrow(train)]
colSums(is.na(combi))
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
  
}

zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
  
}

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",
                               ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"
# years of operation of outlets
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)
# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

Item_MRP_clusters = kmeans(combi$Item_MRP, centers = 4)
table(Item_MRP_clusters$cluster)

combi$Item_MRP_clusters = as.factor(Item_MRP_clusters$cluster)

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                                 ifelse(Outlet_Size == "Medium", 1, 2))]
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
# removing categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#one hot encoding
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)


#skewness
combi$Item_Visibility <- log(combi$Item_Visibility + 1)
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]
skewness(combi$Item_Visibility); skewness(combi$price_per_unit_wt)

num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales := NULL]


linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = train)
summary(linear_model)
