setwd("C:\\Users\\Alessandra\\Documents\\DataScience\\1year\\fds-project")

houseTrain=read.csv("train.csv",stringsAsFactors = F)
houseTest= read.csv("test.csv",stringsAsFactors = F)
houseTest$SalePrice<- NA

house<-rbind(houseTrain,houseTest)
#length(house[1:200,1])
library(tidyverse)
library(corrplot)
# more categories 
for (i in c("Alley", "Fence", "PoolQC","FireplaceQu", "MiscFeature","BsmtQual","BsmtCond","BsmtExposure",
            "BsmtFinType2","GarageType","GarageFinish","GarageQual","GarageCond","MasVnrType", "BsmtFinType1")){
  house[,i]<-house[,i] %>%  replace_na("None")
}

str(house)
length(which(house$GarageYrBlt == "None"))
summary(lm(SalePrice~ MSZoning, data = house ))
house[which(house$GarageYrBlt == "None"),] %>% dplyr::select(YearBuilt, YearRemodAdd,GarageYrBlt)

?select

# data tidyng
house1<- house[!rowSums(is.na(house)) > ncol(house)*.9,]


boxplot(house$GarageYrBlt)

str(house)

facts <- house[,sapply(house,is.character)]
yy <- data.frame(lapply(facts, as.factor))
noume <- house[,sapply(house,is.numeric)]

# correlations ------------------------------------------------------------

cor_numVar <- cor(noume, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")



# removing lotfrontage -----------------------------------------------------
#after seeing the correlTIONS LOT FRONTAGE DOESN T HAve enough particular correlations with the other variables


noume$LotFrontage <- NULL
#summary(house)
#colnames(house)

# removing outliers ------------------------------------------
#house[,27] è MasVnrArea
house[is.na(house[,27]), 27] <- mean(house[,27], na.rm = TRUE)
house[is.na(house[,27]), 27]

colnames(noume) 

n <- length(noume)
upper<-vector(mode='integer',length = n)
lower<-vector(mode='integer',length = n)

noume$SalePrice

for (i in  2:n){
  upper[i]<- mean(noume[,i],na.rm = TRUE)+ 3 *sd(noume[,i],na.rm = TRUE)
  lower[i] <-  mean(noume[,i],na.rm = TRUE) - 3 *sd(noume[,i],na.rm = TRUE) 
}

#upper[38]
#noume[1470,38]

#a <-  subset(noume, noume[,n] >lower[n]  & noume[,n] < upper[n] ) # the problem is here

a <-  subset(noume, noume[,36] >lower[36]  & noume[,36] < upper[36] ) #changed from 37 to 36, it will not remove the outliers at price 
for (i in 2:(n-2)){ # the -1 is now -2
  a<- subset(a, a[,i] >lower[i]  & a[,i] < upper[i] )
}

###it's necessary to take from noume the saleprice column and put it in a

# normalization -----------------------------------------------------------
tempId <- a[,1]
tempSalePrice <- a[,37]
#price_sell<- a[,38]
#a$SalePrice<-NULL
aNormZ <- as.data.frame(scale(a))
#anormZ$SalePrice<-price_sell
aNormZ[,1] <- tempId
aNormZ[,37] <- tempSalePrice
aNormZ$KitchenAbvGr <- NULL
aNormZ$PoolArea <- NULL
aNormZ$BsmtHalfBath <- NULL

# lm ----------------------------------------------------------------------
alias(lm(SalePrice ~.,data=a))
# yy operations and cleaning ----------------------------------------------

##### drop the column street, utilities,condition2,roofmatl,misc_feature,poolQC,Functional

yy$Street<-NULL
yy$Utilities<-NULL
yy$Condition2<-NULL
yy$RoofMatl<-NULL
yy$MiscFeature<-NULL
yy$PoolQC<-NULL
yy$Functional<-NULL
yy[1380, 26] <- "SBrkr"


#which(is.na(yy$Electrical))
#colnames(yy)
#index Electrical is 26, we exchange the unique NA value with the common value 

# work --------------------------------------------------------------------
#plot(yy$Condition2)

yy$Id <- house[,1]
#length(yy$Id)
total <- merge(yy, aNormZ, by = "Id")
total[509,38]

#total$PoolArea<- NULL
#total$KitchenAbvGr<- NULL
#total$BsmtHalfBath<- NULL

#this next doesn't effect anything
total$SaleType<- as.factor(total$SaleType)

TrainSet <- total[1:1460,]
TestSet <- total[1461:2919,]
TestSet$SalePrice <- NULL 

#summary(lm(SalePrice~., data = total))
#summary(lm(SalePrice~Condition1+YrSold, data = total))
#str(noume)

# correlation normalized dataframe ----------------------------------------


cor_numVar <- cor(aNormZ, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

# models ------------------------------------------------------------------

lr <- lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+FullBath+YearBuilt+TotalBsmtSF+YearRemodAdd+OverallCond+
           MSZoning+LotConfig+Neighborhood+ExterQual+BsmtQual+KitchenQual, data = TrainSet)
summary(lr)

lr2 <- lm(SalePrice ~ OverallQual+GrLivArea+GarageCars+YearBuilt+TotalBsmtSF+OverallCond+
            MSZoning+LotConfig+Neighborhood+ExterQual+BsmtQual+KitchenQual, data = TrainSet)
summary(lr2)

#heating most GasA
summary(yy)
plot(yy$Electrical)
unique(yy$MiscFeature)
plot(yy$GarageFinish)

# prediction on Test --------------------------------------------------------------

#houseTest= read.csv("test.csv")
predictTest= predict(lr2, TestSet)

#output <- cbind(houseTest, predictTest)
#NA_test_indices <- which(is.na(output$predictTest))
#houseTest <- houseTest[(!NA_test_indices),]

check=cbind(predictTest, as.integer(TestSet$Id))
#check[700,]
colnames(check)=c('SalePrice', 'Id')
final=check[,c(2,1)]
write.csv(check, file = "submit_bressan.csv",row.names=FALSE)



#-----------------------
SSE=sum((houseTest$SalePrice ~ predictTest)^2)
length(predictTest)