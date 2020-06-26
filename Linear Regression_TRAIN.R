#Linear Regression on Housing Price Prediction
library(ggplot2)
library(ggthemes) 
library(scales)
library(dplyr) 
library(gridExtra)
library(corrplot) 
library(GGally)
library(e1071)
library(DAAG)

#read data file
data= read.csv("Property_Price_Train.csv", na.strings=c("","NA"))
#View(data.train)

#Str of train data.
dim(data)
str(data)
summary(data)


#removing id as unique value
data <- data[-1]

# #detecting and treating missing values
colSums(is.na(data)) 

#% Missing analysis
Missing_index <- data %>% is.na() %>% colMeans() * 100
Missing_index <- sort(round(Missing_index[Missing_index > 0], 2), decreasing  = TRUE)
View(Missing_index)
#barplot(Missing_index)

# % of NA's in dataframe
sum(is.na(data))/prod(dim(data)) *100


# % of NA's contains as row
nrow(data[!complete.cases(data),])/nrow(data) *100


#droping columns having more 75%
drop <- names(Missing_index[c(1:5)])
data <- data[(!colnames(data) %in% drop)]
dim(data)


#segregating Numeric & factor data
data.numeric <- data[sapply(data, is.numeric)]
data.factor <- data[sapply(data, is.factor)]

dim(data.numeric)
dim(data.factor)


#Numerical Data analysis for cleaning
str(data.numeric)
summary(data.numeric[11:16])


#Numeric to factor needed
# Overall_Material
# House_Condition
# Construction_Year
# Remodel_Year
# Kitchen_Above_Grade
# Rooms_Above_Grade
# Fireplaces
# Garage_Size
# Garage_Built_Year
# Month_Sold
# Year_Sold
# "Underground_Full_Bathroom",
# "Underground_Half_Bathroom",
# "Full_Bathroom_Above_Grade",
# "Half_Bathroom_Above_Grade",
# "Bedroom_Above_Grade",   


data$Overall_Material <- as.factor(data$Overall_Material)
data$House_Condition <- as.factor(data$House_Condition)
data$Construction_Year <- as.factor(data$Construction_Year)
data$Remodel_Year <- as.factor(data$Remodel_Year)
data$Kitchen_Above_Grade <- as.factor(data$Kitchen_Above_Grade)
data$Rooms_Above_Grade <- as.factor(data$Rooms_Above_Grade)
data$Fireplaces <- as.factor(data$Fireplaces)
data$Garage_Size <- as.factor(data$Garage_Size)
data$Garage_Built_Year <- as.factor(data$Garage_Built_Year)
data$Month_Sold <- as.factor(data$Month_Sold)
data$Year_Sold <- as.factor(data$Year_Sold)
data$Building_Class <- as.factor(data$Building_Class)
data$Underground_Full_Bathroom<- as.factor(data$Underground_Full_Bathroom)
data$Underground_Half_Bathroom   <- as.factor(data$Underground_Half_Bathroom)
data$Full_Bathroom_Above_Grade   <- as.factor(data$Full_Bathroom_Above_Grade)
data$Half_Bathroom_Above_Grade   <- as.factor(data$Half_Bathroom_Above_Grade)
data$Bedroom_Above_Grade   <- as.factor(data$Bedroom_Above_Grade)


#factor data analysis for cleaning

str(data.factor)
summary(data.factor)

#Highly biased or same value data.
# Utility_Type
# Road_Type


data <- data[(!colnames(data) %in% c("Utility_Type","Road_Type"))]
dim(data)



#Again segregating Numeric & factor data
data.numeric <- data[sapply(data, is.numeric)]
data.factor <- data[sapply(data, is.factor)]

dim(data.numeric)
dim(data.factor)

#checking Na's in target value
any(is.na(data.numeric$Sale_Price))


#Imputation with mean
for(i in seq(data.numeric)) {
     data.numeric[i]<- ifelse(is.na(data.numeric[,i]), 
                              median(data.numeric[,i], na.rm = T), data.numeric[,i])
}



#Now factor one

#mode function
getmode <- function(x) {
     x <- x[!is.na(x)]
     uniqv <- unique(x)
     uniqv[which.max(tabulate(match(x, uniqv)))]
}


#imputation with mode
for(i in seq(data.factor))
     data.factor[,i][is.na(data.factor[,i])] <- getmode(data.factor[,i])

str(data.factor)
summary(data.factor)



#Analysing histogram of each numeric values
numplot <- function(column, df)
{
     ggplot(df, aes_string(x=column))+
          geom_histogram(aes(y=..density..),fill = "grey", color = "black")+
          geom_density(fill='blue', alpha=0.2)+
          xlab(column)
}


np <- lapply(colnames(data.numeric), numplot, df=data.numeric)
do.call("grid.arrange", np)


#Check with skewness
data.skewed <- apply(data.numeric, c(2), skewness)


colnames(data.numeric)
drops <- data.numeric[c("Lot_Size",
                        "Brick_Veneer_Area",
                        "BsmtFinSF2", 
                        "Second_Floor_Area",
                        "LowQualFinSF",
                        "Three_Season_Lobby_Area",  
                        "Screen_Lobby_Area",        
                        "Pool_Area",                
                        "Miscellaneous_Value")] 


np <- lapply(colnames(drops), numplot, df=drops)
do.call("grid.arrange", np)

summary(drops)
View(drops)

drops <- drops[-1]
data.numeric <- data.numeric[(!colnames(data.numeric) %in% colnames(drops))]
dim(data.numeric)



#Outlier 
out_std_check = function(x){
     m=mean(x)
     s=sd(x)
     lc=m-3*s #lower cut-off
     uc=m+3*s
     n=list( val=sum(x>uc | x<lc), lc=lc, uc=uc)
     return(n)
}

np <- apply(data.numeric, c(2), out_std_check)
np

out_std_fix = function(x){
     m=mean(x)
     s=sd(x)
     lc=m-3*s #lower cut-off
     uc=m+3*s
     out_value <- which(x > uc | x < lc)
     x[out_value] <- m
     return(x)
}


data.numeric <- apply(data.numeric, c(2), out_std_fix)
data.numeric <- as.data.frame(data.numeric)
View(data.numeric)


np <- lapply(colnames(data.numeric), numplot, df=data.numeric)
do.call("grid.arrange", np)

apply(data.numeric, c(2), skewness) 



corrplot::corrplot(cor(data.numeric))
corrplot.mixed(cor(data.numeric), lower.col = "black", number.cex = .7)

colnames(data.numeric)
data.numeric <- data.numeric[!colnames(data.numeric) %in% c("Garage_Area","W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area")]



#Now factor analysis

#bar plot for categorical varibale etc.

factplot <- function(column, df)
{
     ggplot(df, aes_string(x=column))+
          geom_bar(fill = "blue", color = "black", alpha= 0.2)+
          xlab(column)
}

#calling all bar plot
fp <- lapply(colnames(data.factor), factplot, df=data.factor)
do.call("grid.arrange", fp)


drps <- c("Land_Outline", "Property_Slope", "Condition1","Condition2" 
          ,"House_Type", "Roof_Quality","Heating_Type" ,
          "BsmtFinType2","Functional_Rate", "Kitchen_Above_Grade",
          "Garage_Quality","Garage_Condition")


data.factor <- data.factor[!colnames(data.factor) %in% drps]

factors <- c("Construction_Year", "Remodel_Year","Neighborhood","Garage_Built_Year",
             "Month_Sold")

data.dummies <- data.factor[!colnames(data.factor) %in% factors]

#Significance ananlysis

annova <- function(x) {
        y <- data.numeric$Sale_Price
        q <- list(summary(aov(y~x)))
        return(q)
}

q <- (summary(aov(y~data.factor$Sale_Condition)))
signify <- apply(data.factor, c(2),annova)

signify


#dealing with year factor.
#these will need transformation like life of house by substraction built & remodled year
#for now just converting them into numeric
data.factor$Construction_Year <- as.numeric(data.factor$Construction_Year)
data.factor$Remodel_Year <- as.numeric(data.factor$Remodel_Year)
data.factor$Garage_Built_Year <- as.numeric(data.factor$Garage_Built_Year)
data.factor$Garage_Finish_Year <- as.numeric(data.factor$Garage_Finish_Year)
data.factor$Month_Sold <- as.numeric(data.factor$Month_Sold)
data.factor$Year_Sold <- as.numeric(data.factor$Year_Sold)


data.factor <- dummy.data.frame(data.factor)
# dim(data.dummies)
# dim(aq)

# data.factor <- data.factor[colnames(data.factor) %in% factors]
# data.factor <- cbind(data.factor, aq)
# 



data<- cbind(data.numeric, data.factor)
str(data)
dim(data)

colnames(data)

#sampling
set.seed(10)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
test=data[-s,]
dim(train)
dim(test)
colnames(test1)
test1 <- test[!colnames(test) %in% "Sale_Price"]


#Applying lm model
model <- lm(Sale_Price~., data=train)
summary(model)



vif(model)
plot(model)

pred <- predict(model, test1)
View(pred)



results <- cbind(pred,test$Sale_Price) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
View(results)



# Grab residuals
res <- residuals(model)
res <- as.data.frame(res)
head(res)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
# plot(model)





#Stepwise modeling

#stepwise sleection:

nullModel<- lm(Sale_Price ~ 1, train)

#summary(nullModel)

fullmodel <- lm(Sale_Price~.,data = (train))
#summary(fullmodel)

fit <- step(nullModel, scope=list(lower=nullModel, upper=fullmodel), direction="both")

#revel the model
fit



model <- lm(formula = Sale_Price ~ Grade_Living_Area + Construction_Year + 
           Total_Basement_Area + Overall_Material8 + Overall_Material7 + 
           BsmtFinSF1 + Fireplaces0 + House_Condition7 + Overall_Material9 + 
           Kitchen_QualityGd + Overall_Material6 + Zoning_ClassRMD + 
           Sale_ConditionNormal + NeighborhoodTimber + NeighborhoodSomerst + 
           Building_Class160 + House_Condition2 + Exterior_ConditionEx + 
           Garage_Size0 + House_Condition8 + Rooms_Above_Grade12 + NeighborhoodCrawfor + 
           NeighborhoodBrkSide + House_Condition6 + House_Condition9 + 
           House_Condition5 + NeighborhoodNridgHt + Sale_TypeConLI + 
           Rooms_Above_Grade11 + Full_Bathroom_Above_Grade3 + Property_ShapeIR3 + 
           Underground_Full_Bathroom0 + Building_Class75 + Garage_Size4 + 
           NeighborhoodBrDale + Exterior_MaterialGd + Exposure_LevelAv + 
           NeighborhoodClearCr + Garage_Size1 + NeighborhoodNoRidge + 
           Rooms_Above_Grade9 + Foundation_TypeW + Exposure_LevelNo + 
           Zoning_ClassCommer + NeighborhoodIDOTRR + Bedroom_Above_Grade0 + 
           Exterior1stStone + Lot_ConfigurationC + Rooms_Above_Grade5 + 
           Bedroom_Above_Grade1 + Exterior1stHdBoard + Exterior2ndVinylSd + 
           Bedroom_Above_Grade8 + Building_Class90 + Underground_Full_Bathroom1 + 
           Overall_Material10 + Garage_Size2 + `Exterior2ndWd Shng` + 
           BsmtFinType1GLQ + Basement_HeightEx + NeighborhoodBlmngtn + 
           GarageAttchd + House_Design2.5Unf + Exterior1stBrkComm, data = train)



summary(model)
plot(model)

# 
# x <- factor(c("A","B","A","C","D","E","A","E","C"))
# x
# library(car)
# x <- recode(x, "c('A', 'B')='A+B';c('D', 'E') = 'D+E'")
# x
