# Loading necessary libraries
library(ggplot2)
library(corrplot)
library(Hmisc)

# Importing the dataset
df <- read.csv("../input/kc_house_data.csv")

# describe function
describe(df)

# checking for null values
sum(is.na(df))

# splitting the dataframe
set.seed(42)
index <- sample(1:nrow(df), floor(0.75*nrow(df)))
train <- df[index, ]
test <- df[-index, ]

# Creating a linear model with all the features
col <- colnames(df)
model1 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above
            +sqft_basement+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15,train)

# Checking summary
summary(model1)


# Checking Relationship between price and all other features
corr <- cor(train[,3:21])
corr
corrplot(corr,method = "color", outline = T, addgrid.col = "darkgray", order="hclust",
         addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "black", number.digits = 2,
         number.cex = 0.75, col = colorRampPalette(c("green4","white","red"))(100))


# Checking Relationship between price, bedrooms, bathrooms, sqft_living and sqft lot
corr1 <- cor(train[,3:7])
corrplot(corr1,method = "color", outline = T, addgrid.col = "darkgray", order="hclust",
         addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "black", number.digits = 2,
         number.cex = 0.75, col = colorRampPalette(c("green4","white","red"))(100))



# Checking Relationship between price, floors, waterfront, view, condition and grade
corr2 <- cor(train[,c(3,8,9,10,11,12)])
corrplot(corr2,method = "color", outline = T, addgrid.col = "darkgray", order="hclust",
         addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "black", number.digits = 2,
         number.cex = 0.75, col = colorRampPalette(c("green4","white","red"))(100))

# Checking Relationship between price, yr built, lat and long
corr3 <- cor(train[,c(3,15,18,19)])
corrplot(corr3,method = "color", outline = T, addgrid.col = "darkgray", order="hclust",
         addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4",
         tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "black", number.digits = 2,
         number.cex = 0.75, col = colorRampPalette(c("green4","white","red"))(100))

# Suitable variables are: sqft_living, bathrooms, grade, view and lat.
# Verifying them using bloxplot

# Nice correlation between Price vs. Sqft_living
boxplot1 <- boxplot(price~sqft_living, data=train, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")

# Nice correlation between Price vs. Bathrooms
boxplot2=boxplot(price~bathrooms, data=train, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Bathrooms", xlab="Bathrooms", ylab="Price")

# Nice correlation between Price vs. Grade
boxplot3=boxplot(price~grade, data=train, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Grade", xlab="Grade", ylab="Price")

# Nice correlation between Price vs. View
boxplot4=boxplot(price~view, data=train, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. View", xlab="View", ylab="Price")

# Price vs. Lat follows normal distribution
boxplot5=boxplot(price~lat, data=train, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Lat", xlab="Lat", ylab="Price")


# Creating a linear model with floors, waterfront, view, condition and grade features
model2 <- lm(price~sqft_living+bathrooms+grade+view+lat,train)

# Checking summary
summary(model2)

# Despite taking the better features, r-squared value dropped. 

# Plotting scatter plot between price and sqft_living to analyze the relationship
plot(train$sqft_living,train$price, main="Sqft_Living vs. Price of House",
     xlab="Sqft_Living", ylab="Price", pch=1)

# As scatterplot is very dense, I will plot aggregated vectors to see the relationship
price_sqftliving <-aggregate(price~sqft_living, FUN=mean, data=train)
plot(price_sqftliving)

# From the plot it can be seen that the relation is exponential and not linear


# Plotting scatter plot between price and bathrooms to analyze the relationship
plot(train$bathrooms,train$price, main="Sqft_Living vs. Price of House",
     xlab="Sqft_Living", ylab="Price", pch=1)


# Plotting scatter plot between price and grade to analyze the relationship
plot(train$grade,train$price, main="Sqft_Living vs. Price of House",
     xlab="Sqft_Living", ylab="Price", pch=1)


# Plotting scatter plot between price and view to analyze the relationship
plot(train$view,train$price, main="Sqft_Living vs. Price of House",
     xlab="Sqft_Living", ylab="Price", pch=1)

# From the above plot iwe can say that we can take the feature as it is.

# Plotting scatter plot between price and lat to analyze the relationship
plot(train$lat,train$price, main="Sqft_Living vs. Price of House",
     xlab="Sqft_Living", ylab="Price", pch=1)

# As scatterplot is very dense, I will plot aggregated vectors to see the relationship
price_lat <-aggregate(price~lat, FUN=mean, data=train)
plot(price_lat)

# The scatterplot is still very dense but at some point exponential relationship is observed

# Using the above features and relationship, I created a new model
model3<-lm(log(price)~log(sqft_living)+bathrooms+grade+view+log(lat),data=train)
summary(model3)
summary(model3)$adj.r.squared

# Using the same above relationship, creating a new model with all the features
model4 <- lm(log(price)~bedrooms+bathrooms+log(sqft_living)+sqft_lot+floors+waterfront+view+condition+grade+sqft_above
             +sqft_basement+yr_built+yr_renovated+zipcode+log(lat)+long+sqft_living15+sqft_lot15,train)
summary(model4)
summary(model4)$adj.r.squared
