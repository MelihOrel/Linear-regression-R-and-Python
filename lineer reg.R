#package
library(dplyr)
library(tidyr)


# Read CSV
df <- read.csv('focus.csv',sep = ',') ; head(df)

# structure of our data frame
str(df)

# Factoring categorical data
df$year <- factor(df$year)
df$transmission <-factor(df$transmission)
df$fuelType <- factor(df$fuelType)
unique(df$engineSize) #(1.0 1.5 1.6 2.3 2.0 1.8 2.5 0.0 1.4) '0.0' is incoherent
#drop
df<-df[-which(df$engineSize == 0.0),]
df$engineSize <- factor(df$engineSize)

#dealing with NA
# find NA
anyNA(df) # FALSE

# Mile to Km
library(swfscMisc)
km <-round(convert.distance(df$mileage,from = 'mi',to = 'km'))
## add KM variable 
df <- df %>% mutate(kilometer = km)
# drop mileage variable
df <- df %>% select(-mileage)

# drop model 
df <- df %>%  select(-model)

#visualization
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(corrgram)
# price density curve
pl1<-ggplot(df, aes(x=price,fill=fuelType))+ geom_density(alpha=.5)
pl2<-ggplot(df, aes(x=price,fill=transmission))+ geom_density(alpha=.5)
grid.arrange(pl1,pl2)

# X,Y plot
ggplot(df, aes(x=kilometer, y=price,color=fuelType)) + geom_point() 

#corrPlot
#only numeric columns
num_cols <- sapply(df, is.numeric)
#corr
corr <- cor(df[,num_cols]) ;corr
corrplot(corr,method='number')


#Train and Test Data
library(caret)
index <- createDataPartition(df$price,p=0.70,list = F)

# Training Data
train <- df[index,]

## Testing Data
test <- df[-index,]


#Model Building
model <- lm(price ~.,data = train) ; summary(model)
# Grab residuals
res <- residuals(model)

# Convert to DataFrame for gglpot
res <- as.data.frame(res)

head(res)
# Histogram of residuals
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)

#Predictions
price_predictions <- predict(model,test)
results <- cbind(round(price_predictions),test$price) 
colnames(results) <- c('pred_price','real_price')
results <- as.data.frame(results)

xrng = range(results$real_price, results$pred_price)
kde1 = density(results$real_price, from = xrng[1L], to = xrng[2L])
kde2 = density(results$pred_price, from = xrng[1L], to = xrng[2L])
matplot(kde1$x, cbind(kde1$y, kde2$y),main='Real(1) vs Predictions(2)')

#Saving results 
openxlsx::write.xlsx(results,'results.xlsx')
