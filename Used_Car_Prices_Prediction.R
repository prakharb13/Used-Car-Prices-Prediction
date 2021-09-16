rm(list=ls())
library(readxl)
library(maps)
library(mapdata)
library(ggplot2)
library(gbm)
library(dplyr)
library(glmnet)


setwd("C:/Users/prakh/Desktop/Intro to ML/Assignment 1/US_Data")

df = read_excel('vehicles.xlsx')

## Understanding the structure of the dataframe
str(df)

## Understanding the dimension of the dataset
dim(df)

## Total Number of Rows with NA
sapply(df, function (x) sum(is.na(x)))
# There are many columns with NA in it

## Removing all rows with NA in the dataset
df_cleaned = na.omit(df)

## % of original dataset left after omitting NAs
nrow(df_cleaned)/nrow(df)
# Only 18.51% of the rows are left which have no NAs

## Converting all categorical features into factors
df_cleaned[sapply(df_cleaned, is.character)] <- 
  lapply(df_cleaned[sapply(df_cleaned, is.character)],as.factor)

#############
# outlier removal 

# price > 0
df_cleaned = df_cleaned[df_cleaned$price >= 0,]

# removing car in salvage category
df_cleaned = df_cleaned[
  df_cleaned$condition != 'salvage',
]

# remove car which are possible vintage
vintage_year = 1980
df_cleaned = df_cleaned[
  df_cleaned$year >= vintage_year,
]
df_cleaned = df_cleaned[
  df_cleaned$year <= 2021,
]

# getting rid of harley-davidson
df_cleaned = df_cleaned[
  df_cleaned$manufacturer != 'harley-davidson',
]

# getting rid of bus
df_cleaned = df_cleaned[
  df_cleaned$type != 'bus',
]

# getting rid of x mile cover
odometer_limit = 350000
df_cleaned = df_cleaned[
  df_cleaned$odometer <= odometer_limit,
]

# consider car which have clean title
df_cleaned = df_cleaned[
  df_cleaned$title_status == 'clean',
]

#  putting a reasonable limit on price
min_price_limit = 1000
df_cleaned = df_cleaned[
  df_cleaned$price >= min_price_limit,
]
max_price_limit = 200000
df_cleaned = df_cleaned[
  df_cleaned$price <= max_price_limit,
]

## EDA
# Checking levels each column of the dataset has
sapply(df_cleaned, function(x) length(levels(x)))

# Top 10 regions with car listings
top_regions = head(sort(table(df_cleaned$region),decreasing = TRUE), 10)
barplot(top_regions,main='Top 10 Regions by Car Listings',
        xlab='region',ylab='Number of car listings',las=2,col='darkred')


# Top 10 manufacturers with car listings
top_manufacturer = head(sort(table(df_cleaned$manufacturer),decreasing = TRUE), 10)
barplot(top_manufacturer,main='Top 10 manufacturers by Car Listings',
        xlab='manufacturer',ylab='Number of car listings',las=2,col='orange')

# Distribution of Condition of Cars
barplot(table(df_cleaned$condition),main='Distribution of Car Conditions',
        xlab='Car condition', ylab='total count of listings',las=2,col='brown')

# Distribution of Price feature
hist(df_cleaned$price,xlab = 'price of a car', ylab = 'count of cars',main='Distribution of price',col='darkred')

# Distribution of Odometer feature
hist(df_cleaned$odometer,xlab = 'odometer of a car', ylab = 'count of cars',main='Distribution of odometer')

# Distribution of Cylinder feature
barplot(table(df_cleaned$cylinders),xlab = 'Cylinders in a car', ylab = 'Count',main='Distribution of Cylinders in ca')

# Distribution of year feature
barplot(table(df_cleaned$year),xlab = 'Year of manufacture of a car', ylab = 'Count',main='Distribution of year of manufacture of car')

# Boxplot of Price vs Year
boxplot(price~year,df_cleaned, main="Price vs Registration year",
        xlab="Year of registration", ylab="Price of car")

# Scatterplot of Price vs Odometer
plot(price~odometer,df_cleaned, main="Price vs Odometer of a car",
     xlab="Price of car", ylab="Odometer reading")


# Geoplot of lat-long
lat_long_df = data.frame(df_cleaned$lat,df_cleaned$long)
names(lat_long_df) = c('lat','long')

lat_long_df <- data.table::as.data.table(lat_long_df)
lat_long_df = lat_long_df[(lat_long_df$lat>20) & (lat_long_df$lat<65)]
lat_long_df = lat_long_df[(lat_long_df$long<-70) & (lat_long_df$long>-150)]

usa <- map_data("state")

ggplot() + geom_polygon(data = usa, 
                        aes(x=long, y = lat, 
                            group = group),fill = NA,
                        col = 'orange')+theme_bw()+xlim(c(-150,-69)) + geom_point(data = lat_long_df, aes(x = long, y = lat, color = "red", alpha = 0.65), size = 0.5, shape = 1) 


## Data Cleaning
# clean dates
df_cleaned$posting_date = as.Date(df_cleaned$posting_date)
df_cleaned$posting_weekday = weekdays(df_cleaned$posting_date)
df_cleaned$logprice = log(df_cleaned$price)


size_df <- df_cleaned$size
type_df <- df_cleaned$type
odo_df <- df_cleaned$odometer
col_df <- df_cleaned$paint_color

#The distribution of size 
#Full sized car dominates the market followed by mid-size and compact
plot(size_df)

simple_size = df_cleaned[df_cleaned$size != 'sub-compact',]

ggplot(simple_size, aes(year, logprice, shape=size, colour=size, fill=size)) +
  geom_smooth(method='lm') +
  labs(x='Age of Car', y='Price (logprice)',
       title = 'Old vs. Price',
       subtitle='Different impact the size of a car having',
       caption='Linear Regression Line')

#The distribution of type 
#Most number of cars sold: Sedan followed by SUV and Truck
plot(type_df)

simple_type = df_cleaned %>% filter(type == 'SUV' | type == 'sedan' | type == 'truck')

ggplot(simple_type, aes(year, logprice, shape=type, colour=type, fill=type)) +
  geom_smooth(method='lm') +
  labs(x='Age of Car', y='Price (logprice)',
       title = 'Old vs. Price',
       subtitle='Different impact the type of a car having',
       caption='Linear Regression Line')

#Does cylinders matter for the appreciation of a car? 
#(Based on the three cylinders 4, 6, 8)
simple_cyl = df_cleaned %>% filter(cylinders == '4 cylinders' | cylinders == '6 cylinders' | cylinders == '8 cylinders')

ggplot(simple_cyl, aes(year, logprice, shape=cylinders, colour=cylinders, fill=cylinders)) +
  geom_smooth(method='lm') +
  labs(x='Age of Car', y='Price (logprice)',
       title = 'Old vs. Price',
       subtitle='Different impact the cylinders of a car having',
       caption='Linear Regression Line')


#Which-colored car is sold most frequently?
plot(col_df)

#Rough relationship between year and price
ggplot(data=df_cleaned, aes(x=year, y=price )) +
  geom_point(shape=10, colour='orange') +
  geom_smooth() +
  labs(title='Old vs. Price', subtitle='Minimum Price limit Exists?',
       y='Price of Used car (US Dollar)',
       x='Age of car',
       caption='blue line = trend')

#The price of used car might be affect by the performance of the car
ggplot(data=df_cleaned, aes(x=odometer, y=price )) +
  geom_point(shape=10, colour='orange') +
  geom_smooth() +
  labs(title='Mileage vs. Price',
       y='Price of Car (US Dollar)',
       x='Odometer (Mile)',
       caption='blue line = trend')

#Does manufacturer affect the price of used car?
ggplot(data=df_cleaned, aes(x=manufacturer, y=price )) +
  geom_point(shape=10, colour='orange') +
  labs(title='Price vs. Brand',
       y='Price of Used car (US Dollar)',
       x='Manufacturer')

#The impact of a car's brand on its apprecation of monetary car-value
simple_brand = df_cleaned %>% filter(manufacturer == 'ford'|manufacturer == 'chevrolet'|manufacturer == 'toyota')

ggplot(simple_brand, aes(year, logprice, shape=manufacturer, colour=manufacturer, fill=manufacturer)) +
  geom_smooth(method='lm') +
  labs(x='Age of Car', y='Price (logprice)',
       title = 'Old vs. Price',
       subtitle='Impact of Brand on Appreciation')



dim(df_cleaned)
##################################################
##################### Modeling  ##################
##################################################

# Variable to get rid of - region, model, title_status, posting_date
drop_vars = c('region', 'model', 'title_status', 'posting_date', 'posting_weekday','lat','long')
car_resale_model = df_cleaned[
  , !colnames(df_cleaned) %in% drop_vars
]

df_cleaned = droplevels(df_cleaned)
car_resale_model = droplevels(car_resale_model)
summary(car_resale_model)
dim(car_resale_model)
# getting cleaned data for the modeling
car_resale_model = subset(car_resale_model, select = -c(price))

# splitting into train and test set
set.seed(1)
n_test = 2000
ind = sample(1:dim(car_resale_model)[1], n_test)

train = car_resale_model[-ind,]
test = car_resale_model[ind,]

train_cv_df = data.frame(matrix(nrow=0, ncol=4))
colnames(train_cv_df) = c('model_type', 'hyperparameter', 'avg_rmse_os', 'avg_rmse_is')

##################################################
############## Linear Regression #################
##################################################
model_type = 'Linear Regression'
hyperparameter = 'all_models'


# starting K-fold validation for Linear regression
k_folds = 5

n_train = dim(train)[1]
n0 = round(n_train/k_folds, 0)

out_MSE = matrix(0, k_folds)
in_MSE = matrix(0, k_folds)
model_lr_list = list()

used = NULL
set = 1:n_train

set.seed(1)
# for loop for each fold
for(j in 1:k_folds){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = train[val,]
  
  model_i = glm(logprice ~ ., family = gaussian, data=train_i)
  prediction_i = predict(model_i, newdata = test_i)
  
  rmse_is_i = sqrt(mean((model_i$residuals)^2))
  in_MSE[j] = rmse_is_i
  
  rmse_os_i = sqrt(mean((test_i$logprice - prediction_i)^2))
  out_MSE[j] = rmse_os_i
  
  model_lr_list[[j]] = model_i
  
  used = union(used,val)
  set = (1:n_train)[-used]  
}

avg_rmse_os = mean(out_MSE)
avg_rmse_is = mean(in_MSE)

train_cv_df = rbind(train_cv_df, cbind(model_type, hyperparameter, avg_rmse_os, avg_rmse_is))


##################################################
############## Ridge Regression #################
##################################################
model_type = 'Ridge Regression'
alpha = 0

lambda_list = c(0.001, 0.01, 0.1)


# starting K-fold validation for Linear regression
k_folds = 5

n_train = dim(train)[1]
n0 = round(n_train/k_folds, 0)

out_MSE = matrix(0, k_folds)
in_MSE = matrix(0, k_folds)
model_rr_list = list()

set.seed(1)
# for loop for each fold
for(lambda in lambda_list){
  
  used = NULL
  set = 1:n_train
  
  for(j in 1:k_folds){
    
    if(n0<length(set)){val = sample(set,n0)}
    if(n0>=length(set)){val=set}
    
    Xtrain_i = model.matrix(~.,subset(train[-val,], select=-logprice))
    Ytrain_i = train[-val,]$logprice
    
    Xtest_i = model.matrix(~.,subset(train[val,], select=-logprice))
    Ytest_i = train[val,]$logprice
    
    model_i = glmnet(Xtrain_i, Ytrain_i, alpha = alpha, lambda = lambda)
    prediction_i = predict(model_i, newx = Xtest_i)
    
    rmse_is_i = sqrt(
      (1 - model_i$dev.ratio)*model_i$nulldev/dim(Xtrain_i)[1])
    in_MSE[j] = rmse_is_i
    
    rmse_os_i = sqrt(mean((Ytest_i - prediction_i)^2))
    out_MSE[j] = rmse_os_i
    
    model_rr_list[[j]] = model_i
    
    used = union(used,val)
    set = (1:n_train)[-used]  
  }
  
  avg_rmse_os = mean(out_MSE)
  avg_rmse_is = mean(in_MSE)
  hyperparameter = lambda
  
  train_cv_df = rbind(train_cv_df, cbind(model_type, hyperparameter, avg_rmse_os, avg_rmse_is))
  
}


##################################################
############## LASSO Regression #################
##################################################
model_type = 'Lasso Regression'
alpha = 1

lambda_list = c(0.001, 0.01, 0.1)

# starting K-fold validation for Linear regression
k_folds = 5

n_train = dim(train)[1]
n0 = round(n_train/k_folds, 0)

out_MSE = matrix(0, k_folds)
in_MSE = matrix(0, k_folds)
model_lasso_list = list()

set.seed(1)
# for loop for each fold
for(lambda in lambda_list){
  
  used = NULL
  set = 1:n_train
  
  for(j in 1:k_folds){
    
    if(n0<length(set)){val = sample(set,n0)}
    if(n0>=length(set)){val=set}
    
    Xtrain_i = model.matrix(~.,subset(train[-val,], select=-logprice))
    Ytrain_i = train[-val,]$logprice
    
    Xtest_i = model.matrix(~.,subset(train[val,], select=-logprice))
    Ytest_i = train[val,]$logprice
    
    model_i = glmnet(Xtrain_i, Ytrain_i, alpha = alpha, lambda = lambda)
    prediction_i = predict(model_i, newx = Xtest_i)
    
    rmse_is_i = sqrt(
      (1 - model_i$dev.ratio)*model_i$nulldev/dim(Xtrain_i)[1])
    in_MSE[j] = rmse_is_i
    
    rmse_os_i = sqrt(mean((Ytest_i - prediction_i)^2))
    out_MSE[j] = rmse_os_i
    
    model_lasso_list[[j]] = model_i
    
    used = union(used,val)
    set = (1:n_train)[-used]  
  }
  
  avg_rmse_os = mean(out_MSE)
  avg_rmse_is = mean(in_MSE)
  hyperparameter = lambda
  
  train_cv_df = rbind(train_cv_df, cbind(model_type, hyperparameter, avg_rmse_os, avg_rmse_is))
  
}

##################################################
#################### Boosting ####################
##################################################
model_type = 'Boosting'

# Boosting Hyperparameters
# (i) depth (ii) number of trees (iii) lamda = shrinkage.
depth_list = c(10,20)
ntree_list = c(50,100)
lambda_list = c(.001,.1)
param_grid = expand.grid(depth_list, ntree_list, lambda_list)

param_len = nrow(param_grid)

# starting K-fold validation for Linear regression
k_folds = 5

n_train = dim(train)[1]
n0 = round(n_train/k_folds, 0)

out_MSE = matrix(0, k_folds)
in_MSE = matrix(0, k_folds)
model_boosting_list = list()

set.seed(1)
for(i in 1:param_len){
  
  used = NULL
  set = 1:n_train
  
  depth = param_grid[i, 1]
  ntree = param_grid[i, 2]
  lambda = param_grid[i, 3]
  print(c(depth, ntree, lambda))
  
  for(j in 1:k_folds){
    
    if(n0<length(set)){val = sample(set,n0)}
    if(n0>=length(set)){val=set}
    
    train_i = train[-val,]
    test_i = train[val,]
    
    model_i = gbm(logprice~., data=train_i, distribution='gaussian',
                  interaction.depth=depth,
                  n.trees=ntree,
                  shrinkage=lambda)
    prediction_i = predict(model_i, newdata=test_i, n.trees=ntree)
    
    rmse_is_i = sqrt(mean(model_i$train.error))
    in_MSE[j] = rmse_is_i
    
    rmse_os_i = sqrt(mean((Ytest_i - prediction_i)^2))
    out_MSE[j] = rmse_os_i
    
    model_boosting_list[[j]] = model_i
    
    used = union(used,val)
    set = (1:n_train)[-used]  
  }
  
  avg_rmse_os = mean(out_MSE)
  avg_rmse_is = mean(in_MSE)
  hyperparameter = paste(depth, ntree, lambda)  
  
  train_cv_df = rbind(train_cv_df, cbind(model_type, hyperparameter, avg_rmse_os, avg_rmse_is))
  
}
avg_rmse_os

train_cv_df

variable_imp = summary.gbm(model_boosting_list[[1]])
variable_imp$rel.inf = as.numeric(variable_imp$rel.inf)
variable_imp$rel.inf <- sort(variable_imp$rel.inf,decreasing = TRUE)
variable_imp
p = ggplot(variable_imp,aes(reorder(var,rel.inf),rel.inf))+geom_bar(stat='identity',fill='steelblue')+ylab('value')+xlab('feature')
p+coord_flip()

##################################################
############## Out of sample testing #############
##################################################
# pvalue are not reported as they are not relevant

# final model selection - lasso regression with lambda= 0.001
set.seed(1)
alpha = 1
lambda = 0.001

Xtrain = model.matrix(~.,subset(train, select=-logprice))
Ytrain = train$logprice

Xtest = model.matrix(~.,subset(test, select=-logprice))
Ytest = test$logprice

model = glmnet(Xtrain, Ytrain, alpha = alpha, lambda = lambda)
prediction = predict(model, newx = Xtest)

rmse_is = sqrt(
  (1 - model$dev.ratio)*model$nulldev/dim(Xtrain)[1])

rmse_os_antilog = sqrt(mean((exp(test$logprice) - exp(prediction))^2))
rmse_os_antilog
rmse_os = sqrt(mean((Ytest - prediction)^2))

print(c(rmse_is, rmse_os))

c(model$beta, model$a0)
summary(model)

plot(prediction, Ytest, main='Actuals vs Prediction', xlab='Predictions', ylab='Actuals')
abline(0, 1, col='red')

residuals = Ytest - prediction
plot(residuals, main='residual plot')
abline(h = 0, col='red')
