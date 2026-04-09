## Setup =========================================================================================================================
library(data.table)
library(dplyr)
library(corrplot)
library(rpart)
library(rpart.plot)	
library(caTools)
library(ggplot2)
library(car)
library(nnet)
library(scales)
library(MASS)
library(e1071)
library(randomForest)
library(xgboost)
library(caret)
library(caretEnsemble)
library(arules)
library(arulesViz)
library(quantreg)
library(earth)
library(readxl)
library(reshape2)
library(tidyr)
library(gridExtra)
library(forecast)
library(pROC)
library(lubridate)
library(pscl)
library(plotmo)
library(knitr)
library(gridExtra)
library(grid)
library(ranger)

setwd('/Users/shelden/Desktop/BAC Datathlon')

d = fread("Theme 2 Cleaned.csv",sep=",",header=TRUE, stringsAsFactors = T)

d[,`Referred a Friend`:= as.factor(`Referred a Friend`)]
d[,`Offer`:= as.factor(`Offer`)]
d[,`Phone Service`:= as.factor(`Phone Service`)]
d[,`Multiple Lines`:= as.factor(`Multiple Lines`)]
d[,`Internet Service`:= as.factor(`Internet Service`)]
d[,`Internet Type`:= as.factor(`Internet Type`)]
d[,`Online Security`:= as.factor(`Online Security`)]
d[,`Online Backup`:= as.factor(`Online Backup`)]
d[,`Device Protection Plan`:= as.factor(`Device Protection Plan`)]
d[,`Premium Tech Support`:= as.factor(`Premium Tech Support`)]
d[,`Streaming TV`:= as.factor(`Streaming TV`)]
d[,`Streaming Movies`:= as.factor(`Streaming Movies`)]
d[,`Streaming Music`:= as.factor(`Streaming Music`)]
d[,`Unlimited Data`:= as.factor(`Unlimited Data`)]
d[,`Contract`:= as.factor(`Contract`)]
d[,`Paperless Billing`:= as.factor(`Paperless Billing`)]
d[,`Payment Method`:= as.factor(`Payment Method`)]
d[,`Gender`:= as.factor(`Gender`)]
d[,`Under 30`:= as.factor(`Under 30`)]
d[,`Senior Citizen`:= as.factor(`Senior Citizen`)]
d[,`Married`:= as.factor(`Married`)]
d[,`Dependents`:= as.factor(`Dependents`)]
d[,`Churn Category`:= as.factor(`Churn Category`)]
d[d == ""] <- NA 

# Setting filtered datasets
churned <- d %>% filter(`Churn Value` == 1)
competitor_churn <- churned %>% filter(`Churn Category` == "Competitor")
unchurned <- d %>% filter(`Churn Value` == 0)
summary(churned)
summary(competitor_churn)
summary(unchurned)

## Exploratory Statistics =========================================================================================================================

dim(d)
colnames(d)

colSums(is.na(d)) # Checking for NA values
# Customer Satisfaction has 5209 NAs

View(d)
summary(d)

str(d) # Confirming the classes of the dataset
lapply(d,unique)


num_vars <- as.data.frame(d)[ , sapply(d, is.numeric)]
cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")
melted_cor <- melt(cor_matrix)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1)) +
  coord_fixed()

# Histogram of Tenure
ggplot(d, aes(`Tenure in Months`, fill = Contract)) +
  geom_histogram(bins = 10, color = "lightgrey", linewidth = 0.2) +
  labs(title="Histogram of tenure for all customers", x=NULL, y="Count") +
  theme_minimal(base_size = 10)+
  scale_x_continuous(breaks = seq(min(churned$`Tenure in Months`), 
                                  max(churned$`Tenure in Months`), 
                                  by = 5))

table(d$`Contract`)
prop.table(table(d$`Contract`)) * 100
#   Attitude      Competitor Dissatisfaction           Other           Price 
#   16.80043        44.99732        16.21188        10.70091        11.28946 


# Exploring the churned segment
summary(churned)

mean(churned$`Tenure in Months`, na.rm = TRUE)
# Mean = 17.979

table(d$`Churn Category`)
prop.table(table(d$`Churn Category`)) * 100
#   Attitude      Competitor Dissatisfaction           Other           Price 
#   16.80043        44.99732        16.21188        10.70091        11.28946 

unchurned %>%
  group_by(`Churn Category`) %>%
  summarise(
    count = n(),
    avg_requests = mean(`Total Customer Svc Requests`, na.rm=TRUE),
    avg_issues = mean(`Product/Service Issues Reported`, na.rm=TRUE),
    avg_satisfaction = mean(`Customer Satisfaction`, na.rm=TRUE)
  )
#   `Churn Category` count avg_requests avg_issues avg_satisfaction
# 1 ""                5174        0.877      0.136             3.76

churned %>%
  group_by(`Churn Category`) %>%
  summarise(
    count = n(),
    avg_tenure = mean(`Tenure in Months`, na.rm=TRUE),
    avg_monthly_charge = mean(`Monthly Charge`, na.rm=TRUE),
    avg_satisfaction = mean(`Customer Satisfaction`, na.rm=TRUE),
    median_contracts = names(which.max(table(Contract)))
  )
#   `Churn Category` avg_tenure avg_monthly_charge avg_satisfaction mode_contracts
#   Attitude               17.3               73.7             2.03 Month-to-Month
#   Competitor             17.7               79.8             1.51 Month-to-Month
#   Dissatisfaction        18.0               77.1             1.78 Month-to-Month
#   Other                  18.3               78.5             2.03 Month-to-Month
#   Price                  19.5               72.6             1.77 Month-to-Month

num_vars_churned <- as.data.frame(churned)[ , sapply(churned, is.numeric)]
cor_matrix_churned <- cor(num_vars_churned, use = "pairwise.complete.obs")
melted_cor_churned <- melt(cor_matrix_churned)
ggplot(data = melted_cor_churned, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1)) +
  coord_fixed()

# Histogram of tenure for churned customers
ggplot(churned, aes(`Tenure in Months`, fill = Contract)) +
  geom_histogram(bins = 15, color = "lightgrey", linewidth = 0.2) +
  labs(title="Histogram of tenure for churned customers", x=NULL, y="Count") +
  theme_minimal(base_size = 10)+
  scale_x_continuous(breaks = seq(min(churned$`Tenure in Months`), 
                                  max(churned$`Tenure in Months`), 
                                  by = 5))

# Exploring the segment that churned for competitors
mean(churned$`Tenure in Months`, na.rm = TRUE)
median(churned$`Tenure in Months`, na.rm = TRUE)
# The mean is 17.979, median is 10 months.
# This indicates a right skew.

summary(competitor_churn)

table(competitor_churn$`Churn Reason`)
prop.table(table(competitor_churn$`Churn Reason`)) * 100
# Competitor had better devices Competitor made better offer  Competitor offered higher download speeds 
# 37.21760                      36.97979                      11.89061 
# Competitor offered more data
# 13.91201

competitor_churn %>%
  group_by(`Churn Reason`) %>%
  summarise(
    count = n(),
    avg_tenure = mean(`Tenure in Months`, na.rm=TRUE),
    avg_monthly_charge = mean(`Monthly Charge`, na.rm=TRUE),
    avg_satisfaction = mean(`Customer Satisfaction`, na.rm=TRUE),
    median_contracts = names(which.max(table(Contract)))
  )
#   Churn Reason                                count     avg_tenure  avg_monthly_charge  avg_satisfaction  median_contracts
#   Competitor had better devices               313       16.2        80.0                1.02              Month-to-Month  
#   Competitor made better offer                311       16.5        78.3                1.8               Month-to-Month  
#   Competitor offered higher download speeds   100       20.4        78.8                1.71              Month-to-Month  
#   Competitor offered more data                117       23.2        84.0                1.94              Month-to-Month  

competitor_churn %>%
  group_by(`Internet Type`) %>%
  summarise(
    count = n(),
    avg_tenure = mean(`Tenure in Months`, na.rm=TRUE),
    avg_monthly_charge = mean(`Monthly Charge`, na.rm=TRUE),
    avg_satisfaction = mean(`Customer Satisfaction`, na.rm=TRUE),
    median_contracts = names(which.max(table(Contract)))
  )

#   `Internet Type` count avg_tenure avg_monthly_charge avg_satisfaction median_contracts
# 1 ""                 22       4.68               20.2             1.75 Month-to-Month  
# 2 "Cable"           370      19.8                83.6             1.57 Month-to-Month  
# 3 "DSL"             233      15.4                77.6             1.40 Month-to-Month  
# 4 "Fiber Optic"     216      18.1                81.7             1.49 Month-to-Month 

ggplot(competitor_churn, aes(x = `Offer`, fill = `Churn Reason`)) +
  geom_bar() +
  labs(y = "Proportion", title = "Churn Reasons for Competitor Category by Offer") +
  theme_minimal()

ggplot(competitor_churn, aes(x = `Offer`, fill = `Churn Reason`)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Churn Reasons for Competitor Category by Offer") +
  theme_minimal()

ggplot(competitor_churn, aes(x = `Churn Reason`, fill = `Contract`)) +
  geom_bar() +
  labs(y = "Proportion", title = "Churn Reasons for Competitor Category by Contract") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(d, aes(x = `Offer`, y = `Monthly Charge`, fill = `Offer`)) +
  geom_boxplot() +
  labs(title = "Monthly Charges by Offer", x = "Offer", y = "Charges ($)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Logistic Regression =========================================================================================================================

#Check the proportion of the dataset 
table(d$`Churn Value`)
prop.table(table(d$`Churn Value`))

set.seed(1)
# Stratified train-test split
train <- sample.split(d$`Churn Value`, SplitRatio = 0.7)

trainset <- subset(d, train == TRUE)
testset <- subset(d, train == FALSE)

dim(trainset)
dim(testset)

model <- glm(`Churn Value` ~ `Number of Referrals` + `Tenure in Months` + `Avg Monthly Long Distance Charges` +
               `Avg Monthly GB Download` + `Monthly Charge` + `Total Regular Charges` +
               `Total Refunds` + `Total Extra Data Charges` + `Total Long Distance Charges` +
               Age + `Number of Dependents` + Latitude + Longitude + Population + CLTV +
               `Total Customer Svc Requests` + `Product/Service Issues Reported` + `Customer Satisfaction`,
             data = trainset, family = binomial)


backward_model <- step(model, direction = "backward", trace = FALSE)
summary(backward_model)


# Final model
model.final <- glm(
  `Churn Value` ~ 
    `Product/Service Issues Reported` +
    `Total Customer Svc Requests` +
    `Number of Dependents` +
    `Total Extra Data Charges` +
    `Monthly Charge` +
    `Tenure in Months` +
    `Number of Referrals`,
  data = trainset,
  family = binomial
)
summary(model.final)

#check VIF
vif_values <- vif(model.final)

# Output VIF values
print(vif_values)


# Predict probabilities (train set)
pred.prob <- predict(model.final, type = "response")

# Predict Y (cutoff 0.5)
pred.class <- ifelse(pred.prob > 0.5, 1, 0)

# Confusion matrix (train set)
confusion <- table(actual = trainset$`Churn Value`, model = pred.class)
print(confusion)
mean(trainset$`Churn Value` == pred.class)

# Logistic Regression Model Test set
pred.prob <- predict(model.final, type = "response", newdata = testset)
pred.class <- ifelse(pred.prob > 0.5, 1, 0)

# Confusion matrix (test set)
table(actual = testset$`Churn Value`, model = pred.class)
mean(testset$`Churn Value` == pred.class)



## CART =========================================================================================================================
exclude_vars=c('City','Zip Code','Latitude','Longitude','Population')

d <- d %>%  
  mutate(    
    `Churn Value` = as.factor(`Churn Value`)  
  )


cart_model <- rpart(`Churn Value` ~ ., data = trainset[,!c('Customer ID','City','Zip Code','Latitude','Longitude','Population')] 
                    ,method = 'class', 
                    control = rpart.control(minsplit = 2, cp = 0))


printcp(cart_model)
## CV error consistently above 1 suggests problem due to imbalanced data.

plotcp(cart_model)
## 2nd tree is optimal

cp1 <- 0.0028

optimised_cart_model <- prune(cart_model, cp = cp1)

print(optimised_cart_model)
rpart.plot(optimised_cart_model,tweak=1.5)
optimised_cart_model$variable.importance
## Only AvgBal is important in optimal CART model m3.

cart.predict <- predict(optimised_cart_model, newdata = testset, type = "class")

table2 <- table(Testset.Actual = testset$`Churn Value`, cart.predict, deparse.level = 2)
table2
optimised_cart_error<-1 - sum(diag(table2)) / sum(table2)
optimised_cart_error

round(prop.table(table2), 3)

