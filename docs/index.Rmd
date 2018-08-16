---
title: "No Name LR Model"
author: "Matt Dunning"
date: "August 14, 2018"
output:
  output: rmarkdown::github_document
---

Packages Used
```{r, message=FALSE, warning=FALSE}

##ml library
#install.packages("caret)
library(caret)
##data transformation
#install.packages("dplyr")
library(dplyr)
##stats package for regression objects
#install.packages("stats")
library(stats)
##hypothesis/ chi-sq testing
#install.packages("car")
library(car)
#logistic regression eval - ROC curve
#install.packages("pROC")
library(pROC)
library(Hmisc)
library(ggplot2)

```

Read in data
```{r}
##Read in March Data,  Source = MW0 in this file
df_input_rmd <- read.csv("C:/Users/mdunning/Documents/Data/No_Name_Analysis_Data/march_name_analysis_2/model/march_name_analysis_2.csv")
#(nrows = 448986)
#head(df_input_rmd)


```


Filter out records with name and clubs close to sams cub
```{r, warning=FALSE}

##Filter out records with names and records in clubs close to sams --- (nrows=158027)
df_temp_rmd <- df_input_rmd %>% filter(noname == 1,!BJsClub1 %in% c(038,
                                             045,
                                             048,
                                             061,
                                             062,
                                             067,
                                             093,
                                             123,
                                             129,
                                             139,
                                             141,
                                             150,
                                             168,
                                             180,
                                             184,
                                             185,
                                             189,
                                             211,
                                             318,
                                             370
                  ))

#head(df_temp_rmd)

##Responses for entire population
table(df_temp_rmd$resp_flag)

```


Subset significant features

```{r}

##Subset Dataset selecting features determined most significant by Stepwise Chi-Squared tests
df_train <- subset(df_temp_rmd, select = c("resp_flag",
                                        "X3yr_RR",
                                        "comp_distance_flag",
                                        "Past_Pds"
))

df_test <- subset(df_temp_rmd, select = c("resp_flag",
                                      "X3yr_RR",
                                      "comp_distance_flag",
                                      "Past_Pds"
))

```


```{r}
##Decribe Data reporting descriptive stats on fetures
describe(df_train)
describe(df_test)
table(df_train$resp_flag)
table(df_test$resp_flag)

```

```{r}
#Histograms showing distributons of continuous features prior to scaling
par(mfrow=c(2,1))
hist(df_train$X3yr_RR)
hist(df_train$Past_Pds)

```


Capping Features for outliers
```{r}
##cap and flooring for variables replaceoutliers with percentile values (so if above this p99 jus assing value for p99)

df_train$Past_Pds[df_train$Past_Pds > 166] <- 166 ####capping a 95 percentile
df_train$X3yr_RR[df_train$X3yr_RR > 33.0] <- 33.0 ####capping at 99 percentile

df_test$Past_Pds[df_test$Past_Pds > 166] <- 166 ####capping a 95 percentile
df_test$X3yr_RR[df_test$X3yr_RR > 33.0] <- 33.0 ####capping at 99 percentile


```


Contents for un-scaled but capped data
```{r}
describe(df_train)
describe(df_test)

```


Rnadomly Sample data into train and tests data splitting 50/50

```{r}

##Randomly sample march data into train and test datasets
set.seed(7)
##Splits into train and test data from temp table 50/50 (nrows = 79013 each)
df_train <- df_train %>%
  group_by(resp_flag) %>%
  sample_frac(size = 0.5 , replace = F)
df_test <- df_test %>%
  group_by(resp_flag) %>%
  sample_frac(size = 0.5 , replace = F)



```



Scale data - Subtract mean divide by standard deviation
```{r}

##Pre-processing -- this function applies scale function to features which subtracts mean then divides by standard deviation 
df_train[,2] <- sapply(df_train[,2], FUN = "scale")
df_test[,2] <- sapply(df_test[,2], FUN = "scale")

df_train[,4] <- sapply(df_train[,4], FUN = "scale")
df_test[,4]<- sapply(df_test[,4], FUN = "scale")

describe(df_train)
describe(df_test)

```


Histograms sowing scaled distributions
```{r}

#Histograms showing distributons
par(mfrow=c(2,1))
hist(df_train$X3yr_RR)
hist(df_train$Past_Pds)

```


Coding Dependent Variable
```{r}

##Sets resp_flag as factor (levels = (0,1)) to avoid issues in logistic regression
#Factor is R's data structure for categorical variables
df_train$resp_flag <- as.factor(df_train$resp_flag)
df_test$resp_flag <- as.factor(df_test$resp_flag)

```

Fitting model
```{r}
#Model fit
fit <- glm(resp_flag ~ ., family = "binomial", data = df_train)

#Chi-Sq results
wald <- Anova(fit, type = "II", test="Wald")
print(wald)       

```

Summary of model fit
```{r}
summary(fit)
```


Train Model and store predictions as column in test set
```{r}

df_train$resp_flag <- as.factor(df_train$resp_flag)
model <- train(resp_flag ~ ., method = "glm", family = "binomial", data = df_train)

##Storing preds
predicts <- predict(model, type = "prob", newdata = df_test)
df_test$preds <- predicts$`1`

head(df_test, 50)

```

Aggregate both actual and predicted resposnes to decile level

```{r}
##Creating decile column on test set based on predictions column
df_test$decile <-with(df_test, cut(preds, 
                                breaks=quantile(preds, probs=seq(0,1, by=0.1)), labels = c(1:10), 
                                include.lowest=TRUE))

###Reporting response rate predictions per decile vs. actual response rates
df_decile_info <- df_test %>%
  group_by(decile) %>%
  summarise(pred_resp = mean(preds),
            actual_resp = mean(as.numeric(resp_flag)-1))

##drop these output vars to run again
#df_test <- df_test[,-which(names(df_test) %in% c("decile", "preds"))]





head(df_decile_info,10)

```

Error Report
```{r}

df_decile_info <- df_decile_info %>%
  mutate(error = abs(pred_resp - actual_resp))


avg_error <- mean(df_decile_info$error)
```

The average error for fitted vs predicted value is **0.0003110 **or **`r round(avg_error*100,2)` percent**.


Contents of data after predictions appended
```{r}
describe(df_test)
```


Predicted vs. Actual graphs
```{r}
plot(df_decile_info$decile,df_decile_info$pred_resp,col="red", xlim = c(10,1), xlab = "Decile", ylab = "Response Rate", main = "Predicted vs Actual Response Rates by Decile")
lines(df_decile_info$decile,df_decile_info$actual_resp, col="red", lwd = 2)
lines(df_decile_info$decile,df_decile_info$pred_resp, col="green", lwd = 2)
legend(3, 0.0045, legend=c("Actual", "Predicted"),
       col=c("red", "green"), lty=1)
```

ROC Curve
```{r}
##ROC Curve Evaluation
roc_obj <- roc(df_test$resp_flag , df_test$preds)
auc(roc_obj)


```

```{r}
plot(roc_obj)

```

Below is the logistic equation for predicting probability of response at the household level using 3yr_RR (response rate), comp_distance_flag, and Past_Pds (Past Paids).Response Rate and Past Paids are standardized by subtracting the mean and dividing by the standard deviation. Response rates and past paids are capped at 99th and 95th percentiles respectively.
Filtered for no name records only, SOURCE = MW0, and BJsClub1 not in (
           038,
           045,
           048,
           061,
           062,
           067,
           093,
           123,
           129,
           139,
           141,
           150,
           168,
           180,
           184,
           185,
           189,
           211,
           318,
           370
           )

Linear Equation: = $-6.45847 + 0.27832X_{x3yr.RR} + 0.53051X_{comp.distance.flag} + 0.14031X_{Past.Pds}$

Logistic Equation for predicting Probability:
(after capped outliers)
$P(resp.flag = 1) = \frac{e^{-6.45847 + 0.27832X_{x3yr.RR} + 0.53051X_{comp.distance.flag} + 0.14031X_{Past.Pds}}}{1 + e^{-6.45847 + 0.27832X_{x3yr.RR} + 0.53051X_{comp.distance.flag} + 0.14031X_{Past.Pds}}}$


(prior to capped outliers)
$P(resp.flag = 1) = \frac{e^{-6.1031288 + 0.2377197X_{x3yr.RR} + 0.2581363X_{comp.distance.flag} + 0.1965876X_{Past.Pds}}}{1 + e^{-6.1031288 + 0.2377197X_{x3yr.RR} + 0.2581363X_{comp.distance.flag} + 0.1965876X_{Past.Pds}}}$
