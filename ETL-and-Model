###


###No name model 
##ml library
library(caret)
#data transformatin
library(dplyr)
#describe data
library(Hmisc)
#
library(randomForest)
#logistic regression eval - ROC curve
library(pROC)
#
library(car)
library(stats)


##Read in data
df_input <- read.csv("C:/Users/mdunning/Documents/Data/No_Name_Analysis_Data/march_name_analysis_2/model/march_name_analysis_2.csv")
View(df_input)


##Filter out records with names, Excluding Source = MW1 from dataset --- (n=160931)
df_temp <- filter(df_input, noname == 1)


##Subset prospect data
##Good vars determined by chi-sq test
good_vars <- c("REC_NBR", 
               "GEOCODE",
               "resp_flag",
               'PTA_Dist_Club',
               'PTA_Club',
               'CLUB',
               'BJs_Mbrs',
               'Mar18_Score',
               'CR_Penetration_Pct',
               'pct_bjs_mbrs',
               'Penetration_Rate',
               'X2yr_Resp',
               'X_3yr_Resp',
               'Mar18_Decile',
               'IC_MU_022418',
               'BJs_Adv',
               'Past_Pds',
               'avg_A26270',
               'avg_N03300',
               'avg_A01000',
               'comp_distance_flag',
               'avg_A11901',
               'avg_A00100',
               'avg_A02650',
               'avg_A00600',
               'avg_A85530',
               'avg_N85300',
               'avg_A03300',
               'avg_A06500',
               'avg_A85300',
               'avg_A10300',
               'avg_N03270',
               'avg_A10960',
               'avg_A05800',
               'avg_A10600',
               'avg_A02900',
               'avg_N26270',
               'avg_N07300',
               'A26270',
               'avg_A07300',
               'avg_A00650',
               'STATEFIPS',
               'avg_N01000',
               'avg_A00300',
               'BJsClub1',
               'avg_N10300',
               'avg_N85530',
               'avg_N59660',
               'avg_A04800',
               'avg_A59660',
               'avg_N59720',
               'avg_A09400',
               'BJsDist1',
               'avg_A03270',
               'avg_N00650',
               'avg_MARS4',
               'avg_N07220',
               'Avg_HH_Inc',
               'avg_A01400',
               'avg_A59720',
               'avg_A00900',
               'avg_N10960',
               'MARS4',
               'avg_A18425',
               'avg_N00600',
               'avg_N11070',
               'avg_N11902',
               'avg_N06500',
               'avg_N00300',
               'avg_N18500',
               'avg_N02900',
               'avg_N07240',
               'avg_N03150',
               'avg_N09600',
               'avg_N11901',
               'avg_A07100',
               'pct_name',
               'avg_A00200',
               'avg_N04470',
               'avg_A07220',
               'avg_A09600',
               'avg_A19300',
               'avg_A18300',
               'avg_N07100',
               'avg_N18425',
               'avg_N18300',
               'New_Moms')

##Subsetting good vars -- n =160,931 with 100 vars (103 with resp_flag, rec_nbr and geocode)
df_temp <- subset(df_temp, select = c(good_vars))



##########TWO approaches to sampling (radnom and stratified)
##stratified smapling
df_train <- df_temp
df_test <- df_temp
set.seed(7)
df_train <- df_train %>%
  group_by(resp_flag) %>%
  sample_frac(size = 0.5 , replace = F)
df_test <- df_test %>%
  group_by(resp_flag) %>%
  sample_frac(size = 0.5 , replace = F)



##Split, train test random sampling
###Split into train and test sets (50/50) ##df now is at n== 160931
set.seed(7)
intrain<-createDataPartition(y=df_temp$resp_flag,p=0.5,list=FALSE)

df_train <- df_temp[ intrain[!is.na(intrain),],] #(n = 80466)
df_test  <- df_temp[-intrain[!is.na(intrain),],] #(n = 80465)
#drop first row due to odd#/2
df_train <- df_train[-1,]



##Drop Geocode and Rec_number columns
##Subsetting out of geocode an d rec_num columns
#first add rec num to row names
rownames(df_train) <- df_train$REC_NBR
rownames(df_test) <- df_test$REC_NBR
df_train <- as.data.frame(subset(df_train, select = c(resp_flag:New_Moms)))
df_test <- as.data.frame(subset(df_test, select = c(resp_flag:New_Moms)))


##Pre-processing -- dividing by standard deviation, there was just better fit when scaled
df_train[,2:85] <- sapply(df_train[,2:85], FUN = "scale")
df_test[,2:85]<- sapply(df_test[,2:85], FUN = "scale")

##Instantiate first model
df_train$resp_flag <- as.factor(df_train$resp_flag)
df_test$resp_flag <- as.factor(df_test$resp_flag)
fit1 <- glm(resp_flag ~ ., family = binomial, data = df_train)


##summary stats p-value
summary(fit1)

##wald chi -sq
wald <- Anova(fit1, type = "II", test="Wald")
print(wald)

#stores feature names as a column
wald$cols <- rownames(wald)

#Take only those features showing chi-sq significance
wald <- wald %>%
  filter(Chisq>2.02)
#
cols <- wald$cols
##append resp_flag
cols <- append(cols, "resp_flag")


##Selecting significant features
df_train <- subset(df_train, select = cols)
df_test <- subset(df_test, select = cols)#


##Instantiate 2nd model
fit2 <- glm(resp_flag ~ ., family = binomial, data = df_train)


##summary stats p-value
summary(fit2)

##wald chi -sq
wald2 <- Anova(fit2, type = "II", test="Wald")
print(wald2)

#stores feature names as a column
wald2$cols <- rownames(wald2)

#Take only those features showing chi-sq value greater than 1
wald2 <- wald2 %>%
  filter(Chisq>2.55)
#
cols <- wald2$cols
##append resp_flag
cols <- append(cols, "resp_flag")


##Selecting significant features
df_train <- subset(df_train, select = cols)
df_test <- subset(df_test, select = cols)####output 10 feats

df_train$resp_flag <- as.factor(df_train$resp_flag)
df_test$resp_flag <- as.factor(df_test$resp_flag)
fit3 <- glm(resp_flag ~ ., family = binomial, data = df_train)

##summary stats p-value
summary(fit3)

##wald chi -sq
wald3 <- Anova(fit3, type = "II", test="Wald")
print(wald3)

#stores feature names as a column
wald3$cols <- rownames(wald3)

#Take only those features showing chi-sq value greater than 1
wald3 <- wald3 %>%
  filter(Chisq>3)
#
cols3 <- wald3$cols
##append resp_flag
cols3 <- append(cols3, "resp_flag")


##Selecting significant features
df_train <- subset(df_train, select = cols3)
df_test <- subset(df_test, select = cols3)####output 10 feats


fit4 <- glm(resp_flag ~ ., family = binomial, data = df_train)

##wald chi -sq
wald4 <- Anova(fit4, type = "II", test="Wald")
print(wald4)

#stores feature names as a column
wald4$cols <- rownames(wald4)

#Take only those features showing chi-sq value greater than 1
wald4 <- wald4 %>%
  filter(Chisq>3)
#
cols4 <- wald4$cols
##append resp_flag
cols4 <- append(cols4, "resp_flag")


##Selecting significant features
df_train <- subset(df_train, select = cols4)
df_test <- subset(df_test, select = cols4)####output 10 feats



fit5 <- glm(resp_flag ~ ., family = binomial, data = df_train)
wald5 <- Anova(fit5, type = "II", test="Wald")
print(wald5)

###
df_train <- subset(df_train, select = c("resp_flag", "X_3yr_Resp", "comp_distance_flag"))
df_test <- subset(df_test, select = c("resp_flag", "X_3yr_Resp", "comp_distance_flag"))



fit6 <- glm(resp_flag ~ ., family = binomial, data = df_train)
wald6 <- Anova(fit6, type = "II", test="Wald")
print(wald6)

model1 <- train(as.factor(resp_flag) ~ ., method = "glm", family = "binomial", data = df_train)


##Stroing predictons
preds <- predict(model1, type = "prob", newdata = df_test)
df_test$preds <- preds$`1`


##Creating decile column
df_test <- df_test %>%
  mutate(decile = ntile(preds, 10))
##drop these output vars to run again
#df_test <- df_test[,-which(names(df_test) %in% c("decile", "preds"))]
##
###Reporting response rate predictioins per decile vs. actual and scoring
df_decile_info_resamp <- df_test %>%
  group_by(decile) %>%
  summarise(dec_resp = mean(preds),
            actual_resp = mean(resp_flag))


##EValuation
roc_obj <- roc(df_test$resp_flag , df_test$preds)
auc(roc_obj)
plot(roc_obj)


