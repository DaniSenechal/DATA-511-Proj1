########################################################
# Project 1
# Danielle Senechal
# DATA 511 Intro to Data Science
# June 9th, 2020
########################################################

set.seed(12345)

library(magrittr)
library(caret)
library(RANN)
library(plyr)
library(rpart)
library(rpart.plot)
library(ggplot2)

proj1 <- read.csv("~/Documents/CCSU/DATA 511/Project 1/proj1")
# View(proj1)

########## 2 ##########
hist(proj1$capital.gain, main = "Histogram of Capital Gain", xlab = "Capital Gain") 
  # histogram with missing values
proj1$capital.gain[proj1$capital.gain == 99999] <- NA 
  # remove code 99999 (missing values)
hist(proj1$capital.gain, main = "Histogram of Capital Gain", xlab = "Capital Gain") 
  # histogram without missing values

cgm <- mean(proj1$capital.gain, na.rm = TRUE); cgm 
  # mean of capital gain w/out missing values
cgsd <- sd(proj1$capital.gain, na.rm = TRUE); cgsd 
  # standard dev of capital gain w/out missing values

imputation_model <- preProcess(proj1, method = c("knnImpute"))
  # model used for predicting
proj1.imp <- predict(imputation_model, proj1)
  # use model to predict values for missing data (standardized)
# View(proj1.imp)

proj1$cg.imp <- round(proj1.imp$capital.gain * cgsd + cgm, 5)
  # unstandardize the data and append the unstandardized precited missing
  # values to the original dataset
# View(proj1)

summary(proj1$capital.gain) 
  # 5 number summary of original capital gain variable (with missing)
summary(proj1$cg.imp)
  # 5 number summary of unstandardized predicted missing values (no more missing)
sd(proj1$cg.imp)
  # standard deviation of unstandardized predicted missing values (no more missing)


########## 3 ##########
proj1$cg.miss <- ifelse(
  test = is.na(proj1$capital.gain == T),
  yes = 1, 
  no = 0
)
  # assign all NAs to 1, 0 otherwise
# View(proj1)

summary(proj1$cg.miss)
  # check to see if successful (min = 0, max = 1)
nrow(proj1[(proj1$cg.miss >= 1),])
  # check to see if successful, should have 69 rows (true)

ct.in.miss <- table(proj1$income, proj1$cg.miss)
colnames(ct.in.miss) <- c("Not Missing", "Missing"); ct.in.miss
  # contingency table, rename the columnn names to reduce confusion

########## 4 ##########
proj1$ID <- 1:nrow(proj1)
  # append an identification row to the data frame
# View(proj1)

proj1[2001,]

########## 5 ##########
ct.in.ms <- table(proj1$income, proj1$marital.status)
  # proportion contingency table
ct.in.ms <- prop.table(ct.in.ms, 2)
  # column percentages
ct.in.ms <- ct.in.ms * 100
  # multiply by 100 to get percentages
ct.in.ms <- round(ct.in.ms, digits = 2); ct.in.ms
  # round to 2 decimal places

names(proj1)[names(proj1)=="marital.status"] <- "marital.status.old"
  # rename column header
# View(proj1)

proj1$marital.status <- revalue(proj1$marital.status.old, 
                                c("Divorced" = "Other", 
                                  "Married-AF-spouse" = "Married", 
                                  "Married-civ-spouse" = "Married", 
                                  "Married-spouse-absent" = "Married", 
                                  "Never-married" = "Other", 
                                  "Separated" = "Other", 
                                  "Widowed" = "Other"))
  # recode categorical variables into two categories
# View(proj1)

ct.in.new.ms <- table(proj1$income, proj1$marital.status)
  # proportion contingency table
ct.in.new.ms <- prop.table(ct.in.new.ms, 2)
  # column percentages
ct.in.new.ms <- ct.in.new.ms * 100
  # multiply by 100 to get percentages
ct.in.new.ms <- round(ct.in.new.ms, digits = 2); ct.in.new.ms
  # round to 2 decimal places

########## 6 ##########
proj1$capgl <- ifelse(
  test = proj1$`cg.imp` != 0 | proj1$capital.loss != 0,
  yes = 1, 
  no = 0
)
  # if capital gains or capital losses does not equal 0, assign 1, otherwise assign 0 
# View(proj1)

ct.in.capgl <- table(proj1$income, proj1$capgl)
  # proportion contingency table
ct.in.capgl <- prop.table(ct.in.capgl, 2)
  # column percentages
ct.in.capgl <- ct.in.capgl * 100
    # multiply by 100 to get percentages
ct.in.capgl <- round(ct.in.capgl, digits = 2); ct.in.capgl
  # round to 2 decimal places
colnames(ct.in.capgl) <- c("Has gains or losses", "Has no gains or losses"); ct.in.capgl
  # contingency table, rename the columnn names to reduce confusion

########## 7 ##########
t.in.rec <- table(proj1$income) %>% prop.table(); t.in.rec
  # proportion contingency table
t.in.rec <- t.in.rec * 100; t.in.rec
  # mulitply by 100 to get percentages
t.in.rec <- round(t.in.rec, digits = 2); t.in.rec
  # round up 2 decimals

clm <- mean(proj1$capital.loss, na.rm = TRUE); clm 
  # mean of capital gain w/out missing values
clsd <- sd(proj1$capital.loss, na.rm = TRUE); clsd 
  # standard dev of capital gain w/out missing values
clm + 3 * clsd

upper.cutoff <- proj1[(proj1$capital.loss > 1307.547),]
  # all capital losses greater than 3 sds (outliers)
nrow(upper.cutoff) 
  # total number of records

t.in.greater <- table(upper.cutoff$income) %>% prop.table(); t.in.greater
  # proportion contingency table
t.in.greater <- t.in.greater * 100; t.in.greater
  # mulitply by 100 to get percentages
t.in.greater <- round(t.in.greater, digits = 2); t.in.greater
  # round up 2 decimals

########## 8 ##########
ib <- rpart(formula = income ~ education, 
            data = proj1, 
            control = rpart.control(
              minbucket = .01 * nrow(proj1), 
              maxdepth = 2))
  # income (target variable) based on education
rpart.plot(ib)
  # plot of decision tree

educ.bin <- cut(proj1$education, breaks = c(-21, 13, 14, 21))
  # bin education

t.in.educ <- table(proj1$income, educ.bin); t.in.educ
  # contingency table of income and education
t.in.educ <- prop.table(t.in.educ, 2)
  # proportion contingency table
t.in.educ <- t.in.educ * 100; t.in.educ
  # mulitply by 100 to get percentages
t.in.educ <- round(t.in.educ, digits = 2); t.in.educ
  # round up 2 decimals

########## 9 ##########
proj1$educ.bin <- educ.bin
  # append binned education values to proj1 data frame
ggplot(proj1) + geom_bar(aes(proj1$educ.bin, fill=proj1$income)) +
  labs(x = "Education Bin", y = "Frequency", fill = "Income Status", 
       title = "Education Relative to Income Status")
  # non-normalized
ggplot(proj1) + geom_bar(aes(proj1$educ.bin, fill=proj1$income), 
                         position = "fill") +
  labs(x = "Education Bin", y = "Frequency", fill = "Income Status", 
       title = "Education Relative to Income Status")
  # normalized

########## 10 ##########
t.in.sex <- table(proj1$income, proj1$sex); addmargins(t.in.sex)
  # contingency table of income and sex
t.in.sex <- prop.table(t.in.sex, 2)
  # proportion contingency table
t.in.sex <- t.in.sex * 100; t.in.sex
  # mulitply by 100 to get percentages
t.in.sex <- round(t.in.sex, digits = 2); t.in.sex
  # round up 2 decimals
addmargins(t.in.sex, 1)
  # add margins

