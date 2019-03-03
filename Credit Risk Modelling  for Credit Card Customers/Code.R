#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations


######################################################
#Business Objective
######################################################

#We need help CredX identify the right customers using predictive models. Using past data of the bank's applicants,
#we need to determine the factors affecting credit risk, create strategies to mitigate the acquisition risk and 
#assess the financial benefit of our project

#####################
#Data understanding
##################### 
#There are two data sets in this project - demographic and credit bureau data
#Demographic/application data: This is obtained from the information provided by the applicants at the time of 
#credit card application. It contains customer-level information on age, gender, income, marital status, etc. 
#Variables :       Description 
#Application ID:   Unique ID of the customers 
#Age :            Age of customer 
#Gender :         Gender of customer 
#Marital Status : Marital status of customer (at the time of application) 
#No of dependents : No. of children of customers 
#Income :         Income of customers 
#Education :      Education of customers 
#Profession :     Profession of customers 
#Type of residence:  Type of residence of customers 
#No of months in current residence: 
#                 No of months in current residence of customers 
#No of months in current company: 
#                 No of months in current company of customers 
#Performance Tag : Status of customer performance (" 1 represents "Default") 


#There are total 71295 in this dataset. 

#Credit bureau: This is taken from the credit bureau and contains variables such as 'number of times 30 DPD or 
#worse in last 3/6/12 months', 'outstanding balance', 'number of trades', etc. 

#Variable :             Description 
#Application ID :       Customer application ID
#No of times 90 DPD or 
#worse in last 6 months :Number of times customer has not paid dues since 90days in last 6 months 
#No of times 60 DPD or 
#worse in last 6 months :Number of times customer has not paid dues since 60 days last 6 months 
#No of times 30 DPD or
#worse in last 6 months :Number of times customer has not paid dues since 30 days days last 6 months 
#No of times 90 DPD or 
#worse in last 12 months :Number of times customer has not paid dues since 90 days days last 12 months
#No of times 60 DPD or 
#worse in last 12 months :Number of times customer has not paid dues since 60 days days last 12 months 
#No of times 30 DPD or
#worse in last 12 months :Number of times customer has not paid dues since 30 days days last 12 months   
#Avgas CC Utilization in last 12 months
#                         ;Average utilization of credit card by customer 
#No of trades opened in last 6 months 
#                         :Number of times the customer has done the trades in last 6 months 
#No of trades opened in last 12 months 
#                         :Number of times the customer has done the trades in last 12 months 
#No of PL trades opened in last 6 months 
#                         :No of PL trades in last 6 month of customer 
#No of PL trades opened in last 12 months 
#                         :No of PL trades in last 12 month of customer 
#No of Inquiries in last 6 months (excluding home & auto loans) 
#                         :Number of times the customers has inquired in last 6 months
#No of Inquiries in last 12 months (excluding home & auto loans) 
#                         :Number of times the customers has inquired in last 12 months 
#Presence of open home loan :Is the customer has home loan (1 represents "Yes") 
#Outstanding Balance      :Outstanding balance of customer
#Total No of Trades       : Number of times the customer has done total trades 
#Presence of open auto loan : Is the customer has auto loan (1 represents "Yes") 
#Performance Tag          :Status of customer performance (" 1 represents "Default") 

#################################
#load library list              #
#################################

#rm(list = ls())
library(Information)
library(rpart)
library(mlr)
library(FSelector)
library(rpart.plot)
library(ggplot2)
library(woe)
library(corrplot)
library(data.table)
library(factoextra)
library(doParallel)
library(stringi)
library(dplyr)
library(stringr)
library(caret)
library(class)
library(DMwR)
library(lme4)
library(caret)
library(e1071)
library(bnstruct)
library(VIM)
library(MASS)
library(car)
library(caTools)
library(fuzzyjoin)
library(mice)
library(grid)
library(gridExtra)



load.libraries <- c('Information', 'rpart', 'mlr', 'FSelector', 'rpart.plot', 'ggplot2', 'woe', 'corrplot', 'data.table', 'factoextra',
                    'doParallel', 'stringi', 'dplyr', 'stringr', 'caret', 'class', 'DMwR', 'lme4', 'caret', 'e1071', 
                    'bnstruct', 'VIM', 'MASS', 'car',  'caTools', 'fuzzyjoin', 'mice', 'mlr', 'grid', 'gridExtra')

install.lib <- load.libraries[!load.libraries %in% installed.packages()]


for(libs in install.lib) install.packages(libs, dependences = TRUE)

sapply(load.libraries, require, character = TRUE)

##################################
#import data sets into dataframes#
##################################
#setwd("C:/Users/pawan/Downloads/DS/domain/capstone project")



credit_buro_df <-
  read.csv(
    "Credit Bureau data.csv"
  )
demographic_df <-
  read.csv("Demographic data.csv")
summary(demographic_df)


str(credit_buro_df)
nrow(credit_buro_df)
# Output: 71295

str(demographic_df)
nrow(demographic_df)
# Output: 71295
# lets find out unique Application.ID
nrow(distinct(dplyr::select(demographic_df, Application.ID)))
# Output: 71292
# Hence we have 71295-71292=3 records as duplicate Lets find out these records

# Lets find out duplicate records
demographic_df_grp <- group_by(demographic_df, Application.ID)
demographic_df_grp_summ <-
  summarise(demographic_df_grp, count = n())
demographic_df_grp_summ_dup <-
  data.frame(demographic_df_grp_summ[demographic_df_grp_summ$count > 1,])
demographic_df_grp_summ_dup
# output :-
# 1 653287861 2
# 2 671989187 2
# 3 765011468 2

# View(demographic_df_deduped)
demographic_df_deduped <- unique(demographic_df)
length(unique(demographic_df$Application.ID))
# Output: 71292

# lets deduplicate demographic as well as credit buro data
demographic_df <- demographic_df[-(which(duplicated(demographic_df$Application.ID))),]
credit_buro_df <- credit_buro_df[-(which(duplicated(credit_buro_df$Application.ID))),]
length(unique(credit_buro_df$Application.ID))
# Output: 71292
length(unique(demographic_df$Application.ID))
# Output: 71292



######## Data cleansing --- demographic_df
# check for NA :-
sapply(demographic_df, function(x)
  sum(is.na(x)))
# Output: There are 3 NA`s in Number.of.dependents and 1425 in Performance.Tag
#1425 NA's in the performance tag are those applicaiton which are rejected and will
#be separated out as validation test set(rejected inference)

#the target variable in both the data frames looks same, let us check if anything is missing
if (sum(demographic_df$Application.ID %in% credit_buro_df$Application.ID) == 71292){
  print('all application in demographic data present in credit bureau , no isssues')
}else{
  print('target variable cannot be directly merged')
}
#merge dataset
master_df <- merge(demographic_df, credit_buro_df, by="Application.ID")

#demographic_df_valid <- demographic_df[(which(is.na(demographic_df$Performance.Tag))),]
#demographic_df <- demographic_df[-(which(is.na(demographic_df$Performance.Tag))),]
# check for blank values
sapply(demographic_df, function(x)
  sum(stri_isempty(stri_trim(x)), na.rm = T))
# Output:-
# Application.ID                     Age
# 0                                    0
# Gender                            Marital.Status..at.the.time.of.application.
# 2                                   6
# No.of.dependents                  Income
# 0                                   0
# Education                         Profession
# 119                                14
# Type.of.residence           No.of.months.in.current.residence
# 8                                           0
# No.of.months.in.current.company   Performance.Tag
# 0                                           0

# Lets replace this by null so that we can impute these using KNN
demographic_df[which(
  !is.na(demographic_df$Marital.Status..at.the.time.of.application.) &
    stringr::str_trim(demographic_df$Marital.Status..at.the.time.of.application.) ==
    ""
), c("Marital.Status..at.the.time.of.application.")] <- NA
demographic_df[which(!is.na(demographic_df$Gender) &
                       stringr::str_trim(demographic_df$Gender) == ""), c("Gender")] <-
  NA
demographic_df[which(
  !is.na(demographic_df$Education) &
    stringr::str_trim(demographic_df$Education) == ""
), c("Education")] <- NA
demographic_df[which(
  !is.na(demographic_df$Profession) &
    stringr::str_trim(demographic_df$Profession) == ""
), c("Profession")] <- NA
demographic_df[which(
  !is.na(demographic_df$Type.of.residence) &
    stringr::str_trim(demographic_df$Type.of.residence) == ""
), c("Type.of.residence")] <- NA


demographic_df = setDT(demographic_df)
str(demographic_df)

# Lets impute those using kNN
#Take backup of performance tag so that we can restore the same if its NA re replaced by K Means
Performance.Tag.bkp <-
  data.frame(Performance.Tag = demographic_df$Performance.Tag)


demographic_df_with_NA <- cbind(demographic_df)

# Lets use KNN to fill out missing values

demographic_df <-
  VIM::kNN(
    demographic_df,
    variable = c(
      "Marital.Status..at.the.time.of.application.",
      "No.of.dependents",
      "Gender",
      "Type.of.residence",
      "Education",
      "Profession"
    ),
    k = 267
  )
demographic_df <- demographic_df[,1:12]
summary(demographic_df)

demographic_df_blank <-
  demographic_df[which(is.na(demographic_df$Marital.Status..at.the.time.of.application.)),]

demographic_df_blank <-
  demographic_df[which(str_trim(demographic_df$Marital.Status..at.the.time.of.application.) ==
                         ""),]

#####################################
#Univariate and Bi-variate analysis #
#####################################
#check for defaulter rate in credit bureau dataframe
summary(factor(credit_buro_df$Performance.Tag))
#defaulter rate= 2948/71292=4.13 %
#1425 NAs are the application which are rejected 
ggplot(credit_buro_df,aes(factor(Performance.Tag))) +geom_bar(fill = "orange")
#clearly the data set is imbalance and we will have to treat it.

#check for defaulter rate
summary(factor(demographic_df$Performance.Tag))
#defaulter rate= 2948/71292=4.13 %
#1425 NAs are the application which are rejected 
ggplot(demographic_df,aes(factor(Performance.Tag))) +geom_bar(fill = "orange")
#clearly the data set is imbalance and we will have to treat it.


### Lets start with all Numeric continueus variables
str(demographic_df)
## 1] Age


quantile(demographic_df$Age,
         probs = seq(0, 1, .01))
#since credit card is granted to only adults age > 18 setting floor limit as 18
demographic_df[which(demographic_df$Age < 18), c("Age")] <-
  18
ggplot(demographic_df,aes(factor(Age)))+geom_bar(fill='orange')

# Binning the age variable and store it into "binning.age".

demographic_df$binning.age <- as.factor(cut(demographic_df$Age, breaks = c(18, 22, 30, 40, 50, 60)))


# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(Performance.Tag ~binning.age, demographic_df, mean),aggregate(Performance.Tag~binning.age, demographic_df, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(demographic_df$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "default_rate", "no_of_defaulters","no.of_applicants")

# Round Off the values

agg_age$default_rate <- format(round(as.numeric(agg_age$default_rate), 2))

agg_age

#-------------------------------------------------------

# Let's see the default rate of each age bucket in the plot

ggplot(agg_age, aes(age, no.of_applicants,label = default_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#clearly we can see defaulter rate is uniform acroos the ages above 22 so binning age won't be much helpful
#demographic_df <- demographic_df[,-1]


## 2] No.of.dependents

## Check for outlier by checking the quantiles
quantile(demographic_df$No.of.dependents,
         probs = seq(0, 1, .01))
# No outliers observed
summary(factor(demographic_df$No.of.dependents))
ggplot(demographic_df,aes(factor(No.of.dependents)))+geom_bar(fill='orange')

#let us see the percentage of default for each 

agg_nof_dep <- demographic_df%>%group_by(No.of.dependents)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag,na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_nof_dep
ggplot(agg_nof_dep, aes(No.of.dependents, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) 

#clearly the rate of defaulters is quiet high if the no of dependents is 3 but same trend is not when it is 4 or 5
#so does not look significant
## 3] Income

# Lets check for outliers
quantile(demographic_df$Income,
         probs = seq(0, 1, .01))
demographic_df[which(demographic_df$Income < 4.5), c("Income")] <-
  4.5
#*****8** cap at 1%
# No outliers observed
summary(demographic_df$Income)

demographic_df$Income_bin=ifelse(demographic_df$Income<=10,"less than 10k",
                                 ifelse(demographic_df$Income>10 & demographic_df$Income<=20,"10 to 20k",
                                        ifelse(demographic_df$Income>20 & demographic_df$Income<=30,"20 to 30k",
                                               ifelse(demographic_df$Income>30 & demographic_df$Income<=40,"30 to 40k",
                                                      ifelse(demographic_df$Income>40 & demographic_df$Income <=50,"40 to 50k","greater than 50k")))))

ggplot(demographic_df,aes(Income))+geom_histogram(fill="orange",color="black",bins = 100)+theme_bw()
summary(demographic_df$Income)
ggplot(demographic_df,aes(factor(Income_bin))) + geom_bar(fill = "orange")
#credit granted is less when the income is greater than 40K
summary(factor(demographic_df$Income_bin))
#let us see default rate for each income bin

agg_Income_bin <- demographic_df%>%group_by(Income_bin)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Income_bin

ggplot(agg_Income_bin, aes(Income_bin, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#defaulter rate is singnificantly high when the income is less than 10K
#if the income is between 10K to 30K, still in defaulter rate is high
#the default rate is vey low comparatively when the income is greater than 50K
## 4] No.of.months.in.current.residence


# Lets check for outliers
quantile(demographic_df$No.of.months.in.current.residence,
         probs = seq(0, 1, .01))
# No outliers observed
ggplot(demographic_df,aes(factor(No.of.months.in.current.residence))) + geom_bar(fill = "orange")

demographic_df$Noy_incurr_res_bin=ifelse(demographic_df$No.of.months.in.current.residence<24,"LT 2 yrs",
                                         ifelse(demographic_df$No.of.months.in.current.residence>=24 & demographic_df$No.of.months.in.current.residence<60,"GE 2 yrs & LT 5 yrs",
                                                ifelse(demographic_df$No.of.months.in.current.residence>60 & demographic_df$No.of.months.in.current.residence <84,"GE 5 yrs & LT 7 yrs","GT 7 yrs")))
summary(factor(demographic_df$Noy_incurr_res_bin))
agg_Noy_incurr_res_bin <- demographic_df%>%group_by(Noy_incurr_res_bin)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))

ggplot(agg_Noy_incurr_res_bin, aes(Noy_incurr_res_bin, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#the default rate is hightest when the current residence is less than 5 years but greater than 2 years
#however the same it is quite low when it is less than 2 years
#so there is not a clear tren that we can see

## 5] No.of.months.in.current.company

# Lets check for outliers
quantile(demographic_df$No.of.months.in.current.company,
         probs = seq(0, 1, .01))
# We observe that there is sudden increase in values frrom 99% to 100%  wich constitutes 1% of the values, so lets
# Cap all values >74 to 74

demographic_df[which(demographic_df$No.of.months.in.current.company > 74), c("No.of.months.in.current.company")] <-
  74


ggplot(demographic_df,aes(factor(No.of.months.in.current.company))) + geom_bar(fill = "orange")

demographic_df$Noy_incurr_cmp_bin=ifelse(demographic_df$No.of.months.in.current.company<24,"LT 2 yrs",
                                         ifelse(demographic_df$No.of.months.in.current.company>=24 & demographic_df$No.of.months.in.current.company <60,"GE 2 yrs & LT 5 yrs",
                                                ifelse(demographic_df$No.of.months.in.current.company>60 & demographic_df$No.of.months.in.current.company  <96,"GE 5 yrs & LT 8 yrs","GT 8 yrs")))
summary(factor(demographic_df$Noy_incurr_cmp_bin))
agg_Noy_incurr_cmp_bin <- demographic_df%>%group_by(Noy_incurr_cmp_bin)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Noy_incurr_cmp_bin

#clearly there is no trend that we can generalise
#the default rate is highest when no of years spent in current company is less than 2 years
#and quiet low when it is greater than 8 years
#
#
ggplot(agg_Noy_incurr_cmp_bin, aes(Noy_incurr_cmp_bin, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Lets Clean categorical data
summary(demographic_df)
# All categorical data seems fine and no NA and no typo
#gender
summary(demographic_df$Gender)
#prop.table(table(demographic_df$Performance.Tag,demographic_df$Gender),1) *100

agg_Gender <- demographic_df%>%group_by(Gender)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = TRUE))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))



ggplot(agg_Gender, aes(Gender, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) 
#defaulter rate is very high for both male and female

#Marital.Status..at.the.time.of.application.

summary(demographic_df$Marital.Status..at.the.time.of.application.)
#prop.table(table(demographic_df$Performance.Tag,demographic_df$Marital.Status..at.the.time.of.application.),1) *100

agg_Marg_sts <- demographic_df%>%group_by(Marital.Status..at.the.time.of.application.)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = TRUE))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))

agg_Marg_sts

ggplot(agg_Marg_sts, aes(Marital.Status..at.the.time.of.application., no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#singles have slightly high percentage of defaulter
#Education
summary(demographic_df$Education)

agg_Edu <- demographic_df%>%group_by(Education)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = TRUE))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))

agg_Edu

ggplot(agg_Edu, aes(Education, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#other education sector have very high defaulter rate however there are very few samples in that category
#overall not a clear distinguisher

#Profession
summary(demographic_df$Profession)

agg_Profession <- demographic_df%>%group_by(Profession)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = TRUE))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))

agg_Profession

ggplot(agg_Profession, aes(Profession, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#Type.of.residence
summary(demographic_df$Type.of.residence)

agg_Typof_residence <- demographic_df%>%group_by(Type.of.residence)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = TRUE))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))

agg_Typof_residence

ggplot(agg_Typof_residence, aes(Type.of.residence, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#highest defaulters are person living in company provides or in parents owned home
#however except others categort all have quite high defaulter rate

# Lets analyse credit_buro_df
summary(credit_buro_df)
#backup
credit_buro_df_bkup <- credit_buro_df
#credit_buro_df_valid <- credit_buro_df[(which(is.na(credit_buro_df$Performance.Tag))),]
#credit_buro_df <- credit_buro_df[-(which(is.na(credit_buro_df$Performance.Tag))),]
summary(credit_buro_df)
# We also observed that we have Presence.of.open.home.loan as 'NA' as 272 and for Outstanding.Balance
# we have NA as 272 , lets take the count of records having home loan as NA and outstanding balance also as NA
nrow(credit_buro_df[which(
  is.na(credit_buro_df$Presence.of.open.home.loan) &
    is.na(credit_buro_df$Outstanding.Balance)
),])
# output : 272 , which are 272/72192 = .3% so we can  remove these records
# We observe that there are NA in Outstanding.Balance so lets remove these records
credit_buro_df <-
  credit_buro_df[which(!is.na(credit_buro_df$Outstanding.Balance)),]
#instead of removing better we impute it using KNN so we dont loose any data
summary(factor(credit_buro_df$Performance.Tag[which(is.na(credit_buro_df$Outstanding.Balance))]))
#(8/264)*100--3 percent are defaulter if we remove them we will loose that data
#as already data set is imabalanced we decide not to remove them
# Lets check for other variables
summary(credit_buro_df)

# There are 786 NA in Avgas.CC.Utilization.in.last.12.months, lets analyse these records
# these are 786/72192 .01% of total data hence can be removed

credit_buro_df <-
  credit_buro_df[which(!is.na(credit_buro_df$Avgas.CC.Utilization.in.last.12.months)),]
#instead of removing better to impute using KNN

credit_buro_df <-
  VIM::kNN(
    credit_buro_df,
    variable = c("Avgas.CC.Utilization.in.last.12.months",
                 "Outstanding.Balance",
                 "No.of.trades.opened.in.last.6.months",
                 "Presence.of.open.home.loan"),
    k = 267
  )

credit_buro_df <- credit_buro_df[,1:19]
# Lets check for other variables
summary(credit_buro_df)

# No more NA apart from Performance.Tag which we will consider as test data

# Lets check for Outlier in each variable
# 1] No.of.times.90.DPD.or.worse.in.last.6.months

# Lets check for outliers
quantile(credit_buro_df$No.of.times.90.DPD.or.worse.in.last.6.months,
         probs = seq(0, 1, .01))
# No outliers observed
ggplot(credit_buro_df,aes(factor(No.of.times.90.DPD.or.worse.in.last.6.months))) + geom_bar(fill = "orange")

agg_Not_90dpd <- credit_buro_df%>%group_by(No.of.times.90.DPD.or.worse.in.last.6.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Not_90dpd

ggplot(agg_Not_90dpd, aes(No.of.times.90.DPD.or.worse.in.last.6.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#clearly as the no time 90 DPD or worst in last 6 months increases, the percentage of defaulters is more

# 2] No.of.times.60.DPD.or.worse.in.last.6.months

# Lets check for outliers
quantile(credit_buro_df$No.of.times.60.DPD.or.worse.in.last.6.months,
         probs = seq(0, 1, .01))
# No outliers observed

agg_Not_60dpd <- credit_buro_df%>%group_by(No.of.times.60.DPD.or.worse.in.last.6.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Not_60dpd


ggplot(agg_Not_60dpd, aes(No.of.times.60.DPD.or.worse.in.last.6.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)  
#again same trend if there is 1 or more than 1 DPD then defaulter rate increases

# 3]  No.of.times.30.DPD.or.worse.in.last.6.months

# Lets check for outliers
quantile(credit_buro_df$No.of.times.30.DPD.or.worse.in.last.6.months,
         probs = seq(0, 1, .01))
# No outliers observed
agg_Not_30dpd <- credit_buro_df%>%group_by(No.of.times.30.DPD.or.worse.in.last.6.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Not_30dpd


ggplot(agg_Not_30dpd, aes(No.of.times.30.DPD.or.worse.in.last.6.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#again this seems important trent , if 1 or more time the percentage of defaulter is very high
# 4]  No.of.times.90.DPD.or.worse.in.last.12.months

# Lets check for outliers
quantile(credit_buro_df$No.of.times.90.DPD.or.worse.in.last.12.months,
         probs = seq(0, 1, .01))
# No outliers observed
agg_Not_90dpd_last12mth <- credit_buro_df%>%group_by(No.of.times.90.DPD.or.worse.in.last.12.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Not_90dpd_last12mth


ggplot(agg_Not_90dpd_last12mth, aes(No.of.times.90.DPD.or.worse.in.last.12.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#defaulter rate is very less if there is 0 no of time 90 dpd in last 12 months

# 5]  No.of.times.60.DPD.or.worse.in.last.12.months

# Lets check for outliers
quantile(credit_buro_df$No.of.times.60.DPD.or.worse.in.last.12.months,
         probs = seq(0, 1, .01))
# No outliers observed
agg_Not_60dpd_last12mth <- credit_buro_df%>%group_by(No.of.times.60.DPD.or.worse.in.last.12.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Not_60dpd_last12mth


ggplot(agg_Not_60dpd_last12mth, aes(No.of.times.60.DPD.or.worse.in.last.12.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#clearly we see a trend as the no of times DPD increases the defaulter rate increases significantly
# 6]  No.of.times.30.DPD.or.worse.in.last.12.months

# Lets check for outliers
quantile(credit_buro_df$No.of.times.30.DPD.or.worse.in.last.12.months,
         probs = seq(0, 1, .01))
# No outliers observed
agg_Not_30dpd_last12mth <- credit_buro_df%>%group_by(No.of.times.30.DPD.or.worse.in.last.12.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Not_30dpd_last12mth


ggplot(agg_Not_30dpd_last12mth, aes(factor(No.of.times.30.DPD.or.worse.in.last.12.months), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#clearly we see a trend as the no of times DPD increases the defaulter rate increases significantly
#exception are when it is 8,9 however the no of applicants are very less in the sample for these

# 7]  Avgas.CC.Utilization.in.last.12.months

# Lets check for outliers
quantile(credit_buro_df$Avgas.CC.Utilization.in.last.12.months,
         probs = seq(0, 1, .01))
#at 95% there is sudden jump so will cap at 94%
credit_buro_df[which(credit_buro_df$Avgas.CC.Utilization.in.last.12.months > 90 ),c("Avgas.CC.Utilization.in.last.12.months")] <- 90
# No outliers observed
agg_avg_ccutil_l12<- credit_buro_df%>%group_by(Avgas.CC.Utilization.in.last.12.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_avg_ccutil_l12


ggplot(agg_avg_ccutil_l12, aes(factor(Avgas.CC.Utilization.in.last.12.months), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

credit_buro_df$avg_ccutil_l12_bin=ifelse(credit_buro_df$Avgas.CC.Utilization.in.last.12.months < 9 ,"LE 8 avg cc util",
                                         ifelse(credit_buro_df$Avgas.CC.Utilization.in.last.12.months>=9 & credit_buro_df$Avgas.CC.Utilization.in.last.12.months < 15,"GE 9 & LT 15 avg cc util",
                                                ifelse(credit_buro_df$Avgas.CC.Utilization.in.last.12.months>=15 & credit_buro_df$Avgas.CC.Utilization.in.last.12.months < 46,"GE 15 & LT 46 avg cc util",
                                                       ifelse(credit_buro_df$Avgas.CC.Utilization.in.last.12.months >=46 & credit_buro_df$Avgas.CC.Utilization.in.last.12.months < 70,"GE 46 & LT 70 avg cc util","GT 70 avg cc util"))))

agg_avg_ccutil_l12_bin<- credit_buro_df%>%group_by(avg_ccutil_l12_bin)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_avg_ccutil_l12_bin


ggplot(agg_avg_ccutil_l12_bin, aes(factor(avg_ccutil_l12_bin), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#clearly if the average utilisation is more then the defaulter rate is high
# 8]  No.of.trades.opened.in.last.6.months

# Lets check for outliers
quantile(credit_buro_df$No.of.trades.opened.in.last.6.months,
         probs = seq(0, 1, .01))
# No outliers observed
agg_Nof_trades_lst6mth <- credit_buro_df%>%group_by(No.of.trades.opened.in.last.6.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Nof_trades_lst6mth


ggplot(agg_Nof_trades_lst6mth, aes(No.of.trades.opened.in.last.6.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#there is no clear trend that we can see..highest defaulter rate is when the no of trades opened is between 2-5

# 9]  No.of.trades.opened.in.last.12.months

# Lets check for outliers
quantile(credit_buro_df$No.of.trades.opened.in.last.12.months,
         probs = seq(0, 1, .01))
# we observe abrupt increase in values from 99% to 100% which constitutes 1 % so lets cap the same
# lets replace all the values > 21 with 21
credit_buro_df[which(credit_buro_df$No.of.trades.opened.in.last.12.months >
                       21), c("No.of.trades.opened.in.last.12.months")] <-
  21

agg_Nof_trades_lst12mth <- credit_buro_df%>%group_by(No.of.trades.opened.in.last.12.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Nof_trades_lst12mth


ggplot(agg_Nof_trades_lst12mth, aes(No.of.trades.opened.in.last.12.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# 10]  No.of.PL.trades.opened.in.last.6.months

# Lets check for outliers
quantile(credit_buro_df$No.of.PL.trades.opened.in.last.6.months,
         probs = seq(0, 1, .01))
# No outliers observed
agg_Nof_pltrades_lst6mth <- credit_buro_df%>%group_by(No.of.PL.trades.opened.in.last.6.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Nof_pltrades_lst6mth


ggplot(agg_Nof_pltrades_lst6mth, aes(No.of.PL.trades.opened.in.last.6.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)


# 11]  No.of.PL.trades.opened.in.last.12.months

# Lets check for outliers
quantile(credit_buro_df$No.of.PL.trades.opened.in.last.12.months,
         probs = seq(0, 1, .01))
# No outliers observed
agg_Nof_pltrades_lst12mth <- credit_buro_df%>%group_by(No.of.PL.trades.opened.in.last.12.months)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Nof_pltrades_lst12mth


ggplot(agg_Nof_pltrades_lst12mth, aes(No.of.PL.trades.opened.in.last.12.months, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)



# 12]  No.of.PL.trades.opened.in.last.12.months

# Lets check for outliers
#quantile(credit_buro_df$No.of.PL.trades.opened.in.last.12.months,
#         probs = seq(0, 1, .01))
## No outliers observed
#
# 13]   No.of.PL.trades.opened.in.last.6.months
#
## Lets check for outliers
#quantile(credit_buro_df$No.of.PL.trades.opened.in.last.6.months,
#         probs = seq(0, 1, .01))
# No outliers observed

# 14]   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.

# Lets check for outliers
quantile(
  credit_buro_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,
  probs = seq(0, 1, .01)
)
# No outliers observed
agg_Nof_inq_lst6mth <- credit_buro_df%>%group_by(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Nof_inq_lst6mth 


ggplot(agg_Nof_inq_lst6mth, aes(factor(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# 15] No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

# Lets check for outliers
quantile(
  credit_buro_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
  probs = seq(0, 1, .01)
)
# clearly the value is incresing abruptly fromm 99% to 100% so lets cap these values
# Lets replace all values  >15 with 15
credit_buro_df[which(
  credit_buro_df$`No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.` >
    15
),
c("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")] <-
  15

agg_Nof_inq_lst12mth <- credit_buro_df%>%group_by(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Nof_inq_lst12mth 


ggplot(agg_Nof_inq_lst12mth, aes(factor(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)


# 16]Presence.of.open.home.loan Outstanding.Balance

# Lets check for outliers
quantile(credit_buro_df$`Presence.of.open.home.loan`,
         probs = seq(0, 1, .01))


# no outliers all data seems correct with values 0 and 1
summary(factor(credit_buro_df$Presence.of.open.home.loan))
agg_presence_hmln <- credit_buro_df%>%group_by(Presence.of.open.home.loan)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_presence_hmln 


ggplot(agg_presence_hmln, aes(factor(Presence.of.open.home.loan), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#if the applicant has homeloan then defaulter rate is quite low as compared to otherwise

# 17] Outstanding.Balance

# Lets check for outliers
quantile(credit_buro_df$`Outstanding.Balance`,
         probs = seq(0, 1, .01))

# lets plot a histogram to visualise more
ggplot(data = credit_buro_df, aes(x = Outstanding.Balance)) + geom_histogram(binwidth =
                                                                               500)
#cap at 99% since there is huge jump from 99 to 100 %
credit_buro_df[which(credit_buro_df$Outstanding.Balance > 4250657.64 ), c("Outstanding.Balance")] <- 4250657.64

ggplot(credit_buro_df, aes(x = "", y = Outstanding.Balance)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for Outstanding.Balance to check for outliers")
# No outliers observed

credit_buro_df$Out_Balance_bin=ifelse(credit_buro_df$Outstanding.Balance < 250000.00 ,"LE 2.5 lakh",
                                      ifelse(credit_buro_df$Outstanding.Balance>=250000.00 & credit_buro_df$Outstanding.Balance < 750000.00,"GE 2.5 & LT 7.5 lakh",
                                             ifelse(credit_buro_df$Outstanding.Balance>=750000.00 & credit_buro_df$Outstanding.Balance < 1250000.00,"GE 7.5 & LT 12.5 lakh",
                                                    ifelse(credit_buro_df$Outstanding.Balance>=1250000.00 & credit_buro_df$Outstanding.Balance < 1750000.00,"GE 12.5 & LT 17.5 lakh",
                                                           ifelse(credit_buro_df$Outstanding.Balance>=1750000.00 & credit_buro_df$Outstanding.Balance < 2500000.00,"GE 17.5 & LT 25 lakh",
                                                                  ifelse(credit_buro_df$Outstanding.Balance >= 2500000.00 & credit_buro_df$Outstanding.Balance < 3200000.00,"GE 25 & LT 32 lakh","GT 32 lakh"))))))

agg_Out_Balance_bin <- credit_buro_df%>%group_by(Out_Balance_bin)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_Out_Balance_bin


ggplot(agg_Out_Balance_bin, aes(Out_Balance_bin, no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#defaulters are more when the outstanding is between 7.5 and 12.5 lakh however no clear trend

# 18] Total.No.of.Trades
# Lets check for outliers
quantile(credit_buro_df$`Total.No.of.Trades`,
         probs = seq(0, 1, .01))
ggplot(credit_buro_df, aes(x = "", y = Total.No.of.Trades)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for Total.No.of.Trades to check for outliers")
# Clearly there is an abrupt increase in values from 99% to 100% from 31 to 44
# so lets cap the values >31 to 31

credit_buro_df[which(credit_buro_df$`Total.No.of.Trades` >
                       31),
               c("Total.No.of.Trades")] <-
  31
ggplot(credit_buro_df, aes(x = "", y = Total.No.of.Trades)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for Total.No.of.Trades to check for outliers")

agg_ttl_nof_trades <- credit_buro_df%>%group_by(Total.No.of.Trades)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_ttl_nof_trades


ggplot(agg_ttl_nof_trades, aes(factor(Total.No.of.Trades), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#between 9-14 there are significant defaulters above 7%

# 18] Presence.of.open.auto.loan
# Lets check for outliers
quantile(credit_buro_df$`Presence.of.open.auto.loan`,
         probs = seq(0, 1, .01))

# No Outliers and all values are of 0 and 1
summary(factor(credit_buro_df$Presence.of.open.auto.loan))
agg_presence_autoln <- credit_buro_df%>%group_by(Presence.of.open.auto.loan)%>%summarise(no_of_applicants=n(), no_of_defaulters=sum(Performance.Tag, na.rm = "TRUE"))%>%mutate(defaulter_rate=round((no_of_defaulters/no_of_applicants)*100,2))%>%arrange(desc(defaulter_rate))
agg_presence_autoln 


ggplot(agg_presence_autoln, aes(factor(Presence.of.open.auto.loan), no_of_applicants,label = defaulter_rate)) + 
  geom_bar(stat = 'identity', fill = "orange") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
#if the applicant has autoloan then defaulter rate is quite low as compared to otherwise
#to check original dataset columns for merging
demographic_df <- demographic_df[,1:12]

#colnames(demographic_df)
credit_buro_df <- credit_buro_df[,1:19]
#colnames(credit_buro_df)

onehotencoder <- function(df_orig) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    if (df_col_typ[rownm, "clmtyp"] == "factor" &
        !(toString(levels(df[, c(toString(df_col_typ[rownm, "clmnm"]))])) == "0, 1")) {
      clmn_obj <- df[toString(df_col_typ[rownm, "clmnm"])]
      dummy_matx <-
        data.frame(model.matrix( ~ . - 1, data = clmn_obj))
      # View(dummy_matx)
      dummy_matx <- dummy_matx[, c(1, 3:ncol(dummy_matx))]
      df[toString(df_col_typ[rownm, "clmnm"])] <- NULL
      df <- cbind(df, dummy_matx)
      df[toString(df_col_typ[rownm, "clmnm"])] <- NULL
    }
  }
  return(df)
}


# Lets write a funtion to calculate ROC AUC:-
plotAndGetROCAUC <- function(predicted_probs, actual_lbls) {
  library(ROCR)
  pred <- ROCR::prediction(predicted_probs, actual_lbls)
  perf <-
    ROCR::performance(pred, measure = 'tpr', x.measure = 'fpr')
  plot(perf)
  abline(a = 0, b = 1)
  auc.z.01 <- ROCR::performance(pred, "auc")
  legend("bottomright",
         paste(round(
           as.numeric(auc.z.01@y.values), digits = 2
         )),
         col = c("green"),
         pch = c(3))
}

perform_fn <- function(cutoff, predicted, actual)
{
  test_pred <-
    factor(ifelse(predicted >= cutoff, "Yes", "No"))
  conf <-
    confusionMatrix(test_pred, actual, positive = "Yes")
  acc <- conf$overall['Accuracy']
  sens <- conf$byClass["Sensitivity"]
  spec <- conf$byClass["Specificity"]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


plot_cutoff <- function(predicted, actual) {
  s = seq(.01, .80, length = 100)
  
  OUT = matrix(0, 100, 3)
  
  for (i in 1:100)
  {
    OUT[i, ] = perform_fn(s[i], predicted, actual)
  }
  plot(
    s,
    OUT[, 1],
    xlab = "Cutoff",
    ylab = "Value",
    cex.lab = 1.5,
    cex.axis = 1.5,
    ylim = c(0, 1),
    type = "l",
    lwd = 2,
    axes = FALSE,
    col = 2
  )
  
  axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
  axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
  lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
  lines(s, OUT[, 3], col = 4, lwd = 2)
  box()
  legend(
    0,
    .50,
    col = c(2, "darkgreen", 4, "darkred"),
    lwd = c(2, 2, 2, 2),
    c("Sensitivity", "Specificity", "Accuracy")
  )
}



##########################################################################################################
#                   Start Modelling on demographic dataset 
##########################################################################################################

# View( demographic_df)
demographic_df_enc <- onehotencoder(demographic_df)
#View(demographic_df_enc)
str(demographic_df_enc)
# Lets remove _imp variables
#demographic_df_enc$Marital.Status..at.the.time.of.application._imp <-
#  NULL
#demographic_df_enc$No.of.dependents_imp <- NULL
#demographic_df_enc$Gender_imp <- NULL
#demographic_df_enc$Type.of.residence_imp <- NULL
#demographic_df_enc$Education_imp <- NULL
#demographic_df_enc$Profession_imp <- NULL


demographic_df_enc$Performance.Tag <-
  factor(demographic_df_enc$Performance.Tag)

# Lets split our data set into test and train set
# Lets remove the records with Performance.Tag.x as null
demographic_df_validation_nonHolders <-
  demographic_df_enc[which(is.na(demographic_df_enc$Performance.Tag)), ]
#View(demog_credit_df_enc_test_nonHolders)
#nrow(demog_credit_df_enc_test_nonHolders)
demographic_df_validation_nonHolders$Application.ID <- NULL

demog_df_enc_train_holders <-
  demographic_df_enc[which(!is.na(demographic_df_enc$Performance.Tag)), ]
#View(demog_credit_df_enc_train_holders)


set.seed(123)
split_indices <-
  sample.split(demog_df_enc_train_holders$Performance.Tag,
               SplitRatio = 0.70)

demog_df_enc_train <-
  demog_df_enc_train_holders[split_indices,]
# Lets store the Application.ID in a new variable to use it later while preparing Score card
Application.ID.train <- demog_df_enc_train["Application.ID"]
# Lets remove the unwanted columns for Logistic regression
demog_df_enc_train["Application.ID"] <- NULL
nrow(demog_df_enc_train)

demog_df_enc_test <-
  demog_df_enc_train_holders[!split_indices,]
# Lets store the Application.ID in a new variable to use it later while preparing Score card
Application.ID.test <- demog_df_enc_test["Application.ID"]
demog_df_enc_test["Performance.Tag.x"] <- NULL
demog_df_enc_test["Application.ID"] <- NULL
nrow(demog_df_enc_test)
# View(demog_credit_df_enc_test)

(count(demog_df_enc_test[which(demog_df_enc_test$Performance.Tag ==
                                1),]) / count(demog_df_enc_train)) * 100
#1.8
count(demog_df_enc_test[which(demog_df_enc_test$Performance.Tag ==
                                1),])
#884
count(demog_df_enc_test[which(demog_df_enc_test$Performance.Tag ==
                                0),])
# 20077

# before modelling lets check if the dataset is balanced or not
count(demog_df_enc_train[which(demog_df_enc_train$Performance.Tag ==
                                 1),]) / count(demog_df_enc_train) * 100
#4:1
count(demog_df_enc_train[which(demog_df_enc_train$Performance.Tag ==
                                   1),])
#2064
count(demog_df_enc_train[which(demog_df_enc_train$Performance.Tag ==
                                   0),])
# 46845


# clearly the data set is unbalanced as total number of '1' are in minority ,i.e. only 4%
# Lets over sample these to make it above 10% using smote:-
# we will use mlr package for the same:-
trainTask_demog <-
mlr::makeClassifTask(data = demog_df_enc_train,
                  target = "Performance.Tag",
                  positive = "1")
testTask_demog <-
  mlr:: makeClassifTask(data = demog_df_enc_test,
                  target = "Performance.Tag",
                  positive = "1")
set.seed(123)
trainTask_smote_data_demog <- mlr::smote(trainTask_demog, rate = 3, nn = 5)
table(getTaskTargets(trainTask_smote_data_demog))

smote_data_demog <- trainTask_smote_data_demog$env$data


logit_demog_1 <-
  glm(Performance.Tag ~ ., data = smote_data_demog, family = 'binomial')
summary(logit_demog_1)


# Lets the ROC AUC and accurary of this model
test_demog_pred <-
  predict(logit_demog_1, newdata = demog_df_enc_test, type = 'response')
#View(test_pred)
summary(test_demog_pred)

test_actual_default_demog <-
  factor(ifelse(demog_df_enc_test$Performance.Tag == 1, "Yes", "No"))

# Lets find out optimal cutoff and find out sensitivity and specificity
plotAndGetROCAUC(test_demog_pred, test_actual_default_demog)
# AUC is .55

plot_cutoff(test_demog_pred, test_actual_default_demog)

# Looking at the plot we observe that the optimal cutoff is at .125
# Lets get the confusion matrix for the same

test_cutoff_pred_demog <-
  factor(ifelse(test_demog_pred >= .125, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_pred_demog, test_actual_default_demog, positive = "Yes")
conf_final
# Accuracy : 0.6231
# Sensitivity : 0.41176
# Specificity : 0.63237
#######################################################################

# Perform stepAIC to remove unwanted variables
logistic_aic <- stepAIC(logit_demog_1, direction = "both")
summary(logistic_aic)

logit_demog_2<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                     EducationOthers + ProfessionSE + Type.of.residenceLiving.with.Parents + 
                     Type.of.residenceOthers, family = "binomial", data = smote_data_demog)
vif(logit_demog_2)

# All the VIF seems good, Lets remove column Type.of.residenceLiving.with.Parents which has high p-value of .0964

logit_demog_3<-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                     EducationOthers + ProfessionSE +  
                     Type.of.residenceOthers, family = "binomial", data = smote_data_demog)
summary(logit_demog_3)
vif(logit_demog_3)


test_demog_pred <-
  predict(logit_demog_3, newdata = demog_df_enc_test, type = 'response')
#View(test_pred)
summary(test_demog_pred)

test_actual_default_demog <-
  factor(ifelse(demog_df_enc_test$Performance.Tag == 1, "Yes", "No"))

# Lets find out optimal cutoff and find out sensitivity and specificity
plotAndGetROCAUC(test_demog_pred, test_actual_default_demog)
# AUC is .55

plot_cutoff(test_demog_pred, test_actual_default_demog)

# Looking at the plot we observe that the optimal cutoff is at .125
# Lets get the confusion matrix for the same

test_cutoff_pred_demog <-
  factor(ifelse(test_demog_pred >= .125, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_pred_demog, test_actual_default_demog, positive = "Yes")
conf_final
# Accuracy : 0.6231
# Sensitivity : 0.41176
# Specificity : 0.63237




#************Lets create a decision tree and Perform CrossValidation and tune the hyperparameters*********************************
# Lets try to tune the hyper parameters of decision tree to get a efficient model
# Lets split our data set into test and train set
# Lets remove the records with Performance.Tag.x as null
#demographic_df$Marital.Status..at.the.time.of.application._imp <-
#  NULL
#demographic_df$No.of.dependents_imp <- NULL
#demographic_df$Gender_imp <- NULL
#demographic_df$Type.of.residence_imp <- NULL
#demographic_df$Education_imp <- NULL
#demographic_df$Profession_imp <- NULL
demog_df_validation_nonHolders <-
  demographic_df[which(is.na(demographic_df$Performance.Tag)), ]
# View(demog_df_validation_nonHolders)
demog_df_validation_nonHolders$Gender <-
  factor(as.character(demog_df_validation_nonHolders$Gender))
demog_df_validation_nonHolders$Marital.Status..at.the.time.of.application. <-
  factor(
    as.character(
      demog_df_validation_nonHolders$Marital.Status..at.the.time.of.application.
    )
  )
demog_df_validation_nonHolders$Education <-
  factor(as.character(demog_df_validation_nonHolders$Education))
demog_df_validation_nonHolders$Profession <-
  factor(as.character(demog_df_validation_nonHolders$Profession))
demog_df_validation_nonHolders$Type.of.residence <-
  factor(as.character(demog_df_validation_nonHolders$Type.of.residence))

demog_df_train_holders <-
  demographic_df[which(!is.na(demographic_df$Performance.Tag)), ]


set.seed(123)
split_indices <-
  sample.split(demog_df_train_holders$Performance.Tag,
               SplitRatio = 0.70)


demog_df_train <-
  demog_df_train_holders[split_indices,]


demog_df_test <-
  demog_df_train_holders[!split_indices,]

test_actual_default_demog <-
  factor(ifelse(demog_df_test$Performance.Tag == 1, "Yes", "No"))
#View(demog_df_train)
trainTask_orig_demog <-
  makeClassifTask(data = demog_df_train,
                  target = "Performance.Tag",
                  positive = "1")
testTask_orig_demog <-
  makeClassifTask(data = demog_df_test,
                  target = "Performance.Tag",
                  positive = "1")
# View(testTask_orig$env$data)

set.seed(123)
trainTask_smote_orig_data_demog <- smote(testTask_orig_demog, rate = 3, nn = 5)
table(getTaskTargets(trainTask_smote_orig_data_demog))

smote_data_orig_demog <- trainTask_smote_orig_data_demog$env$data
# View(smote_data_orig)



# Step2: Get list of hyparparameters to tune
getParamSet("classif.rpart")

# Step4: fold cross validation
set_cv <- makeResampleDesc("CV", iters = 4L)

# Step6: Make parameter set

# Step6: do a grid search
gscontrol <- makeTuneControlGrid()

makeatree_info_auc <-
  makeLearner("classif.rpart",
              predict.type = "prob",
              par.vals = list(parms = list(split = "information")))
makeatree_gini_auc <-
  makeLearner("classif.rpart",
              predict.type = "prob",
              par.vals = list(parms = list(split = "gini")))

set_cv_4fld <- makeResampleDesc("CV", iters = 4L)


gs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 3, upper = 10),
  makeIntegerParam("minbucket", lower = 5, upper = 10),
  makeNumericParam("cp", lower = 0.001, upper = 0.002),
  makeIntegerParam("maxdepth", lower = 10, upper = 30)
)

set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 150L)
# View(trainTask$env$data)
stune_info_auc <-
  tuneParams(
    learner = makeatree_info_auc,
    resampling = set_cv_4fld,
    task = trainTask_smote_orig_data_demog,
    par.set = gs,
    control = rancontrol,
    measures = auc
  )

# minsplit=9; minbucket=5; cp=0.00112; maxdepth=20 : auc.test.mean=0.7250884


t.tree.info.auc <-
  setHyperPars(makeatree_info_auc,
               par.vals = list(
                 minsplit = 9,
                 minbucket = 5,
                 cp = 0.00112,
                 maxdepth = 20
               ))
t.rpart.info.auc <-
  mlr::train(t.tree.info.auc, trainTask_smote_orig_data_demog)
getLearnerModel(t.rpart.info.auc)


#make predictions
tpmodel.info.auc <-
  predict(t.rpart.info.auc, testTask_orig_demog, type = "prob")



# Lets ROC plot and AUC
plotAndGetROCAUC(tpmodel.info.auc$data$prob.1, test_actual_default_demog)
# AUC: .62

plot_cutoff(tpmodel.info.auc$data$prob.1, test_actual_default_demog)

# Looking at the plot we observe that the optimal cutoff is at .08
# Lets get the confusion matrix for the same
# View(tpmodel.info.auc$data)
test_cutoff_demog <-
  factor(ifelse(tpmodel.info.auc$data$prob.1 >= .08, "Yes", "No"))

# Lets check confusion matrix
conf_final <-
  confusionMatrix(test_cutoff_demog, test_actual_default_demog, positive = "Yes")
conf_final
# Sensitivity : 0.42081
# Specificity : 0.63222
# Accuracy : 0.6233

# Reject Inerencing on validation data
reject_probs <-
  predict(t.rpart.info.auc$learner.model,
          demog_df_validation_nonHolders,
          type = "prob")[, 2]
# True positive to total ratio
sum(ifelse(reject_probs >= 0.07, 1, 0)) / nrow(demog_df_validation_nonHolders)
# .6350


gs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 1, upper = 50),
  makeIntegerParam("minbucket", lower = 10, upper = 30),
  makeNumericParam("cp", lower = 0.001, upper = 0.01),
  makeIntegerParam("maxdepth", lower = 10, upper = 30)
)

set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 150L)

stune_gini_auc <-
  tuneParams(
    learner = makeatree_gini_auc ,
    resampling = set_cv,
    task = trainTask_smote_orig_data_demog,
    par.set = gs,
    control = rancontrol,
    measures = auc
  )

# minsplit=44; minbucket=12; cp=0.00144; maxdepth=20 : auc.test.mean=0.7188321

t.tree.gini.auc <-
  setHyperPars(makeatree_gini_auc,
               par.vals = list(
                 minsplit = 44,
                 minbucket = 12,
                 cp = 0.00144,
                 maxdepth = 20
               ))


#train the model
t.rpart.gini.auc <-
  mlr::train(t.tree.gini.auc, trainTask_smote_orig_data_demog)
getLearnerModel(t.rpart.gini.auc)

#make predictions
tpmodel.gini.auc.predi <-
  predict(t.rpart.gini.auc, testTask_orig_demog, type = "prob")



# Lets ROC plot and AUC
plotAndGetROCAUC(tpmodel.gini.auc.predi$data$prob.1, test_actual_default_demog)
# .6 which is even less than the model with split of information

plot_cutoff(tpmodel.gini.auc.predi$data$prob.1, test_actual_default_demog)

# Looking at the plot we observe that the optimal cutoff is at .08
# Lets get the confusion matrix for the same
# View(tpmodel.gini.auc$data)
test_cutoff_gini_demog <-
  factor(ifelse(tpmodel.gini.auc.predi$data$prob.1 >= .08, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_gini_demog, test_actual_default_demog, positive = "Yes")
conf_final
#Accuracy : 0.4507
#Sensitivity : 0.69570
#Specificity : 0.43991


# Reject Inerencing on validation data
reject_probs_demog <-
  predict(t.rpart.gini.auc$learner.model,
          demog_df_validation_nonHolders,
          type = "prob")[, "1"]
nrow(reject_probs_demog)
nrow(demog_df_validation_nonHolders)
# True positive to total ratio
sum(ifelse(reject_probs_demog >= 0.07, 1, 0)) / nrow(demog_df_validation_nonHolders)
# Sensitivity: 0.79

########## Lets try out random forest ##############################
getParamSet("classif.randomForest")

#create a learner
rf <- makeLearner("classif.randomForest", predict.type = "prob")
rf$par.vals <- list(importance = TRUE)



#set tunable parameters
#grid search to find hyperparameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree", lower = 50, upper = 100),
  makeIntegerParam("mtry", lower = 5, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 25)
)

#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)


#set 3 fold cross validation
set_cv <- makeResampleDesc("CV", iters = 3L)


set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 50L)

stune_randomForest_auc <-
  tuneParams(
    learner = rf ,
    resampling = set_cv,
    task = trainTask_smote_orig_data_demog,
    par.set = rf_param,
    control = rancontrol,
    measures = auc
  )


#Result: ntree=93; mtry=5; nodesize=11 : auc.test.mean=0.8746029


#using hyperparameters for modeling
rf.tree <-
  setHyperPars(rf, par.vals = list(
    ntree = 93,
    mtry = 5,
    nodesize = 11
  ))
# ?setHyperPars

#train a models
rforest_demog <- mlr::train(rf.tree, trainTask_smote_orig_data_demog)


#make predictions
rfmodel_predict_demog <- predict(rforest_demog, testTask_orig_demog, type = "prob")

# View(rfmodel$data)

plotAndGetROCAUC(rfmodel_predict_demog$data$prob.1, test_actual_default_demog)
# AUC: 1

plot_cutoff(rfmodel_predict_demog$data$prob.1, test_actual_default_demog)

# Looking at the plot we observe that the optimal cutoff is at .167
# Lets get the confusion matrix for the same

test_cutoff_rf_demog <-
  factor(ifelse(rfmodel_predict_demog$data$prob.1 >= .167, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_rf_demog,
                  test_actual_default_demog,
                  positive = "Yes")
conf_final

# Accuracy : 0.9837
# Sensitivity : 0.98529
# Specificity : 0.98361


# Reject Inerencing on validation data
reject_probs_demog <-
  predict(rforest_demog$learner.model,
          demog_df_validation_nonHolders,
          type = "prob")[, "1"]
# True positive to total ratio
sum(ifelse(reject_probs_demog >= 0.167, 1, 0)) / nrow(demog_df_validation_nonHolders)
# Sensitivity: 0.1957895 clearly the model overfits the training data set
# Hence the model is not stable
################################################## End Modelling on demographic dataset ########################################################

# Lets join credit_buro_df and demographic_df

sapply(demographic_df, function(x)
  sum(is.na(x)))

sapply(demographic_df, function(x)
  sum(!is.na(x) & str_trim(x) == ""))

demog_credit_df <-
  dplyr::inner_join(
    demographic_df,
    credit_buro_df,
    by = "Application.ID",
    type = "inner",
    match = "all"
  )
nrow(demog_credit_df)
# Output: 70243 which seems correct

# lets check NA in our final dataframe

sapply(demog_credit_df, function(x)
  sum(is.na(x)))

sapply(demog_credit_df, function(x)
  sum(!is.na(x) & str_trim(x) == ""))

# Lets remove Performance.Tag.x




nums <- unlist(lapply(demog_credit_df, is.numeric))
demog_credit_df[, nums]

### Ranking variables using penalized IV




# Remove columns with _imp in name
#demog_credit_df <-
#  demog_credit_df[, which(!grepl("imp", colnames(demog_credit_df)))]

# Lets create some derived metrices
# 1] ratio of outstanding balance and income
demog_credit_df$outbal_inc_rtio <-
  demog_credit_df$Outstanding.Balance / (demog_credit_df$Income * 1000 * 12)

demog_credit_df$presence.of.auto.or.home.loan <-
  ifelse(
    demog_credit_df$Presence.of.open.home.loan == 1 |
      demog_credit_df$Presence.of.open.auto.loan == 1,
    1,
    0
  )

# 2] Ratio of Income and number of dependent
demog_credit_df$inc_to_noofdep_rtio <-
  demog_credit_df$Income / demog_credit_df$No.of.dependents


# Lets find out the corelation among variables and plot the same

## EDA

cor_matrix <- cor(demog_credit_df[, c(
  "Age",
  "No.of.dependents",
  "Income",
  "No.of.months.in.current.residence",
  "No.of.months.in.current.company",
  "No.of.times.90.DPD.or.worse.in.last.6.months",
  "No.of.times.60.DPD.or.worse.in.last.6.months",
  "No.of.times.30.DPD.or.worse.in.last.6.months",
  "No.of.times.90.DPD.or.worse.in.last.12.months",
  "No.of.times.60.DPD.or.worse.in.last.12.months",
  "No.of.times.30.DPD.or.worse.in.last.12.months",
  "Avgas.CC.Utilization.in.last.12.months",
  "No.of.trades.opened.in.last.6.months",
  "No.of.trades.opened.in.last.12.months",
  "No.of.PL.trades.opened.in.last.6.months",
  "No.of.PL.trades.opened.in.last.12.months",
  "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
  "Presence.of.open.home.loan",
  "Outstanding.Balance",
  "Total.No.of.Trades",
  "Presence.of.open.auto.loan",
  "outbal_inc_rtio",
  "presence.of.auto.or.home.loan",
  "inc_to_noofdep_rtio"
)])



# Lets flatten this correlation matrix
corr_df <- data.frame(matrix(ncol = 3, nrow = 625))
x <- c("attr1", "attr2", "val")
colnames(corr_df) <- x


row_idx = 1
for (rownm in colnames(cor_matrix)) {
  for (colnm
       in rownames(cor_matrix)) {
    corr_df$attr1[row_idx] <- rownm
    corr_df$attr2[row_idx] <- colnm
    corr_df$val[row_idx] <- cor_matrix[rownm, colnm]
    row_idx = row_idx + 1
  }
}


# Save the corelation matrix to csv which will be use in Tableau for visualization
# write.csv(corr_df, file = "C:\\Users\\owner\\Desktop\\upgrad\\BFS\\Capstone Project\\corrmtx.csv")
demog_credit_df$Presence.of.open.home.loan <-
  as.numeric(demog_credit_df$Presence.of.open.home.loan)
demog_credit_df$Presence.of.open.auto.loan <-
  as.numeric(demog_credit_df$Presence.of.open.auto.loan)

corr_mtx <- cor(demog_credit_df[, c(
  "Age",
  "No.of.dependents",
  "Income",
  "No.of.months.in.current.residence",
  "No.of.months.in.current.company",
  "No.of.times.90.DPD.or.worse.in.last.6.months",
  "No.of.times.60.DPD.or.worse.in.last.6.months",
  "No.of.times.30.DPD.or.worse.in.last.6.months",
  "No.of.times.90.DPD.or.worse.in.last.12.months",
  "No.of.times.60.DPD.or.worse.in.last.12.months",
  "No.of.times.30.DPD.or.worse.in.last.12.months",
  "Avgas.CC.Utilization.in.last.12.months",
  "No.of.trades.opened.in.last.6.months",
  "No.of.trades.opened.in.last.12.months",
  "No.of.PL.trades.opened.in.last.6.months",
  "No.of.PL.trades.opened.in.last.12.months",
  "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
  "Presence.of.open.home.loan",
  "Outstanding.Balance",
  "Total.No.of.Trades",
  "Presence.of.open.auto.loan",
  "outbal_inc_rtio",
  "presence.of.auto.or.home.loan",
  "inc_to_noofdep_rtio"
)])


corrplot(corr_mtx, method = "number",
         number.cex = 9 / 24)

# Lets get the columns with high mean absulute corelation and remove the same
corr_colms <-
  findCorrelation(corr_mtx,
                  verbose = TRUE,
                  cutoff = 0.85,
                  names = TRUE)
corr_colms
# document


########################
#Bi-variate analysis   #
########################
master<- master_df
master_num_col <- sapply(master, is.numeric)
master_num <- master[,master_num_col]


#Finding higly correlated features 
#highly.cor <- findCorrelation(cor(master_num), cutoff=0.85)
# 
#names(master_num[,highly.cor])

#Compute the correlation matrix
cormat <- round(cor(master_num, use="na.or.complete"), digits=2)
head(cormat)

#Create the correlation heatmap with ggplot2
#The package reshape is required to melt the correlation matrix :

#library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

#The function geom_tile()[ggplot2 package] is used to visualize the correlation
#matrix :
#library(ggplot2)
ggheatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Lets remove these columns from the original data set
demog_credit_df <-
  demog_credit_df[, which(!(colnames(demog_credit_df) %in% corr_colms))]
colnames(demog_credit_df)

demog_credit_df$Presence.of.open.auto.loan <-
  as.factor(demog_credit_df$Presence.of.open.auto.loan)

# Lets write it to a csv file which will be used for Tableau Visualization
# write.csv(demog_credit_df, file = "C:\\Users\\owner\\Desktop\\upgrad\\BFS\\Capstone Project\\demog_credit_df.csv", col.names = TRUE)

demog_credit_df$Performance.Tag.y.iv <-
  ifelse(
    demog_credit_df$Performance.Tag.y == 0,
    1,
    ifelse(
      demog_credit_df$Performance.Tag.y == 1,
      0,
      demog_credit_df$Performance.Tag.y
    )
  )

IV <-
  Information::create_infotables(data = demog_credit_df[which(!is.na(demog_credit_df$Performance.Tag.y)),],
                                 y = "Performance.Tag.y.iv",
                                 parallel =
                                   FALSE)


#View(IV$Summary)
str(IV$Tables$Education)


iv_tbl <- data.frame(IV$Summary)
# Lets create a bargraph to check the IV values


iv_table_df <- iv_tbl[iv_tbl["Variable"] != "Performance.Tag.x", ]

# Lets sort by iv values
iv_table_df <- iv_table_df[order(iv_table_df$IV, decreasing = T),]

iv_table_df$Variable <- as.character(iv_table_df$Variable)
#Then turn it back into a factor with the levels in the correct order
iv_table_df$Variable <-
  factor(iv_table_df$Variable, levels = unique(iv_table_df$Variable))
#View(iv_table_df$Variable)
ggplot(iv_table_df, aes(
  x = Variable,
  y =
    IV,
  label = Variable,
  fill = IV
)) +
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# clearly looking at the IV bar plot we get the following significant variables :-
# Variable


# document
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
#Variables with strong predictive power:-
#  .	Outstanding.Balance
#.	Avgas.CC.Utilization.in.last.12.months
#Variables with Medium predictive power:-
#  .	Total.No.of.Tradess
#.	No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
#.	No.of.times.90.DPD.or.worse.in.last.6.months
#.	outbal_inc_rtio
#.	No.of.months.in.current.residence
#.	Income


Information::plot_infotables(IV, "Avgas.CC.Utilization.in.last.12.months")
Information::plot_infotables(IV, "Outstanding.Balance")
Information::plot_infotables(IV, "Total.No.of.Trades")
Information::plot_infotables(IV,
                             "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
Information::plot_infotables(IV, "No.of.times.90.DPD.or.worse.in.last.6.months")
Information::plot_infotables(IV, "outbal_inc_rtio")
Information::plot_infotables(IV, "No.of.months.in.current.residence")
Information::plot_infotables(IV, "Income")


summary(demog_credit_df)

demog_credit_woe <- cbind(demog_credit_df)



# one hot encoder function to convert categorical variable to numeric
data.frame(colnames(demog_credit_df)[1])[1, 1]


#if(levels(demog_credit_df[,c("Presence.of.open.home.loan")])== c(0,1)){
#  print("Hello")
#}
sapply(IV$Tables$Income, class)
#View(IV$Tables$Presence.of.open.auto.loan)
regexpr("B", IV$Tables$Education["Education"])

as.numeric(str_sub("[2,3]", regexpr(",", "[2,3]") + 1, (regexpr("\\]", "[2,3]") -
                                                          1)))
grepl("Age" , names(IV$Tables))
class(IV$Tables)
#View(IV$Tables["Age"][1])

#View(IV$Tables[["Age"]])
regexpr("[2,3]", "\\[")



woe_replace <- function(df_orig, IV, labelCol, keep_original=NULL) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])
    if (!(toString(colmn_nm) %in% labelCol)) {
      if (colmn_nm %in% names(IV$Tables)) {
        column_woe_df <-
          cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
        if (df_col_typ[rownm, "clmtyp"] == "factor" |
            df_col_typ[rownm, "clmtyp"] == "character") {
          df <-
            dplyr::inner_join(
              df,
              column_woe_df[, c(colmn_nm, "WOE")],
              by = colmn_nm,
              type = "inner",
              match = "all"
            )
          if (!(toString(colmn_nm) %in% keep_original)) {
            df[colmn_nm] <- NULL
          }
          colnames(df)[colnames(df) == "WOE"] <- colmn_nm
        } else if (df_col_typ[rownm, "clmtyp"] == "numeric" |
                   df_col_typ[rownm, "clmtyp"] == "integer") {
          column_woe_df$lv <- as.numeric(str_sub(
            column_woe_df[, colmn_nm],
            regexpr("\\[", column_woe_df[, colmn_nm]) + 1,
            regexpr(",", column_woe_df[, colmn_nm]) - 1
          ))
          column_woe_df$uv <- as.numeric(str_sub(
            column_woe_df[, colmn_nm],
            regexpr(",", column_woe_df[, colmn_nm]) + 1,
            regexpr("\\]", column_woe_df[, colmn_nm]) - 1
          ))
          column_woe_df[colmn_nm] <- NULL
          column_woe_df <- column_woe_df[, c("lv", "uv", "WOE")]
          colnames(df)[colnames(df) == colmn_nm] <-
            "WOE_temp2381111111111111697"
          df <-
            fuzzy_inner_join(
              df,
              column_woe_df[, c("lv", "uv", "WOE")],
              by = c(
                "WOE_temp2381111111111111697" = "lv",
                "WOE_temp2381111111111111697" = "uv"
              ),
              match_fun = list(`>=`, `<=`)
            )
          if (toString(colmn_nm) %in% keep_original) {
            colnames(df)[colnames(df) == "WOE_temp2381111111111111697"] <-
              paste(toString(colmn_nm),"_orig")
          } else{
            df["WOE_temp2381111111111111697"] <- NULL
          }
          df["lv"] <- NULL
          df["uv"] <- NULL
          colnames(df)[colnames(df) == "WOE"] <- colmn_nm
        }
      }
    }
  }
  return(df)
}







#View(demog_credit_df)

demog_credit_df_woe <-
  woe_replace(
    demog_credit_df,
    IV,
    c(
      "Performance.Tag.y",
      "Application.ID",
      "Performance.Tag.y.iv",
      "Performance.Tag.x"
    ),c("Outstanding.Balance")
  )
# View(demog_credit_df_woe[which(is.na(demog_credit_df_woe$Performance.Tag.y)),])
# View(demog_credit_df[which(is.na(demog_credit_df$Performance.Tag.y)),])
# View(demog_credit_df_woe[which(is.na(demog_credit_df_woe$Performance.Tag.y)),])
# View(demog_credit_df_woe)

# Lets write the same to a csv file for visualizations with Tableau
#write.csv(
#  demog_credit_df_woe,
#  "C:\\Users\\owner\\Desktop\\upgrad\\BFS\\Capstone Project\\demog_credit_df_woe.csv"
#)


demog_credit_df_woe_corrmtx <- cor(demog_credit_df_woe)
corrplot(demog_credit_df_woe_corrmtx)
ncol(demog_credit_df_woe)

# Lets flatten this correlation matrix
corr_df_woe <- data.frame(matrix(ncol = 3, nrow = 1024))
x <- c("attr1", "attr2", "val")
colnames(corr_df_woe) <- x


row_idx = 1
for (rownm in colnames(demog_credit_df_woe_corrmtx)) {
  for (colnm
       in rownames(demog_credit_df_woe_corrmtx)) {
    corr_df_woe$attr1[row_idx] <- rownm
    corr_df_woe$attr2[row_idx] <- colnm
    corr_df_woe$val[row_idx] <-
      demog_credit_df_woe_corrmtx[rownm, colnm]
    row_idx = row_idx + 1
  }
}

#write.csv(
#  corr_df_woe,
#  "C:\\Users\\owner\\Desktop\\upgrad\\BFS\\Capstone Project\\corr_df_woe.csv"
#)


demog_credit_df_enc <- onehotencoder(demog_credit_df)
demog_credit_df_enc$Presence.of.open.auto.loan <-
  factor(demog_credit_df_enc$Presence.of.open.auto.loan)
#View(demog_credit_df)

summary(demog_credit_df_enc)
# Lets store the Application.ID in a new variable to use it later while preparing Score card
Application.ID.complete <- demog_credit_df_enc["Application.ID"]

summary(demog_credit_df_enc)

demog_credit_df_enc$Performance.Tag.y <-
  factor(demog_credit_df_enc$Performance.Tag.y)
demog_credit_df_enc$Presence.of.open.auto.loan <-
  as.numeric(demog_credit_df_enc$Presence.of.open.auto.loan)

# Lets split our data set into test and train set
# Lets remove the records with Performance.Tag.x as null
demog_credit_df_enc_validation_nonHolders <-
  demog_credit_df_enc[which(is.na(demog_credit_df_enc$Performance.Tag.y)), ]
#View(demog_credit_df_enc_test_nonHolders)
demog_credit_df_enc_validation_nonHolders$Performance.Tag.x <- NULL
demog_credit_df_enc_validation_nonHolders$Performance.Tag.y <- NULL
demog_credit_df_enc_validation_nonHolders$Performance.Tag.y.iv <-
  NULL
demog_credit_df_enc_validation_nonHolders$Application.ID <- NULL

demog_credit_df_enc_train_holders <-
  demog_credit_df_enc[which(!is.na(demog_credit_df_enc$Performance.Tag.y)), ]
#View(demog_credit_df_enc_train_holders)


set.seed(123)
split_indices <-
  sample.split(demog_credit_df_enc_train_holders$Performance.Tag.y,
               SplitRatio = 0.70)

demog_credit_df_enc_train <-
  demog_credit_df_enc_train_holders[split_indices,]
# Lets store the Application.ID in a new variable to use it later while preparing Score card
Application.ID.train <- demog_credit_df_enc_train["Application.ID"]
# Lets remove the unwanted columns for Logistic regression
demog_credit_df_enc_train["Performance.Tag.x"] <- NULL
demog_credit_df_enc_train["Application.ID"] <- NULL
nrow(demog_credit_df_enc_train)

demog_credit_df_enc_test <-
  demog_credit_df_enc_train_holders[!split_indices,]
# Lets store the Application.ID in a new variable to use it later while preparing Score card
Application.ID.test <- demog_credit_df_enc_test["Application.ID"]
demog_credit_df_enc_test["Performance.Tag.x"] <- NULL
demog_credit_df_enc_test["Application.ID"] <- NULL
nrow(demog_credit_df_enc_test)
# View(demog_credit_df_enc_test)

count(demog_credit_df_enc_test[which(demog_credit_df_enc_test$Performance.Tag.y ==
                                       1),]) / count(demog_credit_df_enc_train) * 100
#1.8
count(demog_credit_df_enc_test[which(demog_credit_df_enc_test$Performance.Tag.y ==
                                       1),])
#2031
count(demog_credit_df_enc_test[which(demog_credit_df_enc_test$Performance.Tag.y ==
                                       0),])


# before modelling lets check if the dataset is balanced or not
count(demog_credit_df_enc_train[which(demog_credit_df_enc_train$Performance.Tag.y ==
                                        1),]) / count(demog_credit_df_enc_train) * 100
#6:1
positive <-
  count(demog_credit_df_enc_train[which(demog_credit_df_enc_train$Performance.Tag.y ==
                                          1),])
#2031
negetive <-
  count(demog_credit_df_enc_train[which(demog_credit_df_enc_train$Performance.Tag.y ==
                                          0),])

# Lets delete column Performance.Tag.y.iv
demog_credit_df_enc_train$Performance.Tag.y.iv <- NULL

# clearly the data set is unbalanced as total number of '1' are in minority ,i.e. only 4%
# Lets over sample these to make it above 10% using smote:-
# we will use mlr package for the same:-
trainTask <-
  makeClassifTask(data = demog_credit_df_enc_train,
                  target = "Performance.Tag.y",
                  positive = "1")
testTask <-
  makeClassifTask(data = demog_credit_df_enc_test,
                  target = "Performance.Tag.y",
                  positive = "1")
set.seed(123)
trainTask_smote_data <- smote(trainTask, rate = 3, nn = 5)
table(getTaskTargets(trainTask_smote_data))

smote_data <- trainTask_smote_data$env$data

nrow(smote_data)
# #View(smote_data)
# lets check if the dataset is balanced or not
count(smote_data[which(smote_data$Performance.Tag.y == 1),]) / count(smote_data) *
  100
count(smote_data[which(smote_data$Performance.Tag.y == 1),])
count(smote_data[which(smote_data$Performance.Tag.y == 0),])

# clearly the amout of minority classes have been increased from 4% to 11.6%

# Lest check for duplicates in the final smote data
sum(duplicated(smote_data))
# Output:0 so there are no duplicates


nrow(smote_data)

#*********** Logistic regression on smote data*****************
logit_1 <-
  glm(Performance.Tag.y ~ ., data = smote_data, family = 'binomial')
summary(logit_1)
# View(smote_data)

# Lets the ROC AUC and accurary of this model
test_pred <-
  predict(logit_1, newdata = demog_credit_df_enc_test, type = 'response')
#View(test_pred)
summary(test_pred)

test_actual_default <-
  factor(ifelse(demog_credit_df_enc_test$Performance.Tag.y == 1, "Yes", "No"))

# Lets find out optimal cutoff and find out sensitivity and specificity
plotAndGetROCAUC(test_pred, test_actual_default)
# AUC is .66

plot_cutoff(test_pred, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .375
# Lets get the confusion matrix for the same

test_cutoff_pred <-
  factor(ifelse(test_pred >= .120, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_pred, test_actual_default, positive = "Yes")
conf_final
# Accuracy : 0.6252
# Sensitivity : 0.58736
# Specificity : 0.62691
#######################################################################

# Perform stepAIC to remove unwanted variables
logistic_aic <- stepAIC(logit_1, direction = "both")
summary(logistic_aic)

# Outstanding balance hass high VIF of 311.619753 and is highly corelated to outbal_inc_ratio so lets remomve outbal_inc_ratio and check
logistic_2 <-
  glm(
    formula = Performance.Tag.y ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      Outstanding.Balance + Total.No.of.Trades + Marital.Status..at.the.time.of.application.Single +
      EducationOthers + EducationProfessional + ProfessionSE +
      ProfessionSE_PROF + Type.of.residenceOwned + Type.of.residenceRented,
    family = "binomial",
    data = smote_data
  )

summary(logistic_2)
vif(logistic_2)


# Type.of.residenceOwned has high VIF of 4.366043 and p-value of Type.of.residenceOwned so lets revove this variable
# Lest remove the same and check
logistic_3 <-
  glm(
    formula = Performance.Tag.y ~ Income + No.of.months.in.current.residence +
      No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      Outstanding.Balance + Total.No.of.Trades + Marital.Status..at.the.time.of.application.Single +
      EducationOthers + EducationProfessional + ProfessionSE +
      ProfessionSE_PROF + Type.of.residenceRented,
    family = "binomial",
    data = smote_data
  )

summary(logistic_3)
vif(logistic_3)

# No.of.months.in.current.residence has high Vp-value of 0.119288
# Lest remove the same and check
logistic_4 <-
  glm(
    formula = Performance.Tag.y ~ Income +
      No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      Outstanding.Balance + Total.No.of.Trades + Marital.Status..at.the.time.of.application.Single +
      EducationOthers + EducationProfessional + ProfessionSE +
      ProfessionSE_PROF + Type.of.residenceRented,
    family = "binomial",
    data = smote_data
  )

summary(logistic_4)
vif(logistic_4)

# ProfessionSE  has high p-value of 0.114142
# Lest remove the same and check
logistic_5 <-
  glm(
    formula = Performance.Tag.y ~ Income +
      No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      Outstanding.Balance + Total.No.of.Trades + Marital.Status..at.the.time.of.application.Single +
      EducationOthers + EducationProfessional +
      ProfessionSE_PROF + Type.of.residenceRented,
    family = "binomial",
    data = smote_data
  )

summary(logistic_5)
vif(logistic_5)


# EducationProfessional  has high p-value of 0.109354
# Lest remove the same and check
logistic_6 <-
  glm(
    formula = Performance.Tag.y ~ Income +
      No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      Outstanding.Balance + Total.No.of.Trades + Marital.Status..at.the.time.of.application.Single +
      EducationOthers +
      ProfessionSE_PROF + Type.of.residenceRented,
    family = "binomial",
    data = smote_data
  )

summary(logistic_6)
vif(logistic_6)


# Type.of.residenceRented  has high p-value of 0.109354
# Lest remove the same and check
logistic_7 <-
  glm(
    formula = Performance.Tag.y ~ Income +
      No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months +
      No.of.times.60.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
      Outstanding.Balance + Total.No.of.Trades + Marital.Status..at.the.time.of.application.Single +
      EducationOthers +
      ProfessionSE_PROF,
    family = "binomial",
    data = smote_data
  )

summary(logistic_7)
vif(logistic_7)

# Here most of the variables seems significant so lets consider this model aso our final model

# Here All the variables seems significant having p-value >5 so lets test ouot model
####################################################################
# Lets the ROC AUC and accurary of this model
test_pred <-
  predict(logistic_7, newdata = demog_credit_df_enc_test, type = 'response')
#View(test_pred)
summary(test_pred)

# Lets find out optimal cutoff and find out sensitivity and specificity
plotAndGetROCAUC(test_pred, test_actual_default)
# AUC is .66

plot_cutoff(test_pred, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .125
# Lets get the confusion matrix for the same

test_cutoff_prrobs <-
  factor(ifelse(test_pred >= .125, "Yes", "No"))


conf_final <-
  confusionMatrix(test_cutoff_prrobs, test_actual_default, positive = "Yes")
conf_final
# Accuracy : 0.6476
# Sensitivity : 0.58032
# Specificity : 0.65058
# Clearly the sensitivity is verry less

# Reject Inerencing
reject_probs <-
  predict(logistic_7,
          demog_credit_df_enc_validation_nonHolders,
          type = "response")
# True positive to total ratio
sum(ifelse(reject_probs >= 0.125, 1, 0)) / nrow(demog_credit_df_enc_validation_nonHolders)
# 0.9661871

#######################################################################
# as can been seen the model performace is not so good so lets try out decision tree model

#*********************End of logistic regression on original data*****************************************



# ****************** Start of decision tree on original data**********************************


# View(demog_credit_df_enc_train_dectre)

#### Lets create a Decision tree with gini index
dectree_m1 <-
  rpart(Performance.Tag.y ~ ., data = smote_data, method = "class")

prp(dectree_m1)
summary(dectree_m1)

# View(demog_credit_df_enc_train)

nrow(demog_credit_df_enc_test)
test_pred_m1 <-
  predict(dectree_m1, newdata = demog_credit_df_enc_test, type = 'prob')[, 2]
# View(test_pred)
length(test_pred)
summary(test_pred)



plotAndGetROCAUC(test_pred_m1,
                 test_actual_default)
# Here we are getting AUC=.66 hence no improvement as compared to logistic regression

plot_cutoff(test_pred_m1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .125
# Lets get the confusion matrix for the same

test_cutoff_default <-
  factor(ifelse(test_pred >= .125, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")
conf_final

# Accuracy : 0.6471
# Sensitivity : 0.58032
# Specificity : 0.65058


#### Lets create a Decision tree with information gain
dectree_m2 <-
  rpart(
    Performance.Tag.y ~ .,
    data = smote_data,
    method = "class",
    parms = list(split = "information")
  )

prp(dectree_m2)
summary(dectree_m2)


test_pred_m2 <-
  predict(dectree_m2, newdata = demog_credit_df_enc_test, type = "prob")[, "1"]
# View(test_pred)
summary(test_pred_m2)

# Lets ROC plot and AUC
plotAndGetROCAUC(test_pred_m2, test_actual_default)
# AUC is .63 which is even less than that of infomation gain


#************Lets Perform CrossValidation and tune the hyperparameters*********************************
# View(smote_data)
# Lets try to tune the hyper parameters of decision tree to get a efficient model
# Lets split our data set into test and train set
# Lets remove the records with Performance.Tag.x as null
demog_credit_df_validation_nonHolders <-
  demog_credit_df[which(is.na(demog_credit_df$Performance.Tag.y)), ]
demog_credit_df_validation_nonHolders$Performance.Tag.x <- NULL
demog_credit_df_validation_nonHolders$Performance.Tag.y <- NULL
demog_credit_df_validation_nonHolders$Performance.Tag.y.iv <- NULL
demog_credit_df_validation_nonHolders$Application.ID <- NULL

demog_credit_df_validation_nonHolders$Gender <-
  factor(as.character(demog_credit_df_validation_nonHolders$Gender))
demog_credit_df_validation_nonHolders$Marital.Status..at.the.time.of.application. <-
  factor(
    as.character(
      demog_credit_df_validation_nonHolders$Marital.Status..at.the.time.of.application.
    )
  )
demog_credit_df_validation_nonHolders$Education <-
  factor(as.character(demog_credit_df_validation_nonHolders$Education))
demog_credit_df_validation_nonHolders$Profession <-
  factor(as.character(demog_credit_df_validation_nonHolders$Profession))
demog_credit_df_validation_nonHolders$Type.of.residence <-
  factor(as.character(demog_credit_df_validation_nonHolders$Type.of.residence))
demog_credit_df_validation_nonHolders$Presence.of.open.auto.loan <-
  factor(as.character(
    demog_credit_df_validation_nonHolders$Presence.of.open.auto.loan
  ))

#View(demog_credit_df_validation_nonHolders)

demog_credit_df_train_holders <-
  demog_credit_df[which(!is.na(demog_credit_df$Performance.Tag.y)), ]
#View(demog_credit_df_enc_train_holders)
demog_credit_df_train_holders["Performance.Tag.y.iv"] <- NULL
demog_credit_df_train_holders["Performance.Tag.x"] <- NULL
demog_credit_df_train_holders["Application.ID"] <- NULL
#View(demog_credit_df_train_holders)


set.seed(123)
split_indices <-
  sample.split(demog_credit_df_train_holders$Performance.Tag.y,
               SplitRatio = 0.70)


demog_credit_df_train <-
  demog_credit_df_train_holders[split_indices,]


demog_credit_df_test <-
  demog_credit_df_train_holders[!split_indices,]

test_actual_default <-
  factor(ifelse(demog_credit_df_test$Performance.Tag.y == 1, "Yes", "No"))

trainTask_orig <-
  makeClassifTask(data = demog_credit_df_train,
                  target = "Performance.Tag.y",
                  positive = "1")
testTask_orig <-
  makeClassifTask(data = demog_credit_df_test,
                  target = "Performance.Tag.y",
                  positive = "1")
# View(testTask_orig$env$data)

set.seed(123)
trainTask_smote_orig_data <- smote(trainTask_orig, rate = 3, nn = 5)
table(getTaskTargets(trainTask_smote_orig_data))

smote_data_orig <- trainTask_smote_orig_data$env$data
# View(smote_data_orig)



# Step2: Get list of hyparparameters to tune
getParamSet("classif.rpart")

# Step4: fold cross validation
set_cv <- makeResampleDesc("CV", iters = 4L)

# Step6: Make parameter set

# Step6: do a grid search
gscontrol <- makeTuneControlGrid()

makeatree_info_auc <-
  makeLearner("classif.rpart",
              predict.type = "prob",
              par.vals = list(parms = list(split = "information")))
makeatree_gini_auc <-
  makeLearner("classif.rpart",
              predict.type = "prob",
              par.vals = list(parms = list(split = "gini")))

set_cv_4fld <- makeResampleDesc("CV", iters = 4L)


gs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 3, upper = 10),
  makeIntegerParam("minbucket", lower = 5, upper = 10),
  makeNumericParam("cp", lower = 0.001, upper = 0.002),
  makeIntegerParam("maxdepth", lower = 10, upper = 30)
)

set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 150L)
# View(trainTask$env$data)
stune_info_auc <-
  tuneParams(
    learner = makeatree_info_auc,
    resampling = set_cv_4fld,
    task = trainTask_smote_orig_data,
    par.set = gs,
    control = rancontrol,
    measures = auc
  )

# minsplit=7; minbucket=9; cp=0.001; maxdepth=26 : auc.test.mean=0.829797


t.tree.info.auc <-
  setHyperPars(makeatree_info_auc,
               par.vals = list(
                 minsplit = 7,
                 minbucket = 9,
                 cp = 0.001,
                 maxdepth = 26
               ))
t.rpart.info.auc <-
  mlr::train(t.tree.info.auc, trainTask_smote_orig_data)
getLearnerModel(t.rpart.info.auc)


#make predictions
tpmodel.info.auc <-
  predict(t.rpart.info.auc, testTask_orig, type = "prob")



# Lets ROC plot and AUC
plotAndGetROCAUC(tpmodel.info.auc$data$prob.1, test_actual_default)
# AUC: .65

plot_cutoff(tpmodel.info.auc$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .07
# Lets get the confusion matrix for the same
# View(tpmodel.info.auc$data)
test_cutoff <-
  factor(ifelse(tpmodel.info.auc$data$prob.1 >= .07, "Yes", "No"))

# Lets check confusion matrix
conf_final <-
  confusionMatrix(test_cutoff, test_actual_default, positive = "Yes")
conf_final
# Sensitivity : 0.64943
# Specificity : 0.59962
# Accuracy : 0.6017

# Reject Inerencing on validation data
reject_probs <-
  predict(t.rpart.info.auc$learner.model,
          demog_credit_df_validation_nonHolders,
          type = "prob")[, 2]
nrow(reject_probs)
nrow(demog_credit_df_validation_nonHolders)
# True positive to total ratio
sum(ifelse(reject_probs >= 0.07, 1, 0)) / nrow(demog_credit_df_validation_nonHolders)
# .99


gs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 10, upper = 30),
  makeNumericParam("cp", lower = 0.001, upper = 0.01),
  makeIntegerParam("maxdepth", lower = 10, upper = 30)
)

set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 150L)

stune_gini_auc <-
  tuneParams(
    learner = makeatree_gini_auc ,
    resampling = set_cv,
    task = trainTask_smote_orig_data,
    par.set = gs,
    control = rancontrol,
    measures = auc
  )

# minsplit=33; minbucket=24; cp=0.00102; maxdepth=26 : auc.test.mean=0.8145131

t.tree.gini.auc <-
  setHyperPars(makeatree_gini_auc,
               par.vals = list(
                 minsplit = 33,
                 minbucket = 24,
                 cp = 0.00102,
                 maxdepth = 26
               ))


#train the model
t.rpart.gini.auc <-
  mlr::train(t.tree.gini.auc, trainTask_smote_orig_data)
getLearnerModel(t.rpart.gini.auc)

#make predictions
tpmodel.gini.auc.predi <-
  predict(t.rpart.gini.auc, testTask_orig, type = "prob")



# Lets ROC plot and AUC
plotAndGetROCAUC(tpmodel.gini.auc.predi$data$prob.1, test_actual_default)
# .66 which is even less than the model with split of information

plot_cutoff(tpmodel.gini.auc.predi$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .07
# Lets get the confusion matrix for the same
# View(tpmodel.gini.auc$data)
test_cutoff_gini <-
  factor(ifelse(tpmodel.gini.auc.predi$data$prob.1 >= .07, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_gini, test_actual_default, positive = "Yes")
conf_final
#Accuracy : 0.6106
#Sensitivity : 0.64253
#Specificity : 0.60922


# Reject Inerencing on validation data
reject_probs <-
  predict(t.rpart.gini.auc$learner.model,
          demog_credit_df_validation_nonHolders,
          type = "prob")[, "1"]
nrow(reject_probs)
nrow(demog_credit_df_validation_nonHolders)
# True positive to total ratio
sum(ifelse(reject_probs >= 0.07, 1, 0)) / nrow(demog_credit_df_validation_nonHolders)
# Sensitivity: 0.9697842

########## Lets try out random forest ##############################
getParamSet("classif.randomForest")

#create a learner
rf <- makeLearner("classif.randomForest", predict.type = "prob")
rf$par.vals <- list(importance = TRUE)



#set tunable parameters
#grid search to find hyperparameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree", lower = 50, upper = 100),
  makeIntegerParam("mtry", lower = 5, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 25)
)

#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)


#set 3 fold cross validation
set_cv <- makeResampleDesc("CV", iters = 3L)


set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 50L)

stune_randomForest_auc <-
  tuneParams(
    learner = rf ,
    resampling = set_cv,
    task = trainTask_smote_orig_data,
    par.set = rf_param,
    control = rancontrol,
    measures = auc
  )


#Result: ntree=76; mtry=3; nodesize=10 : auc.test.mean=0.9164863


#using hyperparameters for modeling
rf.tree <-
  setHyperPars(rf, par.vals = list(
    ntree = 76,
    mtry = 3,
    nodesize = 10
  ))
# ?setHyperPars

#train a models
rforest <- mlr::train(rf.tree, trainTask_smote_orig_data)


#make predictions
rfmodel_predict <- predict(rforest, testTask_orig, type = "prob")

# View(rfmodel$data)

plotAndGetROCAUC(rfmodel_predict$data$prob.1, test_actual_default)
# AUC: .65

plot_cutoff(rfmodel_predict$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .05
# Lets get the confusion matrix for the same

test_cutoff_rf <-
  factor(ifelse(rfmodel_predict$data$prob.1 >= .05, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_rf,
                  test_actual_default,
                  positive = "Yes")
conf_final

# Accuracy : 0.553
# Sensitivity : 0.68736
# Specificity : 0.54705


# Reject Inerencing on validation data
reject_probs <-
  predict(rforest$learner.model,
          demog_credit_df_validation_nonHolders,
          type = "prob")[, 2]
nrow(reject_probs)
nrow(demog_credit_df_validation_nonHolders)
# True positive to total ratio
sum(ifelse(reject_probs >= 0.05, 1, 0)) / nrow(demog_credit_df_validation_nonHolders)


# Lets try to implement gradient boosting
getParamSet("classif.gbm")
g.gbm <- makeLearner("classif.gbm", predict.type = "prob")

#specify tuning method
rancontrol <- makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv <- makeResampleDesc("CV", iters = 3L)

#parameters
gbm_par <- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000),
  #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10),
  #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage", lower = 0.01, upper = 1)
)

tune_gbm <-
  tuneParams(
    learner = g.gbm,
    task = trainTask_smote_orig_data,
    resampling = set_cv,
    measures = auc,
    par.set = gbm_par,
    control = rancontrol
  )
tune_gbm
# Result:distribution=bernoulli; n.trees=523; interaction.depth=9; n.minobsinnode=34; shrinkage=0.0612 : auc.test.mean=0.9696665
# distribution=bernoulli; n.trees=883; interaction.depth=5; n.minobsinnode=38; shrinkage=0.0441 : auc.test.mean=0.8833440



# final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

final_gbm <-
  setHyperPars(
    learner = g.gbm,
    par.vals = list(
      distribution = "bernoulli",
      n.trees = 883,
      interaction.depth = 5,
      n.minobsinnode = 38,
      shrinkage = 0.0441
    )
  )

#train
to.gbm <- mlr::train(final_gbm, trainTask_smote_orig_data)

#test
pr.gbm.predic <- predict(to.gbm, testTask_orig)

plotAndGetROCAUC(pr.gbm.predic$data$prob.1, test_actual_default)
#AUC: .66

plot_cutoff(pr.gbm.predic$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .05
# Lets get the confusion matrix for the same

test_cutoff_gb <-
  factor(ifelse(pr.gbm.predic$data$prob.1 >= .05, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_gb,
                  test_actual_default,
                  positive = "Yes")
conf_final
# Accuracy : 0.5632
# Sensitivity : 0.69457 
# Specificity : 0.55743

# Reject Inerencing on validation data
reject_probs <-
  predict(
    to.gbm$learner.model,
    n.trees = 883,
    demog_credit_df_validation_nonHolders,
    type = "response"
  )
nrow(reject_probs)
nrow(demog_credit_df_validation_nonHolders)
# True positive to total ratio
sum(ifelse(reject_probs >= 0.05, 1, 0)) / nrow(demog_credit_df_validation_nonHolders)
# Sensitivity: 0.9501754


##############################################################################
#                                Models using woe
##############################################################################

#Lets store the Aplication.ID to use it later for score card preparation and remove the unwanted variables from the data frame
Application.ID.woe.complete <- demog_credit_df_woe$Application.ID

# View(demog_credit_df_woe)

demog_credit_df_woe_validation_nonHolders <-
  demog_credit_df_woe[which(is.na(demog_credit_df_woe$Performance.Tag.y)), ]
#Lets store the Aplication.ID to use it later for score card preparation and remove the unwanted variables from the data frame
Application.ID.woe.validation <-
  demog_credit_df_woe_validation_nonHolders$Application.ID
demog_credit_df_woe_validation_nonHolders$Performance.Tag.x <- NULL
demog_credit_df_woe_validation_nonHolders$Performance.Tag.y <- NULL
demog_credit_df_woe_validation_nonHolders$Performance.Tag.y.iv <-
  NULL
demog_credit_df_woe_validation_nonHolders$Application.ID <- NULL
Outstanding.Balance.validation<-demog_credit_df_woe_validation_nonHolders$`Outstanding.Balance _orig`
demog_credit_df_woe_validation_nonHolders$`Outstanding.Balance _orig` <- NULL

demog_credit_df_woe_modelling <-
  demog_credit_df_woe[which(!is.na(demog_credit_df_woe$Performance.Tag.y)), ]

# View(demog_credit_df_woe_modelling)
set.seed(123)
split_indices <-
  sample.split(demog_credit_df_woe_modelling$Performance.Tag.y,
               SplitRatio = 0.70)

demog_credit_df_enc_train <-
  demog_credit_df_woe_modelling[split_indices,]
# View(demog_credit_df_enc_train)
#Lets store the Aplication.ID to use it later for score card preparation and remove the unwanted variables from the data frame
Application.ID.woe.train <- demog_credit_df_woe$Application.ID
demog_credit_df_enc_train$Application.ID <- NULL
demog_credit_df_enc_train$Performance.Tag.y.iv <- NULL
demog_credit_df_enc_train$Performance.Tag.x <- NULL


# lets store the original outstanding balance in a variable for later use
Outstanding.Balance.train<-demog_credit_df_enc_train$`Outstanding.Balance _orig`
demog_credit_df_enc_train$`Outstanding.Balance _orig`<-NULL


demog_credit_df_enc_test <-
  demog_credit_df_woe_modelling[!split_indices,]
# View(demog_credit_df_enc_test)
#Lets store the Aplication.ID to use it later for score card preparation and remove the unwanted variables from the data frame
Application.ID.woe.test <- demog_credit_df_enc_test$Application.ID
demog_credit_df_enc_test$Application.ID <- NULL
demog_credit_df_enc_test$Performance.Tag.y.iv <- NULL
demog_credit_df_enc_test$Performance.Tag.x <- NULL

# Lets store the outstanding balance in a variable to use it later while calculating credit loss and
# than remove this column as it is not required for modelling
Outstanding.Balance.Original<-demog_credit_df_enc_test$`Outstanding.Balance _orig`
demog_credit_df_enc_test$`Outstanding.Balance _orig`<-NULL

test_actual_default <-
  factor(ifelse(demog_credit_df_enc_test$Performance.Tag.y == 1, "Yes", "No"))

# View(demog_credit_df_enc_test)
count(demog_credit_df_enc_test[which(demog_credit_df_enc_test$Performance.Tag.y ==
                                       1),]) / count(demog_credit_df_enc_test) * 100
#4.02
count(demog_credit_df_enc_test[which(demog_credit_df_enc_test$Performance.Tag.y ==
                                       1),])
#2031
count(demog_credit_df_enc_test[which(demog_credit_df_enc_test$Performance.Tag.y ==
                                       0),])
# 19490

# before modelling lets check if the dataset is balanced or not
count(demog_credit_df_enc_train[which(demog_credit_df_enc_train$Performance.Tag.y ==
                                        1),]) / count(demog_credit_df_enc_train) * 100
#4.22:1
count(demog_credit_df_enc_train[which(demog_credit_df_enc_train$Performance.Tag.y ==
                                        1),])
#2003
count(demog_credit_df_enc_train[which(demog_credit_df_enc_train$Performance.Tag.y ==
                                        0),])
#45477

# clearly the data set is unbalanced as total number of '1' are in minority ,i.e. only 4%
# Lets over sample these to make it above 10% using smote:-
# we will use mlr package for the same:-
# View(demog_credit_df_enc_train)
trainTask <-
  makeClassifTask(data = demog_credit_df_enc_train,
                  target = "Performance.Tag.y",
                  positive = "1")

testTask <-
  makeClassifTask(data = demog_credit_df_enc_test,
                  target = "Performance.Tag.y",
                  positive = "1")
set.seed(123)

# As stated above since the data is imbalanced lets balance it using smote
trainTask_smote_data <- smote(trainTask, rate = 3, nn = 5)
table(getTaskTargets(trainTask_smote_data))

smote_data <- trainTask_smote_data$env$data
# View(smote_data)
nrow(smote_data)
# #View(smote_data)
# lets check if the dataset is balanced or not
count(smote_data[which(smote_data$Performance.Tag.y == 1),]) / count(smote_data) *
  100
count(smote_data[which(smote_data$Performance.Tag.y == 1),])
count(smote_data[which(smote_data$Performance.Tag.y == 0),])

# View(smote_data)


#*********** Logistic regression on smote data*****************
logit_1 <-
  glm(Performance.Tag.y ~ ., data = smote_data, family = 'binomial')
summary(logit_1)
# View(smote_data)

# Lets the ROC AUC and accurary of this model
test_pred <-
  predict(logit_1, newdata = demog_credit_df_enc_test, type = 'response')
#View(test_pred)
summary(test_pred)

test_actual_default <-
  factor(ifelse(demog_credit_df_enc_test$Performance.Tag.y == 1, "Yes", "No"))

# Lets find out optimal cutoff and find out sensitivity and specificity
plotAndGetROCAUC(test_pred, test_actual_default)
# AUC is .67

plot_cutoff(test_pred, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .12
# Lets get the confusion matrix for the same

test_cutoff_pred <-
  factor(ifelse(test_pred >= .1, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_pred, test_actual_default, positive = "Yes")
conf_final
# Accuracy : 0.5234
#Sensitivity : 0.73624
#Specificity : 0.51399
#######################################################################

# Perform stepAIC to remove unwanted variables
logistic_aic <- stepAIC(logit_1, direction = "both")
summary(logistic_aic)

# Outstanding balance hass high VIF of 311.619753 and is highly corelated to outbal_inc_ratio so lets remomve outbal_inc_ratio and check
logistic_2 <-
  glm(
    formula = Performance.Tag.y ~ Age + No.of.dependents + Income +
      Profession + Type.of.residence + No.of.months.in.current.residence +
      No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.12.months +
      Avgas.CC.Utilization.in.last.12.months + Outstanding.Balance +
      Total.No.of.Trades + Presence.of.open.auto.loan,
    family = "binomial",
    data = smote_data
  )

summary(logistic_2)
vif(logistic_2)

# Lets remove Type.of.residence with high p value of 0.056252
logistic_3 <-
  glm(
    formula = Performance.Tag.y ~ Age + No.of.dependents + Income +
      Profession + No.of.months.in.current.residence +
      No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.12.months +
      Avgas.CC.Utilization.in.last.12.months + Outstanding.Balance +
      Total.No.of.Trades + Presence.of.open.auto.loan,
    family = "binomial",
    data = smote_data
  )

summary(logistic_3)
vif(logistic_3)

# Here most of the variables seems significant so lets consider this model aso our final model

# Here All the variables seems significant having p-value >5 so lets test ouot model
####################################################################
# Lets the ROC AUC and accurary of this model
test_pred <-
  predict(logistic_3, newdata = demog_credit_df_enc_test, type = 'response')
#View(test_pred)
summary(test_pred)

# Lets find out optimal cutoff and find out sensitivity and specificity
plotAndGetROCAUC(test_pred, test_actual_default)
# AUC is .67

plot_cutoff(test_pred, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .1
# Lets get the confusion matrix for the same

test_cutoff_pred <-
  factor(ifelse(test_pred >= .1, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_pred, test_actual_default, positive = "Yes")
conf_final
# Accuracy : 0.5356
# Sensitivity : 0.72133
# Specificity : 0.52744

# Reject Inerencing on validation data
reject_probs <-
  predict(logistic_3,
          demog_credit_df_woe_validation_nonHolders,
          type = "response")
nrow(reject_probs)
nrow(demog_credit_df_woe_validation_nonHolders)
# True positive to total ratio
sum(ifelse(reject_probs >= 0.1, 1, 0)) / nrow(demog_credit_df_woe_validation_nonHolders)
# Sensitivity: 0.9900498


#######################################################################
# as can been seen the model performace is not so good so lets try out decision tree model

#*********************End of logistic regression on woe data*****************************************



# ****************** Start of decision tree on original data**********************************


# View(demog_credit_df_enc_train_dectre)

#### Lets create a Decision tree with gini index
dectree_m1 <-
  rpart(Performance.Tag.y ~ ., data = smote_data, method = "class")
str(smote_data)
prp(dectree_m1)
summary(dectree_m1)

# View(demog_credit_df_enc_train)

nrow(demog_credit_df_enc_test)
test_pred_m1 <-
  predict(dectree_m1, newdata = demog_credit_df_enc_test, type = 'prob')[, 2]
# View(test_pred)
length(test_pred)
summary(test_pred)



plotAndGetROCAUC(test_pred_m1,
                 test_actual_default)
# Here we are getting AUC=.5 hence no improvement as compared to logistic regression

plot_cutoff(test_pred_m1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .125
# Lets get the confusion matrix for the same

test_cutoff_default <-
  factor(ifelse(test_pred >= .125, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")
conf_final

# Accuracy : 0.6096
# Sensitivity : 0.62271
# Specificity : 0.60907


#### Lets create a Decision tree with information gain
dectree_m2 <-
  rpart(
    Performance.Tag.y ~ .,
    data = smote_data,
    method = "class",
    parms = list(split = "information")
  )

prp(dectree_m2)
summary(dectree_m2)


test_pred_m2 <-
  predict(dectree_m2, newdata = demog_credit_df_enc_test, type = "prob")[, 2]
# View(test_pred)
summary(test_pred_m2)

# Lets ROC plot and AUC
plotAndGetROCAUC(test_pred_m2, test_actual_default)
# AUC is .63 which is even less than that of infomation gain


#************Lets Perform CrossValidation and tune the hyperparameters*********************************
# View(smote_data)
# Lets try to tune the hyper parameters of decision tree to get a efficient model



#Feature importance
#im_feat <-
#  generateFilterValuesData(trainTask, method = c("information.gain", "chi.squared"))
#plotFilterValues(im_feat, n.show = 20)


# Step2: Get list of hyparparameters to tune
getParamSet("classif.rpart")

# Step4: fold cross validation
set_cv <- makeResampleDesc("CV", iters = 4L)

# Step6: Make parameter set

# Step6: do a grid search
gscontrol <- makeTuneControlGrid()

makeatree_info_auc <-
  makeLearner("classif.rpart",
              predict.type = "prob",
              par.vals = list(parms = list(split = "information")))
makeatree_gini_auc <-
  makeLearner("classif.rpart",
              predict.type = "prob",
              par.vals = list(parms = list(split = "gini")))

set_cv_4fld <- makeResampleDesc("CV", iters = 4L)


# #View(smote_data)
# lets check if the dataset is balanced or not

gs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 1, upper = 3),
  makeIntegerParam("minbucket", lower = 72, upper = 100),
  makeNumericParam("cp", lower = 0.002, upper = .003),
  makeIntegerParam("maxdepth", lower = 23, upper = 30)
)

set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 500L)
# View(trainTask$env$data)
stune_info_auc <-
  tuneParams(
    learner = makeatree_info_auc,
    resampling = set_cv_4fld,
    task = trainTask_smote_data,
    par.set = gs,
    control = rancontrol,
    measures = auc
  )

# Result: minsplit=1; minbucket=73; cp=0.00228; maxdepth=23 : auc.test.mean=0.7424283
t.tree.info.auc <-
  setHyperPars(makeatree_info_auc,
               par.vals = list(
                 minsplit = 1,
                 minbucket = 73,
                 cp = 0.00228,
                 maxdepth = 23
               ))


#train the model
t.rpart.info.auc <-
  mlr::train(t.tree.info.auc, trainTask_smote_data)
getLearnerModel(t.rpart.info.auc)

#make predictions
tpmodel.info.auc <-
  predict(t.rpart.info.auc, testTask, type = "prob")



# Lets ROC plot and AUC
plotAndGetROCAUC(tpmodel.info.auc$data$prob.1, test_actual_default)
# auc: .65 which is lower than logistic regression model

plot_cutoff(tpmodel.info.auc$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .16
# Lets get the confusion matrix for the same
# View(tpmodel.info.auc$data)
test_cutoff_pred <-
  factor(ifelse(tpmodel.info.auc$data$prob.1 >= .16, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_pred, test_actual_default, positive = "Yes")
conf_final

#Sensitivity : 0.66900
#Specificity : 0.60113
#Accuracy : 0.604

# Reject Inerencing on validation data
reject_probs <-
  predict(t.rpart.info.auc$learner.model,
          demog_credit_df_woe_validation_nonHolders,
          type = "prob")[, "1"]
# True positive to total ratio
sum(ifelse(reject_probs >= 0.1, 1, 0)) / nrow(demog_credit_df_woe_validation_nonHolders)
# Sensitivity: 0.8994169

#### Decision tree using gini index

gs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 1, upper = 5),
  makeIntegerParam("minbucket", lower = 153, upper = 300),
  makeNumericParam("cp", lower = 0.001, upper = 0.002),
  makeIntegerParam("maxdepth", lower = 23, upper = 30)
)

set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 500L)

stune_randomForest_auc <-
  tuneParams(
    learner = makeatree_gini_auc ,
    resampling = set_cv,
    task = trainTask_smote_data,
    par.set = gs,
    control = rancontrol,
    measures = auc
  )
#Result: Result: minsplit=2; minbucket=183; cp=0.00128; maxdepth=29 : auc.test.mean=0.7363733
#   #hypertune the parameters



t.tree.gini.auc <-
  setHyperPars(makeatree_gini_auc,
               par.vals = list(
                 minsplit = 2,
                 minbucket = 183,
                 cp = 0.00128,
                 maxdepth = 29
               ))

t.tree.gini.auc <-
  setHyperPars(makeatree_gini_auc,
               par.vals = t.tree.gini.auc$par.vals)


#train the model
t.rpart.gini.auc <-
  mlr::train(t.tree.gini.auc, trainTask_smote_data)
getLearnerModel(t.rpart.gini.auc)

#make predictions
tpmodel.gini.auc <-
  predict(t.rpart.gini.auc, testTask, type = "prob")

# View(tpmodel.gini.auc$data)
nrow(testTask$env$data)
# Lets ROC plot and AUC
plotAndGetROCAUC(tpmodel.gini.auc$data$prob.1, test_actual_default)
# .5 which is even less than the model with split of information

plot_cutoff(tpmodel.gini.auc$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .167
# Lets get the confusion matrix for the same
# View(tpmodel.gini.auc$data)
test_cutoff_gini <-
  factor(ifelse(tpmodel.gini.auc$data$prob.1 >= .167, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_gini, test_actual_default, positive = "Yes")
conf_final
# Accuracy : 0.9578
# Sensitivity : 0.00000
# Specificity : 1.00000


# Reject Inerencing on validation data
reject_probs <-
  predict(t.rpart.gini.auc$learner.model,
          demog_credit_df_woe_validation_nonHolders,
          type = "prob")[, "1"]
# True positive to total ratio
sum(ifelse(reject_probs >= .167, 1, 0)) / nrow(demog_credit_df_woe_validation_nonHolders)
# Sensitivity: 0.8994169



########## Lets try out random forest ##############################
getParamSet("classif.randomForest")

#create a learner
rf <- makeLearner("classif.randomForest", predict.type = "prob")
rf$par.vals <- list(importance = TRUE)



#set tunable parameters
#grid search to find hyperparameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree", lower = 50, upper = 100),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 25)
)

#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)


#set 3 fold cross validation
set_cv <- makeResampleDesc("CV", iters = 3L)


set.seed(123)
#gscontrol <- makeTuneControlGrid()
rancontrol <- makeTuneControlRandom(maxit = 50L)

stune_randomForest_auc <-
  tuneParams(
    learner = rf ,
    resampling = set_cv,
    task = trainTask_smote_data,
    par.set = rf_param,
    control = rancontrol,
    measures = auc
  )

# # ntree=88; mtry=4; nodesize=10 : auc.test.mean=0.9008749



#using hyperparameters for modeling
rf.tree <-
  setHyperPars(rf, par.vals = list(
    ntree = 88,
    mtry = 4,
    nodesize = 10
  ))
# ?setHyperPars

#train a models
rforest <- mlr::train(rf.tree, trainTask_smote_data)


#make predictions
rfmodel <- predict(rforest, testTask, type = "prob")

# View(rfmodel$data)

plotAndGetROCAUC(rfmodel$data$prob.1, test_actual_default)
# AUC: .62

plot_cutoff(rfmodel$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .02
# Lets get the confusion matrix for the same

test_cutoff_rf <-
  factor(ifelse(rfmodel$data$prob.1 >= .02, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_rf,
                  test_actual_default,
                  positive = "Yes")
conf_final

#Accuracy : 0.4703
#Sensitivity : 0.70396
#Specificity : 0.45998
# Clearly accuracy is more but sensitivity is very less


# Reject Inerencing on validation data
reject_probs <-
  predict(rforest$learner.model,
          demog_credit_df_woe_validation_nonHolders,
          type = "prob")[, "1"]
# True positive to total ratio
sum(ifelse(reject_probs >= 0.02, 1, 0)) / nrow(demog_credit_df_woe_validation_nonHolders)
# Sensitivity: 0.9395044



# Lets try to implement gradient boosting
getParamSet("classif.gbm")
g.gbm <- makeLearner("classif.gbm", predict.type = "prob")

#specify tuning method
rancontrol <- makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv <- makeResampleDesc("CV", iters = 3L)

#parameters
gbm_par <- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 5, upper = 1000),
  #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10),
  #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 5, upper = 80),
  makeNumericParam("shrinkage", lower = 0.0001, upper = .001)
)

tune_gbm <-
  tuneParams(
    learner = g.gbm,
    task = trainTask,
    resampling = set_cv,
    measures = auc,
    par.set = gbm_par,
    control = rancontrol
  )
# Result: distribution=bernoulli; n.trees=475; interaction.depth=10; n.minobsinnode=21; shrinkage=0.0007 : auc.test.mean=0.6748559

final_gbm <-
  setHyperPars(
    learner = g.gbm,
    par.vals = list(
      distribution = "bernoulli",
      n.trees = 475,
      interaction.depth = 10,
      n.minobsinnode = 21,
      shrinkage = 0.0007
    )
  )

#train
to.gbm <- mlr::train(final_gbm, trainTask)

#test
pr.gbm <- predict(to.gbm, testTask)

plotAndGetROCAUC(pr.gbm$data$prob.1, test_actual_default)
#AUC: .68

plot_cutoff(pr.gbm$data$prob.1, test_actual_default)

# Looking at the plot we observe that the optimal cutoff is at .05
# Lets get the confusion matrix for the same

test_cutoff_rf <-
  factor(ifelse(pr.gbm$data$prob.1 >= .04, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_rf,
                  test_actual_default,
                  positive = "Yes")
conf_final
# Sensitivity : 0.73193
# Specificity : 0.54274
# Accuracy : 0.5507


# Reject Inerencing on validation data
reject_probs <-
  predict(
    to.gbm$learner.model,
    n.tree = 475,
    demog_credit_df_woe_validation_nonHolders,
    type = "response"
  )
# True positive to total ratio
sum(ifelse(reject_probs >= 0.04, 1, 0)) / nrow(demog_credit_df_woe_validation_nonHolders)
# Sensitivity: 0.9970845


################ Application ScoreCard ##################
#########################################################################################################
#                            Application Scorecard For Logistic Regression Model                        #
#########################################################################################################
# By  analysing the AUC and sensitivity as well as sensitivity of validation set of various models we conclude that
# 'Logistic  Regression' on data containing woe has best performance as :-
# AUC: 0.67
# Acuracy: 0.534
# Specificity:0.52519
# Sensitivity: 0.7331
# Sensitivity on Validation set: 0.9978134

# So lets use the ouput of this model to calculate the score card on complete data

test_pred_final <-
  predict(logistic_3, newdata = demog_credit_df_enc_test, type = 'response')
nrow(demog_credit_df_enc_test)
nrow(Application.ID.woe.test)
score_card_holders <- data.frame(default_prob = test_pred_final)


# Creating Predict Non Default Column
score_card_holders$NonDefault_prob <-
  1 - score_card_holders$default_prob
# Creating odds Column
score_card_holders$odds <-
  log(score_card_holders$NonDefault_prob / score_card_holders$default_prob)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))

# In our case, PDO = 20, Base Score=400 & odds = 10
Factor = 20 / log(2)   #28.8539

Offset = 400 - (28.8539 * log(10))
#333.5614
score_card_holders$Score = 333.5614 + (28.8539 * score_card_holders$odds)
score_card_holders$Application.ID <- Application.ID.woe.test
# View(score_card_holders$Score)

# Lets create a funtion to plot the scores against sensitivity , Specificity and accuracy:-
plot_cutoff_scores <- function(scores, actual) {
  max_score <- max(scores)
  min_score <- min(scores)
  s = seq(max_score, min_score, length = 100)
  OUT = matrix(0, 100, 3)
  
  for (i in 1:100)
  {
    OUT[i, ] = perform_fn(s[i], scores, actual)
  }
  plot(
    s,
    OUT[, 1],
    xlab = "Cutoff",
    ylab = "Value",
    cex.lab = 1.5,
    cex.axis = 1.5,
    ylim = c(0, 1),
    type = "l",
    lwd = 2,
    axes = FALSE,
    col = 2
  )
  
  
  axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
  axis(
    1,
    seq(min_score, max_score, length = 5),
    seq(min_score, max_score, length = 5),
    cex.lab = 1
  )
  
  lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
  lines(s, OUT[, 3], col = 4, lwd = 2)
  box()
  legend(
    min_score,
    0.5,
    col = c(2, "darkgreen", 4, "darkred"),
    lwd = c(2, 2, 2, 2),
    c("Sensitivity", "Specificity", "Accuracy")
  )
  
}

# since less is the score less is the likelyhood of being default
# Hence lets reverse  the actual  results to get a proper plot and call plot_cutoff_scores

test_actual_default_for_score <-
  factor(ifelse(test_actual_default == "Yes", "No", "Yes"))
plot_cutoff_scores(score_card_holders$Score, test_actual_default_for_score)

# Looking at the plot we observe that the ideal cutoff score is 394
# Lets validate the same against out validation set

# Reject Inerencing on validation data
reject_probs_score <-
  predict(logistic_3,
          demog_credit_df_woe_validation_nonHolders,
          type = "response")


score_card_holders_validation <-
  data.frame(default_prob = reject_probs_score)


# Creating Predict Non Default Column
score_card_holders_validation$NonDefault_prob <-
  1 - score_card_holders_validation$default_prob
# Creating odds Column
score_card_holders_validation$odds <-
  log(
    score_card_holders_validation$NonDefault_prob / score_card_holders_validation$default_prob
  )

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))

# In our case, PDO = 20, Base Score=400 & odds = 10
Factor = 20 / log(2)   
#28.8539

Offset = 400 - (28.8539 * log(10))
#333.5614
score_card_holders_validation$Score = 333.5614 + (28.8539 * score_card_holders_validation$odds)
score_card_holders_validation$Application.ID <-
  Application.ID.woe.validation
# View(score_card_holders_validation$Score)

# True positive to total ratio
sum(ifelse(score_card_holders_validation$Score <= 394, 1, 0)) / nrow(demog_credit_df_woe_validation_nonHolders)
# Sensitivity: 0.9970845
# Hence the cut-off score below which you would not grant credit cards to applicants is 394


predicted_default <- if_else(score_card_holders$Score <=394, 1, 0)
score_card_holders$predicted_default<-predicted_default
actual_default<-if_else(test_actual_default=="Yes",1,0)
result <- data.frame(score=score_card_holders$Score,predicted_default, actual_default,Outstanding.Balance=Outstanding.Balance.Original,Application.ID=score_card_holders$Application.ID)

#View(result)
# nrow(result[which(result$predicted_default==result$actual_default & result$predicted_default==1),])

########################################################################################
# Lets find out the business implications of using this model using KS statistics
########################################################################################

KS_statistics <- data.frame("deciles" = 1:10)
# View(KS_statistics)

nrow(result) / 10
# Output: 2034.8

# View(result)

# Step 1: arrage the result by decreasing probability of churning
result <- result[order(result$score),]
# View(result)

# Step 2:Lets divide the observations into 10 parts and take cumulative churns and
# assign decile to dataframe
for (i in 1:nrow(result)) {
  if ((i %% 2035) != 0) {
    result$decile[i] <- floor((i / 2035) + 1)
  } else{
    result$decile[i] <- floor(i / 2035)
  }
  result$index[i] <- i
}

# View(result)

result_decile_group <- group_by(result, decile)
result_decile <-
  summarise(
    result_decile_group,
    defaulters = sum(actual_default),
    nondefaulters = sum(ifelse(actual_default == 0, 1, 0))
  )
# Lets group by decile and calculate

# View(result_decile)
result_decile$cum_defaulters <- cumsum(result_decile$defaulters)
result_decile$cum_nondefaulters <- cumsum(result_decile$nondefaulters)

# View(result_decile)
result_decile$Observations <- 2035
result_decile$Observations[10] <- 2033
KS_statistics <- result_decile

KS_statistics$percentcum_defaulters <-
  (KS_statistics$cum_defaulters / max(KS_statistics$cum_defaulters)) * 100
KS_statistics$percentcum_nondefaulters <-
  (KS_statistics$cum_nondefaulters / max(KS_statistics$cum_nondefaulters)) * 100

KS_statistics$ks_value <-
  (KS_statistics$percentcum_defaulters - KS_statistics$percentcum_nondefaulters)

# View(KS_statistics)
tbl <- tableGrob(KS_statistics)
find_cell <- function(table, row, col, name = "core-bg") {
  l <- table$layout
  which(l$t == row & l$l == col & l$name == name)
}


for (i in 2:10) {
  ind <- find_cell(tbl, 6, i, "core-bg")
  tbl$grobs[ind][[1]][["gp"]] <-
    gpar(fill = "darkolivegreen1", col = "darkolivegreen4", lwd = 5)
}


grid.arrange(tbl)


# View(KS_statistics)
# Hence taking cutoff at 48.9 of of defaulters the model will predict 74.7 percent of people will default in top 50% of population
ncol(KS_statistics)
KS_statistics_gain_chart<-rbind(c(0,0,0,0,0,0,0,0,0,0),KS_statistics)
# Lets plot gain and lift chart
ggplot(KS_statistics_gain_chart) + 
  geom_line(aes(x=KS_statistics_gain_chart$decile*10,y=KS_statistics_gain_chart$percentcum_defaulters,colour="DevelopedModel")) +
  geom_line(aes(x=KS_statistics_gain_chart$decile*10,y=KS_statistics_gain_chart$decile*10,colour="RandomModel")) +
  scale_colour_manual("", 
                      breaks = c("DevelopedModel", "RandomModel"),
                      values = c("DevelopedModel"="red","RandomModel"= "blue"))+
  xlab("decile") +
  ylab("Gain") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  geom_hline(yintercept=74.7,linetype="dashed", color = "green") +
  geom_vline(xintercept=50,linetype="dashed", color = "green") 
  

#########################################################################################
#               Calculation of Financial benefit
#########################################################################################
# Lets calculate the total credit loss avoided from default model
sum(as.numeric(result[which(result$actual_default==1),"Outstanding.Balance"]))
# Total Revenue Loss:  1431403302$

# Lets calculate revenue save and loss on validation data set
nrow(score_card_holders_validation)
predicted_default_validation <- if_else(score_card_holders_validation$Score <=394, 1, 0)
length(predicted_default_validation)
result_validation <- data.frame(score=score_card_holders_validation$Score,predicted_default_validation, actual_default=1,Outstanding.Balance=Outstanding.Balance.validation,Application.ID=score_card_holders_validation$Application.ID)


#Lets calculate total credit loss saved by our model -------------------------------------@1
sum(as.numeric(result_validation[which(result_validation$predicted_default_validation==1 & result_validation$actual_default==1),"Outstanding.Balance"]))
# 1410532664 $

#Lets calculate total credit loss due to false negetives by our model --------------------@2
sum(as.numeric(result_validation[which(result_validation$predicted_default_validation==0 & result_validation$actual_default==1),"Outstanding.Balance"]))
# 20870638 $

# Hence Total Credit Loss Covered = @1 - @2 = 1410532664 - 20870638 = 1389662026----------------------@3


# View(result)
#Lets calculate total credit loss saved by our model -------------------------------------@4
sum(as.numeric(result[which(result$predicted_default==1 & result$actual_default==1),"Outstanding.Balance"]))
# 756643196 $


#Lets calculate total credit loss due to false negetives by our model --------------------@2
sum(as.numeric(result[which(result$predicted_default==0 & result$actual_default==1),"Outstanding.Balance"]))
# 351763531 $

#Lets calculate total credit loss due to false positive by our model
# Here lests assume that the bank earns an average of 15% from the lended amount---------------------@3
sum(as.numeric(result[which(result$predicted_default==1 & result$actual_default==0),"Outstanding.Balance"]))*.10
# 1613299416 $


# Total revenue saved= @3+@4-@2-@3 =1389662026 + 756643196 - 351763531 - 1613299416




