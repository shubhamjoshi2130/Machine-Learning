#setwd(
#  "C:\\Users\\SnehaChubz\\Desktop\\UpGrad\\Assignment III Hr analytics\\PA-I_Case_Study_HR_Analytics"
#)
#setwd("~/Desktop/hrStudy/")
# import required packages
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(MASS)
library(car)
library(gridExtra)
library(caret)
library(corrplot)
library(e1071)
library(caTools)

# Import employee_survey_data
employee_survey_data <- read.csv("employee_survey_data.csv")
nrow(employee_survey_data)

# Data Cleaning for employee_survey_data
#1 check for NA values
sum(is.na(employee_survey_data))
# as we can see there are several NA values
# To fill these NA values lets take average of the variable with repect to other two values
# A] To fill NA for EnvironmentSatisfaction lets prepare take average of EnvironmentSatisfaction
#    for corresponding values of    WorkLifeBalance and JobSatisfaction
# B] To fill NA for WorkLifeBalance  lets prepare take average of WorkLifeBalance
#    for corresponding values of   EnvironmentSatisfaction  and JobSatisfaction
# C] To fill NA for JobSatisfaction  lets prepare take average of JobSatisfaction
#    for corresponding values of    WorkLifeBalance and EnvironmentSatisfaction

# Filling NA values for EnvironmentSatisfaction
for (i in 1:nrow(employee_survey_data)) {
  if (is.na(employee_survey_data[i, c("EnvironmentSatisfaction")])) {
    JobSatisfaction = employee_survey_data$JobSatisfaction[i]
    WorkLifeBalance = employee_survey_data$WorkLifeBalance[i]
    employee_survey_data$EnvironmentSatisfaction[i] <-
      round(mean(employee_survey_data$EnvironmentSatisfaction[which(
        employee_survey_data$JobSatisfaction == JobSatisfaction &
          employee_survey_data$WorkLifeBalance == WorkLifeBalance
      )], na.rm = TRUE))
    
  }
}


# Filling NA values for JobSatisfaction
for (i in 1:nrow(employee_survey_data)) {
  if (is.na(employee_survey_data[i, c("JobSatisfaction")])) {
    EnvironmentSatisfaction = employee_survey_data$EnvironmentSatisfaction[i]
    WorkLifeBalance = employee_survey_data$WorkLifeBalance[i]
    
    employee_survey_data$JobSatisfaction[i] <-
      round(mean(employee_survey_data$JobSatisfaction[which(
        employee_survey_data$EnvironmentSatisfaction == EnvironmentSatisfaction &
          employee_survey_data$WorkLifeBalance == WorkLifeBalance
      )], na.rm = TRUE))
  }
}


# Filling NA values for EnvironmentSatisfaction
for (i in 1:nrow(employee_survey_data)) {
  if (is.na(employee_survey_data[i, c("WorkLifeBalance")])) {
    JobSatisfaction = employee_survey_data$JobSatisfaction[i]
    EnvironmentSatisfaction = employee_survey_data$EnvironmentSatisfaction[i]
    
    employee_survey_data$WorkLifeBalance[i] <-
      round(mean(employee_survey_data$WorkLifeBalance[which(
        employee_survey_data$JobSatisfaction == JobSatisfaction &
          employee_survey_data$EnvironmentSatisfaction == EnvironmentSatisfaction
      )], na.rm = TRUE))
    
  }
}

# View(employee_survey_data)
# Lets take a record count


# check for NA to validate our conversion
sum(is.na(employee_survey_data))


# Import general_data
general_data <- read.csv("general_data.csv")
str(general_data)
nrow(general_data)
# Output : 4410

# View(general_data)


# check for NA values
is.na(general_data)
sum(is.na(general_data))
# Output : 28 hence there are toal 28 NA values
# Lets check column wise NA Values and print the column names
col_names <- colnames(general_data)
for (i in 1:ncol(general_data)) {
  if (sum(is.na(general_data[, i])) > 0) {
    print(col_names[i])
  }
}
# Output :-
# NumCompaniesWorked,TotalWorkingYears

# Now lets plot a graph and check if there is any relationship between NumCompaniesWorked,TotalWorkingYears

ggplot(data = general_data, aes(x = NumCompaniesWorked, y = TotalWorkingYears)) + geom_point()

sum(is.na(general_data))

# Extract all numeric fields in general_data_numeric
general_data_numeric <-
  general_data[, c(
    "Age",
    "DistanceFromHome",
    "Education",
    "EmployeeCount",
    "EmployeeID",
    "JobLevel",
    "MonthlyIncome",
    "NumCompaniesWorked",
    "PercentSalaryHike",
    "StandardHours",
    "StockOptionLevel",
    "TotalWorkingYears",
    "TrainingTimesLastYear",
    "YearsAtCompany",
    "YearsSinceLastPromotion",
    "YearsWithCurrManager"
  )]
# Now lets prepare a correlation matrix and check if any of the values are dependent
cor(general_data_numeric)
# Omit NA
general_data_numeric <- na.omit(general_data_numeric)
# Correlation between numeric variables

corrplot(cor(general_data_numeric))

# Now as seen from correlation matrix Age and total working years are highly correlated
# so lets create a linear regression on the same to fill up NA values for TotalWorkingYears
ggplot(data = general_data_numeric, aes(x = Age, y = TotalWorkingYears)) + geom_point()
# clearly from the plot we can figure out that linear regression cannot be implemented
# So lets fill the NA of TotalWorkingYears by average of TotalWorkingYears of persons of same age
# Filling NA values for TotalWorkingYears
for (i in 1:nrow(general_data)) {
  if (is.na(general_data[i, c("TotalWorkingYears")])) {
    Age = general_data$Age[i]
    general_data$TotalWorkingYears[i] <-
      round(mean(general_data$TotalWorkingYears[which(general_data$Age == Age)], na.rm = TRUE))
    
  }
}

# Lets check the number of NAs now:-
sum(is.na(general_data))
# Output : 19

# Now since NumCompaniesWorked is not coreelated to any of the numeric fields lets remove the records
# which contains NumCompaniesWorked as NS
general_data <- na.omit(general_data)

# Lets finally check for NA records
sum(is.na(general_data))
# Output :0 hence no NA records

# Lets Merge employee_survey_data and general_data data frame

HR_data <-
  merge(x = general_data,
        y = employee_survey_data,
        by = "EmployeeID",
        all.x = TRUE)

# Import manager_survey_data data
manager_survey_data <- read.csv("manager_survey_data.csv")

# Lets check for NA values in manager_survey_data
sum(is.na(manager_survey_data))
# There are no NA so lets merge the data with our master data frame HR_data

HR_data <-
  merge(x = HR_data,
        y = manager_survey_data,
        by = "EmployeeID",
        all.x = TRUE)

# Import in_time data and out_time data
in_time <- read.csv("in_time.csv")
# View(head(in_time))
nrow(in_time)
# Output: 4410

out_time <- read.csv("out_time.csv")
nrow(out_time)
# Output: 4410

# since count of in_time and out_time are equal we can evaluate them in same loop

# Now lets used this data to calculate the following:-
# 1] Average working hours per week
# 2] Leaves taken in 2015
# Lets create a copy of in_time data frame to fill it up with number of working hours, with same
# column names as original
working_hours <- cbind(in_time)
in_time_colname <- colnames(in_time)
colnames(working_hours)[1] <- "EmployeeID"
for (i in 2:ncol(in_time)) {
  working_hours[, i] <-
    as.numeric(difftime(
      as.POSIXct(out_time[, i], format = "%Y-%m-%d %H:%M:%OS"),
      as.POSIXct(in_time[, i], format = "%Y-%m-%d %H:%M:%OS"),
      units = c("hours")
    ))
  working_hours[which(is.na(working_hours[, i])), i] <- 0
}

# View(head(working_hours))
# Now we have the week of the year wise working hours in working_hours
# Lets convert the same to long format
working_hours_long <- melt(working_hours, id.vars = c("EmployeeID"))
# View(working_hours_long)
colnames(working_hours_long)[colnames(working_hours_long) == "variable"] <-
  "date"
colnames(working_hours_long)[colnames(working_hours_long) == "value"] <-
  "work_time"

# Convert working_hours to fetch week of the year
working_hours_long$week_of_year <-
  as.numeric(strftime(
    as.POSIXct(working_hours_long$date, format = "X%Y.%m.%d"),
    format = "%V"
  ))


working_hours_grpby_empid_weekofyer <-
  group_by(working_hours_long, EmployeeID, week_of_year)


wrk_hrs_weekly_sum <-
  summarise(
    working_hours_grpby_empid_weekofyer,
    weekly_working_time = sum(work_time),
    leavesTaken = sum(ifelse(work_time == 0, 1, 0))
  )

# View(head(wrk_hrs_weekly_sum))

# Lets now group by EmployeeID and find out average weekely working hour for each employee
avg_working_hours_grpby_empid <-
  group_by(wrk_hrs_weekly_sum, EmployeeID)

avg_working_per_week <-
  summarise(
    avg_working_hours_grpby_empid,
    weekly_avg_wrk_hrs = mean(weekly_working_time),
    leavesTaken_yearly = sum(leavesTaken)
  )

# View(avg_working_per_week)


# Merging avg_working_per_week with HR_data DF
HR_data <- merge(x = HR_data,
                 y = avg_working_per_week,
                 by = "EmployeeID",
                 all.x = TRUE)

##################### OUtlier Treatment start***********************************************************
# View(HR_data)

# COnversion of Categorical variables into dummy variables
str(HR_data)
# Lets check for typo errors in categorical variables having factor levels>2
levels(HR_data$JobRole)
levels(HR_data$BusinessTravel)
levels(HR_data$MaritalStatus)
levels(HR_data$Over18)

# Lets check for outliers in numeric variables
# 1] Outliers for Age
quantile(HR_data$Age, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = Age)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for Age to check for outliers")
# Data is evenly distributed and no outliers observed

# 2] Outliers in DistanceFromHome
quantile(HR_data$DistanceFromHome, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = DistanceFromHome)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for DistanceFromHome to check for outliers")
# Data is evenly distributed and no outliers observed

# 3] Outliers in EmployeeCount
quantile(HR_data$EmployeeCount, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = EmployeeCount)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for EmployeeCount to check for outliers")
# Data is evenly distributed and no outliers observed

# 4] Outliers in MonthlyIncome
quantile(HR_data$MonthlyIncome, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = MonthlyIncome)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for MonthlyIncome to check for outliers")
# There is an abrupt change from 93% to 94% , but since the data is significant above it i.e. 7% we
# can leave it as is

# 5]  Outliers in NumCompaniesWorked
quantile(HR_data$NumCompaniesWorked, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = NumCompaniesWorked)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for NumCompaniesWorked to check for outliers")
# no outliers observed

# 5]  Outliers in PercentSalaryHike
quantile(HR_data$PercentSalaryHike, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = PercentSalaryHike)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for PercentSalaryHike to check for outliers")
# no outliers observed

# 6]  Outliers in TotalWorkingYears
quantile(HR_data$TotalWorkingYears, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = TotalWorkingYears)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for TotalWorkingYears to check for outliers")
# from 98% to 99% it is clearly oberved that the values are increasing abruptly,i.e. from 32 to 35
# Hence values above 32 can be treated as outlier and can be replaced with 32
HR_data$TotalWorkingYears[which(HR_data$TotalWorkingYears > 32)] <-
  32
# Lets plot and check again
ggplot(HR_data, aes(x = "", y = TotalWorkingYears)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for TotalWorkingYears to check for outliers")
# Outliers removed

# 7]  Outliers in TrainingTimesLastYear
quantile(HR_data$TrainingTimesLastYear, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = TrainingTimesLastYear)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for TrainingTimesLastYear to check for outliers")
# No OUtliers observed

# 8]  Outliers in YearsAtCompany
quantile(HR_data$YearsAtCompany, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = YearsAtCompany)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for YearsAtCompany to check for outliers")
# Clearly there is an abrupt increase in value from 98% to 99% and from 24 to 31
# so lets replace all YearsAtCompany>24 with 24
HR_data$YearsAtCompany[which(HR_data$YearsAtCompany > 24)] <- 24
# Lets check the result with box plot
ggplot(HR_data, aes(x = "", y = YearsAtCompany)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for YearsAtCompany to check for outliers")
# Outliers removed

# 9]  Outliers in YearsSinceLastPromotion
quantile(HR_data$YearsSinceLastPromotion, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = YearsSinceLastPromotion)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for YearsSinceLastPromotion to check for outliers")
# above 95% there is some increase in values but since it corresponds to 5% of total population
# it cannot be treated

# 10]  Outliers in YearsWithCurrManager
quantile(HR_data$YearsWithCurrManager, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = YearsWithCurrManager)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for YearsWithCurrManager to check for outliers")
# above 99% to 100% there is abrupt increase values from 14 to 17
# so lets replace all the values > 14 to 14
HR_data$YearsWithCurrManager[which(HR_data$YearsWithCurrManager > 14)] <-
  14
# lets verify the same by plottig the same
ggplot(HR_data, aes(x = "", y = YearsWithCurrManager)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for YearsWithCurrManager to check for outliers")
# Outlier removed

# 11]  Outliers in weekely_avg_wrk_hrs
quantile(HR_data$weekly_avg_wrk_hrs, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = weekly_avg_wrk_hrs)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for weekly_avg_wrk_hrs to check for outliers")
# No abrupt changes in values observed

# 12]  Outliers in leavesTaken_yearly
quantile(HR_data$leavesTaken_yearly, seq(0, 1, .01))
# Lets check with box plot
ggplot(HR_data, aes(x = "", y = leavesTaken_yearly)) +
  geom_boxplot() +
  geom_point(col = "red",
             position = "jitter",
             alpha = .2) +
  ggtitle("Box plot for leavesTaken_yearly to check for outliers")
# No abrupt changes in values observed
##########################OUtlier Treatment end***************************************************************************

# HR_bkp<-cbind(HR_data)

# Lets find out the coorelation matrix and keep it for future use
# fetch all the numeric variables
HR_data_Corr_plot_df <-
  HR_data[, c(
    "Age",
    "DistanceFromHome",
    "MonthlyIncome",
    "NumCompaniesWorked",
    "PercentSalaryHike",
    "TotalWorkingYears",
    "TrainingTimesLastYear",
    "YearsAtCompany",
    "YearsSinceLastPromotion",
    "YearsWithCurrManager",
    "weekly_avg_wrk_hrs",
    "leavesTaken_yearly"
  )]

str(HR_data_Corr_plot_df)
corrplot(cor(HR_data_Corr_plot_df), method = "number")

# View(head(HR_data))
# Categorical variable analysis
g1 <-
  ggplot(HR_data, aes(x = BusinessTravel,fill=Attrition)) + ggtitle("BusinessTravel") + xlab("BusinessTravel") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g2 <-
  ggplot(HR_data, aes(x = Department,fill=Attrition)) + ggtitle("Department") + xlab("Department") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g3 <-
  ggplot(HR_data, aes(x = EducationField,fill=Attrition)) + ggtitle("EducationField") + xlab("EducatioField") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g4 <-
  ggplot(HR_data, aes(x = Gender,fill=Attrition)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g5 <-
  ggplot(HR_data, aes(x = EnvironmentSatisfaction,fill=Attrition)) + ggtitle("EnvironmentSatisfaction") + xlab("EnvironmentSatisfaction") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g6 <-
  ggplot(HR_data, aes(x = JobInvolvement,fill=Attrition)) + ggtitle("JobInvolvement") + xlab("JobInvolvement") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g7 <-
  ggplot(HR_data, aes(x = JobLevel,fill=Attrition)) + ggtitle("JobLevel") + xlab("JobLevel") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g8 <-
  ggplot(HR_data, aes(x = JobRole,fill=Attrition)) + ggtitle("JobRole") + xlab("JobRole") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g9 <-
  ggplot(HR_data, aes(x = JobSatisfaction,fill=Attrition)) + ggtitle("JobSatisfaction") + xlab("JobSatisfaction") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g10 <-
  ggplot(HR_data, aes(x = MaritalStatus,fill=Attrition)) + ggtitle("MaritalStatus") + xlab("MaritalStatus") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g11 <-
  ggplot(HR_data, aes(x = StockOptionLevel,fill=Attrition)) + ggtitle("") + xlab("Dependents") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g12 <-
  ggplot(HR_data, aes(x = WorkLifeBalance,fill=Attrition)) + ggtitle("WorkLifeBalance") + xlab("WorkLifeBalance") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
g13 <-
  ggplot(HR_data, aes(x = Attrition,fill=Attrition)) + ggtitle("Attrition") + xlab("Attrition") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.25) + ylab("Percentage")
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, ncol =
               2)







#create dummy variables
HR_data$Attrition <- as.factor(HR_data$Attrition)
dummy_1 <- data.frame(model.matrix(~ Attrition, data = HR_data))
dummy_1 <- dummy_1[, -1]
HR_data <- cbind(HR_data[, -3], dummy_1)
colnames(HR_data)[31] <- "Attrition"

HR_data$BusinessTravel <- as.factor(HR_data$BusinessTravel)
dummy_1 <-
  data.frame(model.matrix(~ BusinessTravel, data = HR_data))
dummy_1 <- dummy_1[, -1]
HR_data <- cbind(HR_data[, -3], dummy_1)

HR_data$Department <- as.factor(HR_data$Department)
dummy_1 <- data.frame(model.matrix(~ Department, data = HR_data))
dummy_1 <- dummy_1[, -1]
HR_data <- cbind(HR_data[, -3], dummy_1)

HR_data$EducationField <- as.factor(HR_data$EducationField)
dummy_1 <-
  data.frame(model.matrix(~ EducationField, data = HR_data))
dummy_1 <- dummy_1[, -1]
HR_data <- cbind(HR_data[, -5], dummy_1)

HR_data$Gender <- as.factor(HR_data$Gender)
dummy_1 <- data.frame(model.matrix(~ Gender, data = HR_data))
dummy_1 <- dummy_1[, -1]
HR_data <- cbind(HR_data[, -6], dummy_1)
colnames(HR_data)[37] <- "Gender"

HR_data$JobRole <- as.factor(HR_data$JobRole)
dummy_1 <- data.frame(model.matrix(~ JobRole, data = HR_data))
dummy_1 <- dummy_1[, -1]
HR_data <- cbind(HR_data[, -7], dummy_1)


HR_data$MaritalStatus <- as.factor(HR_data$MaritalStatus)
dummy_1 <-
  data.frame(model.matrix(~ MaritalStatus, data = HR_data))
dummy_1 <- dummy_1[, -1]
HR_data <- cbind(HR_data[, -7], dummy_1)

# Lets create a backup of our final data frame
HR_data_original<-cbind(HR_data)


#############################backup below#####################
HR_data<-cbind(HR_data_original)
View(head(HR_data))
# since "Over18","StandardHours","EmployeeCount" columns contains same value
# in all the rows lets remove these columns, also remove "EmployeeID" as is a granular
# field and fetch rest of the columns in HR_data_for_Model
str(HR_data)



HR_data <- HR_data[, names(HR_data) != "Over18"]
HR_data <- HR_data[, names(HR_data) != "StandardHours"]
HR_data <- HR_data[, names(HR_data) != "EmployeeCount"]


# View(head(HR_data))
# scale each numeric variable
HR_data$Age <- as.integer(HR_data$Age)
HR_data$Age <- scale(HR_data$Age)
HR_data$DistanceFromHome <- scale(HR_data$DistanceFromHome)
HR_data$Education <- scale(HR_data$Education)
HR_data$JobLevel <- scale(HR_data$JobLevel)
HR_data$MonthlyIncome <- scale(HR_data$MonthlyIncome)
HR_data$NumCompaniesWorked <- scale(HR_data$NumCompaniesWorked)
HR_data$PercentSalaryHike <- scale(HR_data$PercentSalaryHike)
HR_data$StockOptionLevel <- scale(HR_data$StockOptionLevel)
HR_data$TotalWorkingYears <- scale(HR_data$TotalWorkingYears)
HR_data$TrainingTimesLastYear <- scale(HR_data$TrainingTimesLastYear)
HR_data$YearsAtCompany <- scale(HR_data$YearsAtCompany)
HR_data$YearsSinceLastPromotion <-
  scale(HR_data$YearsSinceLastPromotion)
HR_data$YearsWithCurrManager <- scale(HR_data$YearsWithCurrManager)
HR_data$EnvironmentSatisfaction <-
  scale(HR_data$EnvironmentSatisfaction)
HR_data$JobSatisfaction <- scale(HR_data$JobSatisfaction)
HR_data$WorkLifeBalance <- scale(HR_data$WorkLifeBalance)
HR_data$JobInvolvement <- scale(HR_data$JobInvolvement)
HR_data$PerformanceRating <- scale(HR_data$PerformanceRating)
HR_data$weekly_avg_wrk_hrs <- scale(HR_data$weekly_avg_wrk_hrs)
HR_data$leavesTaken_yearly <- scale(HR_data$leavesTaken_yearly)




# Checking attrition rate of employee

AttritionRate <- sum(HR_data$Attrition) / nrow(HR_data)
AttritionRate # 16.10% attrition rate.

# splitting the data between train and test
set.seed(100)

indices <- sample.split(HR_data$Attrition, SplitRatio = 0.7)

train <- HR_data[indices, ]

test <- HR_data[!(indices), ]




#feature selection
model_1 = glm(Attrition ~ ., data = train[,-1], family = "binomial")
summary(model_1)

#stepwise selection
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2)

vif(model_2)
#removing YearsAtCompany
model_3 <-
  glm(
    formula = Attrition ~ Age + Education + MonthlyIncome + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear  +
      YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction +
      JobSatisfaction + WorkLifeBalance + weekly_avg_wrk_hrs +
      BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely +
      DepartmentResearch...Development + DepartmentSales + EducationFieldTechnical.Degree +
      JobRoleManufacturing.Director + JobRoleResearch.Director +
      JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle,
    family = "binomial",
    data = train
  )

summary(model_3)
vif(model_3)

#removing BusinessTravelTravel_Rarely  for hig VIF and lower significance
model_4 <-
  glm(
    formula = Attrition ~ Age + Education + MonthlyIncome + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear  +
      YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction +
      JobSatisfaction + WorkLifeBalance + weekly_avg_wrk_hrs +
      BusinessTravelTravel_Frequently +
      DepartmentResearch...Development + DepartmentSales + EducationFieldTechnical.Degree +
      JobRoleManufacturing.Director + JobRoleResearch.Director +
      JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle,
    family = "binomial",
    data = train
  )

summary(model_4)
vif(model_4)



#removing MaritalStatusMarried  for hig VIF and lower significance
model_5 <-
  glm(
    formula = Attrition ~ Age + Education + MonthlyIncome + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear  +
      YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction +
      JobSatisfaction + WorkLifeBalance + weekly_avg_wrk_hrs +
      BusinessTravelTravel_Frequently +
      DepartmentResearch...Development + DepartmentSales + EducationFieldTechnical.Degree +
      JobRoleManufacturing.Director + JobRoleResearch.Director +
      JobRoleSales.Executive + MaritalStatusSingle,
    family = "binomial",
    data = train
  )

summary(model_5)
vif(model_5)


#removing JobRoleSales.Executive  for hig VIF and lower significance
model_6 <-
  glm(
    formula = Attrition ~ Age + Education + MonthlyIncome + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear  +
      YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction +
      JobSatisfaction + WorkLifeBalance + weekly_avg_wrk_hrs +
      BusinessTravelTravel_Frequently +
      DepartmentResearch...Development + DepartmentSales + EducationFieldTechnical.Degree +
      JobRoleManufacturing.Director + JobRoleResearch.Director +
      MaritalStatusSingle,
    family = "binomial",
    data = train
  )

summary(model_6)
vif(model_6)

#removing EducationFieldTechnical.Degree for hig VIF and lower significance
model_7 <-
  glm(
    formula = Attrition ~ Age + Education + MonthlyIncome + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear  +
      YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction +
      JobSatisfaction + WorkLifeBalance + weekly_avg_wrk_hrs +
      BusinessTravelTravel_Frequently +
      DepartmentResearch...Development + DepartmentSales + 
      JobRoleManufacturing.Director + JobRoleResearch.Director +
      MaritalStatusSingle,
    family = "binomial",
    data = train
  )

summary(model_7)
vif(model_7)


#removing Education for hig VIF and lower significance
model_8 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear  +
      YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction +
      JobSatisfaction + WorkLifeBalance + weekly_avg_wrk_hrs +
      BusinessTravelTravel_Frequently +
      DepartmentResearch...Development + DepartmentSales + 
      JobRoleManufacturing.Director + JobRoleResearch.Director +
      MaritalStatusSingle,
    family = "binomial",
    data = train
  )

summary(model_8)
vif(model_8)


# removing JobRoleManufacturing.Director  for hig VIF and lower significance
model_9 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked +
      TotalWorkingYears + TrainingTimesLastYear  +
      YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction +
      JobSatisfaction + WorkLifeBalance + weekly_avg_wrk_hrs +
      BusinessTravelTravel_Frequently +
      DepartmentResearch...Development + DepartmentSales + 
      JobRoleResearch.Director +
      MaritalStatusSingle,
    family = "binomial",
    data = train
  )

summary(model_9)
vif(model_9)



test_original<-cbind(test)
# test<-cbind(test_original)
test <- test[, names(test) != "Attrition"]
#test <- test[, names(test) != "EmployeeID"]

test_pred <- predict(model_9, newdata = test, type = 'response')
View(test_pred)
summary(test_pred)


test_original$prob <- test_pred
# View(test[,c("Attrition","prob")])
test_pred_attrition <- factor(ifelse(test_original$prob >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test_original$Attrition == 1, "Yes", "No"))


table(test_actual_attrition, test_pred_attrition)

#######################################################################
test_pred_attrition <- factor(ifelse(test_original$prob >= 0.50, "Yes", "No"))

test_conf <-
  confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value.
#

# Let's find out the optimal probalility cutoff

perform_fn <- function(cutoff)
{
  test_pred_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <-
    confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01, .80, length = 100)

OUT = matrix(0, 100, 3)


for (i in 1:100)
{
  OUT[i, ] = perform_fn(s[i])
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


cutoff <- s[which(abs(OUT[, 1] - OUT[, 2]) < 0.01)]


test_cutoff_attrition <- factor(ifelse(test_pred >= 0.45, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test_original)

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition == "Yes", 1, 0)
test_actual_attrition <- ifelse(test_actual_attrition == "Yes", 1, 0)


library(ROCR)
#on testing  data
pred_object_test <- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test <-
  performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart

# plotting the lift chart

# Loading dplyr package
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(predicted_prob))
    predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[, "bucket"] = ntile(-helper[, "predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels), funs(total = n(),
                                    totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}


Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
#calculating non attrition
Attrition_decile$totalNonAttrition <- Attrition_decile$total - Attrition_decile$totalresp 
#calculating cummulative sum of non attrition 
Attrition_decile <-  mutate(
  Attrition_decile,
  CumNonAttrition = cumsum(totalNonAttrition)
)
#calculating %Non-attrition
Attrition_decile$gainNonAttrition <- (Attrition_decile$CumNonAttrition/1105) * 100
#calculating attrition - nonAttrition
Attrition_decile$KS_Stats <- Attrition_decile$Gain - Attrition_decile$gainNonAttrition
#KS Statistics at 3rd decile - 47.93%

