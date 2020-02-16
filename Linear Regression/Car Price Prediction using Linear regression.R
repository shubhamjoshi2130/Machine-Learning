#                     CarPrice_Assignment 
#                     Submitted By:-
#                     Name: Shubham Joshi
#                     Roll Number:  DDA1730102
#                     **Note: 1] All the View() commands have been commented in the code 
#                             2] package caTools has been used to plot correlation matrix, the same has been commented
#                                to avoid compilation issues
# Set the working directory
# setwd("C:\\Users\\SnehaChubz\\Desktop\\UpGrad\\linear regression\\Assignment")
#Test
# Import required packages
library(dplyr)
library(stringr)
library(tidyr)
library(MASS)
library(car)
library(ggplot2)
# install.packages("caTools")
# library(caTools)

# Read the csv data into a dataframe

# cars_df<-read.csv("CarPrice_Assignment.csv")
# str(cars_df)

cars_df<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
# View the data to understand structure
# View(head(cars_df))

# Step#1: Find out total number of rows:-
nrow(cars_df)
# Output: 205 hence there are 205 rows

# Step#2 check for duplicates
nrow(unique(cars_df))
# Output:205 hence there are no duplicates

# Step#3: Lets check for NA values
sum(is.na(cars_df))
# Output is :0 it means there are no NA values

#---- checking if we can create bins for any of the variables----
# "Symboling :Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) " 
# Lets create fators for the same and check
str(factor(cars_df$symboling))
# Output : Factor w/ 6 levels "-2","-1","0",..: 6 6 4 5 5 5 4 4 4 3 ...
# as it can bee seen clearly there are 6 levels lets convert this column to factors:-
cars_df$symboling<-factor(cars_df$symboling)

# Hence we can create bins for the same as:-
#             +2 to +3 ---- riskey
#             +1 to -1 ---- moderate risk
#             -2 to -3 ---- safe
levels(cars_df$symboling)

levels(cars_df$symboling)[1]<-"safe"
levels(cars_df$symboling)
# Output: [1] "safe" "-1"   "0"    "1"    "2"    "3" 
str(cars_df$symboling)
# Output:  Factor w/ 6 levels "safe","-1","0",..: 6 6 4 5 5 5 4 4 4 3 ...

levels(cars_df$symboling)[2:4]<-"moderate risk"
levels(cars_df$symboling)
# Output : "safe"          "moderate risk" "2"             "3" 
str(cars_df$symboling)
# Output: Factor w/ 4 levels "safe","moderate risk",..: 4 4 2 3 3 3 2 2 2 2

levels(cars_df$symboling)[3:4]<-"riskey"
levels(cars_df$symboling)
# Output : [1] "safe"          "moderate risk" "riskey"  
str(cars_df$symboling)
# Output: Factor w/ 3 levels "safe","moderate risk",..: 3 3 2 3 3 3 2 2 2 2 ...

# Now Lets check the summary of the data :-
summary(cars_df)
# Output:-


# Lets take 'enginesize' as out next column and check if it can be converted to bins:-
# Convert into factors and hck fr leels:-
str(factor(cars_df$enginesize))
# Output: Factor w/ 44 levels "61","70","79",..: 19 19 29 12 23 23 23 23 20 20 ...
# since it has 44 levels lets keep it as is for now and lets not convert them to factors

# Lets take 'horsepower' as out next column and check if it can be converted to bins:-
# Convert into factors and hck fr leels:-
str(factor(cars_df$horsepower))
# Output: Factor w/ 59 levels "48","52","55",..: 31 31 46 28 34 30 30 30 41 49 ...
# since it has 59 levels lets keep it as is for now and lets not convert them to factors



# Lets take 'peakrpm' as out next column and check if it can be converted to bins:-
# Convert into factors and hck fr leels:-
str(factor(cars_df$peakrpm))
# Output: Factor w/ 23 levels "4150","4200",..: 11 11 11 17 17 17 17 17 17 17 ...
# since it has 23 levels lets keep it as is for now and lets not convert them to factors


# Lets take 'citympg' as out next column and check if it can be converted to bins:-
# Convert into factors and hck fr leels:-
str(factor(cars_df$citympg))
# Output: Factor w/ 29 levels "13","14","15",..: 9 9 7 12 6 7 7 7 5 4 ...
# since it has 29 levels lets keep it as is for now and lets not convert them to factors


# Lets take 'highwaympg' as out next column and check if it can be converted to bins:-
# Convert into factors and hck fr leels:-
str(factor(cars_df$highwaympg))
# Output: Factor w/ 30 levels "13","14","15",..: 9 9 7 12 6 7 7 7 5 4 ...
# since it has 30 levels lets keep it as is for now and lets not convert them to factors





# There is a variable named CarName which is comprised of two parts - the first word 
# is the name of 'car company' and the second is the 'car model'. For example, chevrolet impala has 
# 'chevrolet' as the car company name and 'impala' as the car model name. 
# We need to consider only company name as the independent variable for the model building. 
# Lets extract the company name in a new variable 'company' and delete variable
# 'CarName' 

cars_df<-separate(cars_df,CarName,into=c("company"),sep="\\s")

#------------------ Data Preparation Start--------------------------------------- 
# View(cars_df)
# Lets check distinct company names
# View(unique(cars_df$company))
# On observing the output the folloing issues have been observed:-
# 1] 'Nissan' and 'nissan' are the same company only the fist letter should be capital
# 2] 'porsche' and 'porcshce' are also same company but there is a spelling mistake
# 3] 'toyota' and 'toyouta' are also same company but there is a spelling mistake
# 4] 'vokswagen' and 'volkswagen' and 'vw' are also same company but there is a spelling mistake and one abbriviation
# 5] 'maxda' and 'mazda' are also same company but there is a spelling mistake

# lets convert all the name into lower case:-
cars_df$company<-str_to_lower(cars_df$company)

# lets convert porcshce to porsche
cars_df$company[which(cars_df$company=="porcshce")]<-"porsche"

# lets convert toyouta to toyota
cars_df$company[which(cars_df$company=="toyouta")]<-"toyota"

# lets convert vokswagen and vw to volkswagen
cars_df$company[which(cars_df$company=="vokswagen" | cars_df$company=="vw")]<-"volkswagen"
# Lets again check distinct company names

# lets convert maxda and vw to mazda
cars_df$company[which(cars_df$company=="maxda")]<-"mazda"

# Lets again check distinct company names
# View(unique(cars_df$company))
# Output: The values are distinct and unique


# Lets check for other categorical variables also for such anamolies which are:-
#         fueltype, aspiration, drivewheel,enginelocation,enginetype,cylindernumber,
#         fuelsystem and company
# View(unique(cars_df$fueltype))
# Output: No anamolies found

# View(unique(cars_df$aspiration))
# Output: No anamolies found

# View(unique(cars_df$drivewheel))
# Output: No anamolies found

# View(unique(cars_df$enginelocation))
# Output: No anamolies found

# View(unique(cars_df$enginetype))
# Output: No anamolies found

# View(unique(cars_df$cylindernumber))
# Output: No anamolies found

# View(unique(cars_df$fuelsystem))
# Output: No anamolies found




# step#3: lets check for categorical variables and convert them into dummy variables
#         looking at the data we observe that the following are the categorical variables :-
#         fueltype, aspiration, drivewheel,enginelocation,enginetype,cylindernumber,
#         fuelsystem and company

# Lets write a function to convert all the character columns of a dataframe
# to dummy variables

fueltype<-model.matrix(~fueltype-1,data=cars_df)
# View(fueltype)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="fueltype")],fueltype)
# View(cars_df)

aspiration<-model.matrix(~aspiration-1,data=cars_df)
# View(aspiration)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="aspiration")],aspiration)
# View(cars_df)

drivewheel<-model.matrix(~drivewheel-1,data=cars_df)
# View(drivewheel)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="drivewheel")],drivewheel)
# View(cars_df)

enginelocation<-model.matrix(~enginelocation-1,data=cars_df)
# View(enginelocation)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="enginelocation")],enginelocation)
# View(cars_df)

enginetype<-model.matrix(~enginetype-1,data=cars_df)
# View(enginetype)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="enginetype")],enginetype)
# View(cars_df)

cylindernumber<-model.matrix(~cylindernumber-1,data=cars_df)
# View(cylindernumber)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="cylindernumber")],cylindernumber)
# View(cars_df)

fuelsystem<-model.matrix(~fuelsystem-1,data=cars_df)
# View(cylindernumber)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="fuelsystem")],fuelsystem)
# View(cars_df)

company<-model.matrix(~company-1,data=cars_df)
# View(cylindernumber)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="company")],company)
# View(cylindernumber)

carbody<-model.matrix(~carbody-1,data=cars_df)
# View(carbody)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="carbody")],carbody)
# View(cars_df)

doornumber<-model.matrix(~doornumber-1,data=cars_df)
# View(doornumber)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="doornumber")],doornumber)
# View(cars_df)

symboling<-model.matrix(~symboling-1,data=cars_df)
# View(symboling)
cars_df<-cbind(cars_df[,-which(colnames(cars_df)=="symboling")],symboling)
# View(cars_df)


# Lets check for outliers in the following variables:-
# wheelbase,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,
# compressionratio,horsepower,peakrpm,citympg,highwaympg
quantile(cars_df$wheelbase,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = wheelbase)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for wheelbase to check for outliers")
# from 99% to 100% it is clearly oberved that the values are increasing abruptly,i.e. from 115.544 to 120.900
# Hence 20.900 can be treated as outlier and can be replaced with 115.44
cars_df$wheelbase[which(cars_df$wheelbase>115.544)]<-115.544

quantile(cars_df$carlength,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = carlength)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for carlength to check for outliers")
# from 99% to 100% it is clearly oberved that the values are increasing abruptly,i.e. from 202.480 to 208.100
# Hence 208.100 can be treated as outlier and can be replaced with 202.480
cars_df$carlength[which(cars_df$carlength>202.480)]<-202.480
# Also here are verry few values which are <155.060 so lets replace the same with 155.060
cars_df$carlength[which(cars_df$carlength<155.060)]<-155.060
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = carlength)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for carlength to check for outliers")

# Outliers for carwidth
quantile(cars_df$carwidth,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = carwidth)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for carwidth to check for outliers")
# below 2% we clearly observe there there are outliers which are < 63.600
# so lets replace all those with 63.600
cars_df$carwidth[which(cars_df$carwidth<63.600)]<-63.600
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = carwidth)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for carwidth to check for outliers")

# Processins outliers for carheight
quantile(cars_df$carheight,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = carheight)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for carheight to check for outliers")
# No outliers observed


# outlier treatment for curbweight
quantile(cars_df$curbweight,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = curbweight)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for curbweight to check for outliers")
# Also here are verry few values which are <1819.72 so lets replace the same with 1819.72
cars_df$curbweight[which(cars_df$curbweight<1819.72)]<-1819.72
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = curbweight)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for curbweight to check for outliers")

#Outlier treatment for enginesize
quantile(cars_df$enginesize,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = enginesize)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for enginesize to check for outliers")
# from 96% to 100% it is clearly oberved that the values are increasing abruptly,i.e. from 209.00 to 326.00
# Hence values greater than 209.00 can be treated as outlier and can be replaced with 209.00
cars_df$enginesize[which(cars_df$enginesize>209.00)]<-209.00
# We also observe that there is a sustantial increase from 2% at 70.00to 3% 90.00 so lets replace all values
# below 90.00 with 90.00
cars_df$enginesize[which(cars_df$enginesize<90.00)]<-90.00
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = enginesize)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for enginesize to check for outliers")

#Outlier treatment for boreratio
quantile(cars_df$boreratio,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = boreratio)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for boreratio to check for outliers")
# from 0% to 1% it is clearly oberved that the values are increasing abruptly,i.e. from 2.54 to 2.9100
# Hence values smaller than 2.9100 can be treated as outlier and can be replaced with 2.9100
cars_df$boreratio[which(cars_df$boreratio<2.9100)]<-2.9100
# We also observe that there is a sustantial increase from 99% at 3.8000 to 100% 3.9400 so lets replace all values
# greater 3.8000 with 3.8000
cars_df$boreratio[which(cars_df$boreratio>3.8000)]<-3.8000
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = boreratio)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for boreratio to check for outliers")

# Outlier treatment for stroke
quantile(cars_df$stroke,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = stroke)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for stroke to check for outliers")
# from 99% to 100% it is clearly oberved that the values are increasing abruptly,i.e. from 3.9000 to 4.1700
# Hence 3.9000 can be treated as outlier and can be replaced with 3.9000
cars_df$stroke[which(cars_df$stroke>3.9000)]<-3.9000
# Also here are verry few values which are <2.6400 so lets replace the same with 2.6400
cars_df$stroke[which(cars_df$stroke<2.6400)]<-2.6400
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = stroke)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for stroke to check for outliers")

# Outlier treatment for compressionratio
quantile(cars_df$compressionratio,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = compressionratio)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for compressionratio to check for outliers")
# There is am abrupt increase in values from 90% at 10.94 to 91% at 21.000 , but the same
# cannot be treates as the value constitute 10% of total polulation so we will ignore the same
# for time being

# Outlier treatment for horsepower
quantile(cars_df$horsepower,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = horsepower)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for horsepower to check for outliers")
# from 99% to 100% it is clearly oberved that the values are increasing abruptly,i.e. from 207.00 to 288.00
# Hence 207.00 can be treated as outlier and values greate than 207.00 can be replaced with 207.00
cars_df$horsepower[which(cars_df$horsepower>207.00)]<-207.00
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = horsepower)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for horsepower to check for outliers")

# Outlier treatment for peakrpm
quantile(cars_df$peakrpm,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = peakrpm)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for peakrpm to check for outliers")
# from 99% to 100% it is clearly oberved that the values are increasing abruptly,i.e. from 6000 to 6600
# Hence 6000 can be treated as outlier and values greter than 6000 can be replaced with 6000
cars_df$peakrpm[which(cars_df$peakrpm>6000)]<-6000
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = peakrpm)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for peakrpm to check for outliers")

# Outlier treatment for citympg
quantile(cars_df$citympg,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = citympg)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for citympg to check for outliers")
# from 98% to 99% it is clearly oberved that the values are increasing abruptly,i.e. from 38.00 to 44.72
# Outliers constitutes 2% which is signifiantly less.
# Hence 38.00 can be treated as outlier and  values greter than 38.00 can be replaced with 38.00
cars_df$citympg[which(cars_df$citympg>38.00)]<-38.00
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = citympg)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for citympg to check for outliers")

# Outlier treatment for highwaympg
quantile(cars_df$highwaympg,seq(0,1,.01))
# Lets check with box plot
ggplot(cars_df,aes(x = "", y = highwaympg)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for highwaympg to check for outliers")
# from 99% to 100% it is clearly oberved that the values are increasing abruptly,i.e. from 49.88 to 54.00
# Outliers constitutes 1% which is signifiantly less.
# Hence 38.00 can be treated as outlier and  values greter than 49.88 can be replaced with 49.88
cars_df$highwaympg[which(cars_df$highwaympg>49.88)]<-49.88
# Lets re-check with box plot
ggplot(cars_df,aes(x = "", y = citympg)) + 
  geom_boxplot() + 
  geom_point(col ="red",position = "jitter",alpha = .2) + 
  ggtitle("Box plot for citympg to check for outliers")


#------------------ Data Preparation End---------------------------------------


# lets extract all numeric variables and crete a correlation matrix on it

cars_numeric_df<-dplyr::select(cars_df,wheelbase
                               ,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,
                        compressionratio,horsepower,peakrpm,citympg,highwaympg)

# Lets get the correlation matrix and save it in notepad for future reference
cor(cars_numeric_df)

# library(corrplot)
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(cor(cars_numeric_df), method="color", col=col(200),  
#         type="upper", order="hclust", 
#         addCoef.col = "black", # Add coefficient of correlation
#         tl.col="black", tl.srt=45, #Text label color and rotation
#         # Combine with significance
#         sig.level = 0.01, insig = "blank",number.cex= 10/ncol(cars_numeric_df),
#         # hide correlation coefficient on the principal diagonal
#         diag=FALSE 
#)




# derived variables
# On looking at coorelation matix we can identify that carwidth and carlength
# high coreelation with curbweight so lets introduce a new variable weight to 
# surface ratio 'weighttosurfacearearatio' as :-

cars_df$weighttosurfacearearatio<-((cars_df$curbweight)/(cars_df$carlength*cars_df$carwidth))

# Also horsepower and curbweight can be combined to get power to weight ration
cars_df$powertoweightratio<-cars_df$horsepower/cars_df$curbweight

# Also horsepower and citympg can be combined to get city milage to power ratio
cars_df$powertocitympg<-cars_df$horsepower/cars_df$citympg

# Also horsepower and citympg can be combined to get highway milage to power ratio
cars_df$powertohighwaympg<-cars_df$horsepower/cars_df$highwaympg

cars_numeric_df<-dplyr::select(cars_df,wheelbase
                               ,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,
                               compressionratio,horsepower,peakrpm,citympg,highwaympg,weighttosurfacearearatio,powertoweightratio,powertocitympg,powertohighwaympg)

# Lets get the latest correlation matrix and save it in notepad for future reference
cor(cars_numeric_df)

# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(cor(cars_numeric_df), method="color", col=col(200),  
#         type="upper", order="hclust", 
#         addCoef.col = "black", # Add coefficient of correlation
#         tl.col="black", tl.srt=45, #Text label color and rotation
#         # Combine with significance
#         sig.level = 0.01, insig = "blank", number.cex= 10/ncol(cars_numeric_df),
#         # hide correlation coefficient on the principal diagonal
#         diag=FALSE 
#)

# Let us remove car_ID since its irrlevent for developing our model
cars_df<-cars_df[,-1]

# Lets split our data into training set and test set
set.seed(100)
colidx <-sample.split(cars_df,SplitRatio = 0.7)
head(cars_df)
train<-subset(cars_df, colidx==TRUE)
test <- subset(cars_df,colidx ==FALSE)


# now lets create our initial model with all the variables:-
model_1<-lm(price~.,data=train)
summary(model_1)
# Output:-
# R-squared:  0.9705,	Adjusted R-squared:  0.9461

# Now lets use stepAIC function get a primarly list of independent variables for our model
# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 



ncol(cars_df)
# Output: 81
# We have a total of 81 variables considered into the model 

#Now let;s run the stepAIC function. 
step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed variables - Bedrooms, furnishingstatussemi.furnished, bbratio, 

# Let's execute this model here, 


model_2 <-
  lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
       stroke + compressionratio + horsepower + peakrpm + highwaympg + 
       fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
       enginelocationfront + enginetypedohc + enginetypel + enginetypeohcf + 
       cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
       `companyalfa-romero` + companyaudi + companybmw + companybuick + 
       companydodge + companyisuzu + companymitsubishi + companyplymouth + 
       companyporsche + companyrenault + companysaab + companyvolkswagen + 
       carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
       weighttosurfacearearatio + powertocitympg, data = train)
# Clearly the model contain 38 variable hence stepAIC function has eliminated 
# 81-38 = 43 insignificant variables
# Lets check the summary and VIF , from here on the below two command will be called after every model
summary(model_2)
# Output: R-squared:  0.9691,	Adjusted R-squared:  0.9575
vif(model_2)

# After looking at summary lest consider significance level.01 and start eleiminating
# variables with sinigicance level >.05

# Lets start removing these independent variables one by one by checking VIF and p-Value 
# and check their effet on R-Square


# Lets start removing the least sginificat variables 

# Lets consider curbweight VIF:393.736735 p-Value:0.000565 which is <.05 hence its significat

# Lets consider weighttosurfacearearatio VIF:135.499232 p-Value:0.010427 which is <.05 hence its significat
# but the same has high correlation with curbweight which is comparatively more significant
# so lets remove weighttosurfacearearatio and see the effect on Adjusted R-square and
# significance of curbweight

model_2.1 <-
  lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
       stroke + compressionratio + horsepower + peakrpm + highwaympg + 
       fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
       enginelocationfront + enginetypedohc + enginetypel + enginetypeohcf + 
       cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
       `companyalfa-romero` + companyaudi + companybmw + companybuick + 
       companydodge + companyisuzu + companymitsubishi + companyplymouth + 
       companyporsche + companyrenault + companysaab + companyvolkswagen + 
       carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
       powertocitympg, data = train)
summary(model_2.1)
#     R-squared:  0.967,	Adjusted R-squared:  0.9551 
#     Since Adjusted R-squared:0.9551 which is near to model_2 R-square : 0.9575
#     Also removing this variable has dcreased the p-Valueof curbweight from 0.001723 to 0.000270
#     As the Adjusted R-square has not changed significantly the variable be removed
vif(model_2.1)

# Lets consider compressionratio with VIF: 96.482612 and p-Value 0.181593 which is >.05 which is less significant
# Lets remove compressionratio
model_3<- lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
               stroke + horsepower + peakrpm + highwaympg + 
               fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
               enginelocationfront + enginetypedohc + enginetypel + enginetypeohcf + 
               cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
               `companyalfa-romero` + companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companyrenault + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_3)
#     R-squared:  0.9665,	Adjusted R-squared:  0.9547  
#     Since Adjusted R-squared:0.9547 which is near to model_2 R-square : 0.9551
#     As the Adjusted R-square has not changed significantly so the variable can be removed
vif(model_3)

# Lets consider horsepower with VIF:62.027340 and p-Value:4.14e-09 which is <.05 and significant
# Hence the same cannot be removed

# Lets consider powertocitympg with VIF: 51.350767 and p-Value: 1.72e-15  which is <.05 and significant
# But the same has high correlation with horsepower and horsepower p-Value is less significant than
# powertocitympg so lets remove horsepower and check its overall impact on Adjusted R-square
model_4<- lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
                        stroke + peakrpm + highwaympg + 
                        fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
                        enginelocationfront + enginetypedohc + enginetypel + enginetypeohcf + 
                        cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
                        `companyalfa-romero` + companyaudi + companybmw + companybuick + 
                        companydodge + companyisuzu + companymitsubishi + companyplymouth + 
                        companyporsche + companyrenault + companysaab + companyvolkswagen + 
                        carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
                        powertocitympg, data = train)
summary(model_4)
#     R-squared:  0.953,	Adjusted R-squared:  0.9372  
#     Since Adjusted R-squared:0.9372 which is less than previous value of 0.9547 
#     but the VIF of powertocitympg has been reduced to 15.420421
#     the same cannot be removed
vif(model_4)


# Now lets try removing powertocitympg and check its impact

model_5<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
              stroke + horsepower + peakrpm + highwaympg + 
              fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
              enginelocationfront + enginetypedohc + enginetypel + enginetypeohcf + 
              cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
              `companyalfa-romero` + companyaudi + companybmw + companybuick + 
              companydodge + companyisuzu + companymitsubishi + companyplymouth + 
              companyporsche + companyrenault + companysaab + companyvolkswagen + 
              carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk`
              , data = train)
summary(model_5)
#     R-squared:  0.9378,	Adjusted R-squared:  0.9168  
#     Since Adjusted R-squared:0.9168 which significantly less than that of model_3 R-square : 0.9547
vif(model_5)
#     and the VIF of horsepoer has been reduced to 18.626551
#     so lets select model_4 above model_5

summary(model_4)
vif(model_4)

# Now lets consider curbweight with VIF: 26.894270 and p-Value : 0.012230 since its <.05 its significant
# And cannot be removed

# Now lets consider wheelbase with VIF: 12.777214 and p-Value : 0.000972 since its <.05 its significant
# And cannot be removed

# Now lets consider highwaympg with VIF: 10.057977 and p-Value : 0.045471 since its <.05 its significant
# And cannot be removed

# Now lets start removing variables with p-value >.05 with highre p-Value variables getting
# removed first:-

# Remove companyrenault
model_6<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
              stroke + peakrpm + highwaympg + 
              fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
              enginelocationfront + enginetypedohc + enginetypel + enginetypeohcf + 
              cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
              `companyalfa-romero` + companyaudi + companybmw + companybuick + 
              companydodge + companyisuzu + companymitsubishi + companyplymouth + 
              companyporsche + companysaab + companyvolkswagen + 
              carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
              powertocitympg, data = train)
summary(model_6)
#      R-squared:  0.953,	Adjusted R-squared:  0.9378 
#     Since Adjusted R-squared:0.9378 which is better than previous value of 0.9372  
#     the variable can be removed
vif(model_6)


# Lets consider enginetypedohc which has  p-Value of 0.887166, 
model_7<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
              stroke + peakrpm + highwaympg + 
              fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
              enginelocationfront + enginetypel + enginetypeohcf + 
              cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
              `companyalfa-romero` + companyaudi + companybmw + companybuick + 
              companydodge + companyisuzu + companymitsubishi + companyplymouth + 
              companyporsche + companysaab + companyvolkswagen + 
              carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
              powertocitympg, data = train)
summary(model_7)
#     R-squared:  0.953,	Adjusted R-squared:  0.9384 
#     Since Adjusted R-squared:0.9384 which is better than previous value of 0.9378  
#     the variable can be removed
vif(model_7)



# Lets consider peakrpm which has  p-Value of 0.868506, 
model_8<-lm(formula = price ~ wheelbase + carlength + carheight + curbweight + 
              stroke + highwaympg + 
              fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
              enginelocationfront + enginetypel + enginetypeohcf + 
              cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
              `companyalfa-romero` + companyaudi + companybmw + companybuick + 
              companydodge + companyisuzu + companymitsubishi + companyplymouth + 
              companyporsche + companysaab + companyvolkswagen + 
              carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
              powertocitympg, data = train)
summary(model_8)
#     R-squared:  0.953,	Adjusted R-squared:  0.9389   
#     Since Adjusted R-squared:0.9389 which is better than previous value of 0.9378  
#     the same can be removed
vif(model_8)

# Lets considr carlength with VIF: 13.681225 and p-Value: 0.297027 which is >.05
# and is less significant hence the same can be removed
model_9<-lm(formula = price ~ wheelbase + carheight + curbweight + 
              stroke + highwaympg + 
              fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
              enginelocationfront + enginetypel + enginetypeohcf + 
              cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
              `companyalfa-romero` + companyaudi + companybmw + companybuick + 
              companydodge + companyisuzu + companymitsubishi + companyplymouth + 
              companyporsche + companysaab + companyvolkswagen + 
              carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
              powertocitympg, data = train)
summary(model_9)
#     R-squared:  0.9525,	Adjusted R-squared:  0.9389   
#     Since Adjusted R-squared:0.9389 which is same as previous value 0.9389  
#     the same can be removed
vif(model_9)

# Lets remove enginetypeohcf with p-Value: 0.867188
model_10<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               stroke + highwaympg + 
               fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
               `companyalfa-romero` + companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_10)
#     R-squared:  0.9525,	Adjusted R-squared:  0.9394   
#     Since Adjusted R-squared:0.9394 which is better previous value 0.9389  
#     the same can be removed
vif(model_10)

# Remove stroke with p-Value:0.925976
model_11<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
               `companyalfa-romero` + companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_11)
#     R-squared:  0.9525,	Adjusted R-squared:   0.94   
#     Since Adjusted R-squared:0.94 which near about previous value 0.9394  
#     the same can be removed
vif(model_11)



# Remove `companyalfa-romero` with p-Value:0.698913
model_12<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd + drivewheel4wd + drivewheelfwd + 
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
               companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_12)
#      R-squared:  0.9524,	Adjusted R-squared:  0.9404   
#     Since Adjusted R-squared:0.9404 which is better than previous value 0.94  
#     the same can be removed
vif(model_12)

# Remove drivewheelfwd with p-Value:0.692609
model_13<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd + drivewheel4wd +  
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
               companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_13)
#     R-squared:  0.9524,	Adjusted R-squared:  0.9409   
#     Since Adjusted R-squared:0.9409 which is better than previous value 0.9404  
#     the same can be removed
vif(model_13)


# Remove drivewheel4wd with p-Value:0.796444
model_14<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
               companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + carbodysedan + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_14)
#     R-squared:  0.9523,	Adjusted R-squared:  0.9414   
#     Since Adjusted R-squared:0.9414 which equal to previous value 0.9409  
#     the same can be removed
vif(model_14)

# Remove carbodysedan with p-Value:0.651498
model_15<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree + fuelsystem4bbl + 
               companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_15)
#     R-squared:  0.9522,	Adjusted R-squared:  0.9418    
#     Since Adjusted R-squared:0.9418 which near to previous value 0.9414  
#     the same can be removed
vif(model_15)

# Remove fuelsystem4bbl with p-Value:0.414533
model_16<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree +  
               companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + carbodyhardtop + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_16)
#     R-squared:  0.952,	Adjusted R-squared:  0.9419   
#     Since Adjusted R-squared:0.9419 which near to previous value 0.9418  
#     the same can be removed
vif(model_16)

# Remove carbodyhardtop with p-Value:0.339825
model_17<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree +  
               companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi + companyplymouth + 
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_17)
#     R-squared:  0.9516,	Adjusted R-squared:  0.942  
#     Since Adjusted R-squared:0.942 which better than value 0.9419  
#     the same can be removed
vif(model_17)


# Remove companyplymouth with p-Value:0.338536
model_18<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree +  
               companyaudi + companybmw + companybuick + 
               companydodge + companyisuzu + companymitsubishi +  
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_18)
#     R-squared:  0.9512,	Adjusted R-squared:  0.942    
#     Since Adjusted R-squared:0.942 which is equal previous value 0.942  
#     the same can be removed
vif(model_18)


# Remove companydodge with p-Value:0.267613
model_19<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive + cylindernumberthree +  
               companyaudi + companybmw + companybuick + 
               companyisuzu + companymitsubishi +  
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_19)
#     R-squared:  0.9507,	Adjusted R-squared:  0.9419   
#     Since Adjusted R-squared:0.9419 which near to previous value 0.942  
#     the same can be removed
vif(model_19)


# Remove cylindernumberthree with p-Value:0.154742
model_20<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive +   
               companyaudi + companybmw + companybuick + 
               companyisuzu + companymitsubishi +  
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_20)
#     R-squared:  0.9498,	Adjusted R-squared:  0.9414    
#     Since Adjusted R-squared:0.9414 which near to previous value 0.9419  
#     the same can be removed
vif(model_20)



# Remove companyisuzu with p-Value:0.217657
model_21<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront + enginetypel +  
               cylindernumberfive +   
               companyaudi + companybmw + companybuick + 
               companymitsubishi +  
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_21)
#     R-squared:  0.9492,	Adjusted R-squared:  0.9411    
#     Since Adjusted R-squared:0.9411 which near to previous value 0.9414  
#     the same can be removed
vif(model_21)

# Remove enginetypel with p-Value:0.289650
model_22<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               cylindernumberfive +   
               companyaudi + companybmw + companybuick + 
               companymitsubishi +  
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_22)
#     R-squared:  0.9487,	Adjusted R-squared:  0.9411    
#     Since Adjusted R-squared:0.9411 which is equal value 0.9411  
#     the same can be removed
vif(model_22)

# Remove cylindernumberfive with p-Value:0.145392
model_23<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               companyaudi + companybmw + companybuick + 
               companymitsubishi +  
               companyporsche + companysaab + companyvolkswagen + 
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_23)
#     R-squared:  0.9478,	Adjusted R-squared:  0.9405    
#     Since Adjusted R-squared:0.9405 which is near to previous value 0.9411  
#     the same can be removed
vif(model_23)


# Remove companyvolkswagen with p-Value:0.185757
model_24<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               companyaudi + companybmw + companybuick + 
               companymitsubishi +  
               companyporsche + companysaab +  
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_24)
#     R-squared:  0.947,	Adjusted R-squared:  0.9401    
#     Since Adjusted R-squared:0.9401 which is near to previous value 0.9405  
#     the same can be removed
vif(model_24)


# Remove companymitsubishi with p-Value:0.131916
model_25<-lm(formula = price ~ wheelbase + carheight + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               companyaudi + companybmw + companybuick + 
               companyporsche + companysaab +  
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_25)
#     R-squared:  0.946,	Adjusted R-squared:  0.9395    
#     Since Adjusted R-squared:0.9395 which is near to previous value 0.9401  
#     the same can be removed
vif(model_25)

# Remove carheight with p-Value:0.089040
model_26<-lm(formula = price ~ wheelbase + curbweight + 
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               companyaudi + companybmw + companybuick + 
               companyporsche + companysaab +  
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_26)
#     R-squared:  0.9448,	Adjusted R-squared:  0.9386    
#     Since Adjusted R-squared:0.9386 which is near to previous value 0.9395  
#     the same can be removed
vif(model_26)


# Remove curbweight with p-Value:0.102899
model_27<-lm(formula = price ~ wheelbase +  
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               companyaudi + companybmw + companybuick + 
               companyporsche + companysaab +  
               carbodyconvertible + `symbolingmoderate risk` + 
               powertocitympg, data = train)
summary(model_27)
#     R-squared:  0.9436,	Adjusted R-squared:  0.9377    
#     Since Adjusted R-squared:0.9377 which is near to previous value 0.9386  
#     the same can be removed
vif(model_27)

# Remove `symbolingmoderate risk` with p-Value:0.078503
model_28<-lm(formula = price ~ wheelbase +  
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               companyaudi + companybmw + companybuick + 
               companyporsche + companysaab +  
               carbodyconvertible +  
               powertocitympg, data = train)
summary(model_28)
#     R-squared:  0.9422,	Adjusted R-squared:  0.9367    
#     Since Adjusted R-squared:0.9367 which is near to previous value 0.9377  
#     the same can be removed
vif(model_28)

# Remove companysaab with p-Value:0.149660
model_29<-lm(formula = price ~ wheelbase +  
               highwaympg + 
               fueltypediesel + aspirationstd +   
               enginelocationfront +   
               companyaudi + companybmw + companybuick + 
               companyporsche + 
               carbodyconvertible +  
               powertocitympg, data = train)
summary(model_29)
#     R-squared:  0.9422,	Adjusted R-squared:  0.9367    
#     Since Adjusted R-squared:0.9367 which is near to previous value 0.9377  
#     the same can be removed
vif(model_29)

# Remove enginelocationfront with p-Value:0.124662
model_30<-lm(formula = price ~ wheelbase +  
               highwaympg + 
               fueltypediesel + aspirationstd +   
               companyaudi + companybmw + companybuick + 
               companyporsche + 
               carbodyconvertible +  
               powertocitympg, data = train)
summary(model_30)
#     R-squared:  0.9401,	Adjusted R-squared:  0.9355     
#     Since Adjusted R-squared:0.9355 which is near to previous value 0.9367  
#     the same can be removed
vif(model_30)

# At his point all our vaiables are significant and <.05 :-

#  Coefficients:      Estimate   Std. Error  t value Pr(>|t|)    
#  (Intercept)        -36222.19    5735.92  -6.315   4.01e-09 ***
#  wheelbase             319.32      48.14   6.633   8.21e-10 ***
#  highwaympg            180.45      54.95   3.284   0.00132 ** 
#  fueltypediesel       2715.83     912.54   2.976   0.00349 ** 
#  aspirationstd        1585.36     561.36   2.824   0.00549 ** 
#  companyaudi          2692.71     888.44   3.031   0.00295 ** 
#  companybmw           8014.32     932.76   8.592   2.42e-14 ***
#  companybuick        11026.94    1045.62  10.546   < 2e-16 ***
#  companyporsche       7049.10    1377.69   5.117   1.10e-06 ***
#  carbodyconvertible   5632.86    1166.04   4.831   3.79e-06 ***
#  powertocitympg       2010.60     118.08  17.027   < 2e-16 ***

# lets predict using this model
Predict_1 <- predict(model_30,test[,-which(colnames(test)=="price")])

(cor(test$price,Predict_1))^2


# View(result_df)
# Output: R-sqared : 0.9170471 
# Which seemes near to Adjusted R-Square of the model which is : 0.9355

# View(data.frame(test$price,Predict_1))


# Lets further try removing highwaympg with VIF :5.023725 and p-Value:0.00132
model_31<-lm(formula = price ~ wheelbase +  
               fueltypediesel + aspirationstd +   
               companyaudi + companybmw + companybuick + 
               companyporsche + 
               carbodyconvertible +  
               powertocitympg, data = train)
summary(model_31)
#     R-squared:  0.9351,	Adjusted R-squared:  0.9306      
#     Since Adjusted R-squared:0.9306 which is significantly less than previous value 0.9355  
#     Hence model_30 is our final model

# *** Note: Model_30 is our final model

# Lets check confirm that the error terms are random

# Lets plot errors against each independent variable and check for trend:-

result_df<-data.frame(error=(test$price-Predict_1),test)

ggplot(result_df,aes(x = powertocitympg, y = error)) + 
  geom_point(col ="red")


ggplot(result_df,aes(x = wheelbase, y = error)) + 
  geom_point(col ="red")

ggplot(result_df,aes(x = highwaympg, y = error)) + 
  geom_point(col ="red")

ggplot(result_df,aes(x = fueltypediesel, y = error)) + 
  geom_point(col ="red")

ggplot(result_df,aes(x = aspirationstd, y = error)) + 
  geom_point(col ="red")

ggplot(result_df,aes(x = companyaudi, y = error)) + 
  geom_point(col ="red")

ggplot(result_df,aes(x = companybuick, y = error)) + 
  geom_point(col ="red")

ggplot(result_df,aes(x = companyporsche, y = error)) + 
  geom_point(col ="red")

ggplot(result_df,aes(x = carbodyconvertible, y = error)) + 
  geom_point(col ="red")

# From the above graphs its clear that the error are random and model is accurate

# Now lets look at the final coeficients:-
summary(model_30)

#                 Final Result:-
# The final equation for accssing the model is :-
# price= wheelbase*319.32 + highwaympg*180.45 + fueltypediesel*2715.83 + aspirationstd*1585.36 +
#       companyaudi*2692.71 + companybmw*8014.32 + companybuick*11026.94 +  companyporsche*7049.10 +
#       carbodyconvertible*5632.86 + powertocitympg*2010.60
# This can be used by the business to understand relation
# The description of each variable is as follows:-
# wheelbase : Weelbase of car 
# highwaympg : Milage of the car on highway
# fueltypediesel : 1 for fuel type diseal and 0 for non diseal car
# aspirationstd : 1 if Aspiration used in a car isstd else 0
# companyaudi : 	1 if the car company is audi else 0
# companybmw :  	1 if the car company is bmw else 0
# companybuick : 	1 if the car company is buick else 0
# companyporsche : 1 if the car company is porsche else 0
# carbodyconvertible : 1 if the car body is convertible else 0
# powertocitympg : Ratio of power to milage given by the car in city
