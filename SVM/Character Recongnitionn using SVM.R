# Import all required libraries
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(stratification)
library(sampling)
library(devtools)
library(splitstackshape)
library(doParallel)




# Import train dataset
mnist_train<-read_csv("mnist_train.csv",col_names = FALSE)

# Import test dataset
mnist_test<-read_csv("mnist_test.csv",col_names = FALSE)

#View(head(mnist_test))


# Check the dimentions of data
dim(mnist_train)
# Output: 60000 x 785

# The given data set has high dimention
# so as a part of dimentionality reduction we will use
# stratified sampling to reduce the rows and PCA to reduce the columns

# Lets group by colum X1 which is the label and find out total rows for each label
mnist_train_grp<-group_by(mnist_train,X1)
mnist_train_count<-summarise(mnist_train_grp,count=n())
# Lets view the same and find out if any information can be obtained
# View(mnist_train_count)
# No relevant informatoin obtained

# Lets combine both mnist_train and mnist_test for data cleansing
mnist_train<-rbind(mnist_train,mnist_test)
#Structure of the dataset
str(mnist_train)

# lets check final dimention of mnist_train
dim(mnist_train)
# Output: 70000 x 785


#checking missing value
sapply(mnist_train, function(x) sum(is.na(x)))
# No missing values Observed

# Lets check for columns with only 0 values
sapply(mnist_train, function(x) sum(ifelse(x==0,1,0)))

# Lets remove the columns with only 0 values
non_zero_cols<-c()
count=1;
for(i in 1:ncol(mnist_train)){
  if(sum(ifelse(mnist_train[,i]==0,1,0))<70000){
    non_zero_cols[count]<-names(mnist_train[,i])
    count=count+1;
  }
}
non_zero_cols
mnist_train<-select(mnist_train,non_zero_cols)

# Lets check the number of columns 
ncol(mnist_train)
# Output: 720 hence 785-720=065 columns removed

# Lets use PCA for dimentionality reduction and reduce the columns and get 10 parameters
mnist_train_pca<-preProcess(x=mnist_train[-1],method="pca",pcaComp = 12)
mnist_train<-predict(mnist_train_pca,mnist_train)
# View(mnist_train)

# Lest move the label column at last
mnist_train<-mnist_train[,c(2:12,1)]
# Extract the original test set which is from row 60000 to 70000
mnist_test<-mnist_train[c(60000:70000),]
mnist_train<-mnist_train[c(1:60000),]


#View(data.frame(train=non_zero_cols,test=non_zero_cols_test))
# ncol(character_df_cleaned)

#checking for duplicate rows
count(unique(mnist_train))
# Output is 60000 which is equal to number of rows hence no duplicates

# Our x1 variable is dependent variable (Label) lets convert the same to factors
mnist_train$X1<-factor(mnist_train$X1)

set.seed(100)

# Now we observ that our training data has 60000records and is most likely
# to overfit, so lets Take stratified sample of the data , 20% or total population
mnist_train_strata<-stratified(mnist_train,c("X1"),size=.20)

# check the structure of the data
str(mnist_train_strata)

# Check the row count
nrow(mnist_train_strata)
# Output: 9000


#Constructing Model
#str(mnist_train_strata)

#Lets create a kernal using Linear Kernel
Model_linear <- ksvm(X1~ ., data = mnist_train_strata, scale = FALSE, kernel = "vanilladot")
# Lest do predition on test sent and find out Accuracy
Eval_linear<- factor(predict(Model_linear, mnist_test[-13]))
#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,factor(mnist_test$X1))
# Output: Accuracy : 0.8235


# Lest develop rbf Kernal model and check its accuracy
Model_rbf <- ksvm(X1~ ., data = mnist_train_strata, scale = TRUE, kernel = "rbfdot")
# Preditions on test data
Eval_rbf<- factor(predict(Model_rbf, mnist_test[-13]))
#confusion matrix - rbf Kernel
confusionMatrix(Eval_rbf,factor(mnist_test$X1))
# Output : Accuracy : 0.9143 we observe that the accuracy has increased 


summary(Model_rbf)

# Lets cross validate our model


trainControl <- trainControl(method="cv", number=5)
# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.



metric <- "Accuracy"

# Making grid of "sigma" and C values.
# Since increasing sigma will lead to better fitting of model lets take sigma from 1 to 5
#grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2))
#grid <- expand.grid(.sigma=c(.075,.1,.125), .C=c(2.5,3,3.5))
#grid <- expand.grid(.sigma=seq(.125, 0.625, by=0.05), .C=seq(3.5, 5, by=0.5))
#grid <- expand.grid(.sigma=c(.05,.575), .C=c(10))
#grid <- expand.grid(.sigma=c(.05,.575), .C=c(20))
#grid <- expand.grid(.sigma=c(0.225), .C=c(15,20,30))
grid <- expand.grid(.sigma=seq(.25, 0.75, by=0.25), .C=seq(0, 20, by=5))
# Since the data is huge hence lets execute the same in a parallal processing environment
# Make a cluster of total cpu cores -1
cl <- makeCluster(detectCores()-1)

# View(mnist_train)
# Register the cluster
registerDoParallel(cl)
# Performing 5-fold cross validation
# Initiate cross validation
fit.svm_radial <- train(X1~., data=mnist_train_strata, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)
# Stop the cluster
stopCluster(cl)

fit.svm_radial
#sigma  C   Accuracy   Kappa    
#0.25    1  0.9148334  0.9053368
#0.25    6  0.9164173  0.9070987
#0.25   11  0.9131673  0.9034858
#0.25   16  0.9118331  0.9020022
#0.50    1  0.9055831  0.8950541
#0.50    6  0.9040000  0.8932945
#0.50   11  0.9015834  0.8906085
#0.50   16  0.9014164  0.8904227
#0.25    0        NaN        NaN
#0.25    5  0.9187517  0.9096941
#0.25   10  0.9155018  0.9060817
#0.25   15  0.9129184  0.9032095
#0.25   20  0.9126682  0.9029309
#0.50    0        NaN        NaN
#0.50    5  0.9038345  0.8931110
#0.50   10  0.9020840  0.8911652
#0.50   15  0.9003339  0.8892205
#0.50   20  0.8993335  0.8881085
#0.75    0        NaN        NaN
#0.75    5  0.8844172  0.8715267
#0.75   10  0.8820842  0.8689343
#0.75   15  0.8820840  0.8689344
#0.75   20  0.8819174  0.8687490

plot(fit.svm_radial)



#After several trial and error We get the best accuracy at sigma=.25 and c=1 of 0.9187517

# Lets create a Model with c=10 and sigma=.225 and check the accuracy on test data

# Lest develop rbf Kernal model and check its accuracy
Model_rbf_2 <- ksvm(X1~ ., data = mnist_train_strata, scale = TRUE, kernel = "rbfdot",C = 1, epsilon = .25)
# Preditions on test data
Eval_rbf_2<- factor(predict(Model_rbf_2, mnist_test[-13]))
#confusion matrix - rbf Kernel
confusionMatrix(Eval_rbf_2,factor(mnist_test$X1))
# Output : Accuracy : 0.9142 
# Thus Model_rbf_2 is our final model
