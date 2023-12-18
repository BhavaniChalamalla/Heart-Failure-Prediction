library('readr')
library('dplyr')
library('e1071')
library('corrplot')
library(caret)


#import data
heart_data<- read_csv('C:/Users/91630/Downloads/heart.csv')
summary(heart_data)

# Check for missing values in the entire dataset
missing_values <- sum(is.na(heart_data))
# Display the number of missing values
cat("Number of missing values in the dataset:", missing_values, "\n")
##### No missing values


# Check for duplicate rows in the entire dataset
duplicate_rows <- heart_data[duplicated(heart_data), ]
print(duplicate_rows)
###### no duplicates


# Separating Data ---------------------------------------------------------
#goal variable                                       
Response<- heart_data$HeartDisease
#categorical variables
categorical_Cols<- heart_data[c("Sex","ChestPainType","RestingECG","ExerciseAngina","ST_Slope","FastingBS")]
#### Numerical Cols
numCols<- heart_data[c("RestingBP","Cholesterol","MaxHR","Oldpeak","Age")]



####### Categorical columns
#create barplot for categorical columns
par(mfrow= c(2,4))
for (col in c(names(categorical_Cols))){
  categorical_Cols %>% pull(col) %>% table %>% barplot(main= col)
}

##### barplot for response variable as it is also categorical
barplot(table(Response), main="HeartDisease")


##### create  dummies
dummy <- dummyVars("~Sex+ChestPainType+RestingECG+ExerciseAngina+ST_Slope", data =heart_data,fullRank = TRUE)
catDummies <- data.frame(predict(dummy, newdata = heart_data))
Cols<- c("Sex","ChestPainType","RestingECG","ExerciseAngina","ST_Slope", "HeartDisease")
heart <-cbind(catDummies,heart_data)
heart<-heart[, -which(names(heart) %in% Cols)]


# Check for near-zero variance
library(caret)
nearzero_var <- nearZeroVar(heart, saveMetrics = TRUE)
# Display the results
print(nearzero_var)
#### No nearzero variance predictors




####### Numerical columns
#### checking for highly correlated variables
# Calculate correlation matrix
library(corrplot)
cor_matrix <- cor(heart)
highcorr<-findCorrelation(cor_matrix)
highcorr
#### no highly correlated ariables

# Create a correlation plot
#corrplot(cor_matrix, method = "circle", type = "full", title = "Correlation Plot of Heart Data", tl.col = "black", tl.srt = 45)


####(OR)

# Calculate correlation matrix
cor_matrix <- cor(heart)

# Create a heatmap
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Heatmap",
        margins = c(10, 10))






# Numerical Data Plots --------------------------------------------------------
par(mar = c(1, 1, 1, 1), oma = c(1,1,1,1))
dev.off()
#create histogram and boxplot for each numerical column
par(mfrow= c(2,5))
for (col in c(names(numCols))){
  numCols %>% pull(col) %>% hist(main= col)
}
for (col in c(names(numCols))){
  numCols %>% pull(col) %>% boxplot(main= col)
}



#From above plots we can see some skewness in Cholesterol and Oldpeak columns.
#### So we want to check the accurate skewness value------------------------------------------------------------
apply(numCols, 2, skewness, na.rm=TRUE)



###### As expected those 2 columns have skewness greater than 0.5 which means they are skewed and need transformation
#### So when trying to do boxcox transformation we are getting errors saying boxcox transformation can be done only to positive values i.e >0
### so now we want to inspect those 2 columns carefully if they contain zeros or negative values 
# Check if "Cholesterol" column contains zeros
zero_count <- sum(numCols$Cholesterol == 0)

# Check if "Cholesterol" column contains negative values
negative_count <- sum(numCols$Cholesterol < 0)

# Print the counts
cat("Count of zeros in Cholesterol column:", zero_count, "\n")
cat("Count of negative values in Cholesterol column:", negative_count, "\n")




#### from above output we can see Cholesterol contains 172 zeros in it 
### But it is impossible for a person to have zero cholesterol so i'll replace all zeroz with mean imputation
#### mean imputation on Cholesterol
library(caret)
# Calculate the mean excluding zeros
mean_chol <- mean(numCols$Cholesterol[numCols$Cholesterol != 0])

# Replace zeros with mean imputation
numCols$Cholesterol[numCols$Cholesterol == 0] <- mean_chol

# Check if "Cholesterol" column contains zeros
zero_count <- sum(numCols$Cholesterol == 0)
zero_count



##### Oldpeak
# Check if "Oldpeak" column contains zeros
zero_count <- sum(numCols$Oldpeak == 0)

# Check if "Oldpeak" column contains negative values
negative_count <- sum(numCols$Oldpeak < 0)


# Print the counts
cat("Count of zeros in Oldpeak column:", zero_count, "\n")
cat("Count of negative values in Oldpeak column:", negative_count, "\n")




##### from above output we can see that Oldpeak column contains both zeroz and negatives
#### It is possible that Oldpeak can be zero. So we thought of adding a constant to Oldpeak
### to choose a constant we checked the minimum value of Oldpeak
min(numCols$Oldpeak)

##### as the minimum value is 2.6 we are adding a constant of 2.7 to oldpeak as we want to remove zeros also
install.packages("moments")
library(moments)
numCols$Oldpeak <- numCols$Oldpeak+2.7


# Check if "Oldpeak" column contains zeros
zero_count <- sum(numCols$Oldpeak == 0)
zero_count

# Check if "Oldpeak" column contains negative values
negative_count <- sum(numCols$Oldpeak < 0)
negative_count




#### checking the plots and skewness after imputations
par(mar = c(1, 1, 1, 1), oma = c(1,1,1,1))
dev.off()
#create histogram and boxplot for each numerical column
par(mfrow= c(2,5))
for (col in c(names(numCols))){
  numCols %>% pull(col) %>% hist(main= col)
}
for (col in c(names(numCols))){
  numCols %>% pull(col) %>% boxplot(main= col)
}


#### skewness
apply(numCols, 2, skewness, na.rm=TRUE)



###### The skewness of Cholesterol is increased and still there are outliers in many columns 
##### so now i want to do transformations
temp<- as.data.frame(numCols)
pre<- preProcess(temp, method = c("BoxCox", "center", "scale"))
numTrans<- predict(pre, temp)

print(pre)

#####Checking histograms for skewness before and after transformation
par(mfrow= c(2,5))
for (col in c(names(numCols))){
  numCols %>% pull(col) %>% hist(main= col)
}
for (col in c(names(numTrans))){
  numTrans %>% pull(col) %>% hist(main= col)
}


#### Histograms are appearing to be normally distributed now 
##### So checking skewness
apply(numTrans, 2, skewness, na.rm=TRUE)
### yes now after center, scale and boxcox, the skewness of all columns got decreased 



##### checking boxpolots for outliers before and after transformations
par(mfrow= c(2,5))
for (col in c(names(numCols))){
  numCols %>% pull(col) %>% boxplot(main= col)
}
for (col in c(names(numTrans))){
  numTrans %>% pull(col) %>% boxplot(main= col)
}


##### There are still outliers in our data
#### so we'll perform spatial sign
# Perform spatial sign transformation
spatSign <- spatialSign(numTrans)

# Convert the result to a data frame
spatSign <- as.data.frame(spatSign)


##### now i want to check the boxplots if they still consist of outliers even after performing spatial sign
par(mfrow= c(2,5))
for (col in c(names(numTrans))){
  numTrans %>% pull(col) %>% boxplot(main= col)
}
for (col in c(names(spatSign))){
  spatSign %>% pull(col) %>% boxplot(main= col)
}



###### now all the outliers are removed and we want to check if the skewness is increased 
apply(numTrans, 2, skewness, na.rm=TRUE)

apply(spatSign, 2, skewness, na.rm=TRUE)
##### By observing the output, the skewness also got reduced.

######Now our data is all set for model selection 



# Remove original numerical columns from heart_data
ContinuousCols<- c("RestingBP","Cholesterol","MaxHR","Oldpeak","Age")
heart<-heart[, -which(names(heart) %in% ContinuousCols)]
dim(heart)

# Append spatSign(Numerical variables df) dataframe to heart_data
heart <- cbind(heart, spatSign)
dim(heart)






# For reproducibility
set.seed(123) 

splitIndex <- createDataPartition(heart_data$HeartDisease, p = 0.8, list = FALSE, times = 1)
# Create training and testing datasets based on the split
train_data <- heart[splitIndex,]
test_data <- heart[-splitIndex,]


# Include the "HeartDisease" response variable in the training and testing datasets
train_response <- heart_data$HeartDisease[splitIndex]
test_response <- heart_data$HeartDisease[-splitIndex]


# Define a resampling method
#ctrl <- trainControl(method = "cv", number = 5)


# Check the data type and levels of the response variable
str(train_response)
str(test_response)

# Convert response variable to a factor with two levels
train_response <- as.factor(train_response)
test_response <- as.factor(test_response)

# Check the levels of your factor variable
levels(train_response)


# Change levels from "0" to "No" and from "1" to "Yes"
levels(train_response) <- c("No", "Yes")
levels(test_response) <- c("No", "Yes")

# Verify that the levels have been changed
levels(train_response)
levels(test_response)




################## Models building
####### Logistic Regression

ctrl <- trainControl(method = "cv", number= 10, 
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(123)
lrFull <- train(x= train_data,
                y = train_response,
                method = "glm",
                preProc = c("center", "scale"),
                family = "binomial",
                metric = "ROC" ,
                trControl = ctrl)
lrFull
summary(lrFull)


lrPred <- predict(lrFull,newdata = test_data)
confusionMatrix(lrPred,test_response)


library(pROC)
FullRoc <- roc(lrFull$pred$obs,lrFull$pred$Yes)
plot(FullRoc, legacy.axes = TRUE, col = "blue", main = "ROC Curve")
auc(FullRoc)





####### LDA 
## Using train function, should add pre-processing
## SET SEED

ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     ##index = list(simulatedTest[,1:4]),
                     savePredictions = TRUE)

set.seed(123)
LDAFull <- train(x = train_data,
                 y = train_response,
                 method = "lda",
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl)
LDAFull
summary(LDAFull)


ldaPred <- predict(LDAFull,newdata = test_data)
confusionMatrix(ldaPred,test_response)


library(pROC)
FullRoc <- roc(LDAFull$pred$obs,LDAFull$pred$Yes)
plot(FullRoc, legacy.axes = TRUE, col = "blue", main = "ROC Curve")
auc(FullRoc)





#####SVM

# Set up the training control with ROC as the summary function
ctrl <- trainControl(method = "cv", number= 10, summaryFunction = twoClassSummary, classProbs = TRUE)

set.seed(123)
svm_model <- train(x = train_data,
                   y = train_response,
                   method = "svmRadial",
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   tuneLength = 14,
                   trControl = ctrl)

svm_model
plot(svm_model)
ggplot(svm_model)+coord_trans(x='log2')

svmRpred <- predict(svm_model, newdata = test_data)
confusionMatrix(svmRpred,test_response)

svmRaccuracy <- data.frame(obs = test_response , pred = svmRpred)
defaultSummary(svmRaccuracy)







# Make predictions on the test data
svmRpred <- predict(svm_model, newdata = test_data)

# Evaluate the model using confusion matrix and other metrics
confusionMatrix(svmRpred, test_response)

# Create ROC curve
svm_probs <- predict(svm_model, newdata = test_data, type = "prob")[, "Yes"]
FullRoc <- roc(test_response, svm_probs)

# Plot the ROC curve
plot(FullRoc, legacy.axes = TRUE, col = "blue", main = "ROC Curve")

# Print AUC
auc_value <- auc(FullRoc)
cat("AUC:", auc_value, "\n")





######KNN

ctrl<- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(123)
knnTune <- train(x = train_data,
                 y = train_response,
                 method = "knn",
                 metric = "ROC",
                 # Center and scaling will occur for new predictions too
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k = 1:15),
                 trControl = ctrl)
knnTune
plot(knnTune)


knnpred <- predict(knnTune, newdata = test_data)
confusionMatrix(knnpred,test_response)

knnaccuracy <- data.frame(obs = test_response , pred = knnpred)
defaultSummary(knnaccuracy)

library(pROC)
FullRoc <- roc(knnTune$pred$obs,knnTune$pred$Yes)
plot(FullRoc, legacy.axes = TRUE, col = "blue", main = "ROC Curve")
auc(FullRoc)



###### Neural Networks
nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
                        .size = c(1:10),
                        ## The next option is to use bagging (see the
                        ## next chapter) instead of different random
                        ## seeds.
                        .bag = T)

ctrl <- trainControl(method = "cv", number = 10, classProbs = T, summaryFunction = twoClassSummary)

set.seed(123)
nnetTune <- train(train_data, train_response,
                  method = "avNNet",
                  metric = "ROC", 
                  tuneGrid = nnetGrid,
                  trControl = ctrl,
                  ## Automatically standardize data prior to modeling
                  ## and prediction
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = FALSE,
                  MaxNWts = 10 * (ncol(train_data) + 1) + 10 + 1,
                  maxit = 500)


nnetTune
plot(nnetTune)







# ######Random Forest
# set.seed(123)
# ctrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)
# 
# # Train the Random Forest model using the train function
# rf_model <- caret::train(x= train_data,
#                   y= train_response,
#                   method = "rf",
#                   metric = "ROC",
#                   preProc = c("center", "scale"),
#                   trControl = ctrl,
#                   tuneGrid = expand.grid(mtry = c(1:4)) )
# 
# # Print the model
# rf_model
# plot(rf_model)
# 
# rfpred <- predict(rf_model, newdata = test_data)
# confusionMatrix(rfpred,test_response)
# 
# rfaccuracy <- data.frame(obs = test_response , pred = rfpred)
# defaultSummary(rfaccuracy)
# 




######PLSDA

ctrl <- trainControl(method= "cv", number= 10, summaryFunction = twoClassSummary,
                     classProbs = TRUE)

## caret contains a built-in function called twoClassSummary that calculates the
## area under the ROC curve, the sensitivity, and the specificity.
set.seed(123)
plsFit2 <- train(x = train_data,
                 y = train_response,
                 method = "pls",
                 tuneGrid = expand.grid(.ncomp = 1:10),
                 preProc = c("center","scale"),
                 metric = "ROC",
                 trControl = ctrl)
plsFit2
plot(plsFit2)




#####Penalized 
ctrl <- trainControl(method = "cv", number= 10, 
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     ##index = list(simulatedTest[,1:4]),
                     savePredictions = TRUE)

glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),
                        .lambda = seq(.01, .2, length = 10))
set.seed(123)
glmnTuned <- train(x=train_data,
                   y = train_response,
                   method = "glmnet",
                   tuneGrid = glmnGrid,
                   preProc = c("center", "scale"),
                   metric = "ROC",
                   trControl = ctrl)
glmnTuned
plot(glmnTuned)



############ Multivariate Adaptive Regression Splines
# Fix the seed so that the results can be reproduced

## marsTuned <- train(solTrainXtrans, solTrainY,
# Explicitly declare the candidate models to test
ctrl = trainControl(method = "cv", number= 10, classProbs = T, summaryFunction = twoClassSummary)

marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:50)  ## Change 38 to 50

set.seed(123)
marsTuned <- train(x=train_data,
                   y = train_response,
                   method = "earth",
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   # Explicitly declare the candidate models to test
                   tuneGrid = marsGrid,
                   trControl = ctrl)

marsTuned
plot(marsTuned)





########### Nearest Shrunken Centroids
ctrl <- trainControl(method= "cv", number= 10, summaryFunction = twoClassSummary,
                     classProbs = TRUE)

## nscGrid <- data.frame(.threshold = 0:4)
nscGrid <- data.frame(.threshold = seq(0,4, by=0.1))

set.seed(123)
nscTuned <- train(x=train_data,
                  y = train_response,
                  method = "pam",
                  preProc = c("center", "scale"),
                  tuneGrid = nscGrid,
                  metric = "ROC",
                  trControl = ctrl)

nscTuned
plot(nscTuned)






####### Nonlinear Discriminant Analysis
library(caret)
ctrl <- trainControl(method= "cv", number= 10, summaryFunction = twoClassSummary,
                     classProbs = TRUE) 

set.seed(123)
mdaFit <- train(x=train_data,
                y = train_response,
                method = "mda",
                metric = "ROC",
                preProc = c("center", "scale"),
                tuneGrid = expand.grid(.subclasses = 1:3),
                trControl = ctrl)
mdaFit
plot(mdaFit)





########## Flexible Discriminant Analysis
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)

ctrl<- trainControl(method = "cv", number = 10, summaryFunction =twoClassSummary, classProbs = T )

set.seed(123)
fdaTuned <- train(x=train_data,
                  y = train_response,
                  method = "fda",
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  # Explicitly declare the candidate models to test
                  tuneGrid = marsGrid,
                  trControl = ctrl)

fdaTuned
plot(fdaTuned)
plot(fdaTuned,main="FDA, degree = 1 and nprune = 6")
fdaPred <- predict(fdaTuned, newdata = simulatedTest[,1:4])
confusionMatrix(data = fdaPred,reference =simulatedTest[,6])






########## Naive Bayes 
install.packages("klaR")
library(klaR)

ctrl<- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = T) 
  
set.seed(123)
nbFit <- train( x=train_data,
                y = train_response,
                method = "nb",
                metric = "ROC",
                preProc = c("center", "scale"),
                ##tuneGrid = data.frame(.k = c(4*(0:5)+1, 20*(1:5)+1, 50*(2:9)+1)), ## 21 is the best
                tuneGrid = data.frame(.fL = 2,.usekernel = TRUE,.adjust = TRUE),
                trControl = ctrl)

nbFit
plot(nbFit)
