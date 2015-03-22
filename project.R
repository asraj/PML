library(caret)
library(gbm)
library(randomForest)

pml <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!", ""))

submission <-read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!", ""))

## Remove first 7 columns
pml<-pml[,-c(1:7)]

## Function to get percentage of NA in a column
propmiss <- function(dataframe) 
        sapply(dataframe,function(x) sum(is.na(x))/length(x))

b<-propmiss(pml)

## Remove all columns with more than 70% NAs
pml <- pml[,names(b[b<0.7])]

## 30% for Training and 70% for testing. 
## Small training set just to speed up training
inTrain <-createDataPartition(pml$classe,p=.2, list=FALSE)
training <- pml[inTrain,]
testing <- pml[-inTrain,]

modFit1 <- train(data=training,classe~., method="gbm", 
                 preProcess=c("center","scale") verbose=FALSE )

confusionMatrix(predict(modelFit,test), test$classe)

#Confusion Matrix and Statistics

#Reference
#Prediction    A    B    C    D    E
#A 3848  134    2    4    8
#B   35 2436  101    8   21
#C   16   82 2251   78   30
#D    7    4   37 2137   28
#E    0    1    4   24 2437

#Overall Statistics

#Accuracy : 0.9546         
#95% CI : (0.9509, 0.958)
#No Information Rate : 0.2844         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.9425         
#Mcnemar's Test P-Value : < 2.2e-16      

#Statistics by Class:

#                     Class: A Class: B Class: C Class: D Class: E
#Sensitivity            0.9852   0.9168   0.9399   0.9494   0.9655
#Specificity            0.9849   0.9851   0.9818   0.9934   0.9974
#Pos Pred Value         0.9630   0.9366   0.9162   0.9657   0.9882
#Neg Pred Value         0.9940   0.9801   0.9872   0.9901   0.9923
#Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
#Detection Rate         0.2802   0.1774   0.1639   0.1556   0.1775
#Detection Prevalence   0.2910   0.1894   0.1789   0.1611   0.1796
#Balanced Accuracy      0.9850   0.9510   0.9609   0.9714   0.9815

## Filter cols for Submission
trainCols <- colnames(training[, -53]) 
submission <- submission[,trainCols]

## Coerce the submission data to  the same type as training.
for (i in 1:length(submission) ) {
        for(j in 1:length(training)) {
                if( length( grep(names(training[i]), names(submission)[j]) ) ==1)  {
                        class(submission[j]) <- class(training[i])
                }      
        }      
}

# Function for Submission
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

## Predict submission
predSubmission <- predict(modelFit, submission)

## Write File
pml_write_files(predSubmission)