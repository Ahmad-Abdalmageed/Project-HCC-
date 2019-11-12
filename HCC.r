# turning warnings off and viewing the tables fully 
options(warn= -1)
options(repr.matrix.max.rows=600, repr.matrix.max.cols=200)

# importing the dataset 
data_dirty <- read.csv("hcc-data.txt", na.strings = "?",col.names = c("Gender"
,"Symptoms"
,"Alcohol"
,"HBsAg"
,"HBeAg"
,"HBcAb"
,"HCVAb"
,"Cirrhosis"
,"Endemic"
,"Smoking"
,"Diabetes"
,"Obesity"
,"Hemochro"
,"AHT"
,"CRI"
,"HIV"
,"NASH"
,"Varices"
,"Spleno"
,"PHT"
,"PVT"
,"Metastasis"
,"Hallmark"
,"Age"
,"Grams_day"
,"Packs_year"
,"PS"
,"Encephalopathy"
,"Ascites"
,"INR"
,"AFP"
,"Hemoglobin"
,"MCV"
,"Leucocytes"
,"Platelets"
,"Albumin"
,"Total_Bil"
,"ALT"
,"AST"
,"GGT"
,"ALP"
,"TP"
,"Creatinine"
,"Nodule"
,"Major_Dim"
,"Dir_Bil"
,"Iron"
,"Sat"
,"Ferritin"
,"Class"))
data_dirty$Class <- as.factor(data_dirty$Class)


head(data_dirty)

summary(data_dirty)

summary(data_dirty)

str(data_dirty)

sapply(data_dirty, function(x) sum(is.na(x)))

library("mice")

# initializing the object with zero iterations 
# setting the methods for both categorical and numerical features 
init = mice(data_dirty, maxit=0, seed=123) 
meth = init$method

# importing categorical features 
meth[c("Gender"
,"Symptoms"
,"Alcohol"
,"HBsAg"
,"HBeAg"
,"HBcAb"
,"HCVAb"
,"Cirrhosis"
,"Endemic"
,"Smoking"
,"Diabetes"
,"Obesity"
,"Hemochro"
,"AHT"
,"CRI"
,"HIV"
,"NASH"
,"Varices"
,"Spleno"
,"PHT"
,"PVT"
,"Metastasis"
,"Hallmark"
,"PS"
,"Encephalopathy"
,"Ascites")] = "cart"

# importing numerical features 
meth[c("Age"
,"Grams_day"
,"Packs_year"
,"INR"
,"AFP"
,"Hemoglobin"
,"MCV"
,"Leucocytes"
,"Platelets"
,"Albumin"
,"Total_Bil"
,"ALT"
,"AST"
,"GGT"
,"ALP"
,"TP"
,"Creatinine"
,"Nodule"
,"Major_Dim"
,"Dir_Bil"
,"Iron"
,"Sat"
,"Ferritin")] = "mean"

# # restricting the imputation of class 
# meth[c("Class")] = ""

imputed <- mice(data_dirty, method = meth, seed = 123)

data_imputed <- complete(imputed)

sapply(data_imputed, function(x) sum(is.na(x)))

summary(data_imputed)

data_imputed$Class <- make.names(data_imputed$Class)
data_imputed$Class <- as.factor(data_imputed$Class) # needed for algorithms below
levels(data_imputed$Class)

# splitting the data
library(caret)

set.seed(123)
trainIndex = createDataPartition(y = data_imputed$Class, p=0.80, list=FALSE, times=1)

x_train <- data_imputed[trainIndex, ]
x_test  <- data_imputed[-trainIndex, ]

nrow(x_train)
nrow(x_test)

# cross validator 
ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary  
)

#setting seed to prevent randomness 
set.seed(123)

# the model training 
glmFit <- train(
  Class ~ .,
  data = x_train,
  method = "glmnet",
  preProc = c("center", "scale"),
  tuneLength = 20,
  trControl = ctrl,
  metric = "Sens"
)

glmFit

ggplot(glmFit)

glmClasses <- predict(glmFit, newdata = x_test)
str(glmClasses)

confusionMatrix(data = glmClasses, x_test$Class)
# levels(x_test$Class)

set.seed(123)

# Model
knnFit <- train(
  Class ~ .,
  data = x_train,
  method = "knn",
  preProc = c("center", "scale"),
  tuneLength = 60,
  trControl = ctrl,
  metric = "Sens")

knnFit

ggplot(knnFit)

knnPredict <- predict(knnFit,newdata = x_test)
confusionMatrix(knnPredict, x_test$Class)

set.seed(123)

# Model
nbFit <- train(
  Class ~ .,
  data = x_train,
  method = "naive_bayes",
  preProc = c("center", "scale"),
  trControl = ctrl,
  metric = "Sens")

nbFit

nbPredict <- predict(nbFit,newdata = x_test)
confusionMatrix(nbPredict, x_test$Class)


