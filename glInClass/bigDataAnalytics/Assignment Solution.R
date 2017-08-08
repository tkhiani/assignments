source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

# Show the structure (ls)
h5ls("./largeDataSets/train.h5/train.h5")


# Reading file
data <- h5read("./largeDataSets/train.h5/train.h5","train")
data_column_names <- data[[5]]
data_column_names <- unlist(data_column_names)

data_matrix <- data[[6]]
data_matrix <- t(data_matrix)

colnames(data_matrix) <- data_column_names

library(data.table)
train <- data.table(data_matrix)

library(dplyr)

df_train = copy_to(sc,
        data.table(t(data.matrix=(unlist(h5read("./largeDataSets/train.h5/train.h5","train"))[[6]]))))
#write.csv(train,"C:/train.csv")

sessionInfo()

###############Data Analysis###########
sum(is.na(train$derived_0))/nrow(train)*100
mid<-round(sapply(train,function(x)sum(is.na(x)))/nrow(train)*100,1)
sort(mid)
# if the missing % is < 15% then we are going to impute the values, we will remove the 
# column from the model dataset

# Approach 1
# remove the features with more than 15% data missing
# prepare a new train dataset
library(dplyr)
df_train1<-select(train,-fundamental_40,-fundamental_27,-fundamental_15,-fundamental_29,-fundamental_30,
                  -fundamental_43,-fundamental_13,-fundamental_14,-fundamental_16,
                  -fundamental_37,-fundamental_44,-fundamental_46,-fundamental_50,
                  -fundamental_60,-fundamental_23,-fundamental_2,-fundamental_11,
                  -fundamental_55,-fundamental_56,-fundamental_8,-fundamental_63,
                  -fundamental_39,-fundamental_54,-derived_2,-derived_4,-fundamental_35,
                  -fundamental_34,-fundamental_47,-fundamental_51,-fundamental_3, 
                  -fundamental_31,-fundamental_22,-fundamental_49,-fundamental_9,
                  -fundamental_24,-fundamental_26,-fundamental_57,-fundamental_28,
                  -fundamental_61,-fundamental_1,-fundamental_6,-fundamental_38,-fundamental_5)
library(mice)
df_train1.1 = mice(df_train1, meth ='sample', seed =111)
mean(df_train1$derived_0,na.rm = T)

df2<-as.data.frame(apply(df_train1,2,function(x){mean(x,na.rm = T)}))

plot(density(df_train1$derived_0,na.rm = T))

# Approach 2
# remove the rows with missing values and create another cleaned dataset
df_train2<-na.omit(train)

# we will go ahead with approach 2 for now, same can be applied on approach 1 
####

# split the dataset into train and test
sample_size <- floor(0.80*nrow(df_train2))

set.seed(123)
train_ind <- sample(seq_len(nrow(df_train2)),size = sample_size)

train = df_train2[train_ind,]
test = df_train2[-train_ind,]

# since the y variable is continuous we should take regression as a method
# for predcition

names(train)

# Linear regression

# assumptions: in order to produce good accuracy your regression model
# should follow the assumptions listed below

#1- the residuals should follow a normal distribution
#2- no multicollineaerity in the predictors
#3- no heteroscedasticity
#4- linearity
#5- no autocorrelation

# model
fit1<- lm(train$y~.,data=train)
summary(fit1)
fit1.1<-step(fit1,direction = "both")

# robust regression is ruled out
library(MASS)
fit2<-rlm(train$y~.,data=train)
summary(fit2)

#Ridge regression (regularized regression method)
# variable subsample for testing purpose only (offline apply it on all obs)
subsample <- train[sample(1:nrow(train),20000,replace=F),]

library(caret)
library(glmnet)
library(elasticnet)

train_control<-trainControl(method='repeatedcv',
                            number=4,
                            repeats=4,
                            verboseIter=F)

lambda <- seq(from=1,to=0,by=-0.001)

ridge_model <- train(x=subsample[,1:108],y=subsample$y,
                     method='glmnet',
                     metric='Rsquared',
                     maximize=T,
                     trControl=train_control,
                     tuneGrid=expand.grid(alpha=0,
                                          lambda=lambda))

#accuracy
mean(ridge_model$resample$Rsquared)

sparklyr::spark_connect(master = 'local')
