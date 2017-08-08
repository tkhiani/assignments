# setting up the environment for spark context
# Livy
library(sparklyr)
sc <- spark_connect(master = "local")
library(dplyr)
iris_tbl <- copy_to(sc, iris)


# getting the data set
#http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data
#http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/

#Attribute Information:
#1) ID number 
#2) Diagnosis (M = malignant, B = benign) 3-32) 
#Ten real-valued features are computed for each cell nucleus: 
#a) radius (mean of distances from center to points on the perimeter) 
#b) texture (standard deviation of gray-scale values) 
#c) perimeter 
#d) area 
#e) smoothness (local variation in radius lengths) 
#f) compactness (perimeter^2 / area - 1.0) 
#g) concavity (severity of concave portions of the contour) 
#h) concave points (number of concave portions of the contour) 
#i) symmetry 
#j) fractal dimension ("coastline approximation" - 1)

bc_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", 
                      header = FALSE, 
                      sep = ",")
colnames(bc_data) <- c("sample_code_number", 
                       "clump_thickness", 
                       "uniformity_of_cell_size", 
                       "uniformity_of_cell_shape", 
                       "marginal_adhesion", 
                       "single_epithelial_cell_size", 
                       "bare_nuclei", 
                       "bland_chromatin", 
                       "normal_nucleoli", 
                       "mitosis", 
                       "classes")

bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))

#identify missing value from the data
bc_data[bc_data == "?"] <- NA

# how many NAs are in the data
length(which(is.na(bc_data)))

# how many samples would we loose, if we removed them?
nrow(bc_data)

nrow(bc_data[is.na(bc_data), ])


#Missing values are imputed with the mice package
# impute missing data
library(mice)

bc_data[,2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute, 1))

bc_data$classes <- as.factor(bc_data$classes)

# how many benign and malignant cases are there?
summary(bc_data$classes)

# exploratory data analysis
library(ggplot2)

ggplot(bc_data, aes(x = classes, fill = classes)) +
  geom_bar()

# response variable for regression
ggplot(bc_data, aes(x = clump_thickness)) +
  geom_histogram(bins = 10)

#do we really need a dimensionality reduction
install.packages("pcaGoPromoter")
install.packages("Biostrings")
install.packages("BiocGenerics")
install.packages("ellipse")
install.packages("XVector")
install.packages("S4Vectors")
install.packages("IRanges")

library(pcaGoPromoter)
library(Biostrings)
library(BiocGenerics)
library(ellipse)
library(XVector)
library(S4Vectors)
library(IRanges)

source("https://bioconductor.org/biocLite.R")
biocLite("pcaGoPromoter")

source("https://bioconductor.org/biocLite.R")
biocLite("zlibbioc")

# perform pca and extract scores
pcaOutput <- pca(t(bc_data[, -1]), printDropped = FALSE, scale = TRUE, center = TRUE)
pcaOutput2 <- as.data.frame(pcaOutput$scores)

# define groups for plotting
pcaOutput2$groups <- bc_data$classes

centroids <- aggregate(cbind(PC1, PC2) ~ groups, pcaOutput2, mean)

# prepare data for representation
conf.rgn  <- do.call(rbind, lapply(unique(pcaOutput2$groups), function(t)
  data.frame(groups = as.character(t),
             ellipse(cov(pcaOutput2[pcaOutput2$groups == t, 1:2]),
                     centre = as.matrix(centroids[centroids$groups == t, 2:3]),
                     level = 0.95),
             stringsAsFactors = FALSE)))

#visualization of pcs results
ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
  geom_polygon(data = conf.rgn, aes(fill = groups), alpha = 0.2) +
  geom_point(size = 2, alpha = 0.6) + 
  scale_color_brewer(palette = "Set1") +
  labs(color = "",
       fill = "",
       x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
       y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance")) 

# feature visualization
library(tidyr)
gather(bc_data, x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = classes, fill = classes)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

#using Machine Learning for parallel processing
# configure multicore
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
library(caret)

# training and validation dataset
set.seed(42)
index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]

# checking the distribution of variables by train and test
library(dplyr)

rbind(data.frame(group = "train", train_data),
      data.frame(group = "test", test_data)) %>%
  gather(x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

# running a GLM procedure
set.seed(42)
model_glm <- caret::train(clump_thickness ~ .,
                          data = train_data,
                          method = "glm",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 10, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))

# print model summary
model_glm

# generate predictions
predictions <- predict(model_glm, test_data)

# plotting the residuals
# model_glm$finalModel$linear.predictors == model_glm$finalModel$fitted.values
data.frame(residuals = resid(model_glm),
           predictors = model_glm$finalModel$linear.predictors) %>%
  ggplot(aes(x = predictors, y = residuals)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# y == train_data$clump_thickness
data.frame(residuals = resid(model_glm),
           y = model_glm$finalModel$y) %>%
  ggplot(aes(x = y, y = residuals)) +
  geom_jitter() +
  geom_smooth(method = "lm")

#actual vs. predicted
data.frame(actual = test_data$clump_thickness,
           predicted = predictions) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_jitter() +
  geom_smooth(method = "lm")

###############Classification Tree Based Model################
library(rpart)
library(rpart.plot)

set.seed(42)
fit <- rpart(classes ~ .,
             data = train_data,
             method = "class",
             control = rpart.control(xval = 10, 
                                     minbucket = 2, 
                                     cp = 0), 
             parms = list(split = "information"))

rpart.plot(fit, extra = 100)

##############random forest implementation###############
set.seed(42)
model_rf <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))
#savePredictions = TRUE, you can access the cross-validation resuls with model_rf$pred
model_rf$pred

# printing the confusion metrix
model_rf$finalModel$confusion

#extracting feature importance
imp <- model_rf$finalModel$importance
imp[order(imp, decreasing = TRUE), ]

#plotting variable importance
# estimate variable importance
importance <- varImp(model_rf, scale = TRUE)
plot(importance)

#predict the test data and print the confusion matrix
confusionMatrix(predict(model_rf, test_data), test_data$classes)

#printing actual vs. predicted results
results <- data.frame(actual = test_data$classes,
                      predict(model_rf, test_data, type = "prob"))

results$prediction <- ifelse(results$benign > 0.5, "benign",
                             ifelse(results$malignant > 0.5, "malignant", NA))

results$correct <- ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) +
  geom_bar(position = "dodge")

#using actual vs. predicted
ggplot(results, aes(x = prediction, y = benign, color = correct, shape = correct)) +
  geom_jitter(size = 3, alpha = 0.6)

#################XGBOOST Model#####################
#XGBoost is a tree ensemble model, which means the sum of predictions from a 
#set of classification and regression trees (CART). In that, XGBoost is similar 
#to Random Forests but it uses a different approach to model training. 
#Can be used for classification and regression tasks.
set.seed(42)
model_xgb <- caret::train(classes ~ .,
                          data = train_data,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 10, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))

#feature importance from xgboost
importance <- varImp(model_xgb, scale = TRUE)
plot(importance)

# predicting the test data
confusionMatrix(predict(model_xgb, test_data), test_data$classes)

# plotting actual vs. predicted data
results <- data.frame(actual = test_data$classes,
                      predict(model_xgb, test_data, type = "prob"))

results$prediction <- ifelse(results$benign > 0.5, "benign",
                             ifelse(results$malignant > 0.5, "malignant", NA))

results$correct <- ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) +
  geom_bar(position = "dodge")

#using ggplot2 generating actual vs. predicted
ggplot(results, aes(x = prediction, y = benign, color = correct, shape = correct)) +
  geom_jitter(size = 3, alpha = 0.6)

# feature selection
library(corrplot)

# calculate correlation matrix
corMatMy <- cor(train_data[, -1])
corrplot(corMatMy, order = "hclust")

#Apply correlation filter at 0.70,
highlyCor <- colnames(train_data[, -1])[findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)]

# which variables are flagged for removal?
highlyCor

#then we remove these variables
train_data_cor <- train_data[, which(!colnames(train_data) %in% highlyCor)]


#RFE- RFE uses a Random Forest algorithm to test combinations of features and 
#rate each with an accuracy score. The combination with the highest score is 
#usually preferential.
set.seed(7)
results_rfe <- rfe(x = train_data[, -1], 
                   y = train_data$classes, 
                   sizes = c(1:9), 
                   rfeControl = rfeControl(functions = rfFuncs, method = "cv", number = 10))

# chosen features
predictors(results_rfe)

#train data on RFE
train_data_rfe <- train_data[, c(1, which(colnames(train_data) %in% predictors(results_rfe)))]

#feature selection using Genetic Algorithm
#The Genetic Algorithm (GA) has been developed based on evolutionary principles of 
#natural selection: It aims to optimize a population of individuals with a given 
#set of genotypes by modeling selection over time. In each generation (i.e. 
#iteration), each individual’s fitness is calculated based on their genotypes. 
#Then, the fittest individuals are chosen to produce the next generation. This 
#subsequent generation of individuals will have genotypes resulting from (re-) 
#combinations of the parental alleles. These new genotypes will again determine 
#each individual’s fitness. This selection process is iterated for a specified 
#number of generations and (ideally) leads to fixation of the fittest alleles 
#in the gene pool.
#This concept of optimization can be applied to non-evolutionary models as well, 
#like feature selection processes in machine learning.
set.seed(27)
model_ga <- gafs(x = train_data[, -1], 
                 y = train_data$classes,
                 iters = 10, # generations of algorithm
                 popSize = 10, # population size for each generation
                 levels = c("malignant", "benign"),
                 gafsControl = gafsControl(functions = rfGA, # Assess fitness with RF
                                           method = "cv",    # 10 fold cross validation
                                           genParallel = TRUE, # Use parallel programming
                                           allowParallel = TRUE))

plot(model_ga) # Plot mean fitness (AUC) by generation

train_data_ga <- train_data[, c(1, which(colnames(train_data) %in% model_ga$ga$final))]

# auto tuning of model parameters: using Grid search with caret
set.seed(42)
model_rf_tune_auto <- caret::train(classes ~ .,
                                   data = train_data,
                                   method = "rf",
                                   preProcess = c("scale", "center"),
                                   trControl = trainControl(method = "repeatedcv", 
                                                            number = 10, 
                                                            repeats = 10, 
                                                            savePredictions = TRUE, 
                                                            verboseIter = FALSE,
                                                            search = "random"),
                                   tuneLength = 15)

model_rf_tune_auto

# accuracy by randomly selecting predictors
plot(model_rf_tune_auto)

# manual greed search algorithms
set.seed(42)
grid <- expand.grid(mtry = c(1:10))

model_rf_tune_man <- caret::train(classes ~ .,
                                  data = train_data,
                                  method = "rf",
                                  preProcess = c("scale", "center"),
                                  trControl = trainControl(method = "repeatedcv", 
                                                           number = 10, 
                                                           repeats = 10, 
                                                           savePredictions = TRUE, 
                                                           verboseIter = FALSE,
                                                           search = "random"),
                                  tuneGrid = grid)

model_rf_tune_man

# plotting the model accuracy

plot(model_rf_tune_man)

# Grid search with h2o: a deep learning library
library(h2o)
h2o.init(nthreads = -1)


bc_data_hf <- as.h2o(bc_data)


# descriptive statistics for exploratory data analysis
h2o.describe(bc_data_hf) %>%
  gather(x, y, Zeros:Sigma) %>%
  mutate(group = ifelse(x %in% c("Min", "Max", "Mean"), "min, mean, max", 
                        ifelse(x %in% c("NegInf", "PosInf"), "Inf", "sigma, zeros"))) %>% 
  ggplot(aes(x = Label, y = as.numeric(y), color = x)) +
  geom_point(size = 4, alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_grid(group ~ ., scales = "free") +
  labs(x = "Feature",
       y = "Value",
       color = "")

#for reshaping and plotting
library(reshape2) # for melting

bc_data_hf[, 1] <- h2o.asfactor(bc_data_hf[, 1])

cor <- h2o.cor(bc_data_hf)
rownames(cor) <- colnames(cor)

melt(cor) %>%
  mutate(Var2 = rep(rownames(cor), nrow(cor))) %>%
  mutate(Var2 = factor(Var2, levels = colnames(cor))) %>%
  mutate(variable = factor(variable, levels = colnames(cor))) %>%
  ggplot(aes(x = variable, y = Var2, fill = value)) + 
  geom_tile(width = 0.9, height = 0.9) +
  scale_fill_gradient2(low = "white", high = "red", name = "Cor.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "", 
       y = "")

#train, test and validation using h2o
splits <- h2o.splitFrame(bc_data_hf, 
                         ratios = c(0.7, 0.15), 
                         seed = 1)

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

response <- "classes"
features <- setdiff(colnames(train), response)

#summary
summary(train$classes, exact_quantiles = TRUE)

summary(valid$classes, exact_quantiles = TRUE)

summary(test$classes, exact_quantiles = TRUE)

# running PCA to reduce features
pca <- h2o.prcomp(training_frame = train,
                  x = features,
                  validation_frame = valid,
                  transform = "NORMALIZE",
                  impute_missing = TRUE,
                  k = 3,
                  seed = 42)

# eigen value computation and representation
eigenvec <- as.data.frame(pca@model$eigenvectors)
eigenvec$label <- features

library(ggrepel)
ggplot(eigenvec, aes(x = pc1, y = pc2, label = label)) +
  geom_point(color = "navy", alpha = 0.7) +
  geom_text_repel()

########## running random forest using h2o################
# setting up hyper parameters
hyper_params <- list(
  ntrees = c(25, 50, 75, 100),
  max_depth = c(10, 20, 30),
  min_rows = c(1, 3, 5)
)

#search criteria 
search_criteria <- list(
  strategy = "RandomDiscrete", 
  max_models = 50,
  max_runtime_secs = 360,
  stopping_rounds = 5,          
  stopping_metric = "AUC",      
  stopping_tolerance = 0.0005,
  seed = 42
)

# using grid search hyper parameter selection
rf_grid <- h2o.grid(algorithm = "randomForest", # h2o.randomForest, 
                    # alternatively h2o.gbm 
                    # for Gradient boosting trees
                    x = features,
                    y = response,
                    grid_id = "rf_grid",
                    training_frame = train,
                    validation_frame = valid,
                    nfolds = 25,                           
                    fold_assignment = "Stratified",
                    hyper_params = hyper_params,
                    search_criteria = search_criteria,
                    seed = 42
)

#### error minimization approach to select models
# performance metrics where smaller is better -> order with decreasing = FALSE
sort_options_1 <- c("mean_per_class_error", "mse", "err", "logloss")

for (sort_by_1 in sort_options_1) {
  
  grid <- h2o.getGrid("rf_grid", sort_by = sort_by_1, decreasing = FALSE)
  
  model_ids <- grid@model_ids
  best_model <- h2o.getModel(model_ids[[1]])
  
  h2o.saveModel(best_model, path="models", force = TRUE)
  
}

# accuracy maximization approach to select best models
# performance metrics where bigger is better -> order with decreasing = TRUE
sort_options_2 <- c("auc", "precision", "accuracy", "recall", "specificity")

for (sort_by_2 in sort_options_2) {
  
  grid <- h2o.getGrid("rf_grid", sort_by = sort_by_2, decreasing = TRUE)
  
  model_ids <- grid@model_ids
  best_model <- h2o.getModel(model_ids[[1]])
  
  h2o.saveModel(best_model, path = "models", force = TRUE)
  
}

# saving the models
files <- list.files(path = "models")
rf_models <- files[grep("rf_grid_model", files)]

for (model_id in rf_models) {
  
  path <- paste0("C:/Documents/models/", model_id)
  best_model <- h2o.loadModel(path)
  mse_auc_test <- data.frame(model_id = model_id, 
                             mse = h2o.mse(h2o.performance(best_model, test)),
                             auc = h2o.auc(h2o.performance(best_model, test)))
  
  if (model_id == rf_models[[1]]) {
    
    mse_auc_test_comb <- mse_auc_test
    
  } else {
    
    mse_auc_test_comb <- rbind(mse_auc_test_comb, mse_auc_test)
    
  }
}

# measuring mean square error
mse_auc_test_comb %>%
  gather(x, y, mse:auc) %>%
  ggplot(aes(x = model_id, y = y, fill = model_id)) +
  facet_grid(x ~ ., scales = "free") +
  geom_bar(stat = "identity", alpha = 0.8, position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = unit(c(0.5, 0, 0, 1.5), "cm")) +
  labs(x = "", y = "value", fill = "")


#####finding out the best models
for (model_id in rf_models) {
  
  best_model <- h2o.getModel(model_id)
  
  finalRf_predictions <- data.frame(model_id = rep(best_model@model_id, 
                                                   nrow(test)),
                                    actual = as.vector(test$classes), 
                                    as.data.frame(h2o.predict(object = best_model, 
                                                              newdata = test)))
  
  finalRf_predictions$accurate <- ifelse(finalRf_predictions$actual == 
                                           finalRf_predictions$predict, 
                                         "yes", "no")
  
  finalRf_predictions$predict_stringent <- ifelse(finalRf_predictions$benign > 0.8, 
                                                  "benign", 
                                                  ifelse(finalRf_predictions$malignant 
                                                         > 0.8, "malignant", "uncertain"))
  
  finalRf_predictions$accurate_stringent <- ifelse(finalRf_predictions$actual == 
                                                     finalRf_predictions$predict_stringent, "yes", 
                                                   ifelse(finalRf_predictions$predict_stringent == 
                                                            "uncertain", "na", "no"))
  
  if (model_id == rf_models[[1]]) {
    
    finalRf_predictions_comb <- finalRf_predictions
    
  } else {
    
    finalRf_predictions_comb <- rbind(finalRf_predictions_comb, finalRf_predictions)
    
  }
}

#####generating predictions################
finalRf_predictions_comb %>%
  ggplot(aes(x = actual, fill = accurate)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ model_id, ncol = 3) +
  labs(fill = "Were\npredictions\naccurate?",
       title = "Default predictions")

#####stringent predictions##############
finalRf_predictions_comb %>%
  subset(accurate_stringent != "na") %>%
  ggplot(aes(x = actual, fill = accurate_stringent)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ model_id, ncol = 3) +
  labs(fill = "Were\npredictions\naccurate?",
       title = "Stringent predictions")

###load the best model
rf_model <- h2o.loadModel("models/rf_grid_model_6")

###print variable importance
h2o.varimp_plot(rf_model)

#h2o.varimp(rf_model)

# print error matrix for train, test and validation
h2o.mean_per_class_error(rf_model, train = TRUE, valid = TRUE, xval = TRUE)

###h2o confusion matrix
h2o.confusionMatrix(rf_model, valid = TRUE)

##plot the scoring model
plot(rf_model,
     timestep = "number_of_trees",
     metric = "classification_error")

###scoring history
plot(rf_model,
     timestep = "number_of_trees",
     metric = "logloss")

# scoring history
plot(rf_model,
     timestep = "number_of_trees",
     metric = "AUC")

# scoring history
plot(rf_model,
     timestep = "number_of_trees",
     metric = "rmse")

###print area under the curve
h2o.auc(rf_model, train = TRUE)
h2o.auc(rf_model, valid = TRUE)
h2o.auc(rf_model, xval = TRUE)

# model performance measurement
perf <- h2o.performance(rf_model, test)
perf

# plot performance
plot(perf)

h2o.logloss(perf)

h2o.mse(perf)

h2o.auc(perf)

head(h2o.metric(perf))

####final predictions
finalRf_predictions <- data.frame(actual = as.vector(test$classes), 
                                  as.data.frame(h2o.predict(object = rf_model, 
                                                            newdata = test)))


###accuracy for final prediction#########
finalRf_predictions$accurate <- ifelse(finalRf_predictions$actual == 
                                         finalRf_predictions$predict, "yes", "no")

finalRf_predictions$predict_stringent <- ifelse(finalRf_predictions$benign > 0.8, "benign", 
                                                ifelse(finalRf_predictions$malignant 
                                                       > 0.8, "malignant", "uncertain"))
finalRf_predictions$accurate_stringent <- ifelse(finalRf_predictions$actual == 
                                                   finalRf_predictions$predict_stringent, "yes", 
                                                 ifelse(finalRf_predictions$predict_stringent == 
                                                          "uncertain", "na", "no"))

finalRf_predictions %>%
  group_by(actual, predict) %>%
  dplyr::summarise(n = n())

###final prediction summary
finalRf_predictions %>%
  group_by(actual, predict_stringent) %>%
  dplyr::summarise(n = n())

###final random forest prediction
finalRf_predictions %>%
  ggplot(aes(x = actual, fill = accurate)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "Were\npredictions\naccurate?",
       title = "Default predictions")

###final random forest stringent prediction
finalRf_predictions %>%
  subset(accurate_stringent != "na") %>%
  ggplot(aes(x = actual, fill = accurate_stringent)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "Were\npredictions\naccurate?",
       title = "Stringent predictions")

#############################################################################
##############Sparklyr implementation########################################
#############################################################################
library(sparklyr)
spark_install(version = "2.0.0")

# local instance
library(sparklyr)
sc <- spark_connect(master = "local", version = "2.0.0")

####theme preparation for graphing and charting
library(tidyr)
library(ggplot2)
library(ggrepel)
library(dplyr)

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      legend.position = "right",
      legend.justification = "top", 
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

###############data set##############
#This dataset is part of the fivethirtyeight package and provides scores for how 
#each person rated their preference of the dishes from several countries. 
#The following categories could be chosen:
#5: I love this country’s traditional cuisine. I think it’s one of the best in the world.
#4: I like this country’s traditional cuisine. I think it’s considerably above average.
#3: I’m OK with this county’s traditional cuisine. I think it’s about average.
#2: I dislike this country’s traditional cuisine. I think it’s considerably below average.
#1: I hate this country’s traditional cuisine. I think it’s one of the worst in the world.
#N/A: I’m unfamiliar with this country’s traditional cuisine.

library(fivethirtyeight)

food_world_cup[food_world_cup == "N/A"] <- NA
food_world_cup[, 9:48][is.na(food_world_cup[, 9:48])] <- 0
food_world_cup$gender <- as.factor(food_world_cup$gender)
food_world_cup$location <- as.factor(food_world_cup$location)


#with machine learning we are trying whether the preference for a country’s 
#cuisine can be predicted based on preferences of other countries’ cuisines, 
#general knowledge and interest in different cuisines, age, gender, income, 
#education level and/ or location.

# calculating percentages per category and country
percentages <- food_world_cup %>%
  select(algeria:vietnam) %>%
  gather(x, y) %>%
  group_by(x, y) %>%
  summarise(n = n()) %>%
  mutate(Percent = round(n / sum(n) * 100, digits = 2))

# rename countries & plot
percentages %>%
  mutate(x_2 = gsub("_", " ", x)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(x = "", y = Percent, fill = y)) + 
  geom_bar(width = 1, stat = "identity") + 
  theme_minimal() +
  coord_polar("y", start = 0) +
  facet_wrap(~ x_2, ncol = 8) +
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "")

#Imputing missing values
library(mice)

dataset_impute <- mice(food_world_cup[, -c(1, 2)],  print = FALSE)
food_world_cup <- cbind(food_world_cup[, 2, drop = FALSE], mice::complete(dataset_impute, 1))

###data transformation
food_world_cup[8:47] <- lapply(food_world_cup[8:47], as.numeric)

countries <- paste(colnames(food_world_cup)[-c(1:7)])

for (response in countries) {
  food_world_cup[paste(response, "trans", sep = "_")] <- food_world_cup[response] / mean(food_world_cup[food_world_cup[response] > 0, response])
}

###plotting the distribution of transformed values
food_world_cup %>%
  gather(x, y, algeria_trans:vietnam_trans) %>%
  mutate(x_2 = gsub("_trans", "", x)) %>%
  mutate(x_2 = gsub("_", " ", x_2)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(y)) +
  geom_density(fill = "navy", alpha = 0.7) +
  my_theme() + 
  facet_wrap(~ x_2, ncol = 8) +
  labs(x = "transformed preference")

# Hypothesis: whether we see a gender bias in preference for some country’s cuisines
food_world_cup_gather <- food_world_cup %>%
  collect %>%
  gather(country, value, algeria:vietnam)

food_world_cup_gather$value <- as.numeric(food_world_cup_gather$value)
food_world_cup_gather$country <- as.factor(food_world_cup_gather$country)

####plotting the preferences by geneder
food_world_cup_gather <- food_world_cup_gather %>%
  mutate(x_2 = gsub("_", " ", country)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2))

order <- aggregate(food_world_cup_gather$value, by = list(food_world_cup_gather$x_2), FUN = sum)

food_world_cup_gather %>%
  mutate(x_2 = factor(x_2, levels = order$Group.1[order(order$x, decreasing = TRUE)])) %>%
  ggplot(aes(x = x_2, y = value, fill = gender)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(fill = "Gender",
       x = "",
       y = "sum of preferences")

###mean preferences by gender
food_world_cup %>%
  collect %>%
  mutate_each_(funs(as.numeric), countries) %>%
  group_by(gender) %>%
  summarise_each_(funs(mean), countries) %>%
  summarise_each_(funs(diff), countries) %>%
  gather(x, y) %>%
  mutate(x_2 = gsub("_", " ", x)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(x = x_2, y = y)) +
  geom_bar(stat = "identity", fill = "navy", alpha = 0.7) +
  my_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "",
       y = "difference\nbetween gender")

####putting the dataset to spark
food_world_cup <- copy_to(sc, food_world_cup)


###Hypothesis: where the countries fall on a 2-dimensional plane of the first 
#two principal components.

# run PCA
pca <- food_world_cup %>%
  mutate_each_(funs(as.numeric), countries) %>%
  ml_pca(features = paste(colnames(food_world_cup)[-c(1:47)]))

###plotting PCA results
library(tibble)
as.data.frame(pca$components) %>%
  rownames_to_column(var = "labels") %>%
  mutate(x_2 = gsub("_trans", "", labels)) %>%
  mutate(x_2 = gsub("_", " ", x_2)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(x = PC1, y = PC2, color = x_2, label = x_2)) + 
  geom_point(size = 2, alpha = 0.6) +
  geom_text_repel() +
  labs(x = paste0("PC1: ", round(pca$explained.variance[1], digits = 2) * 100, "% variance"),
       y = paste0("PC2: ", round(pca$explained.variance[2], digits = 2) * 100, "% variance")) +
  my_theme() + 
  guides(fill = FALSE, color = FALSE)

####Preparing the data for machine learning
food_world_cup <- tbl(sc, "food_world_cup") %>%
  ft_string_indexer(input_col = "interest", output_col = "interest_idx") %>%
  ft_string_indexer(input_col = "gender", output_col = "gender_idx") %>%
  ft_string_indexer(input_col = "age", output_col = "age_idx") %>%
  ft_string_indexer(input_col = "household_income", output_col = "household_income_idx") %>%
  ft_string_indexer(input_col = "education", output_col = "education_idx") %>%
  ft_string_indexer(input_col = "location", output_col = "location_idx") %>%
  ft_string_indexer(input_col = "knowledge", output_col = "knowledge_idx")

###divide the data into training and test sets using the sdf_partition() function
partitions <- food_world_cup %>%
  sdf_partition(training = 0.75, test = 0.25, seed = 753)

##Random Forest algorithm to predict each country’s preference based on all other countries’ preferences and the demographic information
library(lazyeval)

for (response in countries) {
  
  features <- colnames(partitions$training)[-grep(response, colnames(partitions$training))]
  features <- features[grep("_trans|_idx", features)]
  
  fit <- partitions$training %>%
    filter_(interp(~ var > 0, var = as.name(response))) %>%
    ml_random_forest(intercept = FALSE, response = response, features = features, type = "classification")
  
  feature_imp <- ml_tree_feature_importance(sc, fit)
  
  features <- as.character(feature_imp[1:10, 2])
  
  fit <- partitions$training %>%
    filter_(interp(~ var > 0, var = as.name(response))) %>%
    ml_random_forest(intercept = FALSE, response = response, features = features, type = "classification")
  
  partitions$test <- partitions$test %>%
    filter_(interp(~ var > 0, var = as.name(response)))
  
  pred <- sdf_predict(fit, partitions$test) %>%
    collect
  
  pred_2 <- as.data.frame(table(pred[[response]], pred$prediction))
  pred_2$response <- response
  
  pred_sc <- select(pred, -rawPrediction, -probability)
  pred_sc <- copy_to(sc, pred_sc, overwrite = TRUE)
  
  feature_imp$response <- response
  
  f1 <- ml_classification_eval(pred_sc, response, "prediction", metric = "f1")
  wP <- ml_classification_eval(pred_sc, response, "prediction", metric = "weightedPrecision")
  wR <- ml_classification_eval(pred_sc, response, "prediction", metric = "weightedRecall")
  
  ml_eval <- data.frame(response = response,
                        f1 = f1,
                        weightedPrecision = wP,
                        weightedRecall = wR)
  
  if (response == "algeria") {
    feature_imp_df <- feature_imp
    ml_eval_df <- ml_eval
    pred_df <- pred_2
  } else {
    feature_imp_df <- rbind(feature_imp_df, feature_imp)
    ml_eval_df <- rbind(ml_eval_df, ml_eval)
    pred_df <- rbind(pred_df, pred_2)
  }
}

####model evaluation using precision, recall and F1 score
results <- ml_eval_df %>%
  mutate(x_2 = gsub("_", " ", response)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2))

order <- results$x_2[order(results$f1, decreasing = TRUE)]

gather(results, x, y, f1:weightedRecall) %>%
  mutate(x_2 = factor(x_2, levels = order)) %>%
  ggplot(aes(x = x_2, y = y, fill = x)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_fill_brewer(palette = "Set1") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(fill = "", color = "", x = "", y = "value")

###To see what features had been used in more and less successful models
as.data.frame(food_world_cup) %>%
  select_(.dots = c("spain", as.character(feats$feature))) %>%
  gather(x, y, -spain) %>%
  filter(spain > 0) %>%
  group_by(spain, x) %>%
  summarise(mean = mean(y)) %>%
  mutate(x_2 = gsub("_trans", "", x)) %>%
  mutate(x_2 = gsub("_idx", "", x_2)) %>%
  mutate(x_2 = gsub("_", " ", x_2)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(x = x_2, y = spain, fill = mean)) +
  geom_tile(width = 0.9, height = 0.9) +
  scale_fill_gradient2(low = "white", high = "red",  name = "mean") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "Spain")

###feature impportance for greece
feats <- feature_imp_df %>%
  filter(response == "greece") %>%
  slice(1:10)

as.data.frame(food_world_cup) %>%
  select_(.dots = c("greece", as.character(feats$feature))) %>%
  gather(x, y, -greece) %>%
  filter(greece > 0) %>%
  group_by(greece, x) %>%
  summarise(mean = mean(y)) %>%
  mutate(x_2 = gsub("_trans", "", x)) %>%
  mutate(x_2 = gsub("_idx", "", x_2)) %>%
  mutate(x_2 = gsub("_", " ", x_2)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(x = x_2, y = greece, fill = mean)) +
  geom_tile(width = 0.9, height = 0.9) +
  scale_fill_gradient2(low = "white", high = "red",  name = "mean") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "Greece")

## feature importance for italy
feats <- feature_imp_df %>%
  filter(response == "italy") %>%
  slice(1:10)

as.data.frame(food_world_cup) %>%
  select_(.dots = c("italy", as.character(feats$feature))) %>%
  gather(x, y, -italy) %>%
  filter(italy > 0) %>%
  group_by(italy, x) %>%
  summarise(mean = mean(y)) %>%
  mutate(x_2 = gsub("_trans", "", x)) %>%
  mutate(x_2 = gsub("_idx", "", x_2)) %>%
  mutate(x_2 = gsub("_", " ", x_2)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(x = x_2, y = italy, fill = mean)) +
  geom_tile(width = 0.9, height = 0.9) +
  scale_fill_gradient2(low = "white", high = "red",  name = "mean") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "Italy")



