# Install Sparklyr library
library(sparklyr)
library(dplyr)
library(pryr)
library(ggplot2)
library(tidyr)
library(tidyverse)


#Install Spark
#spark_install(version = "2.1.0")
sc <- spark_connect(master = "local")

##################################################################
# no.of.classes * no.of.variables * 50 -- the least no. of records
##### If you get Java error set the following, and make sure######
#> Sys.getenv()
#HADOOP_HOME              C:\Users\Dell\AppData\Local\rstudio\spark\Cache\spark-2.1.0-bin-hadoop2.7\tmp\hadoop
#HOME                     C:/Users/Dell/Documents
#HOMEDRIVE                C:
#  HOMEPATH                 \Users\Dell
#JAVA_HOME                C:\Program Files\Java\jdk1.7.0_60
#LOCALAPPDATA             C:\Users\Dell\AppData\Local
#LOGONSERVER              \\DELL-PC
#NUMBER_OF_PROCESSORS     4
#OS                       Windows_NT
#PATH                     C:\Program
#Files\R\R-3.4.1\bin\i386;C:\GTK\bin;C:\Program
#Files\Common Files\Microsoft Shared\Windows
#Live;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Program
#Files\WIDCOMM\Bluetooth Software\;C:\Program
#Files\Google\Google Apps Migration\;C:\Program
#Files\SAS\SharedFiles\Formats;C:\Program
#Files\Windows Live\Shared;C:\Program
#Files\Google\Google Apps Sync\;C:\Program
#Files\Java\jdk1.7.0_60\jre\bin;C:\Users\Dell\Anaconda2;C:\Users\Dell\Anaconda2\Scripts;C:\Users\Dell\Anaconda2\Library\bin;C:\Program
#Files\TextAI\VisualText\BIN;1234567890;C:\Python27;D:\syslibs;C:\Python27\Scripts;C:\Program
#Files\SSH Communications Security\SSH Secure
#Shell; C:\Program
#Files\Java\jdk1.7.0_60\bin;C:\Program
#Files\R\R-3.2.1\bin
#PATHEXT                  .COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC
#PROCESSOR_ARCHITECTURE   x86
#PROCESSOR_IDENTIFIER     x86 Family 6 Model 42 Stepping 7, GenuineIntel
#PROCESSOR_LEVEL          6
#PROCESSOR_REVISION       2a07
#ProgramData              C:\ProgramData
#ProgramFiles             C:\Program Files
#PSModulePath             C:\Windows\system32\WindowsPowerShell\v1.0\Modules\
#PUBLIC                   C:\Users\Public
#R_ARCH                   /i386
#R_COMPILED_BY            gcc 4.9.3
#R_DOC_DIR                C:/PROGRA~1/R/R-34~1.1/doc
#R_HOME                   C:/PROGRA~1/R/R-34~1.1
#R_LIBS_USER              C:/Users/Dell/Documents/R/win-library/3.4
#R_USER                   C:/Users/Dell/Documents
#RSTUDIO_WINUTILS         C:/Program Files/RStudio/bin/winutils
##################################################################

spark_home_dir()
spark_installed_versions()

# sc <- spark_connect(master = "local", 
#                    spark_home=spark_home_dir(version = "2.1.0"))

#Sys.getenv()

# Install dplyr, it is the backend for Sparklyr pipeline


#load a sample dataset to Spark Connect
iris_tbl <- copy_to(sc, iris, overwrite = TRUE)


#R requires that all your data be analyzed in memory (RAM), on a single machine
#Spark is an open source cluster computing platform. That means that you can 
#spread your data and your computations across multiple machines, effectively 
#letting you analyze an unlimited amount of data. The two technologies 
#complement each other strongly. By using R and Spark together you can write 
#code fast and run code fast

#Spark is still a very new technology, and some niceties like clear error 
#messages aren't there yet. So when things go wrong, it can be hard to 
#understand why.

#sparklyr is newer, and doesn't have a full set of features. There are some 
#things that you just can't do with Spark from R right now. The Scala and 
#Python interfaces to Spark are more mature.

#sparklyr converts your R code into SQL code before passing it to Spark.

#The typical workflow has three steps:
#1-Connect to Spark using spark_connect().
#2-Do some work.
#3-Close the connection to Spark using spark_disconnect()

#sparklyr has some functions such as spark_read_csv() that will read a CSV 
#file into Spark, here is one example that you can try
train <-spark_read_csv(sc,'train','./data/titanic/train.csv',
                       header=T,delimiter = ',')
#copying data using copy_to function of dplyr is a fundamentally slow process.

#You can see a list of all the data frames stored in Spark using 
src_tbls(sc)

####Mini Task####
## Load dplyr

# Explore track_metadata structure

# Connect to your Spark cluster

# Copy data to Spark

# List the data frames available in Spark

####Solution######

# Explore track_metadata structure
# str(data)

# Connect to your Spark cluster
# spark_conn <- spark_connect("local")

# Copy track_metadata to Spark
# track_data_tbl <- copy_to(spark_conn, data)

# List the data frames available in Spark
src_tbls(sc)

# when you copied the data to Spark, copy_to() returned a value. This return 
#value is a special kind of tibble() that doesn't contain any data of its own.
#To explain this, you need to know a bit about the way that tidyverse 
#packages store data. Tibbles are usually just a variant of data.frames 
#that have a nicer print method. However, dplyr also allows them to store 
#data from a remote data source, such as databases, and – as is the case 
#here – Spark. For remote datasets, the tibble object simply stores a 
#connection to the remote data.

#Calling tbl() with a Spark connection, and a string naming the Spark data 
#frame will return the same tibble object that was returned when you used 
#copy_to().

#A useful tool that you will see in this exercise is the object_size() 
#function from the pryr package. This shows you how much memory an object 
#takes up.

# See how big the dataset is
dim(train)
# See how small the tibble is

object_size(sc)
object_size(train)

#If you try to print a tibble that describes data stored in Spark, some 
#magic has to happen, since the tibble doesn't keep a copy of the data 
#itself. The magic is that the print method uses your Spark connection, 
#copies some of the contents back to R, and displays those values as though 
#the data had been stored locally. As you saw earlier in the chapter, 
#copying data is a slow operation, so by default, only 10 rows and as 
#many columns will fit onscreen, are printed.

#If you want to see a summary of what each column contains in the dataset 
#that the tibble refers to, you need to call glimpse()
# Print 5 rows, all columns
print(train, n = 5, width = Inf)

# Examine structure of tibble
str(train)

# Examine structure of data; number of rows is not right
glimpse(train)

#The easiest way to manipulate data frames stored in Spark is to use dplyr 
#syntax. let's look at the key functionality of the dplyr package in R

# on a Spark cluster with huge volume of data to manipulate you need to use


#dplyr has five main actions that you can perform on a data frame. 
# load packages
suppressMessages(library(dplyr))

library(nycflights13)

#putting the flights data into Spark Connect
flights_tbl <- copy_to(sc, nycflights13::flights, "flights",
                       overwrite = TRUE)

# besides just using select() to pick columns...
flights_tbl %>% select(carrier, flight)

# ...you can use the minus sign to hide columns
flights_tbl %>% select(-month, -day)

# hide a range of columns
flights_tbl %>% select(-(dep_time:arr_delay))

# hide any column with a matching name
flights_tbl %>% select(-contains("time"))

# pick columns using a character vector of column names
cols <- c("carrier", "flight", "tailnum")
flights_tbl %>% select(one_of(cols))

# select() can be used to rename columns, though all columns not mentioned are dropped
flights_tbl %>% select(tail = tailnum)

# rename() does the same thing, except all columns not mentioned are kept
flights_tbl %>% rename(tail = tailnum)

# filter() supports the use of multiple conditions
flights_tbl %>% filter(dep_time >= 600, dep_time <= 605)

# between() is a concise alternative for determing if numeric values fall in a range
flights_tbl %>% filter(between(dep_time, 600, 605))

# side note: is.na() can also be useful when filtering
flights_tbl %>% filter(!is.na(dep_time))

# slice() filters rows by position
flights_tbl %>% slice(1000:1005) #not supported for this version

# keep the first three rows within each group
flights_tbl %>% group_by(month, day) %>% slice(1:3) #may not work

# sample three rows from each group
flights_tbl %>% group_by(month, day) %>% sample_n(3)

# keep three rows from each group with the top dep_delay
flights_tbl %>% group_by(month, day) %>% top_n(3, dep_delay) %>% select(year, month, day, dep_delay, distance) %>% arrange(desc(dep_delay))

# also sort by dep_delay within each group
flights_tbl %>% group_by(month, day) %>% top_n(3, dep_delay) %>% 
  arrange(desc(dep_delay))

# unique rows can be identified using unique() from base R
# flights_tbl %>% select(origin, dest) %>% unique() # does not work as desired

# dplyr provides an alternative that is more "efficient"
flights_tbl %>% select(origin, dest) %>% distinct()

# when chaining, you don't have to include the parentheses if there are no arguments
flights_tbl %>% select(origin, dest) %>% distinct

# mutate() creates a new variable (and keeps all existing variables)
flights_tbl %>% mutate(speed = distance/air_time*60)

# transmute() only keeps the new variables
flights_tbl %>% transmute(speed = distance/air_time*60)

# example data frame with row names
flights_tbl %>% head()

# add_rownames() turns row names into an explicit variable
#library(tibble)
#flights %>% rownames_to_column("model") %>% head()

# summarise() can be used to count the number of rows in each group
flights_tbl %>% group_by(month) %>% summarise(cnt = n())

# tally() and count() can do this more concisely
flights_tbl %>% group_by(month) %>% tally()
flights_tbl %>% count(month)

# you can sort by the count
flights_tbl %>% group_by(month) %>% summarise(cnt = n()) %>% arrange(desc(cnt))

# tally() and count() have a sort parameter for this purpose
flights_tbl %>% group_by(month) %>% tally(sort=TRUE)
flights_tbl %>% count(month, sort=TRUE)

# you can sum over a specific variable instead of simply counting rows
flights_tbl %>% group_by(month) %>% summarise(dist = sum(distance))

# tally() and count() have a wt parameter for this purpose
flights_tbl %>% group_by(month) %>% tally(wt = distance)
flights_tbl %>% count(month, wt = distance)

# group_size() returns the counts as a vector
flights_tbl %>% group_by(month) %>% group_size()

# n_groups() simply reports the number of groups
flights_tbl %>% group_by(month) %>% n_groups()

# group by two variables, summarise, arrange (output is possibly confusing)
flights_tbl %>% group_by(month, day) %>% summarise(cnt = n()) %>% 
  arrange(desc(cnt)) %>% print(n = 40)

# ungroup() before arranging to arrange across all groups
flights_tbl %>% group_by(month, day) %>% summarise(cnt = n()) %>% 
  ungroup() %>% arrange(desc(cnt))

# data_frame() example
data_frame(a = 1:6, b = a*2, c = 'string', 'd+e' = 1) %>% glimpse()

# data.frame() example
data.frame(a = 1:6, c = 'string', 'd+e' = 1) %>% glimpse()

# create two simple data frames
(a <- data_frame(color = c("green","yellow","red"), num = 1:3))
(b <- data_frame(color = c("green","yellow","pink"), size = c("S","M","L")))

a_tbl <- copy_to(sc, a)
b_tbl <- copy_to(sc, b)

# only include observations found in both "a" and "b" (automatically joins on variables that appear in both tables)
inner_join(a_tbl, b_tbl)

# include observations found in either "a" or "b"
full_join(a_tbl, b_tbl)

# include all observations found in "a"
left_join(a_tbl, b_tbl)

# include all observations found in "b"
right_join(a_tbl, b_tbl)

# right_join(a, b) is identical to left_join(b, a) except for column ordering
left_join(b_tbl, a_tbl)

# filter "a" to only show observations that match "b"
semi_join(a_tbl, b_tbl)

# filter "a" to only show observations that don't match "b"
anti_join(a_tbl, b_tbl)

# sometimes matching variables don't have identical names
b_tbl <- b_tbl %>% rename(col = color)

# specify that the join should occur by matching "color" in "a" with "col" in "b"
inner_join(a_tbl, b_tbl, by=c("color" = "col"))

# specify that you want to see more rows
flights_tbl %>% print(n = 15)

# specify that you want to see ALL rows (don't run this!)
flights_tbl %>% print(n = Inf)

# specify that you want to see all columns
flights_tbl %>% print(width = Inf)

# show up to 1000 rows and all columns
flights_tbl %>% View()

# set option to see all columns and fewer rows
options(dplyr.width = Inf, dplyr.print_min = 6)

# reset options (or just close R)
options(dplyr.width = NULL, dplyr.print_min = 10)

# base R approach to view all flights on January 1
flights[flights$month==1 & flights$day==1, ]

# dplyr approach
# note: you can use comma or ampersand to represent AND condition
filter(flights_tbl, month==1, day==1)

# use pipe for OR condition
filter(flights_tbl, carrier=="AA" | carrier=="UA")

# you can also use %in% operator
filter(flights_tbl, carrier %in% c("AA", "UA"))

#Mutating joins:Mutating joins allow you to combine variables from multiple tables
flights2 <- flights_tbl %>% select(year:day, hour, origin, dest, tailnum, carrier)

#copy another table having carrier name
airlines<-copy_to(sc,airlines)

flights2 %>% 
  left_join(airlines)

#Controlling how the tables are matched: each mutating join takes an 
#argument by that controls which variables are used to match observations 
#in the two tables
weather<-copy_to(sc,weather)

flights2 %>% left_join(weather)

#A character vector, by = "x". Like a natural join, but uses only some of 
#the common variables. For example, flights and planes have year columns, 
#but they mean different things so we only want to join by tailnum.
planes<-copy_to(sc,planes)

flights2 %>% left_join(planes, by = "tailnum")

#A named character vector: by = c("x" = "a"). This will match variable x 
#in table x to variable a in table b. The variables from use will be used 
#in the output
airports<-copy_to(sc,airports)
flights2 %>% left_join(airports, c("dest" = "faa"))

flights2 %>% left_join(airports, c("origin" = "faa"))

#Filtering joins: Filtering joins match obserations in the same way as 
#mutating joins, but affect the observations, not the variables. There are 
#two types:
  
#semi_join(x, y) keeps all observations in x that have a match in y.
#anti_join(x, y) drops all observations in x that have a match in y.

#there are many flights in the nycflights13 dataset that don’t have a 
#matching tail number in the planes table:
flights_tbl %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

#Connecting to Databases: dplyr can connect to a database as if the data was 
#loaded into a data frame, Use the same syntax for local data frames and 
#databases, Only generates SELECT statements, Currently supports SQLite, 
#PostgreSQL/Redshift, MySQL/MariaDB, BigQuery, MonetDB
my_db <- src_sqlite("my_db.sqlite3",create = TRUE)

# connect to the "flights" table in that database
flights_tbl <- tbl(my_db, "flights") #currently no table present

# more filter commands
filter(flights_tbl, dest %in% c("SFO", "OAK"))
filter(flights_tbl, dest == "SFO" | dest == "OAK")

# Not this! # Please correct the syntax
#filter(flights_tbl, dest == "SFO" | "OAK")
#filter(flights_tbl, hour >= 0 | == 5)
#filter(flights_tbl, hour >= 0 & == 5)
#filter(flights_tbl, dep_delay > 60)
#filter(flights_tbl, arr_delay > 2 * dep_delay) 

# variable selection based on column names
select(flights_tbl, arr_delay, dep_delay)
select(flights_tbl, arr_delay:dep_delay)
select(flights_tbl, ends_with("delay"))
select(flights_tbl, contains("delay"))

flights_tbl %>% group_by(year) %>% sample_n(3)

# feature construction
flights_tbl %>%
  # Select columns
  select(year, arr_delay) %>%
  # Mutate columns
  mutate(delay_hour = arr_delay / 60) %>%
  # Summarize columns
  summarize(mean_delay_hour = mean(delay_hour))


###running SQL Queries on the DB
library(DBI)
flights_preview <- dbGetQuery(sc, "SELECT * FROM flights LIMIT 10")
flights_preview

#########Comparing Differnet ML Classifiers###########

##https://www.kaggle.com/c/titanic/data

#titanic <- read.csv("./data/titanic/train.csv",header = T)
#titanic_tbl<-copy_to(sc,titanic,overwrite = T)
titanic_tbl <- train

#Spark SQL transforms
#Use feature transforms with Spark SQL. Create new features and modify 
#existing features with dplyr syntax.

#Family_Size:	Create number of siblings and parents
#Pclass:	Format passenger class as character not numeric
#Embarked:	Remove a small number of missing records
#Age:	Impute missing age with average age

# Transform features with Spark SQL API
glimpse(titanic_tbl)
titanic2_tbl <- titanic_tbl %>% 
  mutate(Family_Size = SibSp + Parch + 1L) %>% 
  mutate(Pclass = as.character(Pclass)) %>%
  filter(!is.na(Embarked)) %>%
  mutate(Age = if_else(is.na(Age), mean(Age), Age)) %>%
  sdf_register("titanic2")

#sdf_register is used to save our table for later analysis

#Spark ML transforms
#Use feature transforms with Spark ML. Use ft_bucketizer to bucket 
#family sizes into groups

# Transform family size with Spark ML API
glimpse(titanic2_tbl)
glimpse(titanic_tbl)
distinct(titanic2_tbl, Family_Size)
titanic_final_tbl <- titanic2_tbl %>%
  mutate(Family_Size = as.numeric(Family_size)) %>%
  sdf_mutate(
    Family_Sizes = ft_bucketizer(Family_Size, splits = c(1,2,5,12))
  ) %>%
  mutate(Family_Sizes = as.character(as.integer(Family_Sizes))) %>%
  sdf_register("titanic_final")

# mutate is a dplyr command that accesses the Spark SQL API whereas 
# sdf_mutate is a sparklyr command that accesses the Spark ML API

#Train-validation split
# Partition the data
partition <- titanic_final_tbl %>% 
  mutate(Survived = as.numeric(Survived), SibSp = as.numeric(SibSp), Parch = as.numeric(Parch)) %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Family_Sizes) %>%
  sdf_partition(train = 0.75, test = 0.25, seed = 8585)

# Create table references
train_tbl <- partition$train
test_tbl <- partition$test

#Train multiple machine learning algorithms on the training data. Score the test data with the fitted models.

# Model survival as a function of several predictors
ml_formula <- formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family_Sizes)

# Train a logistic regression model
(ml_log <- ml_logistic_regression(train_tbl, ml_formula))

## Decision Tree
(ml_dt <- ml_decision_tree(train_tbl, ml_formula))

## Random Forest
(ml_rf <- ml_random_forest(train_tbl, ml_formula))

## Gradient Boosted Tree
(ml_gbt <- ml_gradient_boosted_trees(train_tbl, ml_formula))

## Naive Bayes
(ml_nb <- ml_naive_bayes(train_tbl, ml_formula))

## Neural Network
(ml_nn <- ml_multilayer_perceptron(train_tbl, ml_formula, layers = c(11,15,2)))

#Score the test data with the trained models
# Bundle the models into a single list object
ml_models <- list(
  "Logistic" = ml_log,
  "Decision Tree" = ml_dt,
  "Random Forest" = ml_rf,
  "Gradient Boosted Trees" = ml_gbt,
  "Naive Bayes" = ml_nb,
  "Neural Net" = ml_nn
)

# Create a function for scoring
score_test_data <- function(model, data=test_tbl){
  pred <- sdf_predict(model, data)
  select(pred, Survived, prediction)
}

# Score all the models
ml_score <- lapply(ml_models, score_test_data)

#Examine performance metrics: lift, AUC, and accuracy. Also examine 
#feature importance to see what features are most predictive of 
#survival.

# Lift function
calculate_lift <- function(scored_data) {
  scored_data %>%
    mutate(bin = ntile(desc(prediction), 10)) %>% 
    group_by(bin) %>% 
    summarize(count = sum(Survived)) %>% 
    mutate(prop = count / sum(count)) %>% 
    arrange(bin) %>% 
    mutate(prop = cumsum(prop)) %>% 
    select(-count) %>% 
    collect() %>% 
    as.data.frame()
}

# Initialize results
ml_gains <- data.frame(bin = 1:10, prop = seq(0, 1, len = 10), model = "Base")

# Calculate lift
for(i in names(ml_score)){
  ml_gains <- ml_score[[i]] %>%
    calculate_lift %>%
    mutate(model = i) %>%
    rbind(ml_gains, .)
}

# Plot results
library(ggplot2)
ggplot(ml_gains, aes(x = bin, y = prop, colour = model)) +
  geom_point() + geom_line() +
  ggtitle("Lift Chart for Predicting Survival - Test Data Set") + 
  xlab("") + ylab("")

#AUC and accuracy
# Function for calculating accuracy
calc_accuracy <- function(data, cutpoint = 0.5){
  data %>% 
    mutate(prediction = if_else(prediction > cutpoint, 1.0, 0.0)) %>%
    ml_classification_eval("prediction", "Survived", "accuracy")
}

# Calculate AUC and accuracy
perf_metrics <- data.frame(
  model = names(ml_score),
  AUC = 100 * sapply(ml_score, ml_binary_classification_eval, "Survived", "prediction"),
  Accuracy = 100 * sapply(ml_score, calc_accuracy),
  row.names = NULL, stringsAsFactors = FALSE)

# Plot results
library(tidyr)
gather(perf_metrics, metric, value, AUC, Accuracy) %>%
  ggplot(aes(reorder(model, value), value, fill = metric)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  xlab("") +
  ylab("Percent") +
  ggtitle("Performance Metrics")

#Feature importance
# Initialize results
feature_importance <- data.frame()

# Calculate feature importance
for(i in c("Decision Tree", "Random Forest", "Gradient Boosted Trees")){
  feature_importance <- ml_tree_feature_importance(sc, ml_models[[i]]) %>%
    mutate(Model = i) %>%
    mutate(importance = as.numeric(levels(importance))[importance]) %>%
    mutate(feature = as.character(feature)) %>%
    rbind(feature_importance, .)
}

# Plot results
feature_importance %>%
  ggplot(aes(reorder(feature, importance), importance, fill = Model)) + 
  facet_wrap(~Model) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  xlab("") +
  ggtitle("Feature Importance")

#Compare run times
# Number of reps per model
n <- 2

# Format model formula as character
format_as_character <- function(x){
  x <- paste(deparse(x), collapse = "")
  x <- gsub("\\s+", " ", paste(x, collapse = ""))
  x
}

# Create model statements with timers
format_statements <- function(y){
  y <- format_as_character(y[[".call"]])
  y <- gsub('ml_formula', ml_formula_char, y)
  y <- paste0("system.time(", y, ")")
  y
}

# Convert model formula to character
ml_formula_char <- format_as_character(ml_formula)

# Create n replicates of each model statements with timers
all_statements <- sapply(ml_models, format_statements) %>%
  rep(., n) %>%
  parse(text = .)

# Evaluate all model statements
library(tidyverse)
res  <- map(all_statements, eval)

# Compile results
result <- data.frame(model = rep(names(ml_models), n),
                     time = sapply(res, function(x){as.numeric(x["elapsed"])})) 

# Plot
result %>% ggplot(aes(time, reorder(model, time))) + 
  geom_boxplot() + 
  geom_jitter(width = 0.4, aes(colour = model)) +
  scale_colour_discrete(guide = FALSE) +
  xlab("Seconds") +
  ylab("") +
  ggtitle("Model training times")

#############A Sample Case Study##################

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


#This dataset is part of the fivethirtyeight package and provides scores for 
#how each person rated their preference of the dishes from several countries. 
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
food_world_cup[, 9:48][is.na(food_world_cup[, 9:48])] <- 0
food_world_cup$gender <- as.factor(food_world_cup$gender)
food_world_cup$location <- as.factor(food_world_cup$location)

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
#Transforming preference values
food_world_cup[8:47] <- lapply(food_world_cup[8:47], as.numeric)

countries <- paste(colnames(food_world_cup)[-c(1:7)])
countries_L <- colnames(food_world_cup)[-c(1:7)]
str(countries_L)
for (response in countries) {
  food_world_cup[paste(response, "trans", sep = "_")] <- 
    food_world_cup[response] / mean(food_world_cup[food_world_cup[response] > 0, response])
}

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

#Most liked cuisines and gender biases
food_world_cup_gather <- food_world_cup %>%
  collect %>%
  gather(country, value, algeria:vietnam)

food_world_cup_gather$value <- as.numeric(food_world_cup_gather$value)
food_world_cup_gather$country <- as.factor(food_world_cup_gather$country)


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

#putting the processed data into spark
food_world_cup <- copy_to(sc, food_world_cup, overwrite = TRUE)

#applying feature reduction techniques
#Principal Component Analysis (PCA)
pca <- food_world_cup %>%
  mutate_each_(funs(as.numeric), countries) %>%
  ml_pca(features = paste(colnames(food_world_cup)[-c(1:47)]))

library(tibble)
as.data.frame(pca$components) %>%
  rownames_to_column(var = "labels") %>%
  mutate(x_2 = gsub("_trans", "", labels)) %>%
  mutate(x_2 = gsub("_", " ", x_2)) %>%
  mutate(x_2 = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x_2, perl = TRUE)) %>%
  mutate(x_2 = gsub("And", "and", x_2)) %>%
  ggplot(aes(x = PC1, y = PC2, color = x_2, label = x_2)) + 
  geom_point(size = 2, alpha = 0.6) +
  #geom_text_repel() +
  labs(x = paste0("PC1: ", round(pca$explained.variance[1], digits = 2) * 100, "% variance"),
       y = paste0("PC2: ", round(pca$explained.variance[2], digits = 2) * 100, "% variance")) +
  my_theme() + 
  guides(fill = FALSE, color = FALSE)

#Preparing the data for machine learning pipeline
food_world_cup <- tbl(sc, "food_world_cup") %>%
  ft_string_indexer(input_col = "interest", output_col = "interest_idx") %>%
  ft_string_indexer(input_col = "gender", output_col = "gender_idx") %>%
  ft_string_indexer(input_col = "age", output_col = "age_idx") %>%
  ft_string_indexer(input_col = "household_income", output_col = "household_income_idx") %>%
  ft_string_indexer(input_col = "education", output_col = "education_idx") %>%
  ft_string_indexer(input_col = "location", output_col = "location_idx") %>%
  ft_string_indexer(input_col = "knowledge", output_col = "knowledge_idx")

#data partition
partitions <- food_world_cup %>%
  sdf_partition(training = 0.75, test = 0.25, seed = 753)

#Modeling
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

#Model evaluation
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

#plotting the values of the 10 final features for classifying Spanish, Greece and Italian food preference categories 1 - 5 in the original dataset.
feats <- feature_imp_df %>%
  filter(response == "spain") %>%
  slice(1:10)

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

#How to provide more scalability to the ML processes

# Configure cluster (c3.4xlarge 30G 16core 320disk)
conf <- spark_config()
conf$'sparklyr.shell.executor-memory' <- "20g"
conf$'sparklyr.shell.driver-memory' <- "20g"
conf$spark.executor.cores <- 16
conf$spark.executor.memory <- "20G"
conf$spark.yarn.am.cores  <- 16
conf$spark.yarn.am.memory <- "20G"
conf$spark.executor.instances <- 8
conf$spark.dynamicAllocation.enabled <- "false"
conf$maximizeResourceAllocation <- "true"
conf$spark.default.parallelism <- 32

# Connect to cluster
sc <- spark_connect(master = "yarn-client", config = conf, version = '2.0.3')

# disconnect Spark after completion of work
#spark_disconnect(sc)
