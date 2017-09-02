# Use SparkR machine learning capabilities in order to predict property value in relation to other variables in the 2013 American Community Survey (Links to an external site.)Links to an external site. dataset.
# In the class you have seen how SparkR (Links to an external site.) Links to an external site. introduces data types and functions that are very similar to what we are used to when using regular R libraries.
# While building a linear model check the significance of each of the variables involved in building such a predictor for property value.
# Dataset Url: http://www2.census.gov/acs2013_1yr/pums/csv_hus.zip (Links to an external site.)Links to an external site. 
# Steps:
  # Create a SparkSQL context and load data
  # Prepare(Refine) your data (Imputation and casting)
  # Prepare a train / test data split
  # Train a linear model and summarize your model
  # Evaluate your model using the test data
  # Give conclusions or provide your insights
# Details on the data set: 
# Explations for every field: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict13.txt
# Replicate weights: https://usa.ipums.org/usa/repwt.shtml. Weights can be usefulf to understand the range (condifence interval) of a specific value but we decided to omit it. 
# Allocations where used to fill values: https://usa.ipums.org/usa/flags.shtml. Hence does not make sense for us to include it into our dataset
library(sparklyr)
library(dplyr)
library(ggplot2)

# establish the spark context
sc <- spark_connect(master = "local")

# read housing data files and combine all the rows
housingA <- spark_read_csv(sc, name = "housinga", path = "./largeDataSets/usHousingPrices/ss13husa.csv")
housingB <- spark_read_csv(sc, name = "housingb", path = "./largeDataSets/usHousingPrices/ss13husb.csv")
housing <- sdf_bind_rows(housingA, housingB, id = "housing")
housing <- sdf_register(housing, "housing")

# Free up the memory
dplyr::db_drop_table(sc, "housingA")
dplyr::db_drop_table(sc, "housingB")
rm(housingA, housingB)

# Avoid replicate weights & allocations and use the required features
# Remove all the rows where the property value is not known
housing <- housing %>% select(RT:WORKSTAT) 
housing <- housing %>% filter(!is.na(VALP) && RT == "H") 

# Property value is not availble for other than Housing Unit. 
  # Hence we can further filter the dataset
  # Also most of the independent variables are NA for vacant or just sold household. Hence it would be better to use a different model for such households.
housing %>% group_by(TYPE) %>% summarise(mean(VALP), n())
housing <- housing %>% filter(TYPE == 1)
occupied <- housing %>% filter(is.na(VACS)) %>% sdf_register("occupied")
glimpse(occupied)
glimpse(others)

# Free up the memory
dplyr::db_drop_table(sc, "housing")
rm(housing)

# Identify missing values and understand how to deal with it
  # Igonre RT, SERIALNO, WGTP, VACS, TYPE, ADJHSG, ADJINC as they are not needed to determine the housing prices
  
  # for factors replace NA with 
    # With 0 or if we can create a dummy for NA then we leave it as its own level
      # ACCESS, ACR, AGS, BATH, BLD, BROADBAND, BUS, COMPOTHX, DIALUP, DSL, FIBEROP
      # FS, HANDHELD, HFL, MODEM, MRGI, MRGT, MRGX, OTHSVCEX, REFR, RWAT, RWATPR
      # SATELLITE, SINK, STOV, TEL, TEN, TOIL, VEH, FES, FPARC, HHL, HHT, HUGCL, HUPAC
      # HUPAOC, HUPARC, KIT, LNGI, MULTG, MV, YBL, BPP, NR, PARTNER, PLM, PSF
      # R18, R60, R65, RESMODE, SMX, SRNT, SSMC, SVAL, TAXP, WIF, WKEXREL, WORKSTAT
    # Too many levels:
      # PUMA, ST, TAXP, WKEXREL, WORKSTAT
    # Seems Highly related
      # PUMA, DIVISON, REGION, ST
      # WKEXREL, WORKSTAT, WIF
      # R18, NRC, NOC, HUPARC, HUPAC, FPARC 
      # R60, R65, NPP, HUGCL
      # MV, YBL
      # LNGI, HHL
  # for numeric fields replace NA with 0 or mean or median
    # NA with 0:
      # NP, BDSP, CONP, INSP, MHP, MRGP, RMSP, RNTP, SMP, WATP, FINCP, GRNTP 
      # GRPIP, HINCP, NOC, NPF, NRC, OCPIP, SMOCP 
    # ELEP, FULP, WATP: NA, 1, 2 with 0 
    # GASP: NA, 1, 2, 3 with 0

occupied <- occupied %>% mutate(
  ELEP = ifelse(is.na(ELEP) || ELEP == 1 || ELEP == 2, 0, ELEP),
  GASP = ifelse(is.na(GASP) || GASP == 1 || GASP == 2 || GASP == 3, 0, GASP),
  FULP = ifelse(is.na(FULP) || FULP == 1 || FULP == 2, 0, FULP),
  WATP = ifelse(is.na(WATP) || WATP == 1 || WATP == 2, 0, WATP))

occupied <- na.replace(occupied, 0)

# Understanding the distribution of the property value
occupied <- occupied %>% mutate(VALP = VALP/1000)
# Create a new variable to track house prices greater than 2M
occupied <- occupied %>% mutate(ABOVE2M = if_else(VALP > 2000, 1, 0))

occupied %>% select(VALP) %>% collect %>% ggplot(aes(VALP)) + geom_histogram(bins = 100)

# Identify the factors for which we shall create dummies
occupied %>% 
  select(ABOVE2M, ST, VALP) %>%
  group_by(ABOVE2M, ST) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ST, y = ..count..)) + 
  geom_bar() +
  facet_wrap(~ABOVE2M)

occupied %>% 
  select(ABOVE2M, TAXP, VALP) %>%
  group_by(ABOVE2M, TAXP) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = TAXP, y = ..count..)) + 
  geom_bar() +
  facet_wrap(~ABOVE2M)

occupied %>% 
  select(ABOVE2M, YBL, VALP) %>%
  group_by(ABOVE2M, YBL) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = YBL, y = ..count..)) + 
  geom_bar() +
  facet_wrap(~ABOVE2M)

occupied %>%
  select(ABOVE2M, BLD, VALP) %>%
  group_by(ABOVE2M, BLD) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = BLD, y = ..count..)) + 
  geom_bar() +
  facet_wrap(~ABOVE2M)

occupied %>% 
  select(ABOVE2M, INSP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(INSP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

occupied %>% 
  select(ABOVE2M, SMOCP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(SMOCP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

occupied %>% 
  select(ABOVE2M, HINCP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(HINCP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

occupied %>% 
  select(ABOVE2M, FINCP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(FINCP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

occupied %>% 
  select(ABOVE2M, MRGP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(MRGP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

occupied %>% 
  select(ABOVE2M, BDSP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(BDSP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

occupied %>% 
  select(ABOVE2M, RMSP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(RMSP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

occupied %>% 
  select(ABOVE2M, ELEP) %>%
  group_by(ABOVE2M) %>%
  summarise(mean = mean(ELEP)) %>%
  collect() %>% data.frame() %>%
  ggplot(aes(x = ABOVE2M, y = mean)) + 
  geom_point() 

# Adjust all numeric fields in 1000's except where feature represents number of children/people...
occupied <- occupied %>% mutate(
  ELEP = ELEP/1000,
  GASP = GASP/1000,
  FULP = FULP/1000,
  WATP = WATP/1000, 
  CONP = CONP/1000, 
  INSP = INSP/1000, 
  MHP = MHP/1000, 
  MRGP = MRGP/1000, 
  RNTP = RNTP/1000, 
  SMP = SMP/1000, 
  GRNTP = GRNTP/1000, 
  FINCP = FINCP/1000, 
  HINCP = HINCP/1000, 
  OCPIP = OCPIP/1000, 
  SMOCP = SMOCP/1000,
  ADJINC = ADJINC/1000000,
  ADJHSG = ADJHSG/1000000)

# Apply adjusment factor for constanst currencies
  # ADJHSG - CONP, ELEP, FULP, GASP, GRNTP, INSP, SMOCP, RNTP, SMP, WATP. 
  # Not applied to AGS & TAXP  
  # ADJINC - FINCP, HINCP  
occupied <- occupied %>% mutate(
  ELEP = ELEP*ADJHSG,
  GASP = GASP*ADJHSG,
  FULP = FULP*ADJHSG,
  WATP = WATP*ADJHSG,
  CONP = CONP*ADJHSG, 
  INSP = INSP*ADJHSG, 
  SMP = SMP*ADJHSG, 
  GRNTP = GRNTP*ADJHSG, 
  SMOCP = SMOCP*ADJHSG,
  FINCP = FINCP*ADJINC, 
  HINCP = HINCP*ADJINC)

# Split into test and train or use cross-validation
partition <- sdf_partition(occupied, train = 0.7, test = 0.3, seed = 1099)
n <- colnames(occupied)

# Let us try to understand how to classify houses above 2M
e <- c('VALP','RT', 'SERIALNO', 'ADJHSG', 'ADJINC', 'VACS', 'TYPE', 'ABOVE2M')
f <- as.formula(paste("ABOVE2M ~", paste(n[!n %in% e], collapse = " + ")))

# Let's identify the important variables
fitToClassify <- ml_random_forest(x = partition$train, f, max.depth = 5, num.trees = 100, type = "classification")
feature_imp <- ml_tree_feature_importance(sc, fitToClassify)
feature_imp

# let's try to cluster
c <- tbl_vars(partition$train)
e <- c('RT', 'SERIALNO', 'ADJHSG', 'ADJINC', 'VACS', 'TYPE', 'ABOVE2M')
c <- c[!c %in% e]
clusters <- ml_kmeans(x = partition$train, centers = 2, features = c)
clusters$centers

partition$train %>%
  select(ABOVE2M, TAXP) %>%
  collect %>%
  ggplot(aes(y = VALP, x = TAXP)) +
  geom_point(data = clusters$centers, aes(y = VALP, x = TAXP), size = 60, alpha = 0.1) +
  geom_point(aes(y = VALP, x = TAXP), size = 2, alpha = 0.5)

# Dependent & Indepedent Variables
# Remove columns - WGTP, ADJHSG, ADJINC & columns where we have dummies
e <- c('VALP','RT', 'SERIALNO', 'ADJHSG', 'ADJINC', 'VACS', 'TYPE')
f <- as.formula(paste("VALP ~", paste(n[!n %in% e], collapse = " + ")))

# Let's identify the important variables
fit0 <- ml_random_forest(x = partition$train, f, max.depth = 5, num.trees = 100, type = "regression")
summary(fit0)
feature_imp <- ml_tree_feature_importance(sc, fit0)

# Let us now run regression two variants
  # a. on all the 52 variables
  # b. 6 variables that seems to the most important > 2%
n <- as.character(feature_imp[1:52, 2])
f <- as.formula(paste("VALP ~", paste(n, collapse = " + ")))
fit <- ml_linear_regression(x = partition$train, f)
summary(fit)

# Let's re-run regression removing the variables that are have significant p-value
e <- c('HHT','PARTNER', 'REGION', 'MRGX', 'MV','BUS')
f <- as.formula(paste("VALP ~", paste(features[!features %in% e], collapse = " + ")))
fit1 <- ml_linear_regression(x = partition$train, f)
summary(fit1)

# Run regression only on the most important variables > 2%. Trying to check if a simpler model would be sufficient
f <- as.formula("VALP ~ TAXP + SMOCP + MRGP + INSP + HINCP + DIVISION + FINCP + ST")
fit2 <- ml_linear_regression(x = partition$train, f)
summary(fit2)

# As Fit 1 has a better r^2 and a root mean square error we choose fit1 which makes use of 46 variables
# Evaluate performance with an error function
pTest <- sdf_predict(fit1, partition$test)

# Error function for lm
sumOfSquaredErrors <- pTest %>% select(VALP, prediction) %>% mutate(error = abs(VALP - prediction)^2) %>% summarise(se = sum(error)) %>% collect 
noOfObservations <- pTest %>% summarise(n = n()) %>% collect

rmse <- sqrt(sumOfSquaredErrors$se/noOfObservations$n)
rmse

sumOfSquaredErrors <- pTest %>% filter(VALP >= 2000) %>% select(VALP, prediction) %>% mutate(error = abs(VALP - prediction)^2) %>% summarise(se = sum(error)) %>% collect 
noOfObservations <- pTest %>% filter(VALP >= 2000) %>% summarise(n = n()) %>% collect

rmseForGreaterThan2M <- sqrt(sumOfSquaredErrors$se/noOfObservations$n)
rmseForGreaterThan2M

sumOfSquaredErrors <- pTest %>% filter(VALP < 2000) %>% select(VALP, prediction) %>% mutate(error = abs(VALP - prediction)^2) %>% summarise(se = sum(error)) %>% collect 
noOfObservations <- pTest %>% filter(VALP < 2000) %>% summarise(n = n()) %>% collect

rmseForLessThan2M <- sqrt(sumOfSquaredErrors$se/noOfObservations$n)
rmseForLessThan2M

# Plot Actual vs Prediction
pTest %>% 
  select(SERIALNO, VALP, prediction) %>%
  collect %>%
  ggplot() +
  geom_point(aes(x = row_number(SERIALNO), y = VALP), colour = "Red") +
  geom_point(aes(x = row_number(SERIALNO), y = prediction), colour = "Blue") +
  labs(
    x = "Observations",
    y = "Red - Actual, Blue - Predicted",
    title = "Linear Regression"
  )

# Plot Actual vs Prediction where property value is greater than 2000 K
# Poor accuracy where property value is greater than 2M. It might be good to have 2 models one for homes less than 2M and those above 2M
pTest %>% 
  filter(VALP > 2000) %>% 
  select(SERIALNO, VALP, prediction) %>%
  collect %>%
  ggplot() +
  geom_point(aes(x = row_number(SERIALNO), y = VALP), colour = "Red") +
  geom_point(aes(x = row_number(SERIALNO), y = prediction), colour = "Blue") +
  labs(
    x = "Observations",
    y = "Red - Actual, Blue - Predicted",
    title = "Linear Regression"
  )

spark_disconnect(sc)
rm(e, f, fFields, fit, i, n, occupied, partition, 
   pTest, rmse, sc, sumOfSquaredErrors, noOfObservations,
   rmseForGreaterThan2M, rmseForLessThan2M)
