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
# Remove columns - WGTP, ADJHSG, ADJINC
housing <- housing %>% select(RT:WORKSTAT) 
housing <- housing %>% filter(!is.na(VALP) && RT == "H") 



# Property value is not availble for other than Housing Unit. 
  # Hence we can further filter the dataset
  # Also most of the independent variables are NA for vacant or just sold household. Hence it would be better to use a different model for such households.
housing %>% group_by(TYPE) %>% summarise(mean(VALP), n())
housing <- housing %>% filter(TYPE == 1)
occupied <- housing %>% filter(is.na(VACS)) %>% sdf_register("occupied")
others <- housing %>% filter(!is.na(VACS)) %>% sdf_register("others")
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
n <- colnames(occupied)
f <- as.formula(paste("VALP ~", paste(n[!n %in% c("VALP", "RT", "SERIALNO", "WGTP", "ADJHSG", "ADJINC", "VACS", "TYPE", "NP")], collapse = " + ")))
fit <- ml_linear_regression(x = occupied, f)

summary(fit)
pVALP <- sdf_predict(fit, occupied)
pVALP %>% select(VALP, prediction) %>% mutate(error = abs(VALP - prediction)^2) %>% summarise(sum(error)) 

pVALP %>% 
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

# Convert to factors or numeric fields as required
#for(i in 1:7){
#  ml_create_dummy_variables(x=dat,colnames(dat)[i],reference=NULL)
#}

# Apply adjusment factor for constanst currencies
  # ADJHSG
    # CONP, ELEP, FULP, GASP, GRNTP, INSP, SMOCP, RNTP, SMP, WATP. 
    # Not applied to AGS & TAXP  
  
  # ADJINC
    # FINCP, HINCP  


# Create dummies or treat levels in a factor accordingly

# Split into test and train or use cross-validation

# Dependent & Indepedent Variables

# Run regression and determine if we remove certain independent variables

# Evaluate performance with an error function
