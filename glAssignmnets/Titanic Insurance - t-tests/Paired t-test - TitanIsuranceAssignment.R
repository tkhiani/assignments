#load the insurance data
insurance <- read.csv("./glAssignmnets/Titanic Insurance - t-tests/InsuranceData.csv")


# Check for normality for sum assured based on the old scheme
# Null hypothesis is true - data is normally distributed
shapiro.test(insurance$Old.Scheme)

# Check for normality for sum assured based on the new scheme
# Null hypothesis is true - data is normally distributed
shapiro.test(insurance$New.Scheme)

# We use a paired t-test we can identify if the new scheme is more effective than an old scheme. Here we use a Paired t-test as we have the sum assured sales for the same employee across two schemes

# Hence at 5% level of significance there is not enough statistical evidence that sales using the new scheme is more than that of the old scheme. 
t.test(insurance$New.Scheme, insurance$Old.Scheme, alternative = "greater", paired = TRUE, conf.level = 0.95)

# However at 10% level of significance there is statistical evidence that sales using the new scheme is more than that of the old scheme.
t.test(insurance$New.Scheme, insurance$Old.Scheme, alternative = "greater", paired = TRUE, conf.level = 0.90)