
# Missing data practice 4: 
# Missing values imputation with EM algorithm and Joint Gaussian Distribution - Multivariate case

# We will use the dataset Ozone:

# First, load the requirement libraries 

library(VIM)
library(norm) # For EM algorithm 

# Load the dateset
# Data ozone about air pollution: 112 observations collected during summer of 2001 in Rennes, France. 
# The variables available are:
# maxO3 (maximum daily ozone)
# maxO3v (maximum daily ozone the previous day)
# T9
# T12 (temperature at midday)
# T15 (temperature at 3pm)
# Vx12 (projection of the wind speed vector on the east-west axis at midday)
# Vx9 and Vx15 as well as the Nebulosity (cloud) Ne9, Ne12, Ne15
# Aim: analyse the relationship between the maximum daily ozone (maxO3) level and the other meteorological variables.

ozone <- read.table("ozoneNA.csv", header=TRUE, sep=",", row.names=1)
View(ozone)

# We keep only the continuous variables (first 11 columns)
miss_ozone <- ozone[,1:11]   
miss_ozone[1:20,]

summary(miss_ozone)

# Visualization 

aggr(miss_ozone, col=c('navyblue','yellow'), sortVar = TRUE)

# Imputation
# We suppose that the data is drawn from a multivariate normal distribution with 
# parameter theta = (mu, Sigma) (mu: mean vector, Sigma: covariance matrix)
# Step 1: Estimate M and S from the incompleta dataset with EM

pre_param <- prelim.norm(as.matrix(miss_ozone))
thetahat <- em.norm(pre_param) # run EM algorithm, compute MLE

# Get estimated parameter

estimate <- getparam.norm(pre_param, thetahat) 
estimate$mu

estimate$sigma

# Step 2: Impute the missing data using by drawing from the conditional distribution X_miss|X_obs

rngseed(1e5)
ozone_imputed <- imp.norm(pre_param, thetahat, miss_ozone)
ozone_imputed 

# Perform regression 
#pairs(ozone_imputed)
model_ozone <- lm(maxO3 ~ T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v, data = ozone_imputed)
summary(model_ozone)





