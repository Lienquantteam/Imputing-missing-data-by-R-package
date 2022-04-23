
# Missing data practice 5:
# Single and Multiple Imputation with PCA

# We use again the Ozone dataset 

ozone <- airquality
ozone <- read.table("C:/Users/LIEN PHAM/Desktop/Langara/Seminar - applied statistics/Buoi 1 xu ly dl khuyet bang PCA/PCA data missing/ozoneNA.csv", header=TRUE, sep=",", row.names=1)
View(ozone)

# We keep only the continuous variables (first 11 columns)
miss_ozone <- ozone[,1:11]   
miss_ozone[1:20,]

summary(miss_ozone)

####################################################################
#
# SINGLE IMPUTATION WITH PCA 
#
####################################################################

# We use the package missMDA to perform a PCA with missing values

library(missMDA)
library(FactoMineR) # for PCA(.) function 

# Step 1: Estimate the number of dimensions 

nb_dim <- estim_ncpPCA(miss_ozone, method.cv = "GCV")
nb_dim

plot(0:(length(nb_dim$criterion)-1), nb_dim$criterion, xlab = "Number of dimensions", 
        ylab = "MSEP", col = 'red', type = 'l') 
# So, we choose number of dimensions = 2

# Step 2: Impute the missing values

miss_ozone_PCA <- imputePCA(miss_ozone, ncp = nb_dim$ncp) # iterative PCA algorithm

str(miss_ozone_PCA)
miss_ozone_PCA$completeObs

sum(is.na(miss_ozone)) #292

# Perform PCA on the imputed dataset

imputed_ozone <- cbind.data.frame(miss_ozne_PCA$completeObs, ozone$WindDirection)

imputed_ozone_pca <- PCA(imputed_ozone, quanti.sup = 1, quali.sup = 12, ncp = nb_dim$ncp, graph = FALSE)

# Variances of the principal components
eigenvalues <- imputed_ozone_pca$eig
eigenvalues

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), main = "Variances",
        xlab = "Principal Components", ylab = "Percentage of variances", col ="steelblue")

lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], type="b", pch=19, col = "red")

# Graph of individuals
plot(imputed_ozone_pca, hab = 12, lab = "quali") # , lab = "quali"

# Graph of variables
plot(imputed_ozone_pca, choix = "var") 

#scores (principal components)
head(imputed_ozone_pca$ind$coord) 

# The dimdesc() function allows to describe the dimensions
ozone_dimdesc <- dimdesc(imputed_ozone_pca, axes=c(1,2))

ozone_dimdesc$Dim.1$quanti
ozone_dimdesc$Dim.2$quanti

ozone_dimdesc$Dim.1$quali
ozone_dimdesc$Dim.2$quali


####################################################################
#
# MULTIPLE MPUTATION (MI) WITH PCA
#
####################################################################

# USE PACKAGE missMDA

# Step 1: Generate multiple imputed datasets

ozone_MIPCA <- MIPCA(miss_ozone, ncp = 2, nboot = 10) # MI with PCA using 2 dimensions 

# Show the first 20 rows of the first imputed dataset

round(ozone_MIPCA$res.MI[[1]][1:20,], 3)

# Step 2: Check the uncertainty of the imputed values
plot(ozone_MIPCA, choice = "ind.sup")

plot(ozone_MIPCA, choice= "var")

# These plots show that the variability across different imputations is small and 
# we can interpret the PCA results with confidence

# Step 3: Perform regression

res_MIPCA <- lapply(ozone_MIPCA$res.MI, as.data.frame)
fit_ozone_MIPCA  <- lapply(res_MIPCA,lm, formula = "maxO3 ~ T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v")


# Step 4: Aggregate the results of Regression
# Use package 'mice'

library(mice)

poolMIPCA <- pool(as.mira(fit_ozone_MIPCA))
summary(poolMIPCA)


####################################################################
#
# MULTIPLE MPUTATION (MI) WITH PACKAGE 'mice'
# mice = Multivariate Imputation by Chained Equations
####################################################################


# The following input arguments are used in mice for multiple imputation:
# m: Number of imputed datasets (default is m=5)
# seed: Random seed for reproducible results
# method: method to use to impute missing values (default method for imputation of numeric variables is PMM)

library(mice)
?mice

# Impute missing value with m = 50 replications
ozone_mice <- mice(miss_ozone, m = 10, defaultMethod = "norm.boot", printFlag = FALSE) 

# To view the multiply imputed datasets, use the complete function.
complete(ozone_mice,1) #First complete dataset with imputed values

# Peform regression 

lm_onzone_mice <- with(ozone_mice, lm(maxO3 ~ T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v))

# Aggregate the results of Regression with Multiple Imputation according to Rubin’s rule

pool_ozone_mice <- pool(lm_onzone_mice)
summary(pool_ozone_mice)

# Exercise: 
# Load the dataset 'nhanes' integrated in package 'mice'
# The dataset nhanes contains 25 observations on the following 4 variables:
# age: Age group (1 = 20-39, 2 = 40-59, 3 = 60+)
# bmi: Body mass index (kg/m^2)
# hyp: Hypertensive (1 = no, 2 = yes)
# chl: Total serum cholesterol (mg/dL)

# Perform a multiple imputation with 'mice', 
# then build a regression model chl ~ age + bmi  using imputed datasets
