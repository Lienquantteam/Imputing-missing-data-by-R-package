
# Bài thực hành số 1: cài đặt gói và mô tả dữ liệu 

# Install necessary packages for missing data imputation 
# We will install the following packages: 
# VIM, naniar, visdat, Amelia, mice, mtvnorm, ggplot2, missMDA, FactoMineR

# If you already installed one of these packages, you can skip the corresponding command  

install.packages("VIM")
install.packages("naniar")
install.packages("visdat")
install.packages("Amelia")
install.packages("mice")
install.packages("mtvnorm")
install.packages("ggplot2")
install.packages("missMDA")
install.packages("FactoMineR")

# PRACTICAL LESSION 1: Missing Data Visualization 

# WITH Iris dataset 
# Load the data

dat_iris <- iris 
head(dat_iris) # Show first 10 lines of the data

# Iris is a complete dataset, hence we will make missing values 

n <- dim(dat_iris)[1] # get the number of observations 

iris_miss <- dat_iris

p_miss <- 0.30 # We make 30% of missing values under MCAR (Missing Completely At Random)

miss_inds <- replicate(4, runif(n) < p_miss) # Make missing values only for the first 4 columns 
miss_inds <- cbind(miss_inds, rep(FALSE, n))

iris_miss[miss_inds] <- NA

iris_miss[1:20,] # Print first 20 lines of Iris dataset with 30% of missing values


# VISUALIZATION

# USE PACKAGE NANIAR
# References: http://naniar.njtierney.com/articles/getting-started-w-naniar.html


library(naniar)
library(visdat)

# vis_dat visualises the whole dataframe at once, 
# and provides information about the class of the data input into R, 
# as well as whether the data is missing or not.

vis_dat(iris_miss)

# The function vis_miss provides a summary of whether the data is missing or not. 
# It also provides the amount of missings in each columns.

vis_miss(iris_miss)

gg_miss_var(iris_miss)

pct_miss(iris_miss) # show percentage of missng values in iris_miss

# Or use gg_miss_var(iris_miss, show_pct = TRUE)

n_miss(iris_miss) # number of missing values of the data.frame iris

n_miss(iris_miss$Sepal.Length) # number of missing values of the variables Sepal.Lenght

n_complete(iris_miss) # without missing value


library(ggplot2)

# using  geom_miss_point() with ggplot

ggplot(iris_miss, aes(x = Petal.Length, y = Petal.Width)) + geom_miss_point()

# with facet!

ggplot(iris_miss, aes(x = Petal.Length, y = Petal.Width)) + geom_miss_point() + facet_wrap(~ Species)


# USE PACKAGE VIM

# The function aggr (package VIM) calculates and represents the number of missing entries in each variable 
# and for certain combinations of variables which tend to be missing simultaneously

library(VIM)
miss_plot <- summary(aggr(iris_miss, col=c('navyblue','yellow'), sortVar = TRUE))$combinations

# miss_plot <- aggr(iris_miss, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE,
#                   labels=names(iris_miss), cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))

# Show combinations between variables 

head(miss_plot[rev(order(res[,2])),])

# Or we can show matrix plot 

matrixplot(iris_miss, sortby = 2)

# The VIM function marginplot creates a scatterplot with additional information on the missing values. 
# The points for which x (resp. y) is missing are represented in red along the y (resp. x) axis.

marginplot(iris_miss[,c("Sepal.Length","Sepal.Width")])

# Exercises:

# 1) Load the datast "airquality" (integraded in the naniar package) visualize missing values.
air_dat <- airquality
head(air_dat)

# 2) Import the ozone dataset (ozoneNA.csv) and make some visualizations of the data.

# 3) Import the ecological dataset (ecological.csv) and make some visualizations of the data.
