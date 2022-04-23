
# Bài thực hành số 3: điền khuyết bằng phương pháp KNN (K-Nearest Neighbor)

# Data House_price: 
# price: Giá nhà được bán ra.
# sqft_living15: Diện tích trung bình của 15 ngôi nhà gần nhất trong khu dân cư.
# floors: Số tầng của ngôi nhà được phân loại từ 1-3.5.
# condition: Điều kiện kiến trúc của ngôi nhà từ 1 − 5, 1: rất tệ và 5: rất tốt.
# sqft_above: Diện tích ngôi nhà.
# sqft_living: Diện tích khuôn viên nhà.

setwd('C:/Users/LIEN PHAM/Desktop/Langara/Seminar - applied statistics/Buoi 1 xu ly dl khuyet bang PCA/PCA data missing')

house <- read.csv("house_price.csv", header = TRUE)

names(house)

n <- 1:10000

houseKNN <- house[, c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")]

houseKNN$floors <- as.factor(houseKNN$floors)
houseKNN$condition <- as.factor(houseKNN$condition)

head(houseKNN)

#XD mo hinh hoi quy boi truoc khi lam khuyet du lieu

M1 <- lm(price ~ condition + floors + sqft_living15 + sqft_above+ sqft_living, data = houseKNN)
summary(M1)


# Tao cac gia tri khuyet cho cac bien "condition", "price", "sqft_above"

p_mis <- 0.25

houseKNN[, c("condition", "price", "sqft_above")] <- 
  lapply(houseKNN[, c("condition", "price", "sqft_above")], 
         function(x){ 
           x[runif(n) < p_mis] <- NA 
           x } )

View(houseKNN)

# Visualization

library(naniar)
library(VIM)

vis_miss(houseKNN)

gg_miss_var(houseKNN)

aggr(houseKNN, sortVar = TRUE)

matrixplot(houseKNN, sortby = 2)

## K-Nearest Neighbors imputation
# Aggregation of the k nearest neighbors is used to imputed value. 
# The kind of aggregation and distance depends on the type of the variable.

# The 'kNN' function
# - dist_var: vector of variable names to be used for calculating the distances
# - weights: numeric vector containing a weight for each distance variable 
# - numFun: function for aggregating the k nearest neighbors for numerical variables
# - catFun: function for aggregating the k nearest neighbors for categorical variables, 

## IT WILL TAKE several minutes 


houseKNN_imputed <- kNN(houseKNN, dist_var = c("sqft_living15", "floors", "sqft_living" ), k = 5, imp_var = FALSE)
end <- Sys.time()
print(runtime <- end -start)

View(houseKNN_imputed)

sum(is.na(houseKNN_imputed))

# XD mo hinh hoi quy tuyen tinh cho gia nha (price)

M2 <- lm(price ~ condition + floors + sqft_living15 + sqft_above+ sqft_living, data = houseKNN_imputed)
summary(M2)



