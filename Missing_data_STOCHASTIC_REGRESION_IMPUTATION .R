
# Bài thực hành số 2: các phương pháp điền khuyết (imputation) đơn giản
# Mean, Regression, Stochastics Regression imputation 

# Sử dụng iris dataset 

head(iris) 
pairs(iris[,1:4]) # Vẽ các đồ thị phân tán của 4 biến đầu tiên 

# Ta sử dụng hai biến là Peta.Length và Petal.Width

iris_petal <- iris[,3:4]

# Tạo giá trị khuyết trong biến Petal.Lenght 

n <- dim(iris_petal)[1]
p_miss <- 0.5
index_NA <- sample(1:n, p_miss*n) 

iris_petal[index_NA, 2] <- NA # Tạo 25% giá trị khuyết trong biến Petal.Width
iris_petal 

# Visualization
library(naniar)
library(visdat)
library(VIM)
library(ggplot2)

vis_miss(iris_petal)
marginplot(iris_petal)


# COMLETE CASE ANALYSIS 

iris_petal_CC <- iris_petal[complete.cases(iris_petal),] 
# Hoặc iris_petal_CC <- na.omit(iris_petal)

dim(iris_petal_CC)
iris_petal_CC


# MEAN IMPUTATION 

iris_petal_Mean <- iris_petal

iris_petal_Mean[index_NA, 2]  <- mean(iris_petal[,2], na.rm = TRUE)

imputed <- ((1:n) %in% index_NA)
ggplot(iris_petal_Mean) + ggtitle("Mean imputation") + 
      aes(x=Petal.Length, y=Petal.Width, colour = imputed) + geom_point()

# REGRESSION IMPUTAION 

iris_reg <- lm(Petal.Width ~ Petal.Length, data = iris_petal)
summary(iris_reg)

iris_petal_Reg <- iris_petal 
iris_petal_Reg[index_NA, 2] <- predict(iris_reg, iris_petal[index_NA, 1, drop = F])

ggplot(iris_petal_Reg) + ggtitle("Regression imputation") + 
      aes(x=Petal.Length, y=Petal.Width, colour = imputed) + geom_point()

# STOCHASTIC REGRESION IMPUTATION 

iris_petal_StochReg <- iris_petal

sig <- (summary(iris_reg))$sig

iris_petal_StochReg[index_NA, 2] <- iris_petal_Reg[index_NA, 2] + rnorm(length(index_NA), 0, sig)

ggplot(iris_petal_StochReg) + ggtitle("Stochastic regression imputation") + 
      aes(x=Petal.Length, y=Petal.Width, colour = imputed) + geom_point()

# SO SÁNH CÁC PHƯƠNG PHÁP ĐIỀN KHUYẾT: 
# Đối với mỗi bội dữ liệu điền khuyết được bởi từng phương pháp, tính trung bình mẫu, độ lệch chuẩn mẫu, hệ số tương quan 
# với Petal.Length và khoảng tin cậy 

data_all  <- cbind.data.frame(iris$Petal.Width, iris_petal_Mean[,2], iris_petal_Reg[, 2],  iris_petal_StochReg[,2])
mean_vec <- apply(data_all, 2, mean)
sd_vec <- apply(data_all, 2, sd)
cor_vec <- apply(data_all, 2, cor, iris_petal[,1])
lower <- mean_vec - qt(.975, n-1) * sd_vec/sqrt(n)
upper <- mean_vec + qt(.975, n-1) * sd_vec/sqrt(n)
width <- upper - lower

result <-  rbind.data.frame(mean_vec, sd_vec, cor_vec, lower, upper, width)
result <- round(result, 4)
colnames(result) <-   c("ORIGINAL", "MEAN","REG", "STOCH")
rownames(result) <- c("Mean", "STD", "Correlation", "Lower bound", "Upper bound ", "Width CI")
print(result)


