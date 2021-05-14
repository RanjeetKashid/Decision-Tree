##### Company Data #####

install.packages("C50")
library(C50)
library(tree)

company_data <- read.csv(file.choose())
summary(company_data)
sum(is.na(company_data))

# Categorizing o/p

company_data$category <- NA
company_data$category[company_data$Sales <= 5] = "Low Sale"
company_data$category[company_data$Sales > 5 & company_data$Sales <= 9] = "Medium Sale"
company_data$category[company_data$Sales >= 9] = "High Sale"

company_data$category = factor(company_data$category, levels = c("Low Sale", "Medium Sale", "High Sale"))


# Splitting data

library(caTools)

split <- sample.split(company_data$category, SplitRatio = 0.70)
split
table(split)
cd_train <- subset(company_data, split == TRUE)
cd_test  <- subset(company_data, split == FALSE)
table(cd_train$category)
table(cd_test$category)
cd_train <- cd_train[,-1]
cd_test <- cd_test[,-1]


# Building Model

cd_c5.0 <- C5.0(cd_train[,-11],cd_train$category)
plot(cd_c5.0)

# training accuracy
pred_train <- predict(cd_c5.0,cd_train[,-11])
table(pred_train,cd_train$category)
mean(pred_train == cd_train$category)

# testing model
pred_test <- predict(cd_c5.0,cd_test[,-11])
table(pred_test,cd_test$category)
mean(pred_test == cd_test$category)


##### Fraud Check #####

fraud_data <- read.csv(file.choose())
summary(fraud_data)
str(fraud_data)
sum(is.na(fraud_data))

# Categorizing o/p

fraud_data$category <- NA
fraud_data$category[fraud_data$Taxable.Income <= 30000] = "Risky"
fraud_data$category[fraud_data$Taxable.Income > 30000] = "Good"

fraud_data$category = factor(fraud_data$category, levels = c("Risky", "Good"))
str(fraud_data)

# Splitting data

split_fd <- sample.split(fraud_data$category, SplitRatio = 0.70)
table(split_fd)
fd_train <- subset(fraud_data, split == TRUE)
fd_test  <- subset(fraud_data, split == FALSE)
table(fd_train$category)
table(fd_test$category)
fd_train <- fd_train[,-3]
fd_test <- fd_test[,-3]


# Building Model

fd_c5.0 <- C5.0(fd_train[,-6],fd_train$category)
plot(fd_c5.0)

# training accuracy
pred_train_fd <- predict(fd_c5.0,fd_train[,-6])
table(pred_train_fd,fd_train$category)
mean(pred_train_fd == fd_train$category)

# testing model
pred_test_fd <- predict(fd_c5.0,fd_test[,-6])
table(pred_test_fd,fd_test$category)
mean(pred_test_fd == fd_test$category)

# Using tree
fd_tree <- tree(fd_train$category ~., data = fd_train[,-6])
plot(fd_tree)
text(fd_tree,pretty = 0)

pred_test_fd_tree <- as.data.frame(predict(fd_tree, newdata = fd_test[,-6]))
pred_test_fd_tree["final"] <- NA
pred_test_df <- predict(fd_tree,newdata = fd_test[,-6])
pred_test_fd_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

table(pred_test_fd_tree$final)
mean(pred_test_fd_tree$final == fd_test$category)
table(pred_test_fd_tree$final,fd_test$category)


