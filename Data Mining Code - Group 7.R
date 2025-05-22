first_data <- read.csv("credit_card_transactions.csv")
second_data <- read.csv("fraud_data.csv")

# ----------------------- DATA CLEANING -----------------------
#
#
#Check rows and columns
nrow(first_data)
ncol(first_data)
colnames(first_data)

nrow(second_data)
ncol(second_data)
colnames(second_data)

#Check duplicated
sum(duplicated(first_data))
sum(duplicated(second_data))

# Check and remove null
sum(is.na(first_data))
first_data <- na.omit(first_data)

#Check Null values
sum(is.na(second_data))

# ----------------------- DATA TRANSFORMATION (PT. 1) -----------------------
#
#
#SEPARATE YEAR DATE TIME TRANSACTION AS WELL AS CALCULATE DATE OF BIRTH
#FIRST DATA
first_data$trans_date_trans_time <- as.POSIXct(first_data$trans_date_trans_time, format = "%Y-%m-%d %H:%M:%S")
first_data$trans_year <- as.numeric(format(as.Date(first_data$trans_date_trans_time, format = "%Y-%m-%d %H:%M:%S"), "%Y"))
first_data$trans_month <- as.numeric(format(as.Date(first_data$trans_date_trans_time, format = "%Y-%m-%d %H:%M:%S"), "%m"))
first_data$trans_date <- as.numeric(format(as.Date(first_data$trans_date_trans_time, format = "%Y-%m-%d %H:%M:%S"), "%d"))
first_data$trans_hour <- as.numeric(format(strptime(first_data$trans_date_trans_time, format = "%Y-%m-%d %H:%M:%S"), "%H"))
first_data$trans_minute <- as.numeric(format(strptime(first_data$trans_date_trans_time, format = "%Y-%m-%d %H:%M:%S"), "%M"))
first_data$trans_date_trans_time <- NULL

#CHANGE DOB INTO AGE
library(lubridate)
dob <- ymd(first_data$dob)
years <- year(dob)
curr_year <- year(Sys.Date())

first_data$age <- curr_year - years
first_data$dob <- NULL

summary(first_data)

#HEATMAP FOR FIRST DATA
library(corrplot)
numeric_vars <- first_data[sapply(first_data, is.numeric)]
cor_matrix <- cor(numeric_vars, use = 'complete.obs')

corrplot(cor_matrix, method = 'color', tl.cex = 0.8)

#SECOND DATA
second_data$trans_year <- as.numeric(format(as.Date(second_data$trans_date_trans_time, format = "%d-%m-%Y %H:%M"), "%Y"))
second_data$trans_month <- as.numeric(format(as.Date(second_data$trans_date_trans_time, format = "%d-%m-%Y %H:%M"), "%m"))
second_data$trans_date <- as.numeric(format(as.Date(second_data$trans_date_trans_time, format = "%d-%m-%Y %H:%M"), "%d"))
second_data$trans_hour <- as.numeric(format(strptime(second_data$trans_date_trans_time, format = "%d-%m-%Y %H:%M"), "%H"))
second_data$trans_minute <- as.numeric(format(strptime(second_data$trans_date_trans_time, format = "%d-%m-%Y %H:%M"), "%M"))
second_data$trans_date_trans_time <- NULL

#DOB TO AGE
dob <- dmy(second_data$dob)
years <- year(dob)
curr_year <- year(Sys.Date())

second_data$age <- curr_year - years
second_data$dob <- NULL

second_data$is_fraud <- as.numeric(second_data$is_fraud)

numeric_vars <- second_data[sapply(second_data, is.numeric)]
cor_matrix <- cor(numeric_vars, use = 'complete.obs')

corrplot(cor_matrix, method = 'color', tl.cex = 0.8)

summary(second_data)

# ----------------------- DATA SELECTION -----------------------
#
#
common_columns <- intersect(colnames(first_data), colnames(second_data))

first_data <- first_data[, common_columns]
second_data <- second_data[, common_columns]
sampled_first_data <- first_data[sample(nrow(first_data), 15000), ]

# ----------------------- DATA INTEGRATION -----------------------
#
#
combined_data <- rbind(sampled_first_data, second_data)
combined_data <- combined_data[!duplicated(combined_data), ]
combined_data$is_fraud <- substr(combined_data$is_fraud, 1, 1)
combined_data$is_fraud <- as.numeric(combined_data$is_fraud)
nrow(combined_data)

library(digest)


# ----------------------- EXPLORATORY DESIGN ANALYSIS -----------------------
#
#
library(GGally)
library(isotree)
library(ggplot2)
library(dplyr)
library(tidyr)

# Histogram for Transaction Amount
ggplot(combined_data, aes(x =amt)) +
  geom_histogram(binwidth = 50, fill = 'skyblue', color = 'black') +
  labs(title = 'Distribution of Transaction Amounts', x = 'Transaction Amount', y = 'Frequency')

# Density Plot for Age
ggplot(combined_data, aes(x = age)) +
  geom_density(fill = 'lightgreen') +
  labs(title = 'Age Distribution of Customers', x = 'Age', y = 'Density')

# Box Plot for Transaction Amount by Merchant
ggplot(combined_data, aes(x = merchant, y = amt)) +
  geom_boxplot() +
  labs(title = 'Transaction Amount by Merchant', x = 'Merchant', y = 'Transaction Amount') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Box Plot for Age by Job Category
ggplot(combined_data, aes(x = job, y = age)) +
  geom_boxplot() +
  labs(title = 'Age Distribution by Job Category', x = 'Job Category', y = 'Age') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# CORRELATION MATRIX
library(corrplot)
numeric_vars <- combined_data[sapply(combined_data, is.numeric)]
cor_matrix <- cor(numeric_vars, use = 'complete.obs')

# Plot heatmap
corrplot(cor_matrix, method = 'color', tl.cex = 0.8)

summary(combined_data)

#Barplot for categories with fraud transaction
fraud_transactions <- combined_data %>% filter(is_fraud == 1)

#JOB CATEGORY
job_fraud_counts <- fraud_transactions %>%
  count(job, name = "fraud_count") %>% arrange(desc(fraud_count))

top_15_fraud_counts <- head(job_fraud_counts, 15)


ggplot(top_15_fraud_counts, aes(x = reorder(job, fraud_count), y = fraud_count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Fraudulent Transactions by Job", x = "Job", y = "Number of Fraudulent Transactions") +
  coord_flip() +
  theme_minimal()

# JOB with no fraud
non_fraud_transactions <- combined_data %>% filter(is_fraud == 0)

# Count job occurrences for non-fraudulent transactions
job_non_fraud_counts <- non_fraud_transactions %>%
  count(job, name = "non_fraud_count") %>%
  arrange(desc(non_fraud_count))

top_15_jobs <- head(job_non_fraud_counts, 15)

ggplot(top_15_jobs, aes(x = reorder(job, non_fraud_count), y = non_fraud_count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Non Fraudulent Transactions by Job", x = "Job", y = "Number of Non Fraudulent Transactions") +
  coord_flip() +
  theme_minimal()

#COMPARISON
# Combine counts for fraud and non-fraud transactions
job_comparison <- combined_data %>%
  group_by(job) %>%
  summarise(
    fraud_count = sum(is_fraud == 1),
    non_fraud_count = sum(is_fraud == 0)
  ) %>%
  pivot_longer(cols = c(fraud_count, non_fraud_count), names_to = "type", values_to = "count")

job_comparison_20 <- head(job_comparison, 20)

ggplot(job_comparison_20, aes(x = reorder(job, count), y = count, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Fraudulent vs Non-Fraudulent Transactions by Job", x = "Job", y = "Number of Transactions") +
  coord_flip() +
  theme_minimal()

# Stacked bar chart
ggplot(job_comparison, aes(x = reorder(job, count), y = count, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Fraudulent vs Non-Fraudulent Transactions by Job", x = "Job", y = "Number of Transactions") +
  coord_flip() +
  theme_minimal()

# Plot transactions by hour
transactions_by_hour <- combined_data %>%
  group_by(trans_hour) %>%
  summarise(transaction_count = n())

ggplot(transactions_by_hour, aes(x = trans_hour, y = transaction_count)) +
  geom_line(color = 'purple') +
  labs(title = 'Transactions by Hour of Day', x = 'Hour', y = 'Number of Transactions')

#Plot transaction by hour based on is_fraud
anomaly_by_hour <- combined_data %>%
  group_by(trans_hour, is_fraud) %>%
  summarise(transaction_count = n(), .groups = "drop")

ggplot(anomaly_by_hour, aes(x = trans_hour, y = transaction_count, fill = as.factor(is_fraud))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Transactions by Hour (Grouped by Fraud Class)",
       x = "Hour of Day",
       y = "Number of Transactions",
       fill = "Fraud (0 = No, 1 = Yes)") +
  scale_fill_manual(values = c("lightblue", "red")) +
  theme_minimal()

#Plot transaction by date based on is_fraud
anomaly_by_hour <- combined_data %>%
  group_by(trans_date, is_fraud) %>%
  summarise(transaction_count = n(), .groups = "drop")

ggplot(anomaly_by_hour, aes(x = trans_date, y = transaction_count, fill = as.factor(is_fraud))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Transactions by Date (Grouped by Fraud Class)",
       x = "Date",
       y = "Number of Transactions",
       fill = "Fraud (0 = No, 1 = Yes)") +
  scale_fill_manual(values = c("lightblue", "red")) +
  theme_minimal()

#Plot city_population by date based on is_fraud
ggplot(combined_data, aes(x = city_pop, fill = as.factor(is_fraud))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Non-Fraud", "Fraud")) +
  labs(title = "Density Plot of City Population by Fraud Class",
       x = "City Population",
       y = "Density",
       fill = "Fraud Class") +
  coord_cartesian(xlim = c(0, 50000)) +
  theme_minimal()

unique(combined_data$merchant)

#Scatter plot based on amt for is_fraud
ggplot(combined_data, aes(x = amt, y = as.factor(is_fraud), color = as.factor(is_fraud))) +
  geom_jitter(alpha = 0.6, size = 2, width = 0, height = 0.1) +
  scale_color_manual(values = c("blue", "red"), labels = c("Non-Fraud", "Fraud")) +
  labs(title = "Scatter Plot of Transaction Amount by Fraud Class",
       x = "Transaction Amount",
       y = "Fraud Class (0 = Non-Fraud, 1 = Fraud)",
       color = "Fraud Class") +
  theme_minimal()

#Density Plot based on amount
ggplot(combined_data, aes(x = amt, fill = as.factor(is_fraud))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Non-Fraud", "Fraud")) +
  labs(title = "Density Plot of Transaction Amount by Fraud Class",
       x = "Transaction Amount",
       y = "Density",
       fill = "Fraud Class") +
  coord_cartesian(xlim = c(0, 2000)) +
  theme_minimal()
summary(combined_data)

#Frequency for transaction category based on is_fraud
category_counts <- combined_data %>%
  group_by(category, is_fraud) %>%
  summarise(transaction_count = n(), .groups = "drop")

ggplot(category_counts, aes(x = reorder(category, transaction_count), y = transaction_count, fill = as.factor(is_fraud))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Non-Fraud", "Fraud")) +
  labs(title = "Transaction Frequency by Category and Fraud Class",
       x = "Category",
       y = "Number of Transactions",
       fill = "Fraud Class") +
  coord_flip() +
  theme_minimal()

# ----------------------- DATA TRANSFORMATION (PT. 2) -----------------------
#
#
#PRE-PROCESSING
#Frequency Encoding (FOR JOB)
frequency_table <- table(combined_data$job)
frequency_encoding <- frequency_table[combined_data$job] / length(combined_data$job)
combined_data$job_freq <- as.numeric(frequency_encoding)

combined_data$job_freq
combined_data$job <- NULL

# Frequency Encoding (FOR MERCHANT)
frequency_merchant_table <- table(combined_data$merchant)
frequency_merchant_encoding <- frequency_merchant_table[combined_data$merchant] / length(combined_data$merchant)
combined_data$merchant_freq <- as.numeric(frequency_merchant_encoding)
combined_data$merchant_freq
combined_data$merchant <- NULL

summary(combined_data)

#ONEHOT ENCODING (FOR CATEGORY AND STATE)
library("fastDummies")

combined_data <- dummy_cols(combined_data, select_columns = "category", remove_first_dummy = FALSE, remove_selected_columns = TRUE)
combined_data <- dummy_cols(combined_data, select_columns = "state", remove_first_dummy = FALSE, remove_selected_columns = TRUE)
summary(combined_data)

#Frequency Encoding (FOR CITY)
frequency_city_table <- table(combined_data$city)
frequency_city_encoding <- frequency_city_table[combined_data$city] / length(combined_data$city)
combined_data$city_freq <- as.numeric(frequency_city_encoding)
combined_data$city <- NULL

# DROP TRANS NUM
combined_data$trans_num <- NULL

# Remove NA values
nrow(combined_data)
sum(is.na(combined_data))
combined_data <- na.omit(combined_data)

unsupervised_data <- combined_data %>% select(-is_fraud)
nrow(unsupervised_data)

#Extra Pre-processing for model performance
min_max_norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

unsupervised_data$amt <- min_max_norm(unsupervised_data$amt)
unsupervised_data$city_pop <- min_max_norm(unsupervised_data$city_pop)

summary(unsupervised_data)

# ----------------------- DATA MINING -----------------------
#
#
# ====================== 1. ISOLATION FOREST ====================== 
iso_model <- isolation.forest(
  unsupervised_data,
  ntrees = 1000,
  sample_size = 256,
  ndim = 1,
  nthreads = 4
)

anomaly_scores <- predict(iso_model, unsupervised_data)
combined_data$anomaly_score <- anomaly_scores

threshold <- quantile(anomaly_scores, 0.80)

combined_data$anomaly_pred <- ifelse(anomaly_scores >= threshold, 1, 0)

# Confusion matrix
table(Predicted = combined_data$anomaly_pred, Actual = combined_data$is_fraud)


# Calculate performance metrics
library(caret)
conf_matrix <- confusionMatrix(
  factor(combined_data$anomaly_pred),
  factor(combined_data$is_fraud),
  positive = "1"
)
print(conf_matrix)

ggplot(combined_data, aes(x = anomaly_score, color = factor(is_fraud))) +
  geom_density() +
  labs(title = "Anomaly Score Distribution by Class", x = "Anomaly Score", color = "Actual Class")

colnames(unsupervised_data)

# Remove features with low correlation to is_fraud
stripped_cols <- colnames(unsupervised_data)[0:28]
unsupervised_df_2 <- select(unsupervised_data, all_of(stripped_cols)) 
unsupervised_df_2$lat <- NULL
unsupervised_df_2$long <- NULL
unsupervised_df_2$city_pop <- NULL
unsupervised_df_2$merch_lat <- NULL
unsupervised_df_2$merch_long <- NULL
unsupervised_df_2$merchant_freq <- NULL
unsupervised_df_2$trans_year <- NULL
unsupervised_df_2$trans_month <- NULL
unsupervised_df_2$trans_date <- NULL
unsupervised_df_2$trans_minute <- NULL

summary(unsupervised_df_2)

# --------------------------------------------------------
# Second isolation forest with only important features
# --------------------------------------------------------
iso_model2 <- isolation.forest(
  unsupervised_df_2,
  ntrees = 1000,
  sample_size = 256,
  ndim = 1,
  nthreads = 4
)

anomaly_scores2 <- predict(iso_model2, unsupervised_df_2)
# combined_data$anomaly_score <- anomaly_scores

threshold2 <- quantile(anomaly_scores2, 0.80)

# combined_data$anomaly_pred <- ifelse(anomaly_scores >= threshold, 1, 0)
anomaly_scores2_cluster <- ifelse(anomaly_scores2 >= threshold2, 1, 0)

# Confusion matrix
table(Predicted = anomaly_scores2_cluster, Actual = combined_data$is_fraud)

# Calculate performance metrics
library(caret)
conf_matrix <- confusionMatrix(
  factor(anomaly_scores2_cluster),
  factor(combined_data$is_fraud),
  positive = "1"
)
print(conf_matrix)

ggplot(combined_data, aes(x = anomaly_scores2, color = factor(is_fraud))) +
  geom_density() +
  labs(title = "Anomaly Score Distribution by Class", x = "Anomaly Score", color = "Actual Class")

colnames(unsupervised_df_2)

# ---------------------------------------
# Isolation Forest for Dataset 1 only
# ---------------------------------------
first_data_test <- sampled_first_data %>% select(-is_fraud)

iso_model3 <- isolation.forest(
  first_data_test,
  ntrees = 1000,
  sample_size = 256,
  ndim = 1,
  nthreads = 4
)

anomaly_scores3 <- predict(iso_model3, first_data_test)
threshold3 <- quantile(anomaly_scores3, 0.80)
anomaly_scores3_cluster <- ifelse(anomaly_scores3 >= threshold3, 1, 0)

# Confusion matrix
table(Predicted = anomaly_scores3_cluster, Actual = sampled_first_data$is_fraud)

# Calculate performance metrics
conf_matrix <- confusionMatrix(
  factor(anomaly_scores3_cluster),
  factor(sampled_first_data$is_fraud),
  positive = "1"
)
print(conf_matrix)

ggplot(sampled_first_data, aes(x = anomaly_scores3, color = factor(is_fraud))) +
  geom_density() +
  labs(title = "Anomaly Score Distribution (Dataset 1)", x = "Anomaly Score", color = "Actual Class")

# ---------------------------------------
# Isolation Forest for Dataset 2 only
# ---------------------------------------
sum(is.na(second_data))
second_data <- na.omit(second_data)

sec_data_test <- second_data %>% select(-is_fraud)
sec_data_test <- na.omit(sec_data_test)

iso_model4 <- isolation.forest(
  sec_data_test,
  ntrees = 1000,
  sample_size = 256,
  ndim = 1,
  nthreads = 4
)

anomaly_scores4 <- predict(iso_model4, sec_data_test)
threshold4 <- quantile(anomaly_scores4, 0.80)
anomaly_scores4_cluster <- ifelse(anomaly_scores4 >= threshold4, 1, 0)

# Confusion matrix
table(Predicted = anomaly_scores4_cluster, Actual = second_data$is_fraud)

# Calculate performance metrics
conf_matrix <- confusionMatrix(
  factor(anomaly_scores4_cluster),
  factor(second_data$is_fraud),
  positive = "1"
)
print(conf_matrix)

ggplot(second_data, aes(x = anomaly_scores4, color = factor(is_fraud))) +
  geom_density() +
  labs(title = "Anomaly Score Distribution (Dataset 2)", x = "Anomaly Score", color = "Actual Class")

# ====================== 2. SUPPORT VECTOR MACHINE ====================== 
#Make is_fraud a factor
combined_data$is_fraud <- as.factor(combined_data$is_fraud)

#Dataset Split
set.seed(123)
shuffled_data <- combined_data[sample(1:nrow(combined_data)), ]


trainIndex <- createDataPartition(shuffled_data$is_fraud, p = 0.70, list = FALSE)

dfTrain <- shuffled_data[trainIndex, ]
dfTest <- shuffled_data[-trainIndex, ]

#Train Model
library(e1071)
svm_model <- svm(
  is_fraud ~ .,
  data = dfTrain,
  kernel = "radial",
  cost = 1,
  gamma = 0.1,
  probability = TRUE
)

print(svm_model)


predictions <- predict(svm_model, newdata = dfTest, probability = TRUE)
prob_values <- attr(predictions, "probabilities")[, "1"]

conf_matrix <- confusionMatrix(predictions, dfTest$is_fraud, positive = "1")
print(conf_matrix)

#VISUALIZE ROC curve
library(pROC)
roc_obj <- roc(dfTest$is_fraud, prob_values, levels = c("0", "1"))
plot(roc_obj, col = "blue", main = sprintf("SVM ROC (AUC = %.3f)", auc(roc_obj)))

# --------------------------------------------------------
# Second SVM with only important features
# --------------------------------------------------------

unsupervised_data_2 <- shuffled_data
stripped_cols <- colnames(shuffled_data)[0:29]
unsupervised_df_2 <- select(shuffled_data, all_of(stripped_cols))
unsupervised_df_2$lat <- NULL
unsupervised_df_2$long <- NULL
unsupervised_df_2$city_pop <- NULL
unsupervised_df_2$merch_lat <- NULL
unsupervised_df_2$merch_long <- NULL
unsupervised_df_2$merchant_freq <- NULL
unsupervised_df_2$trans_year <- NULL
unsupervised_df_2$trans_month <- NULL
unsupervised_df_2$trans_date <- NULL
unsupervised_df_2$trans_minute <- NULL


trainIndex <- createDataPartition(unsupervised_df_2$is_fraud, p = 0.70, list = FALSE)

dfTrain <- unsupervised_df_2[trainIndex, ]
dfTest <- unsupervised_df_2[-trainIndex, ]

#TRAIN MODEL
library(e1071)
svm_model <- svm(
  is_fraud ~ .,
  data = dfTrain,
  kernel = "radial",
  cost = 1,
  gamma = 0.1,
  probability = TRUE
)

print(svm_model)


predictions <- predict(svm_model, newdata = dfTest, probability = TRUE)
prob_values <- attr(predictions, "probabilities")[, "1"]

conf_matrix <- confusionMatrix(predictions, dfTest$is_fraud, positive = "1")
print(conf_matrix)


#Visualize ROC Curve
roc_obj <- roc(dfTest$is_fraud, prob_values, levels = c("0", "1"))
plot(roc_obj, col = "blue", main = sprintf("SVM ROC (AUC = %.3f)", auc(roc_obj)))


# ====================== 3. K-MEANS CLUSTERING ====================== 

install.packages("ClusterR")
install.packages("cluster")

# Loading package
library(ClusterR)
library(cluster)

# Check for NA values
sum(is.na(unsupervised_data))
unsupervised_data <- na.omit(unsupervised_data)

set.seed(240) # Setting seed
kmeans_model <- kmeans(unsupervised_data, centers = 2, nstart = 20)
kmeans_model

unsupervised_data <- unsupervised_data[, colnames(kmeans_model$centers)]
distances <- apply(unsupervised_data, 1, function(row) 
  min(sapply(1:2, function(i) sum((row - kmeans_model$centers[i, ])^2))))

dim(unsupervised_data)
dim(kmeans_model$centers)

length(distances)        # Length of the distances vector
nrow(combined_data)      # Number of rows in the dataframe

combined_data$distance <- distances

threshold <- quantile(distances, 0.80)

combined_data$anomaly_pred <- ifelse(distances >= threshold, 1, 0)

columns_used <- colnames(kmeans_model$centers)
print(columns_used)

# Confusion matrix

length(combined_data$anomaly_pred)
length(combined_data$is_fraud)


table(Predicted = combined_data$anomaly_pred, Actual = combined_data$is_fraud)


# Calculate performance metrics
library(caret)
conf_matrix <- confusionMatrix(
  factor(combined_data$anomaly_pred),
  factor(combined_data$is_fraud),
  positive = "1"
)
print(conf_matrix)

ggplot(combined_data, aes(x = distances, color = factor(is_fraud))) +
  geom_density() +
  labs(title = "Distance Distribution by Class", x = "Distance to Nearest Cluster Center", color = "Actual Class")

stripped_cols <- colnames(unsupervised_data)[0:28]
unsupervised_df_2 <- select(unsupervised_data, all_of(stripped_cols)) 
unsupervised_df_2$lat <- NULL
unsupervised_df_2$long <- NULL
unsupervised_df_2$city_pop <- NULL
unsupervised_df_2$merch_lat <- NULL
unsupervised_df_2$merch_long <- NULL
unsupervised_df_2$merchant_freq <- NULL
unsupervised_df_2$trans_year <- NULL
unsupervised_df_2$trans_month <- NULL
unsupervised_df_2$trans_date <- NULL
unsupervised_df_2$trans_minute <- NULL

summary(unsupervised_df_2)

set.seed(240) # Setting seed
kmeans_model <- kmeans(unsupervised_df_2, centers = 2, nstart = 20)
kmeans_model

unsupervised_df_2 <- unsupervised_df_2[, colnames(kmeans_model$centers)]
distances <- apply(unsupervised_df_2, 1, function(row) 
  min(sapply(1:2, function(i) sum((row - kmeans_model$centers[i, ])^2))))

combined_data$distance <- distances

threshold <- quantile(distances, 0.80)

combined_data$anomaly_pred <- ifelse(distances >= threshold, 1, 0)

# Confusion matrix
table(Predicted = combined_data$anomaly_pred, Actual = combined_data$is_fraud)


# Calculate performance metrics
library(caret)
conf_matrix <- confusionMatrix(
  factor(combined_data$anomaly_pred),
  factor(combined_data$is_fraud),
  positive = "1"
)
print(conf_matrix)

ggplot(combined_data, aes(x = distances, color = factor(is_fraud))) +
    geom_density() +
  labs(title = "Distance Distribution by Class", x = "Distance to Nearest Cluster Center", color = "Actual Class")

print(kmeans_model$cluster)
print(kmeans_model$centers)

data_with_clusters <- cbind(unsupervised_data, Cluster = kmeans_model$cluster)
print(data_with_clusters)

colnames(data_with_clusters)

data_with_clusters <- data_with_clusters %>%
  mutate(anomaly_pred = ifelse(Cluster == 1, "anomalous", "normal"))

ggplot(data_with_clusters, aes(x = amt, y = trans_hour, color = as.factor(Cluster))) +
  geom_point(size = 3) +
  labs(color = "Cluster") +
  theme_minimal()

ggplot(data_with_clusters, aes(x = amt, y = category_shopping_net, color = anomaly_pred)) +
  geom_point(size = 3) +
  labs(color = "Anomaly Prediction") +
  theme_minimal()

# ======================================================

# PCA

# Perform PCA on the data (assuming you want to use all columns for PCA)
pca_result <- prcomp(unsupervised_data, scale. = TRUE)

# Get the first two principal components
pca_data <- data.frame(pca_result$x[, 1:2])  # First two PCs

# Add the K-means cluster assignments to the PCA result
pca_data$Cluster <- factor(kmeans_model$cluster)

pca_data <- pca_data %>%
  mutate(anomaly_pred = ifelse(Cluster == 2, "anomalous", "normal"))

# Plot the clusters without x and y axes
ggplot(pca_data, aes(x = PC1, y = PC2, color = anomaly_pred)) +
  geom_point(size = 3) +
  labs(color = "Cluster") +
  theme_minimal()

sum(pca_data$anomaly_pred == "anomalous")
sum(pca_data$anomaly_pred == "normal")

# Performance metrics PCA
pca_data_conf_matrix <- pca_data %>%
  mutate(Cluster = ifelse(Cluster == 2, 1, 0))

conf_matrix <- confusionMatrix(
  factor(pca_data_conf_matrix$Cluster),
  factor(combined_data$is_fraud),
  positive = "1"
)
print(conf_matrix)

# Confusion matrix PCA
table(Predicted = pca_data_conf_matrix$Cluster, Actual = combined_data$is_fraud)



