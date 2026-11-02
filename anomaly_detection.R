library(randomForest)
install.packages("solitude")
library(solitude)

data <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_engineered.csv")

cat_cols <- c("Ship.Mode", "Segment", "Region", "Category", "Sub.Category")
data[cat_cols] <- lapply(data[cat_cols], as.factor)

model <- readRDS("C:/Users/SHREEJITA/Downloads/profit_model.rds")

nrow(data)

data$Predicted_Profit <- predict(model, data)
data$Residual <- data$Profit - data$Predicted_Profit
data$Abs_Residual <- abs(data$Residual)

summary(data[, c("Residual", "Abs_Residual")])


iso_features <- data[, c("Sales", "Profit", "Discount", "Quantity", "Profit_Margin", "Discount_Impact", "Abs_Residual")]

iso_model <- isolationForest$new(sample_size = 256, num_trees = 100)
iso_model$fit(iso_features)

data$Anomaly_Score <- iso_model$scores$anomaly_score

summary(data$Anomaly_Score)


data$Anomaly_Score <- iso_model$predict(iso_features)$anomaly_score

summary(data$Anomaly_Score)

threshold <- quantile(data$Anomaly_Score, 0.95)

data$Is_Anomaly <- ifelse(data$Anomaly_Score >= threshold, 1, 0)

table(data$Is_Anomaly)

anomalies <- data[data$Is_Anomaly == 1, ]
normal    <- data[data$Is_Anomaly == 0, ]

cat("--- ANOMALIES ---\n")
cat("Avg Sales:", round(mean(anomalies$Sales), 2), "\n")
cat("Avg Profit:", round(mean(anomalies$Profit), 2), "\n")
cat("Avg Discount:", round(mean(anomalies$Discount), 2), "\n")
cat("Avg Profit Margin:", round(mean(anomalies$Profit_Margin), 4), "\n")

cat("\n--- NORMAL ---\n")
cat("Avg Sales:", round(mean(normal$Sales), 2), "\n")
cat("Avg Profit:", round(mean(normal$Profit), 2), "\n")
cat("Avg Discount:", round(mean(normal$Discount), 2), "\n")
cat("Avg Profit Margin:", round(mean(normal$Profit_Margin), 4), "\n")

write.csv(data, "C:/Users/SHREEJITA/Downloads/superstore_with_anomalies.csv", row.names = FALSE)
