install.packages("iml")
library(iml)
library(randomForest)

data <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_with_anomalies.csv")

cat_cols <- c("Ship.Mode", "Segment", "Region", "Category", "Sub.Category")
data[cat_cols] <- lapply(data[cat_cols], as.factor)

model <- readRDS("C:/Users/SHREEJITA/Downloads/profit_model.rds")

nrow(data)

features <- data[, c("Sales", "Discount", "Quantity", "Profit_Margin", "Discount_Impact", "Category", "Region", "Segment", "Ship.Mode")]

predictor <- Predictor$new(model, data = features, y = data$Profit)


install.packages("glmnet")
library(glmnet)
install.packages("gower")
library(gower)


explanation <- LocalModel$new(predictor, x.interest = features[worst, ], k = 5)

plot(explanation)


anomaly_data <- data[data$Is_Anomaly == 1, ]

anomaly_data$Explanation <- paste0(
  "Sales of $", round(anomaly_data$Sales, 2),
  " in ", anomaly_data$Category,
  " (", anomaly_data$Region, " region)",
  " with ", anomaly_data$Discount * 100, "% discount.",
  " Profit margin: ", round(anomaly_data$Profit_Margin * 100, 1), "%.",
  ifelse(anomaly_data$Profit < 0, " LOSS-MAKING transaction.", " Low profitability detected.")
)

head(anomaly_data[, c("Sales", "Profit", "Discount", "Category", "Explanation")], 3)

write.csv(anomaly_data[, c("Sales", "Profit", "Discount", "Quantity", "Category", "Region", "Segment", "Profit_Margin", "Discount_Impact", "Anomaly_Score", "Explanation")], 
          "C:/Users/SHREEJITA/Downloads/anomaly_report.csv", 
          row.names = FALSE)
