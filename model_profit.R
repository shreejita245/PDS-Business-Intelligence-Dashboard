install.packages("randomForest")
library(randomForest)

data <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_engineered.csv")

cat_cols <- c("Ship.Mode", "Segment", "Region", "Category", "Sub.Category")
data[cat_cols] <- lapply(data[cat_cols], as.factor)

nrow(data)

set.seed(42)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train <- data[train_index, ]
test  <- data[-train_index, ]

nrow(train)
nrow(test)

model <- randomForest(Profit ~ Sales + Discount + Quantity + Discount_Impact + Profit_Margin + Category + Region + Segment + Ship.Mode, data = train, ntree = 100, importance = TRUE)

print(model)

varImpPlot(model, main = "Feature Importance")


predictions <- predict(model, test)

rmse <- sqrt(mean((predictions - test$Profit)^2))
mae  <- mean(abs(predictions - test$Profit))
r2   <- cor(predictions, test$Profit)^2

cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")
cat("R²:", round(r2, 4), "\n")


saveRDS(model, "C:/Users/SHREEJITA/Downloads/profit_model.rds")
