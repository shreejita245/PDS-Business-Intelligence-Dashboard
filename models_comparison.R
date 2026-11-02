pkgs <- c("randomForest","rpart","e1071","class","gbm",
          "glmnet","naivebayes","caret","pROC","ggplot2")
new <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new)

library(randomForest)
library(rpart)
library(e1071)
library(class)
library(gbm)
library(glmnet)
library(naivebayes)
library(caret)
library(pROC)
library(ggplot2)

data <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_with_anomalies.csv",
                 stringsAsFactors = FALSE)
cat_cols <- c("Ship.Mode","Segment","Region","Category","Sub.Category")
data[cat_cols] <- lapply(data[cat_cols], as.factor)
data <- data[is.finite(data$Profit_Margin), ]

set.seed(42)
idx   <- sample(1:nrow(data), 0.8 * nrow(data))
train <- data[idx,  ]
test  <- data[-idx, ]

features <- c("Sales","Discount","Quantity","Profit_Margin",
              "Discount_Impact","Category","Region","Segment","Ship.Mode")
target   <- "Profit"
cls_target <- "Is_Loss"

X_train <- train[, features]
X_test  <- test[,  features]
y_train <- train[[target]]
y_test  <- test[[target]]
y_cls_train <- as.factor(train[[cls_target]])
y_cls_test  <- as.factor(test[[cls_target]])

metrics <- function(pred, actual) {
  rmse <- sqrt(mean((pred - actual)^2))
  mae  <- mean(abs(pred - actual))
  ss_res <- sum((actual - pred)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2   <- 1 - ss_res / ss_tot
  c(RMSE = round(rmse,2), MAE = round(mae,2), R2 = round(r2,4))
}

results <- data.frame(
  Model = character(), RMSE = numeric(), MAE = numeric(), R2 = numeric(),
  stringsAsFactors = FALSE
)

add_result <- function(name, pred) {
  m <- metrics(pred, y_test)
  results <<- rbind(results, data.frame(
    Model = name, RMSE = m["RMSE"], MAE = m["MAE"], R2 = m["R2"]
  ))
}

cat("Training models...\n")

# 1. Linear Regression
lm_mod  <- lm(Profit ~ Sales + Discount + Quantity + Profit_Margin +
                Discount_Impact, data = train)
add_result("Linear Regression", predict(lm_mod, test))

# 2. Ridge Regression
X_mat_tr <- model.matrix(Profit ~ Sales + Discount + Quantity +
                           Profit_Margin + Discount_Impact, data = train)[,-1]
X_mat_te <- model.matrix(Profit ~ Sales + Discount + Quantity +
                           Profit_Margin + Discount_Impact, data = test)[,-1]
ridge_cv <- cv.glmnet(X_mat_tr, y_train, alpha = 0)
add_result("Ridge Regression", as.numeric(predict(ridge_cv, X_mat_te, s = "lambda.min")))

# 3. Lasso Regression
lasso_cv <- cv.glmnet(X_mat_tr, y_train, alpha = 1)
add_result("Lasso Regression", as.numeric(predict(lasso_cv, X_mat_te, s = "lambda.min")))

# 4. Decision Tree
dt_mod <- rpart(Profit ~ Sales + Discount + Quantity + Profit_Margin +
                  Discount_Impact + Category + Region + Segment + Ship.Mode,
                data = train, method = "anova",
                control = rpart.control(minsplit = 20, cp = 0.001))
add_result("Decision Tree", predict(dt_mod, test))

# 5. Random Forest (existing model re-trained here for comparison)
rf_mod <- randomForest(Profit ~ Sales + Discount + Quantity + Profit_Margin +
                         Discount_Impact + Category + Region + Segment + Ship.Mode,
                       data = train, ntree = 100)
add_result("Random Forest", predict(rf_mod, test))

# 6. Gradient Boosting
gbm_mod <- gbm(Profit ~ Sales + Discount + Quantity + Profit_Margin +
                 Discount_Impact + Category + Region + Segment + Ship.Mode,
               data = train, distribution = "gaussian",
               n.trees = 200, interaction.depth = 4,
               shrinkage = 0.05, verbose = FALSE)
add_result("Gradient Boosting", predict(gbm_mod, test, n.trees = 200))

# 7. SVR (Support Vector Regression)
svr_mod <- svm(Profit ~ Sales + Discount + Quantity + Profit_Margin +
                 Discount_Impact,
               data = train, kernel = "radial", cost = 1, epsilon = 0.1)
add_result("SVR", predict(svr_mod, test))

# 8. KNN Regression (numeric features only)
knn_x_tr <- scale(train[, c("Sales","Discount","Quantity",
                              "Profit_Margin","Discount_Impact")])
knn_x_te <- scale(test[,  c("Sales","Discount","Quantity",
                              "Profit_Margin","Discount_Impact")])
knn_pred <- knn(knn_x_tr, knn_x_te, y_train, k = 7)
add_result("KNN (k=7)", as.numeric(as.character(knn_pred)))

# 9. Tuned Random Forest (hyperparameter tuning)
rf_tuned <- randomForest(Profit ~ Sales + Discount + Quantity + Profit_Margin +
                           Discount_Impact + Category + Region + Segment + Ship.Mode,
                         data = train, ntree = 200, mtry = 4, nodesize = 5)
add_result("Random Forest (Tuned)", predict(rf_tuned, test))

# 10. Tuned GBM (hyperparameter tuning)
gbm_tuned <- gbm(Profit ~ Sales + Discount + Quantity + Profit_Margin +
                   Discount_Impact + Category + Region + Segment + Ship.Mode,
                 data = train, distribution = "gaussian",
                 n.trees = 300, interaction.depth = 6,
                 shrinkage = 0.03, bag.fraction = 0.8, verbose = FALSE)
add_result("GBM (Tuned)", predict(gbm_tuned, test, n.trees = 300))

cat("\n=== MODEL COMPARISON (Regression — Profit Prediction) ===\n")
results <- results[order(results$RMSE), ]
print(results)
write.csv(results, "C:/Users/SHREEJITA/Downloads/model_comparison.csv", row.names = FALSE)

# ---- Classification models (predicting Is_Loss) ----
cat("\n=== CLASSIFICATION: Predicting Loss Orders ===\n")

cls_results <- data.frame(
  Model = character(), Accuracy = numeric(), Precision = numeric(),
  Recall = numeric(), F1 = numeric(), AUC = numeric(),
  stringsAsFactors = FALSE
)

add_cls <- function(name, pred_prob, pred_class, actual) {
  cm  <- confusionMatrix(as.factor(pred_class), as.factor(actual), positive = "1")
  auc_val <- tryCatch(as.numeric(auc(roc(as.numeric(actual),
                                         as.numeric(pred_prob),
                                         quiet = TRUE))), error = function(e) NA)
  cls_results <<- rbind(cls_results, data.frame(
    Model     = name,
    Accuracy  = round(cm$overall["Accuracy"] * 100, 1),
    Precision = round(cm$byClass["Precision"] * 100, 1),
    Recall    = round(cm$byClass["Recall"] * 100, 1),
    F1        = round(cm$byClass["F1"] * 100, 1),
    AUC       = round(auc_val, 4)
  ))
}

# Logistic Regression
lr_mod  <- glm(Is_Loss ~ Sales + Discount + Quantity + Profit_Margin +
                 Discount_Impact, data = train, family = binomial)
lr_prob <- predict(lr_mod, test, type = "response")
add_cls("Logistic Regression", lr_prob, ifelse(lr_prob > 0.5, 1, 0), y_cls_test)

# Naive Bayes
nb_train <- train[, c("Sales","Discount","Quantity","Profit_Margin",
                       "Discount_Impact","Is_Loss")]
nb_train$Is_Loss <- as.factor(nb_train$Is_Loss)
nb_test  <- test[,  c("Sales","Discount","Quantity","Profit_Margin",
                       "Discount_Impact","Is_Loss")]
nb_test$Is_Loss  <- as.factor(nb_test$Is_Loss)
nb_mod   <- naive_bayes(Is_Loss ~ ., data = nb_train)
nb_prob  <- predict(nb_mod, nb_test, type = "prob")[, "1"]
add_cls("Naive Bayes", nb_prob, predict(nb_mod, nb_test), y_cls_test)

# Decision Tree classifier
dt_cls <- rpart(Is_Loss ~ Sales + Discount + Quantity + Profit_Margin +
                  Discount_Impact + Category + Region + Segment + Ship.Mode,
                data = train, method = "class")
dt_prob <- predict(dt_cls, test, type = "prob")[, "1"]
add_cls("Decision Tree", dt_prob, predict(dt_cls, test, type = "class"), y_cls_test)

# Random Forest classifier
rf_cls <- randomForest(as.factor(Is_Loss) ~ Sales + Discount + Quantity +
                         Profit_Margin + Discount_Impact +
                         Category + Region + Segment + Ship.Mode,
                       data = train, ntree = 100)
rf_prob <- predict(rf_cls, test, type = "prob")[, "1"]
add_cls("Random Forest", rf_prob, predict(rf_cls, test), y_cls_test)

# SVM classifier
svm_cls <- svm(as.factor(Is_Loss) ~ Sales + Discount + Quantity +
                 Profit_Margin + Discount_Impact,
               data = train, kernel = "radial",
               probability = TRUE, cost = 1)
svm_pred  <- predict(svm_cls, test, probability = TRUE)
svm_prob  <- attr(svm_pred, "probabilities")[, "1"]
add_cls("SVM", svm_prob, svm_pred, y_cls_test)

cat("\n=== CLASSIFICATION RESULTS ===\n")
cls_results <- cls_results[order(-cls_results$AUC), ]
print(cls_results)
write.csv(cls_results, "C:/Users/SHREEJITA/Downloads/classification_comparison.csv",
          row.names = FALSE)

# ---- Save plots ----
cat("\nGenerating comparison plots...\n")

png("C:/Users/SHREEJITA/Downloads/model_rmse_comparison.png",
    width = 1000, height = 600, res = 120)
par(mar = c(10, 5, 3, 2), bg = "#0d1117", col.axis = "#8b949e",
    col.main = "#e6edf3", col.lab = "#8b949e")
barplot(results$RMSE,
        names.arg = results$Model,
        las = 2, col = ifelse(results$RMSE == min(results$RMSE), "#3fb950", "#58a6ff"),
        main = "RMSE by Model (lower is better)",
        ylab = "RMSE", border = NA)
dev.off()

png("C:/Users/SHREEJITA/Downloads/model_r2_comparison.png",
    width = 1000, height = 600, res = 120)
par(mar = c(10, 5, 3, 2), bg = "#0d1117", col.axis = "#8b949e",
    col.main = "#e6edf3", col.lab = "#8b949e")
barplot(results$R2,
        names.arg = results$Model,
        las = 2, col = ifelse(results$R2 == max(results$R2), "#3fb950", "#58a6ff"),
        main = "R-squared by Model (higher is better)",
        ylab = "R^2", border = NA, ylim = c(0, 1))
dev.off()

png("C:/Users/SHREEJITA/Downloads/feature_importance.png",
    width = 900, height = 700, res = 120)
par(bg = "#0d1117", col.axis = "#8b949e",
    col.main = "#e6edf3", col.lab = "#8b949e")
varImpPlot(rf_tuned, col = "#58a6ff", main = "Feature Importance (Tuned RF)",
           bg = "#0d1117")
dev.off()

png("C:/Users/SHREEJITA/Downloads/roc_curves.png",
    width = 900, height = 700, res = 120)
par(bg = "#0d1117", col.axis = "#8b949e",
    col.main = "#e6edf3", col.lab = "#8b949e")
roc_lr  <- roc(as.numeric(y_cls_test), lr_prob,  quiet = TRUE)
roc_rf  <- roc(as.numeric(y_cls_test), rf_prob,  quiet = TRUE)
roc_svm <- roc(as.numeric(y_cls_test), svm_prob, quiet = TRUE)
plot(roc_lr,  col = "#58a6ff", main = "ROC Curves - Classification Models",
     lwd = 2)
plot(roc_rf,  col = "#3fb950", add = TRUE, lwd = 2)
plot(roc_svm, col = "#d29922", add = TRUE, lwd = 2)
legend("bottomright",
       legend = c(paste0("Logistic (AUC=", round(auc(roc_lr),  3), ")"),
                  paste0("Random Forest (AUC=", round(auc(roc_rf),  3), ")"),
                  paste0("SVM (AUC=", round(auc(roc_svm), 3), ")")),
       col = c("#58a6ff","#3fb950","#d29922"), lwd = 2, bg = "#161b22",
       text.col = "#e6edf3")
dev.off()

png("C:/Users/SHREEJITA/Downloads/confusion_matrix_rf.png",
    width = 700, height = 700, res = 120)
cm_rf <- confusionMatrix(predict(rf_cls, test),
                         as.factor(test$Is_Loss), positive = "1")
cm_table <- as.data.frame(cm_rf$table)
ggplot(cm_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "#30363d") +
  geom_text(aes(label = Freq), color = "white", size = 8, fontface = "bold") +
  scale_fill_gradient(low = "#161b22", high = "#1f6feb") +
  labs(title = "Confusion Matrix - Random Forest Classifier",
       x = "Predicted", y = "Actual") +
  theme_minimal() +
  theme(plot.background  = element_rect(fill = "#0d1117"),
        panel.background = element_rect(fill = "#0d1117"),
        text             = element_text(color = "#e6edf3"),
        axis.text        = element_text(color = "#8b949e", size = 12))
dev.off()

cat("\nAll done! Files saved to Downloads:\n")
cat("  model_comparison.csv\n")
cat("  classification_comparison.csv\n")
cat("  model_rmse_comparison.png\n")
cat("  model_r2_comparison.png\n")
cat("  feature_importance.png\n")
cat("  roc_curves.png\n")
cat("  confusion_matrix_rf.png\n")
