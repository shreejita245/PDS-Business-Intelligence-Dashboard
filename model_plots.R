# ================================================================
# model_plots.R
# Individual visualisations for every ML model (15 total)
# Run this AFTER models_comparison.R, or run standalone.
# Saves:
#   - 1 PNG per regression model  (predicted vs actual + residuals)
#   - 1 PNG per classifier        (confusion matrix + ROC curve)
#   - reg_preds_all.csv           (for Shiny dashboard)
#   - cls_probs_all.csv           (for Shiny dashboard ROC)
#   - confusion_all.csv           (for Shiny dashboard confusion matrix)
# ================================================================

pkgs <- c("randomForest","rpart","e1071","class","gbm",
          "glmnet","naivebayes","caret","pROC","ggplot2","dplyr")
new  <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new)

library(randomForest); library(rpart);     library(e1071)
library(class);        library(gbm);       library(glmnet)
library(naivebayes);   library(caret);     library(pROC)
library(ggplot2);      library(dplyr)

OUT <- "C:/Users/SHREEJITA/Downloads/"

# ── Data ────────────────────────────────────────────────────────
data <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_with_anomalies.csv",
                 stringsAsFactors = FALSE)
cat_cols <- c("Ship.Mode","Segment","Region","Category","Sub.Category")
data[cat_cols] <- lapply(data[cat_cols], as.factor)
data <- data[is.finite(data$Profit_Margin), ]

set.seed(42)
idx   <- sample(1:nrow(data), 0.8 * nrow(data))
train <- data[idx, ];  test <- data[-idx, ]

y_test     <- test$Profit
y_cls_test <- as.factor(test$Is_Loss)

# ── Train all 15 models ─────────────────────────────────────────
cat("Training all 15 models...\n")

# 1. Linear Regression
lm_mod <- lm(Profit ~ Sales + Discount + Quantity + Profit_Margin +
               Discount_Impact, data = train)
pred_lm <- predict(lm_mod, test)

# 2. Ridge
X_tr <- model.matrix(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact, train)[,-1]
X_te <- model.matrix(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact, test)[,-1]
ridge_cv <- cv.glmnet(X_tr, train$Profit, alpha = 0)
pred_ridge <- as.numeric(predict(ridge_cv, X_te, s = "lambda.min"))

# 3. Lasso
lasso_cv <- cv.glmnet(X_tr, train$Profit, alpha = 1)
pred_lasso <- as.numeric(predict(lasso_cv, X_te, s = "lambda.min"))

# 4. Decision Tree (regression)
dt_reg <- rpart(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact+
                  Category+Region+Segment+Ship.Mode,
                data = train, method = "anova",
                control = rpart.control(minsplit = 20, cp = 0.001))
pred_dt_reg <- predict(dt_reg, test)

# 5. Random Forest
rf_mod <- randomForest(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact+
                         Category+Region+Segment+Ship.Mode,
                       data = train, ntree = 100)
pred_rf <- predict(rf_mod, test)

# 6. GBM
gbm_mod <- gbm(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact+
                 Category+Region+Segment+Ship.Mode,
               data = train, distribution = "gaussian",
               n.trees = 200, interaction.depth = 4, shrinkage = 0.05, verbose = FALSE)
pred_gbm <- predict(gbm_mod, test, n.trees = 200)

# 7. SVR
svr_mod <- svm(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact,
               data = train, kernel = "radial", cost = 1, epsilon = 0.1)
pred_svr <- predict(svr_mod, test)

# 8. KNN
knn_sc_tr <- scale(train[, c("Sales","Discount","Quantity","Profit_Margin","Discount_Impact")])
knn_sc_te <- scale(test[,  c("Sales","Discount","Quantity","Profit_Margin","Discount_Impact")])
knn_pred  <- knn(knn_sc_tr, knn_sc_te, train$Profit, k = 7)
pred_knn  <- as.numeric(as.character(knn_pred))

# 9. Random Forest Tuned
rf_tuned <- randomForest(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact+
                           Category+Region+Segment+Ship.Mode,
                         data = train, ntree = 200, mtry = 4, nodesize = 5)
pred_rf_tuned <- predict(rf_tuned, test)

# 10. GBM Tuned
gbm_tuned <- gbm(Profit ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact+
                   Category+Region+Segment+Ship.Mode,
                 data = train, distribution = "gaussian",
                 n.trees = 300, interaction.depth = 6, shrinkage = 0.03,
                 bag.fraction = 0.8, verbose = FALSE)
pred_gbm_tuned <- predict(gbm_tuned, test, n.trees = 300)

# ── Classification models ────────────────────────────────────────
lr_mod  <- glm(Is_Loss ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact,
               data = train, family = binomial)
lr_prob <- predict(lr_mod, test, type = "response")
lr_cls  <- ifelse(lr_prob > 0.5, 1, 0)

nb_tr   <- train[, c("Sales","Discount","Quantity","Profit_Margin","Discount_Impact","Is_Loss")]
nb_te   <- test[,  c("Sales","Discount","Quantity","Profit_Margin","Discount_Impact","Is_Loss")]
nb_tr$Is_Loss <- as.factor(nb_tr$Is_Loss)
nb_te$Is_Loss <- as.factor(nb_te$Is_Loss)
nb_mod  <- naive_bayes(Is_Loss ~ ., data = nb_tr)
nb_prob <- predict(nb_mod, nb_te, type = "prob")[, "1"]
nb_cls  <- as.numeric(as.character(predict(nb_mod, nb_te)))

dt_cls  <- rpart(Is_Loss ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact+
                   Category+Region+Segment+Ship.Mode,
                 data = train, method = "class")
dt_prob <- predict(dt_cls, test, type = "prob")[, "1"]
dt_cls_pred <- as.numeric(as.character(predict(dt_cls, test, type = "class")))

rf_cls  <- randomForest(as.factor(Is_Loss) ~ Sales+Discount+Quantity+Profit_Margin+
                          Discount_Impact+Category+Region+Segment+Ship.Mode,
                        data = train, ntree = 100)
rf_prob <- predict(rf_cls, test, type = "prob")[, "1"]
rf_cls_pred <- as.numeric(as.character(predict(rf_cls, test)))

svm_cls <- svm(as.factor(Is_Loss) ~ Sales+Discount+Quantity+Profit_Margin+Discount_Impact,
               data = train, kernel = "radial", probability = TRUE, cost = 1)
svm_pred <- predict(svm_cls, test, probability = TRUE)
svm_prob <- attr(svm_pred, "probabilities")[, "1"]
svm_cls_pred <- as.numeric(as.character(svm_pred))

# ── Save CSVs for Shiny dashboard ───────────────────────────────
cat("Saving dashboard CSVs...\n")

reg_preds <- rbind(
  data.frame(Model="Linear Regression",     Actual=y_test, Predicted=pred_lm),
  data.frame(Model="Ridge Regression",      Actual=y_test, Predicted=pred_ridge),
  data.frame(Model="Lasso Regression",      Actual=y_test, Predicted=pred_lasso),
  data.frame(Model="Decision Tree",         Actual=y_test, Predicted=pred_dt_reg),
  data.frame(Model="Random Forest",         Actual=y_test, Predicted=pred_rf),
  data.frame(Model="Gradient Boosting",     Actual=y_test, Predicted=pred_gbm),
  data.frame(Model="SVR",                   Actual=y_test, Predicted=pred_svr),
  data.frame(Model="KNN (k=7)",             Actual=y_test, Predicted=pred_knn),
  data.frame(Model="Random Forest (Tuned)", Actual=y_test, Predicted=pred_rf_tuned),
  data.frame(Model="GBM (Tuned)",           Actual=y_test, Predicted=pred_gbm_tuned)
)
write.csv(reg_preds, paste0(OUT, "reg_preds_all.csv"), row.names = FALSE)

cls_probs <- rbind(
  data.frame(Model="Logistic Regression", Actual=as.numeric(as.character(y_cls_test)), Prob=lr_prob),
  data.frame(Model="Naive Bayes",         Actual=as.numeric(as.character(y_cls_test)), Prob=nb_prob),
  data.frame(Model="Decision Tree",       Actual=as.numeric(as.character(y_cls_test)), Prob=dt_prob),
  data.frame(Model="Random Forest",       Actual=as.numeric(as.character(y_cls_test)), Prob=rf_prob),
  data.frame(Model="SVM",                 Actual=as.numeric(as.character(y_cls_test)), Prob=svm_prob)
)
write.csv(cls_probs, paste0(OUT, "cls_probs_all.csv"), row.names = FALSE)

cls_pred_list <- list(
  "Logistic Regression" = lr_cls,
  "Naive Bayes"         = nb_cls,
  "Decision Tree"       = dt_cls_pred,
  "Random Forest"       = rf_cls_pred,
  "SVM"                 = svm_cls_pred
)
cm_all <- do.call(rbind, lapply(names(cls_pred_list), function(nm) {
  ct <- table(Predicted = cls_pred_list[[nm]],
              Actual    = as.numeric(as.character(y_cls_test)))
  df <- as.data.frame(ct); df$Model <- nm; df
}))
write.csv(cm_all, paste0(OUT, "confusion_all.csv"), row.names = FALSE)

# ── Helper: dark ggplot theme ────────────────────────────────────
dark_gg <- function() {
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#0d1117", color = NA),
    panel.background = element_rect(fill = "#0d1117", color = NA),
    panel.grid.major = element_line(color = "#21262d"),
    panel.grid.minor = element_blank(),
    text             = element_text(color = "#e6edf3"),
    axis.text        = element_text(color = "#8b949e"),
    axis.title       = element_text(color = "#8b949e"),
    plot.title       = element_text(color = "#e6edf3", size = 13, face = "bold"),
    plot.subtitle    = element_text(color = "#8b949e", size = 10),
    legend.background = element_rect(fill = "#161b22", color = NA),
    legend.text      = element_text(color = "#e6edf3")
  )
}

# ================================================================
# REGRESSION: individual PNG per model
# ================================================================
cat("\nGenerating regression model PNGs...\n")

reg_models <- list(
  list(name="Linear Regression",     pred=pred_lm),
  list(name="Ridge Regression",      pred=pred_ridge),
  list(name="Lasso Regression",      pred=pred_lasso),
  list(name="Decision Tree",         pred=pred_dt_reg),
  list(name="Random Forest",         pred=pred_rf),
  list(name="Gradient Boosting",     pred=pred_gbm),
  list(name="SVR",                   pred=pred_svr),
  list(name="KNN (k=7)",             pred=pred_knn),
  list(name="Random Forest (Tuned)", pred=pred_rf_tuned),
  list(name="GBM (Tuned)",           pred=pred_gbm_tuned)
)

for (m in reg_models) {
  nm       <- m$name
  preds    <- m$pred
  resids   <- y_test - preds
  rmse_val <- round(sqrt(mean(resids^2)), 2)
  r2_val   <- round(1 - sum(resids^2) / sum((y_test - mean(y_test))^2), 4)
  safe_nm  <- gsub("[^A-Za-z0-9]", "_", nm)

  df_reg <- data.frame(Actual = y_test, Predicted = preds, Residual = resids)

  p1 <- ggplot(df_reg, aes(x = Actual, y = Predicted)) +
    geom_point(color = "#58a6ff", alpha = 0.4, size = 1.8) +
    geom_abline(slope = 1, intercept = 0, color = "#3fb950", linewidth = 1, linetype = "dashed") +
    labs(title    = paste(nm, "— Predicted vs Actual"),
         subtitle = paste0("RMSE = ", rmse_val, "  |  R² = ", r2_val),
         x = "Actual Profit ($)", y = "Predicted Profit ($)") +
    dark_gg()

  p2 <- ggplot(df_reg, aes(x = Predicted, y = Residual)) +
    geom_point(color = "#a371f7", alpha = 0.4, size = 1.8) +
    geom_hline(yintercept = 0, color = "#f85149", linewidth = 1, linetype = "dashed") +
    labs(title    = paste(nm, "— Residual Plot"),
         subtitle = "Residual = Actual – Predicted  |  Should be random around 0",
         x = "Predicted Profit ($)", y = "Residual ($)") +
    dark_gg()

  # Combine side by side using base graphics layout
  png(paste0(OUT, "plot_reg_", safe_nm, ".png"), width = 1600, height = 700, res = 140)
  gridExtra::grid.arrange(p1, p2, ncol = 2)
  dev.off()

  cat("  Saved: plot_reg_", safe_nm, ".png\n", sep="")
}

# ================================================================
# CLASSIFICATION: individual PNG per model
# ================================================================
cat("\nGenerating classification model PNGs...\n")

# Pre-compute ROC objects
roc_list <- list(
  "Logistic Regression" = roc(as.numeric(as.character(y_cls_test)), lr_prob,  quiet=TRUE),
  "Naive Bayes"         = roc(as.numeric(as.character(y_cls_test)), nb_prob,  quiet=TRUE),
  "Decision Tree"       = roc(as.numeric(as.character(y_cls_test)), dt_prob,  quiet=TRUE),
  "Random Forest"       = roc(as.numeric(as.character(y_cls_test)), rf_prob,  quiet=TRUE),
  "SVM"                 = roc(as.numeric(as.character(y_cls_test)), svm_prob, quiet=TRUE)
)

for (nm in names(cls_pred_list)) {
  safe_nm  <- gsub("[^A-Za-z0-9]", "_", nm)
  roc_obj  <- roc_list[[nm]]
  auc_val  <- round(auc(roc_obj), 4)

  # Confusion matrix tile
  ct_df <- as.data.frame(table(
    Predicted = factor(cls_pred_list[[nm]], levels=c(0,1)),
    Actual    = factor(as.numeric(as.character(y_cls_test)), levels=c(0,1))
  ))
  total_n <- sum(ct_df$Freq)
  ct_df$Pct <- paste0(round(ct_df$Freq / total_n * 100, 1), "%")
  ct_df$Label <- paste0(ct_df$Freq, "\n(", ct_df$Pct, ")")

  p_cm <- ggplot(ct_df, aes(x=Predicted, y=Actual, fill=Freq)) +
    geom_tile(color="#30363d", linewidth=0.8) +
    geom_text(aes(label=Label), color="white", size=5.5, fontface="bold") +
    scale_fill_gradient(low="#161b22", high="#1f6feb", guide="none") +
    scale_x_discrete(labels=c("0"="Predicted: Profit","1"="Predicted: Loss")) +
    scale_y_discrete(labels=c("0"="Actual: Profit","1"="Actual: Loss")) +
    labs(title=paste(nm, "— Confusion Matrix"),
         subtitle=paste0("AUC = ", auc_val),
         x=NULL, y=NULL) +
    dark_gg() +
    theme(axis.text=element_text(size=10))

  # ROC curve
  roc_df <- data.frame(
    Specificity = roc_obj$specificities,
    Sensitivity = roc_obj$sensitivities
  )
  p_roc <- ggplot(roc_df, aes(x=1-Specificity, y=Sensitivity)) +
    geom_line(color="#3fb950", linewidth=1.5) +
    geom_area(fill="#3fb950", alpha=0.1) +
    geom_abline(slope=1, intercept=0, color="#f85149", linetype="dashed", linewidth=0.8) +
    annotate("text", x=0.7, y=0.15,
             label=paste0("AUC = ", auc_val),
             color="#3fb950", size=5, fontface="bold") +
    labs(title=paste(nm, "— ROC Curve"),
         subtitle="Higher AUC = better classifier",
         x="False Positive Rate (1 - Specificity)",
         y="True Positive Rate (Sensitivity)") +
    dark_gg()

  if (!requireNamespace("gridExtra", quietly=TRUE)) install.packages("gridExtra")
  library(gridExtra)

  png(paste0(OUT, "plot_cls_", safe_nm, ".png"), width=1600, height=700, res=140)
  grid.arrange(p_cm, p_roc, ncol=2)
  dev.off()

  cat("  Saved: plot_cls_", safe_nm, ".png\n", sep="")
}

cat("\n=== ALL DONE ===\n")
cat("Regression PNGs (10 files):     plot_reg_*.png\n")
cat("Classification PNGs (5 files):  plot_cls_*.png\n")
cat("Dashboard CSVs:                 reg_preds_all.csv, cls_probs_all.csv, confusion_all.csv\n")
cat("All saved to:", OUT, "\n")
