
library(randomForest)

data <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_with_anomalies.csv")
cat_cols <- c("Ship.Mode", "Segment", "Region", "Category", "Sub.Category")
data[cat_cols] <- lapply(data[cat_cols], as.factor)


categories <- levels(data$Category)

threshold_results <- data.frame(
  Category         = character(),
  Safe_Discount    = numeric(),
  Avg_Loss_Above   = numeric(),
  stringsAsFactors = FALSE
)

for (cat in categories) {
  cat_data <- data[data$Category == cat, ]

  discount_levels <- seq(0, 0.8, by = 0.05)

  for (d in discount_levels) {
    subset_above <- cat_data[cat_data$Discount >= d, ]
    if (nrow(subset_above) > 10) {
      avg_profit <- mean(subset_above$Profit)
      if (avg_profit < 0) {
        threshold_results <- rbind(threshold_results, data.frame(
          Category       = cat,
          Safe_Discount  = round((d - 0.05) * 100, 0),
          Avg_Loss_Above = round(avg_profit, 2)
        ))
        break
      }
    }
  }
}

cat("\n=== LAYER 2: Discount Thresholds ===\n")
print(threshold_results)

write.csv(threshold_results,
          "C:/Users/SHREEJITA/Downloads/discount_thresholds.csv",
          row.names = FALSE)


regional_risk <- aggregate(
  cbind(Profit, Sales, Is_Anomaly, Is_Loss) ~ Region,
  data = data,
  FUN  = function(x) round(mean(x), 4)
)

regional_counts <- as.data.frame(table(data$Region))
colnames(regional_counts) <- c("Region", "Total_Orders")

anomaly_counts <- aggregate(Is_Anomaly ~ Region, data = data, FUN = sum)
colnames(anomaly_counts)[2] <- "Anomaly_Count"

total_loss <- aggregate(
  Profit ~ Region,
  data   = data[data$Profit < 0, ],
  FUN    = function(x) round(sum(x), 2)
)
colnames(total_loss)[2] <- "Total_Loss_Exposure"

regional_risk <- merge(regional_risk, regional_counts, by = "Region")
regional_risk <- merge(regional_risk, anomaly_counts,  by = "Region")
regional_risk <- merge(regional_risk, total_loss,      by = "Region")

regional_risk$Anomaly_Rate_Pct <- round(
  regional_risk$Anomaly_Count / regional_risk$Total_Orders * 100, 2
)

regional_risk$Risk_Score <- rank(
  -regional_risk$Anomaly_Rate_Pct + regional_risk$Profit * 0.01
)

regional_risk <- regional_risk[order(regional_risk$Anomaly_Rate_Pct, decreasing = TRUE), ]

cat("\n=== LAYER 3: Regional Risk Scores ===\n")
print(regional_risk[, c("Region", "Total_Orders", "Anomaly_Count",
                         "Anomaly_Rate_Pct", "Total_Loss_Exposure",
                         "Profit")])

write.csv(regional_risk,
          "C:/Users/SHREEJITA/Downloads/regional_risk.csv",
          row.names = FALSE)


subcat_stats <- aggregate(
  cbind(Profit, Sales, Discount, Is_Loss, Is_Anomaly, Profit_Margin) ~ Sub.Category,
  data = data,
  FUN  = mean
)

subcat_counts <- as.data.frame(table(data$Sub.Category))
colnames(subcat_counts) <- c("Sub.Category", "Order_Count")

subcat_stats <- merge(subcat_stats, subcat_counts, by = "Sub.Category")

subcat_stats$Loss_Rate_Pct <- round(subcat_stats$Is_Loss * 100, 2)
subcat_stats$Avg_Profit    <- round(subcat_stats$Profit, 2)
subcat_stats$Avg_Margin    <- round(subcat_stats$Profit_Margin * 100, 2)
subcat_stats$Avg_Discount  <- round(subcat_stats$Discount * 100, 2)

subcat_stats <- subcat_stats[order(subcat_stats$Loss_Rate_Pct, decreasing = TRUE), ]

cat("\n=== LAYER 4: Sub-Category Risk Ranking ===\n")
print(subcat_stats[, c("Sub.Category", "Order_Count", "Avg_Profit",
                        "Avg_Margin", "Avg_Discount", "Loss_Rate_Pct")])

write.csv(subcat_stats,
          "C:/Users/SHREEJITA/Downloads/subcategory_analysis.csv",
          row.names = FALSE)

cat("\nAll three layers complete. CSVs saved to Downloads.\n")
