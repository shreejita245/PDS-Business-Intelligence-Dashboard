
library(DBI)
library(RSQLite)

data <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_with_anomalies.csv",
                 stringsAsFactors = FALSE)

con <- dbConnect(SQLite(), "C:/Users/SHREEJITA/Downloads/superstore.db")

dbWriteTable(con, "orders",       data,                         overwrite = TRUE)
dbWriteTable(con, "anomalies",    data[data$Is_Anomaly == 1, ], overwrite = TRUE)
dbWriteTable(con, "loss_orders",  data[data$Profit < 0, ],      overwrite = TRUE)

cat("Tables written:", paste(dbListTables(con), collapse = ", "), "\n")

cat("\n--- Query 1: Avg profit by region ---\n")
print(dbGetQuery(con, "
  SELECT Region,
         ROUND(AVG(Profit), 2)   AS Avg_Profit,
         ROUND(AVG(Discount), 3) AS Avg_Discount,
         COUNT(*)                AS Orders
  FROM orders
  GROUP BY Region
  ORDER BY Avg_Profit DESC
"))

cat("\n--- Query 2: Top 5 loss-making sub-categories ---\n")
print(dbGetQuery(con, "
  SELECT [Sub.Category],
         COUNT(*)             AS Loss_Orders,
         ROUND(SUM(Profit),2) AS Total_Loss
  FROM loss_orders
  GROUP BY [Sub.Category]
  ORDER BY Total_Loss ASC
  LIMIT 5
"))

cat("\n--- Query 3: Anomaly count by category ---\n")
print(dbGetQuery(con, "
  SELECT Category,
         COUNT(*) AS Anomaly_Count,
         ROUND(AVG(Anomaly_Score), 4) AS Avg_Score
  FROM anomalies
  GROUP BY Category
  ORDER BY Anomaly_Count DESC
"))

dbDisconnect(con)
cat("\nDatabase saved to superstore.db\n")
