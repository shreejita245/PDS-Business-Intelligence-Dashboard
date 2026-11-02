# ================================================================
# Business Performance Intelligence Platform
# Shiny Dashboard -- app.R
# ================================================================

pkgs <- c("shiny", "bslib", "plotly", "DT", "dplyr", "randomForest")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(randomForest)

# ---- Load data ----
data_full    <- read.csv("C:/Users/SHREEJITA/Downloads/superstore_with_anomalies.csv",
                         stringsAsFactors = FALSE)
cat_cols     <- c("Ship.Mode", "Segment", "Region", "Category", "Sub.Category")
data_full[cat_cols] <- lapply(data_full[cat_cols], as.factor)

anomaly_df   <- data_full[data_full$Is_Anomaly == 1, ]
threshold_df <- read.csv("C:/Users/SHREEJITA/Downloads/discount_thresholds.csv")
regional_df  <- read.csv("C:/Users/SHREEJITA/Downloads/regional_risk.csv")
subcat_df    <- read.csv("C:/Users/SHREEJITA/Downloads/subcategory_analysis.csv")
model        <- readRDS("C:/Users/SHREEJITA/Downloads/profit_model.rds")

# ---- ML Lab CSVs (graceful) ----
ml_reg_df <- tryCatch(read.csv("C:/Users/SHREEJITA/Downloads/reg_preds_all.csv"),  error=function(e) NULL)
ml_cls_df <- tryCatch(read.csv("C:/Users/SHREEJITA/Downloads/cls_probs_all.csv"),  error=function(e) NULL)
ml_cm_df  <- tryCatch(read.csv("C:/Users/SHREEJITA/Downloads/confusion_all.csv"),  error=function(e) NULL)

reg_model_choices <- if (!is.null(ml_reg_df)) unique(ml_reg_df$Model) else
  c("Linear Regression","Ridge Regression","Lasso Regression",
    "Decision Tree","Random Forest","Gradient Boosting",
    "SVR","KNN (k=7)","Random Forest (Tuned)","GBM (Tuned)")

cls_model_choices <- if (!is.null(ml_cls_df)) unique(ml_cls_df$Model) else
  c("Logistic Regression","Naive Bayes","Decision Tree","Random Forest","SVM")

# ---- KPIs ----
total_sales   <- round(sum(data_full$Sales) / 1e6, 2)
total_profit  <- round(sum(data_full$Profit) / 1e3, 1)
anomaly_count <- sum(data_full$Is_Anomaly)
avg_margin    <- round(mean(data_full$Profit_Margin[is.finite(data_full$Profit_Margin)]) * 100, 1)
loss_exposure <- round(sum(data_full$Profit[data_full$Profit < 0]) / 1e3, 1)

# ---- Dark plotly layout ----
dark_layout <- function(p, title = NULL) {
  p %>% layout(
    title         = list(text = title, font = list(color = "#e6edf3", size = 13)),
    paper_bgcolor = "#161b22",
    plot_bgcolor  = "#161b22",
    font          = list(color = "#8b949e", size = 12),
    xaxis         = list(gridcolor = "#21262d", zerolinecolor = "#30363d",
                         tickfont  = list(color = "#8b949e")),
    yaxis         = list(gridcolor = "#21262d", zerolinecolor = "#30363d",
                         tickfont  = list(color = "#8b949e")),
    legend        = list(font = list(color = "#e6edf3"), bgcolor = "#161b22"),
    margin        = list(t = 36, r = 16, b = 40, l = 60)
  )
}

# ================================================================
# CSS
# ================================================================
css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
*,body{font-family:'Inter',-apple-system,BlinkMacSystemFont,sans-serif!important;}
body{background:#0d1117!important;color:#e6edf3!important;}
.navbar{background:#161b22!important;border-bottom:1px solid #30363d!important;padding:0 24px!important;}
.navbar-brand{color:#58a6ff!important;font-weight:700!important;font-size:15px!important;}
.nav-link{color:#8b949e!important;font-size:13px!important;padding:16px 14px!important;}
.nav-link.active{color:#e6edf3!important;border-bottom:2px solid #58a6ff!important;}
.nav-link:hover{color:#e6edf3!important;}
.card{background:#161b22!important;border:1px solid #30363d!important;border-radius:6px!important;margin-bottom:16px;}
.card-header{background:#161b22!important;border-bottom:1px solid #21262d!important;color:#8b949e!important;font-size:11px!important;font-weight:600!important;text-transform:uppercase;letter-spacing:1px;padding:10px 16px!important;}
.card-body{padding:16px!important;}
.kpi{background:#161b22;border:1px solid #30363d;border-radius:6px;padding:18px 20px;margin-bottom:16px;position:relative;overflow:hidden;}
.kpi::before{content:'';position:absolute;left:0;top:0;bottom:0;width:3px;}
.kpi-blue::before{background:#58a6ff;}.kpi-green::before{background:#3fb950;}
.kpi-red::before{background:#f85149;}.kpi-yellow::before{background:#d29922;}
.kpi-purple::before{background:#a371f7;}.kpi-teal::before{background:#39d353;}
.kpi-label{font-size:10px;color:#8b949e;text-transform:uppercase;letter-spacing:1px;font-weight:600;margin-bottom:6px;}
.kpi-value{font-size:26px;font-weight:700;color:#e6edf3;line-height:1;}
.kpi-sub{font-size:11px;color:#6e7681;margin-top:4px;}
.section-label{font-size:10px;font-weight:600;color:#8b949e;text-transform:uppercase;letter-spacing:1.2px;margin:20px 0 10px;padding-bottom:8px;border-bottom:1px solid #21262d;}
.tcard{background:#161b22;border:1px solid #30363d;border-radius:6px;padding:20px;text-align:center;margin-bottom:12px;}
.tcard-pct{font-size:40px;font-weight:700;color:#d29922;line-height:1;}
.tcard-name{font-size:13px;color:#e6edf3;margin-top:6px;font-weight:500;}
.tcard-loss{font-size:11px;color:#f85149;margin-top:4px;}
.wi-result{background:#161b22;border:1px solid #30363d;border-radius:6px;padding:28px;text-align:center;margin-bottom:12px;}
.wi-val{font-size:52px;font-weight:700;line-height:1;}
.wi-pos{color:#3fb950;}.wi-neg{color:#f85149;}
.wi-label{font-size:11px;color:#8b949e;text-transform:uppercase;letter-spacing:1px;margin-top:8px;}
.wi-note{font-size:12px;color:#6e7681;margin-top:10px;}
.tab-content>.tab-pane{padding:20px 4px 0!important;}
table.dataTable thead th{background:#21262d!important;color:#8b949e!important;font-size:10px!important;font-weight:600!important;text-transform:uppercase;letter-spacing:0.5px;border-bottom:1px solid #30363d!important;}
table.dataTable tbody tr{background:#161b22!important;color:#e6edf3!important;}
table.dataTable tbody tr:hover{background:#1c2128!important;}
table.dataTable tbody td{border-top:1px solid #21262d!important;font-size:12px;}
.dataTables_wrapper,.dataTables_info,.dataTables_length label,.dataTables_filter label{color:#8b949e!important;font-size:11px!important;}
.dataTables_filter input,.dataTables_length select{background:#21262d!important;border:1px solid #30363d!important;color:#e6edf3!important;border-radius:4px;padding:2px 6px;}
.paginate_button{background:#21262d!important;color:#8b949e!important;border:1px solid #30363d!important;border-radius:4px!important;}
.paginate_button.current{background:#1f6feb!important;color:#fff!important;border-color:#1f6feb!important;}
.selectize-input,.selectize-dropdown{background:#21262d!important;border-color:#30363d!important;color:#e6edf3!important;font-size:13px!important;}
.selectize-dropdown-content .option{color:#e6edf3!important;}
.selectize-dropdown-content .option.active{background:#1f6feb!important;}
label{color:#8b949e!important;font-size:11px!important;text-transform:uppercase;letter-spacing:0.8px;}
.irs--shiny .irs-bar{background:#1f6feb!important;border-color:#1f6feb!important;}
.irs--shiny .irs-handle{border-color:#58a6ff!important;}
.irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to{background:#1f6feb!important;}
.irs--shiny .irs-line{background:#30363d!important;}
.irs--shiny .irs-grid-text{color:#6e7681!important;}
.irs--shiny .irs-min,.irs--shiny .irs-max{color:#6e7681!important;}
::-webkit-scrollbar{width:6px;height:6px;}
::-webkit-scrollbar-track{background:#0d1117;}
::-webkit-scrollbar-thumb{background:#30363d;border-radius:3px;}
::-webkit-scrollbar-thumb:hover{background:#58a6ff;}
.ml-info-box{background:#161b22;border:1px solid #30363d;border-radius:6px;padding:14px 18px;margin-bottom:16px;}
.ml-metric{display:inline-block;margin-right:28px;}
.ml-metric-val{font-size:22px;font-weight:700;color:#58a6ff;}
.ml-metric-lbl{font-size:10px;color:#8b949e;text-transform:uppercase;letter-spacing:0.8px;display:block;margin-top:2px;}
.ml-no-data{text-align:center;padding:60px 20px;color:#8b949e;font-size:13px;background:#161b22;border:1px solid #30363d;border-radius:6px;}
.ml-no-data code{color:#58a6ff;background:#21262d;padding:2px 6px;border-radius:3px;font-size:12px;}
"

# ================================================================
# UI
# ================================================================
ui <- page_navbar(
  title = "BPI Platform",
  theme = bs_theme(version=5, bg="#0d1117", fg="#e6edf3", primary="#1f6feb"),
  tags$head(tags$style(HTML(css))),

  nav_panel("Command Center",
    fluidRow(
      column(2,div(class="kpi kpi-blue",div(class="kpi-label","Total Revenue"),div(class="kpi-value",paste0("$",total_sales,"M")),div(class="kpi-sub","9,994 transactions"))),
      column(2,div(class="kpi kpi-green",div(class="kpi-label","Total Profit"),div(class="kpi-value",paste0("$",total_profit,"K")),div(class="kpi-sub",paste0(avg_margin,"% avg margin")))),
      column(2,div(class="kpi kpi-red",div(class="kpi-label","Anomalies"),div(class="kpi-value",anomaly_count),div(class="kpi-sub","5% of all orders"))),
      column(2,div(class="kpi kpi-yellow",div(class="kpi-label","Loss Exposure"),div(class="kpi-value",paste0("$",abs(loss_exposure),"K")),div(class="kpi-sub","from loss orders"))),
      column(2,div(class="kpi kpi-purple",div(class="kpi-label","Model R^2"),div(class="kpi-value","84.3%"),div(class="kpi-sub","Random Forest"))),
      column(2,div(class="kpi kpi-teal",div(class="kpi-label","Worst Sub-Cat"),div(class="kpi-value","Tables"),div(class="kpi-sub","63.6% loss rate")))
    ),
    fluidRow(
      column(6,card(card_header("Profit by Category"),plotlyOutput("p_cat",height="260px"))),
      column(6,card(card_header("Anomaly Rate by Region"),plotlyOutput("p_region",height="260px")))
    ),
    fluidRow(column(12,card(card_header("Sales vs Profit  --  anomalies highlighted in red"),plotlyOutput("p_scatter",height="280px"))))
  ),

  nav_panel("Anomaly Hunter",
    fluidRow(
      column(3,selectInput("f_cat","Category",choices=c("All",levels(data_full$Category)))),
      column(3,selectInput("f_reg","Region",choices=c("All",levels(data_full$Region)))),
      column(3,selectInput("f_type","Filter",choices=c("All Anomalies","Loss-Making Only","High Discount (>30%)"))),
      column(3,br(),div(style="padding-top:6px;font-size:13px;color:#8b949e;",textOutput("ah_n")))
    ),
    DTOutput("tbl_anomaly")
  ),

  nav_panel("Discount Intelligence",
    div(class="section-label","Safe discount ceiling by category"),
    fluidRow(lapply(seq_len(nrow(threshold_df)),function(i){
      column(4,div(class="tcard",
        div(class="tcard-pct",paste0(threshold_df$Safe_Discount[i],"%")),
        div(class="tcard-name",threshold_df$Category[i]),
        div(class="tcard-loss",paste0("Avg loss above ceiling: -$",abs(threshold_df$Avg_Loss_Above[i])))
      ))
    })),
    div(class="section-label","What-if profit simulator"),
    fluidRow(
      column(4,card(card_header("Configure Order"),
        selectInput("wi_cat","Category",choices=levels(data_full$Category)),
        selectInput("wi_reg","Region",choices=levels(data_full$Region)),
        selectInput("wi_seg","Segment",choices=levels(data_full$Segment)),
        selectInput("wi_ship","Ship Mode",choices=levels(data_full$Ship.Mode)),
        sliderInput("wi_sales","Order Value ($)",10,5000,500,10),
        sliderInput("wi_discount","Discount (%)",0,80,0,5),
        sliderInput("wi_qty","Quantity",1,14,3,1)
      )),
      column(8,card(card_header("Predicted Outcome"),uiOutput("wi_display"),plotlyOutput("wi_curve",height="240px")))
    )
  ),

  nav_panel("Regional Risk",
    fluidRow(
      column(7,card(card_header("Anomaly Rate & Loss by Region"),plotlyOutput("p_reg_risk",height="300px"))),
      column(5,card(card_header("Risk Table"),DTOutput("tbl_region")))
    )
  ),

  nav_panel("Sub-Category Intel",
    fluidRow(
      column(7,card(card_header("Loss Rate by Sub-Category"),plotlyOutput("p_subcat",height="400px"))),
      column(5,card(card_header("Full Rankings"),DTOutput("tbl_subcat")))
    )
  ),

  nav_panel("ML Lab",
    fluidRow(
      column(3,
        card(card_header("Select Model"),
          selectInput("ml_type","Model Type",
            choices=c("Regression (Profit Prediction)"="regression",
                      "Classification (Loss Detection)"="classification")),
          uiOutput("ml_model_selector"),
          br(),
          div(style="font-size:11px;color:#6e7681;line-height:1.7;",
            "Run ",tags$code("model_plots.R")," first to generate chart data.",br(),
            "Each model shows its own dedicated visualisations.")
        )
      ),
      column(9,
        uiOutput("ml_metrics_bar"),
        fluidRow(
          column(6,card(card_header(uiOutput("ml_chart1_title")),uiOutput("ml_chart1_ui"))),
          column(6,card(card_header(uiOutput("ml_chart2_title")),uiOutput("ml_chart2_ui")))
        )
      )
    )
  )
)

# ================================================================
# SERVER
# ================================================================
server <- function(input, output, session) {

  # Command Center
  output$p_cat <- renderPlotly({
    df <- aggregate(Profit ~ Category, data_full, sum)
    df$col <- ifelse(df$Profit > 0, "#3fb950", "#f85149")
    plot_ly(df, x=~Category, y=~round(Profit,0), type="bar",
      marker=list(color=~col,line=list(color="transparent",width=0)),
      hovertemplate="<b>%{x}</b><br>$%{y:,.0f}<extra></extra>") %>% dark_layout()
  })

  output$p_region <- renderPlotly({
    df <- regional_df[order(regional_df$Anomaly_Rate_Pct), ]
    plot_ly(df, x=~Anomaly_Rate_Pct, y=~Region, type="bar", orientation="h",
      marker=list(color="#f85149",line=list(color="transparent",width=0)),
      hovertemplate="<b>%{y}</b><br>%{x:.1f}% anomaly rate<extra></extra>") %>% dark_layout()
  })

  output$p_scatter <- renderPlotly({
    norm <- data_full[data_full$Is_Anomaly==0, ]
    anom <- data_full[data_full$Is_Anomaly==1, ]
    plot_ly() %>%
      add_trace(data=norm,x=~Sales,y=~Profit,type="scatter",mode="markers",name="Normal",
        marker=list(color="#58a6ff",opacity=0.25,size=4),
        hovertemplate="$%{x:.0f} -> $%{y:.0f}<extra>Normal</extra>") %>%
      add_trace(data=anom,x=~Sales,y=~Profit,type="scatter",mode="markers",name="Anomaly",
        marker=list(color="#f85149",opacity=0.85,size=6,symbol="x"),
        hovertemplate="$%{x:.0f} -> $%{y:.0f}<extra>Anomaly</extra>") %>%
      dark_layout()
  })

  # Anomaly Hunter
  anom_filtered <- reactive({
    df <- anomaly_df
    if (input$f_cat  != "All") df <- df[df$Category == input$f_cat, ]
    if (input$f_reg  != "All") df <- df[df$Region   == input$f_reg, ]
    if (input$f_type == "Loss-Making Only")      df <- df[df$Profit   <  0,   ]
    if (input$f_type == "High Discount (>30%)") df <- df[df$Discount > 0.3, ]
    df
  })
  output$ah_n <- renderText({ paste0(nrow(anom_filtered()), " anomalies shown") })
  output$tbl_anomaly <- renderDT({
    base_cols <- c("Category","Sub.Category","Region","Sales","Profit","Discount","Profit_Margin","Anomaly_Score")
    raw <- anom_filtered(); has_expl <- "Explanation" %in% colnames(raw)
    sel_cols  <- if (has_expl) c(base_cols,"Explanation") else base_cols
    col_names <- if (has_expl) c("Category","Sub-Cat","Region","Sales","Profit","Disc","Margin","Score","Explanation")
                 else           c("Category","Sub-Cat","Region","Sales","Profit","Disc","Margin","Score")
    df <- raw[, sel_cols]
    df$Sales <- round(df$Sales,2); df$Profit <- round(df$Profit,2)
    df$Discount <- paste0(round(df$Discount*100,0),"%")
    df$Profit_Margin <- paste0(round(df$Profit_Margin*100,1),"%")
    df$Anomaly_Score <- round(df$Anomaly_Score,4)
    datatable(df,rownames=FALSE,colnames=col_names,options=list(pageLength=10,scrollX=TRUE,dom="ftip")) %>%
      formatStyle("Profit",color=styleInterval(0,c("#f85149","#3fb950")),fontWeight="bold") %>%
      formatStyle("Anomaly_Score",background=styleColorBar(c(0.5,0.8),"#f85149"),
        backgroundSize="100% 60%",backgroundRepeat="no-repeat",backgroundPosition="center")
  })

  # What-If
  wi_predict <- function(discount_pct) {
    cat_med <- median(data_full$Profit_Margin[data_full$Category==input$wi_cat & is.finite(data_full$Profit_Margin)],na.rm=TRUE)
    d <- discount_pct/100
    obs <- data.frame(Sales=input$wi_sales,Discount=d,Quantity=input$wi_qty,Profit_Margin=cat_med,
      Discount_Impact=d*input$wi_sales,
      Category=factor(input$wi_cat,levels=levels(data_full$Category)),
      Region=factor(input$wi_reg,levels=levels(data_full$Region)),
      Segment=factor(input$wi_seg,levels=levels(data_full$Segment)),
      Ship.Mode=factor(input$wi_ship,levels=levels(data_full$Ship.Mode)))
    predict(model,obs)
  }
  output$wi_display <- renderUI({
    p <- round(wi_predict(input$wi_discount),2)
    cls <- if (p>=0) "wi-pos" else "wi-neg"; pfx <- if (p>=0) "+" else "-"
    div(class="wi-result",
      div(class=paste("wi-val",cls),paste0(pfx,"$",abs(p))),
      div(class="wi-label","Predicted Profit"),
      div(class="wi-note",paste0("Discount cost: -$",round((input$wi_discount/100)*input$wi_sales,2),"  -  Quantity: ",input$wi_qty))
    )
  })
  output$wi_curve <- renderPlotly({
    disc_seq <- seq(0,80,by=5); profits <- sapply(disc_seq,wi_predict)
    cols <- ifelse(profits>=0,"#3fb950","#f85149")
    plot_ly(x=~disc_seq,y=~round(profits,2),type="scatter",mode="lines+markers",
      line=list(color="#58a6ff",width=2),marker=list(color=cols,size=7),
      hovertemplate="%{x}% discount -> $%{y:.2f}<extra></extra>") %>%
      add_segments(x=input$wi_discount,xend=input$wi_discount,y=min(profits)-5,yend=max(profits)+5,
        line=list(color="#d29922",dash="dot",width=1),showlegend=FALSE) %>%
      add_segments(x=0,xend=80,y=0,yend=0,line=list(color="#f85149",dash="dash",width=1),showlegend=FALSE) %>%
      dark_layout("Profit across discount levels")
  })

  # Regional Risk
  output$p_reg_risk <- renderPlotly({
    df <- regional_df[order(regional_df$Anomaly_Rate_Pct), ]
    plot_ly(df) %>%
      add_bars(x=~Anomaly_Rate_Pct,y=~Region,orientation="h",name="Anomaly Rate %",
        marker=list(color="#f85149"),
        hovertemplate="<b>%{y}</b><br>%{x:.1f}% anomaly rate<extra></extra>") %>%
      dark_layout()
  })
  output$tbl_region <- renderDT({
    df <- regional_df[order(-regional_df$Anomaly_Rate_Pct),c("Region","Total_Orders","Anomaly_Count","Anomaly_Rate_Pct","Total_Loss_Exposure")]
    df$Anomaly_Rate_Pct <- paste0(df$Anomaly_Rate_Pct,"%")
    df$Total_Loss_Exposure <- paste0("-$",abs(df$Total_Loss_Exposure))
    datatable(df,rownames=FALSE,colnames=c("Region","Orders","Anomalies","Rate","Loss Exposure"),options=list(pageLength=5,dom="t"))
  })

  # Sub-Category Intel
  output$p_subcat <- renderPlotly({
    df <- subcat_df[order(subcat_df$Loss_Rate_Pct), ]
    df$col <- ifelse(df$Loss_Rate_Pct>40,"#f85149",ifelse(df$Loss_Rate_Pct>15,"#d29922","#3fb950"))
    plot_ly(df,x=~Loss_Rate_Pct,y=~reorder(Sub.Category,Loss_Rate_Pct),type="bar",orientation="h",
      marker=list(color=~col,line=list(color="transparent",width=0)),
      hovertemplate="<b>%{y}</b><br>%{x:.1f}% loss rate<extra></extra>") %>% dark_layout()
  })
  output$tbl_subcat <- renderDT({
    df <- subcat_df[order(-subcat_df$Loss_Rate_Pct),c("Sub.Category","Order_Count","Avg_Profit","Avg_Margin","Avg_Discount","Loss_Rate_Pct")]
    datatable(df,rownames=FALSE,colnames=c("Sub-Category","Orders","Avg Profit","Margin %","Disc %","Loss %"),
      options=list(pageLength=10,dom="tip")) %>%
      formatStyle("Loss_Rate_Pct",color=styleInterval(c(15,40),c("#3fb950","#d29922","#f85149")),fontWeight="bold") %>%
      formatStyle("Avg_Profit",color=styleInterval(0,c("#f85149","#3fb950")))
  })

  # ================================================================
  # ML Lab
  # ================================================================

  output$ml_model_selector <- renderUI({
    if (input$ml_type == "regression")
      selectInput("ml_model","Model Name",choices=reg_model_choices)
    else
      selectInput("ml_model","Model Name",choices=cls_model_choices)
  })

  ml_no_data <- function() {
    div(class="ml-no-data",HTML("&#128202; No data yet.<br>Run <code>model_plots.R</code> first, then refresh."))
  }

  output$ml_metrics_bar <- renderUI({
    req(input$ml_model)
    if (input$ml_type == "regression") {
      if (is.null(ml_reg_df)) return(NULL)
      df <- ml_reg_df[ml_reg_df$Model == input$ml_model, ]
      if (nrow(df) == 0) return(NULL)
      rmse <- round(sqrt(mean((df$Predicted - df$Actual)^2)), 2)
      mae  <- round(mean(abs(df$Predicted - df$Actual)), 2)
      r2   <- round(1 - sum((df$Actual - df$Predicted)^2) / sum((df$Actual - mean(df$Actual))^2), 4)
      div(class="ml-info-box",
        span(class="ml-metric",span(class="ml-metric-val",rmse),span(class="ml-metric-lbl","RMSE ($)")),
        span(class="ml-metric",span(class="ml-metric-val",mae), span(class="ml-metric-lbl","MAE ($)")),
        span(class="ml-metric",span(class="ml-metric-val",r2),  span(class="ml-metric-lbl","R\u00B2"))
      )
    } else {
      if (is.null(ml_cls_df)) return(NULL)
      df <- ml_cls_df[ml_cls_df$Model == input$ml_model, ]
      if (nrow(df) == 0) return(NULL)
      pred_cls <- as.numeric(df$Prob > 0.5)
      acc  <- round(mean(pred_cls == df$Actual) * 100, 1)
      tp   <- sum(pred_cls == 1 & df$Actual == 1)
      fp   <- sum(pred_cls == 1 & df$Actual == 0)
      fn   <- sum(pred_cls == 0 & df$Actual == 1)
      prec <- round(if ((tp+fp)>0) tp/(tp+fp)*100 else 0, 1)
      rec  <- round(if ((tp+fn)>0) tp/(tp+fn)*100 else 0, 1)
      suppressMessages({ suppressWarnings({ library(pROC) }) })
      auc_val <- tryCatch(round(as.numeric(auc(roc(df$Actual, df$Prob, quiet=TRUE))), 4), error=function(e) "—")
      div(class="ml-info-box",
        span(class="ml-metric",span(class="ml-metric-val",paste0(acc,"%")), span(class="ml-metric-lbl","Accuracy")),
        span(class="ml-metric",span(class="ml-metric-val",paste0(prec,"%")),span(class="ml-metric-lbl","Precision")),
        span(class="ml-metric",span(class="ml-metric-val",paste0(rec,"%")), span(class="ml-metric-lbl","Recall")),
        span(class="ml-metric",span(class="ml-metric-val",auc_val),         span(class="ml-metric-lbl","AUC"))
      )
    }
  })

  output$ml_chart1_title <- renderUI({
    if (is.null(input$ml_type) || input$ml_type=="regression") "Predicted vs Actual Profit" else "Confusion Matrix"
  })
  output$ml_chart2_title <- renderUI({
    if (is.null(input$ml_type) || input$ml_type=="regression") "Residual Plot" else "ROC Curve"
  })

  output$ml_chart1_ui <- renderUI({
    if (input$ml_type=="regression") {
      if (is.null(ml_reg_df)) return(ml_no_data()); plotlyOutput("ml_plot1_reg",height="320px")
    } else {
      if (is.null(ml_cm_df))  return(ml_no_data()); plotlyOutput("ml_plot1_cls",height="320px")
    }
  })
  output$ml_chart2_ui <- renderUI({
    if (input$ml_type=="regression") {
      if (is.null(ml_reg_df)) return(ml_no_data()); plotlyOutput("ml_plot2_reg",height="320px")
    } else {
      if (is.null(ml_cls_df)) return(ml_no_data()); plotlyOutput("ml_plot2_cls",height="320px")
    }
  })

  # Regression: Predicted vs Actual
  output$ml_plot1_reg <- renderPlotly({
    req(input$ml_model, ml_reg_df)
    df <- ml_reg_df[ml_reg_df$Model == input$ml_model, ]
    if (nrow(df)==0) return(plot_ly() %>% dark_layout("No data"))
    rng <- range(c(df$Actual, df$Predicted))
    plot_ly(df, x=~Actual, y=~Predicted, type="scatter", mode="markers",
      marker=list(color="#58a6ff",opacity=0.45,size=5),
      hovertemplate="Actual: $%{x:.1f}<br>Predicted: $%{y:.1f}<extra></extra>") %>%
      add_trace(x=rng, y=rng, type="scatter", mode="lines",
        line=list(color="#3fb950",dash="dash",width=1.5),
        name="Perfect fit", showlegend=TRUE, hoverinfo="skip") %>%
      dark_layout(paste0(input$ml_model," — Predicted vs Actual"))
  })

  # Regression: Residuals
  output$ml_plot2_reg <- renderPlotly({
    req(input$ml_model, ml_reg_df)
    df <- ml_reg_df[ml_reg_df$Model == input$ml_model, ]
    if (nrow(df)==0) return(plot_ly() %>% dark_layout("No data"))
    df$Residual <- df$Actual - df$Predicted
    cols <- ifelse(df$Residual >= 0, "#3fb950", "#f85149")
    plot_ly(df, x=~Predicted, y=~Residual, type="scatter", mode="markers",
      marker=list(color=cols,opacity=0.5,size=5),
      hovertemplate="Pred: $%{x:.1f}<br>Residual: $%{y:.1f}<extra></extra>") %>%
      add_segments(x=min(df$Predicted), xend=max(df$Predicted), y=0, yend=0,
        line=list(color="#d29922",dash="dash",width=1.5), showlegend=FALSE) %>%
      dark_layout(paste0(input$ml_model," — Residual Plot"))
  })

  # Classification: Confusion Matrix heatmap
  output$ml_plot1_cls <- renderPlotly({
    req(input$ml_model, ml_cm_df)
    df <- ml_cm_df[ml_cm_df$Model == input$ml_model, ]
    if (nrow(df)==0) return(plot_ly() %>% dark_layout("No data"))
    val <- function(p,a){ v <- df$Freq[df$Predicted==p & df$Actual==a]; if(length(v)==0) 0L else v }
    z_mat  <- matrix(c(val(0,0),val(1,0),val(0,1),val(1,1)), nrow=2, byrow=FALSE)
    total  <- sum(z_mat)
    labels <- c("Profit (0)","Loss (1)")
    text_mat <- matrix(paste0(z_mat,"<br>(",round(z_mat/total*100,1),"%)"), nrow=2)
    plot_ly(x=labels, y=labels, z=z_mat, type="heatmap",
      colorscale=list(c(0,"#161b22"),c(1,"#1f6feb")), showscale=FALSE,
      text=text_mat,
      hovertemplate="Predicted: %{x}<br>Actual: %{y}<br>Count: %{text}<extra></extra>") %>%
      layout(xaxis=list(title="Predicted",tickfont=list(color="#8b949e")),
             yaxis=list(title="Actual",tickfont=list(color="#8b949e"),autorange="reversed")) %>%
      dark_layout(paste0(input$ml_model," — Confusion Matrix"))
  })

  # Classification: ROC Curve
  output$ml_plot2_cls <- renderPlotly({
    req(input$ml_model, ml_cls_df)
    df <- ml_cls_df[ml_cls_df$Model == input$ml_model, ]
    if (nrow(df)==0) return(plot_ly() %>% dark_layout("No data"))
    suppressMessages({ suppressWarnings({ library(pROC) }) })
    roc_obj <- tryCatch(roc(df$Actual, df$Prob, quiet=TRUE), error=function(e) NULL)
    if (is.null(roc_obj)) return(plot_ly() %>% dark_layout("ROC unavailable"))
    auc_val <- round(as.numeric(auc(roc_obj)), 4)
    roc_df  <- data.frame(FPR=1-roc_obj$specificities, TPR=roc_obj$sensitivities)
    plot_ly(roc_df, x=~FPR, y=~TPR, type="scatter", mode="lines",
      line=list(color="#3fb950",width=2.5),
      fill="tozeroy", fillcolor="rgba(63,185,80,0.08)",
      name=paste0("AUC = ",auc_val),
      hovertemplate="FPR: %{x:.3f}<br>TPR: %{y:.3f}<extra></extra>") %>%
      add_segments(x=0,xend=1,y=0,yend=1,
        line=list(color="#f85149",dash="dash",width=1), showlegend=FALSE) %>%
      dark_layout(paste0(input$ml_model," — ROC Curve  |  AUC = ",auc_val))
  })
}

shinyApp(ui, server)
