# ============================================================
# SAC Retail Dashboard  |  Shiny + ggplot2 + dplyr
# Requires: shiny, ggplot2, dplyr, readxl
# Usage: Place script in same folder as SAC_Retailer_Dataset.xlsx
#        then run: shiny::runApp("retail_dashboard.R")
# ============================================================

# install.packages(c("shiny","ggplot2","dplyr","readxl"))  # run once if needed

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# ── Load & Prepare Data ─────────────────────────────────────
df_raw <- read_excel("SAC_Retailer_Dataset.xlsx", sheet = "Retailer Data")

# Standardise column names (replace spaces/special chars with dots)
names(df_raw) <- make.names(names(df_raw))

# Helper: readxl may return Date or numeric (Excel serial) – handle both
to_date <- function(x) {
  if (inherits(x, c("Date","POSIXct","POSIXlt"))) as.Date(x)
  else as.Date(as.numeric(x), origin = "1899-12-30")
}

df <- df_raw %>%
  mutate(
    Order.Date   = to_date(Order.Date),
    Ship.Date    = to_date(Ship.Date),
    Total_Cost   = Shipping.Costs + Storage.Costs +
                   Personnel.Cost + Selling.Costs + Purchasing.Costs,
    Profit_Check = Sales - Total_Cost
  ) %>%
  filter(!is.na(Sales), !is.na(Profit), !is.na(Order.Date))

# ── UI ──────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { font-family: Arial, sans-serif; background: #f7f7f7; }
    .kpi-box { background: #fff; border: 1px solid #ddd; border-radius: 4px;
               padding: 14px; text-align: center; margin-bottom: 12px; }
    .kpi-label { font-size: 12px; color: #888; text-transform: uppercase;
                 letter-spacing: 0.5px; margin-bottom: 4px; }
    .kpi-value { font-size: 22px; font-weight: bold; color: #222; }
    .insight-box { background: #fff; border-left: 4px solid #3a7abf;
                   padding: 12px 16px; border-radius: 2px; font-size: 14px;
                   line-height: 1.7; }
  "))),
  
  titlePanel("SAC Retail Business Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(width = 2,
      selectInput("cat",  "Category",
                  choices = c("All", sort(unique(df$Category)))),
      selectInput("seg",  "Segment",
                  choices = c("All", sort(unique(df$Segment)))),
      selectInput("ship", "Ship Mode",
                  choices = c("All", sort(unique(df$Ship.Mode)))),
      dateRangeInput("dates", "Date Range",
                     start = min(df$Order.Date),
                     end   = max(df$Order.Date),
                     min   = min(df$Order.Date),
                     max   = max(df$Order.Date))
    ),
    
    mainPanel(width = 10,
      
      # ── KPI Row ─────────────────────────────────────────
      fluidRow(
        column(3, div(class="kpi-box",
          div(class="kpi-label", "Total Sales"),
          div(class="kpi-value", textOutput("kpi_sales")))),
        column(3, div(class="kpi-box",
          div(class="kpi-label", "Total Profit"),
          div(class="kpi-value", textOutput("kpi_profit")))),
        column(3, div(class="kpi-box",
          div(class="kpi-label", "Avg Discount"),
          div(class="kpi-value", textOutput("kpi_disc")))),
        column(3, div(class="kpi-box",
          div(class="kpi-label", "Profit Margin"),
          div(class="kpi-value", textOutput("kpi_margin"))))
      ),
      
      # ── Plots Row 1 ─────────────────────────────────────
      fluidRow(
        column(6, plotOutput("plot_scatter",  height = "290px")),
        column(6, plotOutput("plot_cat_bar",  height = "290px"))
      ),
      
      # ── Plots Row 2 ─────────────────────────────────────
      fluidRow(
        column(6, plotOutput("plot_disc",     height = "290px")),
        column(6, plotOutput("plot_monthly",  height = "290px"))
      ),
      
      hr(),
      h4("Automated Insights"),
      uiOutput("insights")
    )
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output) {
  
  # Reactive filtered data
  filt <- reactive({
    d <- df
    if (input$cat  != "All") d <- filter(d, Category  == input$cat)
    if (input$seg  != "All") d <- filter(d, Segment   == input$seg)
    if (input$ship != "All") d <- filter(d, Ship.Mode == input$ship)
    d <- filter(d, Order.Date >= input$dates[1], Order.Date <= input$dates[2])
    d
  })
  
  # ── KPIs ────────────────────────────────────────────────
  fmt_k  <- function(x) paste0("$", format(round(x/1e3), big.mark=","), "K")
  fmt_m  <- function(x) paste0("$", format(round(x/1e6, 1)), "M")
  
  output$kpi_sales  <- renderText(fmt_m(sum(filt()$Sales)))
  output$kpi_profit <- renderText(fmt_k(sum(filt()$Profit)))
  output$kpi_disc   <- renderText(paste0(round(mean(filt()$Discount)*100, 1), "%"))
  output$kpi_margin <- renderText({
    d <- filt()
    paste0(round(sum(d$Profit) / sum(d$Sales) * 100, 1), "%")
  })
  
  # ── Plot 1: Sales vs Profit scatter ─────────────────────
  output$plot_scatter <- renderPlot({
    ggplot(filt(), aes(Sales, Profit)) +
      geom_point(alpha = 0.35, size = 1.6, color = "#444") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Sales vs Profit", x = "Sales ($)", y = "Profit ($)") +
      theme_minimal(base_size = 12)
  })
  
  # ── Plot 2: Profit by Category bar chart ────────────────
  output$plot_cat_bar <- renderPlot({
    filt() %>%
      group_by(Category) %>%
      summarise(Total_Profit = sum(Profit), .groups = "drop") %>%
      ggplot(aes(reorder(Category, Total_Profit), Total_Profit,
                 fill = Total_Profit > 0)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "#c0392b")) +
      scale_y_continuous(labels = scales::comma) +
      coord_flip() +
      labs(title = "Profit by Category", x = NULL, y = "Total Profit ($)") +
      theme_minimal(base_size = 12)
  })
  
  # ── Plot 3: Discount vs Profit (with trend line) ─────────
  output$plot_disc <- renderPlot({
    ggplot(filt(), aes(Discount, Profit)) +
      geom_point(alpha = 0.3, size = 1.5, color = "#555") +
      geom_smooth(method = "lm", se = TRUE, color = "#e74c3c",
                  fill = "#f1948a", linewidth = 0.8) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::percent) +
      labs(title = "Discount vs Profit", x = "Discount (%)", y = "Profit ($)") +
      theme_minimal(base_size = 12)
  })
  
  # ── Plot 4: Monthly Sales trend ──────────────────────────
  output$plot_monthly <- renderPlot({
    filt() %>%
      mutate(Month = as.Date(format(Order.Date, "%Y-%m-01"))) %>%
      group_by(Month) %>%
      summarise(Total_Sales = sum(Sales), .groups = "drop") %>%
      arrange(Month) %>%
      ggplot(aes(Month, Total_Sales)) +
      geom_line(color = "#2c3e50", linewidth = 0.8) +
      geom_point(size = 1.8, color = "#2c3e50") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
      labs(title = "Monthly Sales Trend", x = NULL, y = "Sales ($)") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
  })
  
  # ── Automated Insights ───────────────────────────────────
  output$insights <- renderUI({
    d <- filt()
    req(nrow(d) > 0)
    
    # 1. Top-performing category
    top_cat <- d %>%
      group_by(Category) %>%
      summarise(p = sum(Profit), .groups = "drop") %>%
      arrange(desc(p)) %>% slice(1)
    
    # 2. Loss-making segments
    seg_perf <- d %>%
      group_by(Segment) %>%
      summarise(p = sum(Profit), .groups = "drop")
    loss_segs <- seg_perf %>% filter(p < 0) %>% pull(Segment)
    
    # 3. Discount-profit relationship
    corr <- cor(d$Discount, d$Profit, use = "complete.obs")
    
    # Build insight lines
    i1 <- paste0("\U2022  Top category: <b>", top_cat$Category, "</b> — ",
                 "$", format(round(top_cat$p/1e3, 1), big.mark=","), "K total profit.")
    
    i2 <- if (length(loss_segs) > 0)
      paste0("\U2022  Loss-making segments: <b>", paste(loss_segs, collapse=", "), "</b>. Review pricing or cost structure.")
    else
      "\U2022  All segments are currently <b>profitable</b>."
    
    i3 <- if (corr < -0.15)
      paste0("\U2022  Discounts <b>reduce profit</b> (r = ", round(corr, 2), "). High discounting is hurting margins.")
    else if (corr > 0.1)
      paste0("\U2022  Discounts show a <b>positive correlation</b> with profit (r = ", round(corr, 2), "). Volume gains offset cost.")
    else
      paste0("\U2022  Discount impact is <b>negligible</b> (r = ", round(corr, 2), "). Other cost drivers dominate.")
    
    # Count rows with negative profit
    loss_pct <- round(mean(d$Profit < 0) * 100, 1)
    i4 <- paste0("\U2022  <b>", loss_pct, "%</b> of transactions are loss-making in current filter.")
    
    div(class = "insight-box",
        HTML(paste(i1, i2, i3, i4, sep = "<br/>")))
  })
}

# ── Run ─────────────────────────────────────────────────────
shinyApp(ui, server)
