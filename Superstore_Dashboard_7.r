# app.R
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load Data
df <- read.csv("D:/LPU Sem 2/Data Cleaning with R/dataset/Sample_Superstore_With_NA.csv", encoding = "ISO-8859-1")
#finding no. of NAs
nrow(df[!complete.cases(df),])
summary(df)
# Clean missing values
df$Sales[is.na(df$Sales)] <- mean(df$Sales, na.rm = TRUE)
df$Profit[is.na(df$Profit)] <- mean(df$Profit, na.rm = TRUE)
df$Postal.Code[is.na(df$Postal.Code)] <- get_mode(df$Postal.Code)

#Now again checking NAs
nrow(df[!complete.cases(df),])
df$Order.Date <- as.Date(df$Order.Date, "%m/%d/%Y")


dashboard_ui <- dashboardPage(
  dashboardHeader(title = "Superstore Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("year", "Select Year:", choices = sort(unique(year(df$Order.Date))), selected = max(year(df$Order.Date))),
      selectInput("category", "Select Category:", choices = unique(df$Category), selected = unique(df$Category)[1])
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_sales"),
      valueBoxOutput("total_profit"),
      valueBoxOutput("total_orders")
    ),
    fluidRow(
      box(plotlyOutput("monthly_sales"), width = 12)
    ),
    fluidRow(
      box(plotlyOutput("sales_segment"), width = 6),
      box(plotlyOutput("profit_subcategory"), width = 6)
    ),
    fluidRow(
      box(plotlyOutput("profit_region"), width = 12)
    )
  )
)

# Server
dashboard_server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df %>%
      filter(year(Order.Date) == input$year, Category == input$category)
  })
  
  output$total_sales <- renderValueBox({
    valueBox(
      value = paste0("$", format(sum(filtered_data()$Sales), big.mark = ",", digits = 2)),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$total_profit <- renderValueBox({
    valueBox(
      value = paste0("$", format(sum(filtered_data()$Profit), big.mark = ",", digits = 2)),
      subtitle = "Total Profit",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$total_orders <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$Order.ID)),
      subtitle = "Total Orders",
      icon = icon("receipt"),
      color = "purple"
    )
  })
  
  output$monthly_sales <- renderPlotly({
    monthly <- filtered_data() %>%
      mutate(Month = floor_date(Order.Date, "month")) %>%
      group_by(Month) %>%
      summarise(Sales = sum(Sales))
    
    plot_ly(monthly, x = ~Month, y = ~Sales, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Monthly Sales Trend", xaxis = list(title = "Month"), yaxis = list(title = "Sales"))
  })
  
  output$sales_segment <- renderPlotly({
    seg <- filtered_data() %>%
      group_by(Segment) %>%
      summarise(Sales = sum(Sales))
    
    plot_ly(seg, labels = ~Segment, values = ~Sales, type = 'pie') %>%
      layout(title = "Sales by Segment")
  })
  
  output$profit_subcategory <- renderPlotly({
    sub_cat <- filtered_data() %>%
      group_by(Sub.Category) %>%
      summarise(Profit = sum(Profit)) %>%
      arrange(Profit)
    
    plot_ly(sub_cat, x = ~Profit, y = ~reorder(Sub.Category, Profit), type = 'bar', orientation = 'h') %>%
      layout(title = "Profit by Sub-Category", yaxis = list(title = ""))
  })
  
  output$profit_region <- renderPlotly({
    region_profit <- filtered_data() %>%
      group_by(Region) %>%
      summarise(Profit = sum(Profit))
    
    plot_ly(region_profit, x = ~Region, y = ~Profit, type = 'bar') %>%
      layout(title = "Profit by Region")
  })
}

# Run App
shinyApp(ui = dashboard_ui, server = dashboard_server)