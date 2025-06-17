library(shiny)
library(DT)
library(VIM)
library(ggplot2)
library(mice)

ui <- fluidPage(
  titlePanel(" Missing Data Imputation Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("select_columns"),
      selectInput("method", "Imputation Method", 
                  choices = c("Mean" = "mean", 
                              "Median" = "median", 
                              "kNN (from VIM)" = "knn",
                              "MICE" = "mice")),
      actionButton("impute_btn", "Impute Missing Values", icon = icon("magic"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Missingness Plot", plotOutput("missing_plot")),
        tabPanel("Original Data", DTOutput("data_table")),
        tabPanel("Imputed Data", DTOutput("imputed_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$select_columns <- renderUI({
    req(raw_data())
    numeric_vars <- names(raw_data())[sapply(raw_data(), is.numeric)]
    checkboxGroupInput("columns", "Select Columns to Impute", 
                       choices = numeric_vars, selected = numeric_vars)
  })
  
  output$missing_plot <- renderPlot({
    req(raw_data())
    aggr(raw_data(), numbers = TRUE, sortVars = TRUE, 
         cex.axis = 0.7, gap = 3, ylab = c("Missing Data", "Pattern"))
  })
  
  output$data_table <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(scrollX = TRUE))
  })
  
  imputed_data <- eventReactive(input$impute_btn, {
    df <- raw_data()
    cols <- input$columns
    
    if (input$method == "mean") {
      df[cols] <- lapply(df[cols], function(x) {
        ifelse(is.na(x), mean(x, na.rm = TRUE), x)
      })
    } else if (input$method == "median") {
      df[cols] <- lapply(df[cols], function(x) {
        ifelse(is.na(x), median(x, na.rm = TRUE), x)
      })
    } else if (input$method == "knn") {
      df <- kNN(df, variable = cols, k = 5, imp_var = FALSE)
    } else if (input$method == "mice") {
      imputed <- mice(df[, cols], m = 1, method = "pmm", printFlag = FALSE)
      df[cols] <- complete(imputed)
    }
    
    df
  })
  
  output$imputed_table <- renderDT({
    req(imputed_data())
    datatable(imputed_data(), options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server)




