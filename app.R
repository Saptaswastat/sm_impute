library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

# Define UI
ui <- fluidPage(
  titlePanel("Clinical Trial Design Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Total Patients", 100, min = 10),
      numericInput("p_t", "Success Probability (Treatment)", 0.7, min = 0, max = 1, step = 0.05),
      numericInput("p_c", "Success Probability (Control)", 0.5, min = 0, max = 1, step = 0.05),
      selectInput("design", "Allocation Design", choices = c("CRD", "PW", "RPW")),
      actionButton("run", "Simulate", icon = icon("play"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Allocation Bar Chart", plotOutput("allocationPlot")),
        tabPanel("Cumulative Allocation", plotOutput("cumulativeAllocPlot")),
        tabPanel("Rolling Success Rate", plotOutput("successRatePlot")),
        tabPanel("Success/Failure Stacked Bar", plotOutput("stackedBarPlot")),
        tabPanel("Final Allocation Pie", plotOutput("piePlot")),
        tabPanel("Summary Table", tableOutput("summaryTable"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  simulate_trials <- eventReactive(input$run, {
    n <- input$n
    p_t <- input$p_t
    p_c <- input$p_c
    design <- input$design
    
    allocation <- rep(NA, n)
    outcomes <- rep(NA, n)
    
    urn_t <- 1
    urn_c <- 1
    
    for (i in 1:n) {
      if (design == "CRD") {
        group <- sample(c("T", "C"), 1)
        
      } else if (design == "PW") {
        if (i == 1) {
          group <- sample(c("T", "C"), 1)
        } else {
          group <- if (outcomes[i-1] == 1) allocation[i-1] else sample(c("T", "C"), 1)
        }
        
      } else if (design == "RPW") {
        total <- urn_t + urn_c
        prob_t <- urn_t / total
        group <- ifelse(runif(1) < prob_t, "T", "C")
      }
      
      # Simulate outcome
      if (group == "T") {
        success <- rbinom(1, 1, p_t)
        if (design == "RPW" && success == 1) urn_t <- urn_t + 1
      } else {
        success <- rbinom(1, 1, p_c)
        if (design == "RPW" && success == 1) urn_c <- urn_c + 1
      }
      
      allocation[i] <- group
      outcomes[i] <- success
    }
    
    data.frame(Patient = 1:n, Group = allocation, Outcome = outcomes)
  })
  
  # 1. Allocation Bar Chart
  output$allocationPlot <- renderPlot({
    df <- simulate_trials()
    ggplot(df, aes(x = Patient, fill = Group)) +
      geom_bar(stat = "count") +
      labs(title = "Patient Allocation Over Time", x = "Patient", y = "Cumulative Count") +
      theme_minimal()
  })
  
  # 2. Cumulative Allocation Line Plot
  output$cumulativeAllocPlot <- renderPlot({
    df <- simulate_trials()
    df$T_C <- ifelse(df$Group == "T", 1, 0)
    df$C_C <- ifelse(df$Group == "C", 1, 0)
    df$cum_T <- cumsum(df$T_C)
    df$cum_C <- cumsum(df$C_C)
    
    ggplot(df, aes(x = Patient)) +
      geom_line(aes(y = cum_T, color = "Treatment")) +
      geom_line(aes(y = cum_C, color = "Control")) +
      labs(title = "Cumulative Allocation", y = "Number of Patients") +
      scale_color_manual(values = c("Treatment" = "blue", "Control" = "red")) +
      theme_minimal()
  })
  
  # 3. Rolling Success Rate Plot
  output$successRatePlot <- renderPlot({
    df <- simulate_trials()
    df$Group <- factor(df$Group)
    df$RollingSuccess <- zoo::rollapply(df$Outcome, width = 10, FUN = mean, fill = NA, align = "right")
    
    ggplot(df, aes(x = Patient, y = RollingSuccess, color = Group)) +
      geom_line() +
      labs(title = "Rolling Success Rate (10-patient window)", y = "Success Rate") +
      theme_minimal()
  })
  
  # 4. Stacked Bar of Success/Failure
  output$stackedBarPlot <- renderPlot({
    df <- simulate_trials()
    summary_df <- df %>%
      group_by(Group, Outcome) %>%
      summarise(n = n(), .groups = "drop")
    
    ggplot(summary_df, aes(x = Group, y = n, fill = factor(Outcome))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("0" = "grey", "1" = "green"), name = "Outcome", labels = c("Failure", "Success")) +
      labs(title = "Success/Failure by Group") +
      theme_minimal()
  })
  
  # 5. Pie Chart of Final Allocation
  output$piePlot <- renderPlot({
    df <- simulate_trials()
    pie_df <- df %>%
      count(Group)
    
    ggplot(pie_df, aes(x = "", y = n, fill = Group)) +
      geom_col(width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Final Allocation Proportions") +
      scale_fill_manual(values = c("T" = "#0073C2FF", "C" = "#EFC000FF"))
  })
  
  # Summary Table
  output$summaryTable <- renderTable({
    df <- simulate_trials()
    df %>%
      group_by(Group) %>%
      summarise(
        Allocated = n(),
        Successes = sum(Outcome),
        SuccessRate = round(mean(Outcome), 3),
        .groups = "drop"
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
