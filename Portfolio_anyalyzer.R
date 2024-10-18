library(shiny)
library(shinydashboard)
library(quantmod)
library(PerformanceAnalytics)

# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Financial Portfolio Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Portfolio Analysis", tabName = "portfolio", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "portfolio",
              fluidRow(
                box(title = "Input Data", status = "primary", solidHeader = TRUE, width = 12,
                    textInput("tickers", "Enter Tickers (comma-separated)", "AAPL,GOOGL,MSFT"),
                    numericInput("investment", "Total Investment ($)", 10000),
                    actionButton("analyze", "Analyze")
                ),
                box(title = "Portfolio Summary", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("summary")
                ),
                box(title = "Risk-Return Visualization", status = "warning", solidHeader = TRUE, width = 12,
                    plotOutput("riskReturnPlot")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  observeEvent(input$analyze, {
    tickers <- unlist(strsplit(input$tickers, ","))
    investment <- input$investment
    
    # Fetch stock data
    prices <- NULL
    for (ticker in tickers) {
      stock_data <- getSymbols(ticker, auto.assign = FALSE)
      prices <- cbind(prices, Cl(stock_data))
    }
    colnames(prices) <- tickers
    
    # Calculate returns
    returns <- na.omit(Return.calculate(prices))
    portfolio_returns <- rowSums(returns * (investment / length(tickers)))
    
    # Portfolio summary
    output$summary <- renderPrint({
      summary(portfolio_returns)
    })
    
    # Risk-return plot
    output$riskReturnPlot <- renderPlot({
      charts.PerformanceSummary(returns, main = "Risk-Return Profile")
    })
  })
}

# Run the app
shinyApp(ui, server)

