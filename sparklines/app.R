#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
dir_r <- "C:/Users/user/Desktop/Aaron/R/Shiny apps/stock-analysis/data/"
read_tibble <- function(x, date_format = "%Y-%m-%d") {
    x %>%
        fread(fill = TRUE) %>% 
        as_tibble() %>% 
        mutate(across(which(sapply(., class) == "integer64" ), as.numeric)) %>% 
        mutate(across(contains("date"), ~as.Date(.x, date_format)))
}


# Load data
prices_from_tickers <- list.files(dir_r, pattern = "Prices from Tickers", full.names = TRUE) %>% max()

monthly_prices <- 
    read_tibble(prices_from_tickers) %>% 
    select(date, ticker, price.adjusted) %>% 
    arrange(date) %>% 
    group_by(ticker) %>%
    pivot_wider(names_from = "ticker", values_from = price.adjusted) %>%
    fill(-date) %>%
    slice(endpoints(date, on = "months"))



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("lookback",
                        "Number of years:",
                        min = 1,
                        max = 20,
                        value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
            sparklineOutput("GLD_sparkline")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    
    prices <- reactive({

        ####
        # input <- list(lookback = 10)
        ####

        prices_sorted <-
            monthly_prices[order(monthly_prices$date), ]

        prices_filtered <-
            prices_sorted[prices_sorted$date >=
                              (max(prices_sorted$date) -
                                   years(as.numeric(input$lookback))), ]
        prices_filtered
    })
    
    
    output$GLD_sparkline <- renderSparkline({
        
        sparkline(values = prices()$GLD, #%>% 
                  # round(0) %>% 
                  # na.trim(., sides = "left"),
                  type = "line",
                  width = 190,
                  height = 40)
    })
    
    
    
    
    
    #     output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
