
renderDataTable <- shiny::renderDataTable
withTags <- shiny::withTags


mod_returns_returns_ui <- function(id, ticker_choices) {
    ns <- NS(id)
    
    tagList(
        br(),
        fluidRow(
            class = "lightbox",
            # column(12,
                   column(4,
                          h4("Large-Mid-Small Cap equities"),
                          tags$ul(tags$li(h5(
                                      "SPY - Large cap U.S. Equities, blend 
                                      style")), 
                                  tags$li(h5("MDY - S&P Midcap 400 ETF Trust, 
                                           seeks to provide investment results 
                                           that, before expenses, correspond to 
                                           the S&P MidCap 400 Index")), 
                                  tags$li(h5("IWM - iShares Russell 2000 ETF, 
                                           seeks to track the investment results 
                                           of an index composed of small-cap
                                           U.S. equities"))
                                     ),
                                  h4("Intl.and emerging markets"),
                          tags$ul(tags$li(h5("EFA - iShares EAFE (index of large- 
                                           and mid-cap companies in developed
                                           countries ex/ U.S. and Canada)")), 
                                  tags$li(h5("EEM - iShares MSCI Emerging Markets 
                                           ETF, exposure to large and mid-sized
                                           companies in emerging markets"))
                                     )
                                  
                          ),
                   column(4,
                          
                              h4("Bonds"),
                          tags$ul(tags$li(h5("AGG - U.S. Total bond market, all-
                                           term")),
                                  tags$li(h5("TIP")), 
                                  tags$li(h5("TLT - Shares 20+ year Bonds")), 
                                  tags$li(h5("LQD - iShares Investment Grade 
                                           Corporate Bonds, U.S."))
                                     ),
                                  h4("Commodities"),
                          tags$ul(tags$li(h5("GSG - iShares S&P GSCI Commodity-
                                           Indexed, seeks to track the results 
                                           of a fully collateralized investment 
                                           in futures contracts on an index 
                                           composed of a diversified group of
                                           commodities futures"))
                                     )
                            
                          ),
                   column(4,
                          
                              h4("Real Estate"),
                          tags$ul(tags$li(h5("RWR - SPDR Dow Jones REIT ETF, seeks 
                                           to provide investment results that,
                                           before fees and expenses, correspond 
                                           to the return of the Dow Jones U.S.
                                           Select REIT Index, designed to serve 
                                           as a prox for direct real estate
                                           investing")),
                                  tags$li(h5("RWX - SPDR Dow Jones International 
                                           Real Estate ETF, seeks to provide
                                           investment results that, before fees 
                                           and expenses, correspond to the 
                                           total return of the Dow Jones Global
                                           ex-U.S. Select Real Estate Securities
                                           Index, access to publicly traded
                                           real estate securities in non-U.S.
                                           developed and emerging markets")), 
                                  tags$li(h5("MBB - iShares MBS ETF, seeks to 
                                           track the investment results of an 
                                           index composed of investment-grade
                                           mortgage-backed securities"))
                                     ),
                                  h4("Cash"),
                          tags$ul(tags$li(h5("SHV - iShares Short Treasury Bond 
                                           ETF, seeks to track the investment
                                           results of an index composed of U.S.
                                           Treasury bonds with remaining 
                                           maturities one year or less")))
                               )
                           
            ),
        # br(),
        fluidRow(
            class = "lightbox",
            h4("Inputs"),
                pickerInput(
                    width = 240,
                    inputId = ns("tickers"),
                    label = "Select Tickers:",
                    choices = list(
                        Equities = c("SPY", "MDY", "IWM"),
                        `International & Emerging Markets` = c("EFA", "EEM"),
                        Bonds = c("AGG", "TIP", "TLT", "LQD"),
                        Commodities = "GSG",
                        `Real Estate` = c("RWR", "RWX", "MBB")),
                    selected = ticker_choices[1:5],
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                ),
                shinyWidgets::radioGroupButtons(
                    width = 330,
                    ns("date_lookback"),
                    label = "Period:",
                    choices = c("All", "10Y", "5Y", "1Y", "3M"),
                    selected = "All"
                ),
                actionButton(
                    width = 100,
                    ns("showPlot"),
                    "Plot",
                    icon("caret-right"),
                    class = "btn-primary"
                    )            
                ),
        # br(),
        fluidRow(
            class = "lightbox",
            h4("Cumulative Returns Plot"),
            plotOutput(ns("returns_plot"))
            ),
        # br(),
        fluidRow(
            class = "lightbox",
            column(4,
            h4(title = "Asset Performance"),
            plotOutput(ns("plot1"), height = 300,
                       # Equivalent to: click = clickOpts(id = "plot_click")
                       click = ns("plot1_click"),
                       brush = brushOpts(
                           id = ns("plot1_brush")
                       )
                       )
            ),
            column(8,
                   h4("Points near click"),
                   DT::dataTableOutput(ns("click_info")),
                   h4("Brushed points"),
                   DT::dataTableOutput(ns("brush_info"))
                   )
            ),
        pre(" ")
    )
}


mod_returns_returns_server <- function(id, monthly_rets, brush_data) {
    moduleServer(id,
                 function(input, output, session) {
                     
                     
                     
                     min_date <- reactive({
                         switch(
                             input$date_lookback,
                             `All` = min(monthly_rets$date),
                             `10Y` = max(monthly_rets$date) %m-% years(10),
                             `5Y` = max(monthly_rets$date) %m-% years(5),
                             `1Y` = max(monthly_rets$date) %m-% years(1),
                             `3M` = as.Date(max(monthly_rets$date)) %m-% months(3)
                         )
                     })
                     
                    
                     
                     output$returns_plot <- renderPlot({
                         # Show message if input tickers are not selected
                         validate(
                             need(input$showPlot, "Please click 'Plot'."),
                             need(
                                 isolate(input$tickers) != "",
                                 "Please select tickers and click 'Plot'"
                             )
                         )
                         
                         # validate(need(input$w1 + input$w2 + input$w3 + input$w4 + input$w5 == 100, "The portfolio weights must sum to 100%!"))
                         
                         
                         
                         
                         isolate(
                             # returns_plot <-
                                 
                             ##########
                             # For testing
                             # min_date <- function() {"2012-12-31"}
                             # input <- list(tickers = c("GSG", "MBB", "TLT"))
                             ##########
                             
                             
                             monthly_rets %>%
                                 filter(date >= min_date()) %>%
                                 select(date, isolate(input$tickers)) %>%
                                 pivot_longer(-date,
                                              names_to = "ticker",
                                              values_to = "return") %>%
                                 group_by(ticker) %>%
                                 arrange(date) %>%
                                 mutate(idx = round(cumprod(
                                     1 + return
                                 ), 2)) %>%
                                 mutate(label = if_else(
                                     date == max(date), ticker, NA_character_
                                 )) %>%
                                 ggplot(aes(date, idx, color = ticker)) +
                                 geom_line(size = 1.5, na.rm = TRUE) +
                                 theme_minimal() +
                                 labs(
                                     x = NULL,
                                     y = "Index Level",
                                     title = "How much did assets grow over time?"
                                 ) +
                                 # theme_solarized() +
                                 theme(legend.title = element_blank())
                         )
                         
                         
                         
                         # ggplotly(returns_plot) %>%
                         #     config(displayModeBar = FALSE)
                     })
                     
                     
                     ret_stats <- 
                         monthly_rets %>% 
                         pivot_longer(-date, names_to = "ticker", values_to = "ret") %>% 
                         arrange(ticker, date) %>% 
                         group_by(ticker) %>%
                         summarize(mean_ret = mean(ret)*12,
                                   sd = sd(ret)*sqrt(12),
                                   dd = downside_deviation(ret),
                                   max_drawdown = max_drawdown(ret)) %>%
                         mutate(across(where(is.numeric), ~round(.x, 3)))

                     
                     output$plot1 <- renderPlot({
                         ggplot(ret_stats, aes(max_drawdown, mean_ret)) + 
                             geom_point(na.rm = TRUE)
                     })
                     
                     
                     output$click_info <- renderDataTable({
                         # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
                         # were a base graphics plot, we'd need those.
                         nearPoints(ret_stats, input$plot1_click)
                         # options = list(lengthChange = FALSE)
                     })
                     
                     
                     output$brush_info <- renderDataTable({
                         brushedPoints(ret_stats, input$plot1_brush)
                         # options = list(lengthChange = FALSE)
                     })
                     
                 })
}
