

mod_dashboard_ui <- function(id, inflation) {
    
    ns <- NS(id)
    

    tagList(
        
        br(),
        br(),
        fluidRow(
            valueBox(
                width = 4,
                value = paste0((inflation %>% pull(level) %>% last()) %>% 
                                   round(1), "%"),
                subtitle = HTML("Inflation, annualized <br> Year Ended: ", 
                inflation %>% pull(date) %>% last() %>% as.character(), 
                "<br>(CPIAUCNS)"),
                color = "aqua"#,
                # icon = icon("money")#,
                # href = ""
            ),
            
            valueBox(
                width = 4,
                value = yields_monthly %>% slice(n()) %>% 
                    filter(symbol == '20 year') %>% pull(yield) - 
                    yields_monthly %>% slice(n()) %>% filter(symbol == '1 year') %>%
                    pull(yield),
                subtitle = HTML("Yield Curve Spread <br> (Treasury: 20yr - 1yr)"),
                color = "blue",
                icon = icon("cogs")
            ),
            valueBox(
                width = 4,
                value = paste0(100*round(monthly_rets %>% arrange(date) %>% 
                                             select(SPY) %>% pull(SPY) %>% last(), 
                                         3), "%"),
                subtitle = HTML("SPY Return <br> Month ended: ", monthly_rets %>%
                                    arrange(date) %>% select(date, SPY) %>% 
                                    pull(date) %>% last() %>% as.character()),
                color = "green", #navy, fuchsia, purple, maroon, aqua, yellow
                icon = icon("chart-line")
            
            ),
            
            box(
                width = 4,
                prettyRadioButtons(ns("lookback"), 
                                   "Lookback (years)",
                                   choiceNames = list("1", "3", "5",
                                                      "10", "20", "Max"),
                                   choiceValues = list(1, 3, 5, 10, 20, 200),
                                   # choices = c(1, 3, 10, 20, 50),
                                   inline = TRUE,
                                   fill = TRUE)
            ),
            
            box(
                title = "Adjusted Prices (last 20 years)",
                width = 12,
                # infoBox(
                    # wellPanel(
                    column(4,
                        h5("SPY"),
                        sparklineOutput(ns("SPY")),
                        h5("GLD"),
                        sparklineOutput(ns("GLD"))
                    ),
                    column(4,
                        h5("VIXY"),
                        sparklineOutput(ns("VIXY")),
                        h5("DXY - U.S. dollar index"),
                        sparklineOutput(ns("DXY"))
                    ),
                    column(4,
                        h5("XRT - Retail Sector"),
                        sparklineOutput(ns("XRT")),
                        h5("XLV - Healthcare"),
                        sparklineOutput(ns("XLV"))
                    ),
                    column(4,
                        h5("XLE - Energy"),
                        sparklineOutput(ns("XLE")),
                        h5("XLF - Financial"),
                        sparklineOutput(ns("XLF"))
                    ),
                    column(4,
                        h5("XLI - Industrials"),
                        sparklineOutput(ns("XLI")),
                        h5("XLP - Consumer staples"),
                        sparklineOutput(ns("XLP"))
                    ),
                    column(4,
                        h5("XLU - Utilities"),
                        sparklineOutput(ns("XLU")),
                        h5("XLY - Consumer discretionary"),
                        sparklineOutput(ns("XLY"))
                    )
            )
                      
        ),
        fluidRow(
        box(
            id = ns("tabcard"),
            width = 12,
            title = "A card with tabs",
            tabsetPanel(
                id = ns("tabsetpanel"),
                tabPanel(
                    tabName = "Quotes",
                    title = "Quotes",
                    active = TRUE,
                    tags$blockquote(
                        HTML(
                            "I can calculate the motions of the heavenly bodies, but not the madness of the people. - <b class='quote_author'>Isaac Newton</b>"
                        )
                    ),
                    tags$blockquote(
                        HTML(
                            "Be fearful when others are greedy and greedy when others are fearful. - <b class='quote_author'>Warren Buffett</b>"
                        )
                    ),
                    tags$blockquote(
                        HTML(
                            "Only buy something that you'd be perfectly willing to hold if the market shut down for 10 years. - <b class='quote_author'>Warren Buffett</b>"
                        )
                    ),
                    tags$blockquote(
                        HTML(
                            "Stop trying to predict the direction of the stock market, the economy, interest rates, or elections. - <b class='quote_author'>Warren Buffett</b>"
                        )
                    )#,
                    # bs4Quote("Blablabla", status = "warning"),
                    # bs4Quote("Blablabla", status = "fuchsia")
                    
                    
                    
                ),
                tabPanel(
                    tabName = "Tab 2",
                    active = FALSE,
                    title = "Watchlist Table",
                    # fluidRow(
                        DTOutput(ns("ratios_table"), height = "500px")
                    # )
                    
                    
                    
                    
                ),
                tabPanel(
                    tabName = "About",
                    title = "About",
                    active = FALSE,
                    # create manually
                    # create in bs4dash and copy html and css
                    # bs4Dash::bs4UserCard(
                    # fluidRow(
                        # div(
                    br(),
                    box(
                        h4("Aaron Hardy", class = "muted"),
                        h5("App creator"),
                        width = 8,
                        tags$image(src = "https://media-exp1.licdn.com/dms/image/C5603AQHbVucr-vb2tQ/profile-displayphoto-shrink_400_400/0/1613798311833?e=1619049600&v=beta&t=6kOFbA2nHQbZXJn2ljdSzAy9kLXWQ_r8U3XJXgOPkgo"),
                        br(),
                        br(),
                        p("Thank you for using this app. I designed it to make investing more fun and more accessible to investors."),
                        br(),
                        p(HTML("If you would like to see more of my work, please visit my <a href = 'https://www.linkedin.com/in/aaron-hardy-651b2410/'>LinkedIn</a> profile.")),
                        br(),
                        p(HTML("I also maintain a blog at <a href = 'https://www.freeanalystnotes.com/'>FreeAnalystNotes.com</a>. and
                <a href = 'https://www.investwithr.com'>InvestWithR.com</a>")),
                        div(actionButton(inputId = ns("toggleAdvanced"),
                                         label = "Show email",
                                         class="btn", # #707070
                                         style="width: 130px;
                                         color: black; 
                                         border: none; margin: 6px 0 6px 0;
                                         box-shadow: 1px 3px 5px grey; margin-top: 25px;"), #background-color: aqua;
                            shinyjs::hidden(
                                div(id = ns("advanced"), a("aaronhardy6@gmail.com", href="aaronhardy6@gmail.com"))
                            )
                        )
                        ),
                    
                    
                    # div(style="display: flex; align-items: center;",
                            # div(
                            # div(tags$img(src="https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ", 
                            #             alt="Aaron Hardy", 
                            #             style="width: 180px; border-radius: 5px;
                            #             box-shadow: 3px 5px 10px grey; margin-top: 25px;")
                            #     ),
                            # 
                            # 
                            # ),
                            #         # style="float: right;"),
                            # div(style="margin: 15px;",
                            #     h2("Aaron Hardy"),
                            #     h4("App Creator"),
                                
                        #     )
                        # )
                # )
                        
                        # box(
                        #     width = 12,
                        #     # src = "https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ",
                        #     status = "info",
                        #     h2("App Creator"),
                        #     h3("Aaron Hardy"),
                        #     # elevation = 2,
                        #     tags$img(src="https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ",
                        #              style="width:100%px; height: 120px;
                        #              border-radius: 5px;
                        #              box-shadow: 3px 5px 10px grey;"),
                        #     p("Thank you for using this app. I designed it to make investing more fun and more accessible to serious investors."),
                        #     p(HTML("If you would like to see more of my work, please visit my <a href = 'https://www.linkedin.com/in/aaron-hardy-651b2410/'>LinkedIn</a> profile.")),
                        #     p(HTML("I also maintain a blog at <a href = 'https://www.freeanalystnotes.com/'>FreeAnalystNotes.com</a>. and
                        #     <a href = 'https://www.investwithr.com'>InvestWithR.com</a>"))
                        # 
                        # )
                    
                )
            )
            
        ),
        ),
        fluidRow(
            box(
                solidHeader = FALSE,
                title = "Status summary",
                background = NULL,
                width = 4,
                status = "primary"
                ),
            box(
                solidHeader = FALSE,
                title = "Practice area",
                background = NULL,
                width = 8,
                status = "primary",
                p("hello")
            )
        )
    )
    
    
}



mod_dashboard_server <- function(id) {
    moduleServer(id,
                 function(input, output, session) {
                     
                     shinyjs::onclick("toggleAdvanced",
                                      shinyjs::toggle(id = "advanced", 
                                                      anim = TRUE, animType =
                                                          "slide", time = 0.5))
                     
                     
                     prices <- reactive({
                         
                        prices_sorted <- 
                             monthly_prices[order(monthly_prices$date), ]
                        
                         prices_filtered <- prices_sorted[prices_sorted$date >= 
                                               (max(prices_sorted$date) - 
                                               years(as.numeric(input$lookback))), ]
                         })
                     
                     
                     output$GLD <- renderSparkline({
                         sparkline(values = prices()$GLD %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                        })
                     
                     output$SPY <- renderSparkline({
                         sparkline(values = prices()$`^GSPC` %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                    
                     output$VIXY <- renderSparkline({
                         sparkline(values = prices()$VIXY %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$DXY <- renderSparkline({
                         sparkline(values = prices()$DXY %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$XRT <- renderSparkline({
                         sparkline(values = prices()$XRT %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$XLV <- renderSparkline({
                         sparkline(values = prices()$XLV %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$XLE <- renderSparkline({
                         sparkline(values = prices()$XLE %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$XLF <- renderSparkline({
                         sparkline(values = prices()$XLF %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
           
                     output$XLI <- renderSparkline({
                         sparkline(values = prices()$XLI %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$XLP <- renderSparkline({
                         sparkline(values = prices()$XLP %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$XLU <- renderSparkline({
                         sparkline(values = prices()$XLU %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
                     output$XLY <- renderSparkline({
                         sparkline(values = prices()$XLY %>% round(0) %>% 
                                       na.trim(., sides = "left"),
                                   type = "line", width = 190, height = 40)
                     })
                     
     
                     output$ratios_table <- renderDT({
                         ratios_wide %>% 
                                     group_by(ticker) %>%
                                     slice(n()) %>%
                                     ungroup() %>%
                                     arrange(desc(decision_ebit_to_ev_6m_forward)) %>%
                                     head(30) %>%
                                     select(ticker, name_edgar, industry_edgar,
                                            fundamentals_date,
                                            decision_ebit_to_ev_6m_forward,
                                            free_cash_flow_to_assets,
                                            gross_margin_stability_8Y) %>%
                                     mutate(across(where(is.numeric), ~signif(.x, 3))) %>% 
                             datatable(class = 'cell-border striped', 
                                       rownames = FALSE,
                                       colnames = c('Ticker' = 'ticker', 'Company' = 'name_edgar', 'Industry' = 'industry_edgar'),
                                       options = list(scrollX = TRUE, scrollY = "300px", searching = FALSE, pageLength = 30, paging = FALSE)) %>% 
                             # formatStyle(`font-size` = '10px') %>%
                             formatStyle(columns = 'decision_ebit_to_ev_6m_forward', backgroundColor = styleInterval(cuts = 5, c('aquamarine', 'white')))
                         
                     }) #, fillContainer = TRUE) 
                     
                     
                     # output$SP500 <- renderPlot(
                     #     
                     #     monthly_rets %>% 
                     #         arrange(date) %>% 
                     #         slice(-1) %>% 
                     #         select(date, SPY = "^GSPC") %>%
                     #         mutate(index = cumprod(1 + SPY)) %>%
                     #         ggplot(aes(date, index)) +
                     #         geom_line() +
                     #         # theme_minimal() +
                     #         labs(title = "SP500", y = "", x = "") +
                     #         theme_clean()
                     # )
                         
                     
                     
                     
                     
                     
                     
                     # output$ratios_table <- function() {
                     #     table <-
                     #         ratios %>% 
                     #         group_by(ticker) %>% 
                     #         slice(n()) %>% 
                     #         ungroup() %>% 
                     #         arrange(desc(ebit_to_ev)) %>% 
                     #         head(30) %>% 
                     #         select(ticker, short_name, industry, rounded_date, ebit_to_ev,
                     #                free_cash_flow_to_assets, gross_margin_stability_8y) %>% 
                     #         mutate(across(where(is.numeric), ~signif(.x, 3))) %>% 
                     #         knitr::kable("html") %>% 
                     #         kable_styling("striped", full_width = FALSE) %>% 
                     #         scroll_box(height = "500px") %>% 
                     #         kable_styling(bootstrap_options = c("striped", "hover"))
                     #    # table$ebit_to_ev <- cell_spec(table$ebit_to_ev, color = ifelse(table$ebit_to_ev > 0, "blue", "red"))
                     #    return(table)
                     # }
                 })
}
                     