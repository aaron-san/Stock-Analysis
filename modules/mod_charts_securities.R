

notes <- read_file("data/notes.txt")


# plot <- 
#   USArrests |>
#   e_charts(UrbanPop) |>
#   e_line(Assault) |>
#   e_area(Murder, y_index = 1, x_index = 1) |>
#   e_y_axis(gridIndex = 1) |>
#   e_x_axis(gridIndex = 1) |>
#   e_grid(height = "35%") |>
#   e_grid(height = "35%", top = "50%") |>
#   e_toolbox_feature("dataZoom", title = list(zoom = "zoom", back = "back")) |>
#   e_datazoom(type = "slider", start = 20, end = 70)
# 
# 
# plot %>% e_theme("vintage") **
# plot %>% e_theme("shine") **
# plot %>% e_theme_custom('{"color":["#ff715e","#ffaf51"]}')




# Custom plot function
custom_echart <- function(data, title, subtitle) {
  
  ####
  # data <- 
  #   ratios %>% 
  #   filter(industry_damodaran == "transportation") %>%
  #   select(ticker, report_date, cash_ratio) %>%
  #   pivot_longer(where(is.numeric),
  #                names_to = "ratio",
  #                values_to = "value") %>%
  #   drop_na(value)
  ####
  
  data %>%
    e_chart(x = report_date) %>%
    e_line(serie = value,
           symbol = 'emptyCircle',
           symbolSize = 5) %>%
    e_tooltip(trigger = "axis", backgroundColor = 'white') %>%
    e_grid(right = "15%", top = "20%") %>%
    e_title(text = title,
            subtext = subtitle,
            left = "center",
            top = "5%") %>%
    e_legend(
      orient = "vertical",
      type = "scroll",
      top = "15%",
      right = "5",
      selector = list(
        list(type = 'inverse', title = 'Invert'),
        list(type = 'all', title = 'Reset')
      )
    ) %>%
    e_datazoom(type = "slider", 
               start = 70,
               end = 100) %>% 
    e_theme("shine") %>%
    e_toolbox_feature("dataZoom", selector = list(
      start = '20',
      end = '80'
    )) %>%
    e_toolbox_feature(feature = "reset") %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
}



plotUI <- function(id, ticker_choices, selected_tickers) {
  tagList(
    withSpinner(
      echarts4rOutput(
        NS(id, "plot"),
        height = "400px"),
      # click = ns("plot_click")),
      type = 8,
      color = "darkgray"
    ),
    # checkboxGroupButtons("group_choices", "Select group:",
    #                      choices = group_choices),
    div(
      style = 'display:inline-block; vertical-align: top;',
      pickerInput(
        width = 240,
        NS(id, "select"),
        label = "Select Tickers:",
        choices = sort(ticker_choices),
        # choices = list(
        #   Equities = c("SPY", "MDY", "IWM"),
        #   `International & Emerging Markets` = c("EFA", "EEM"),
        #   Bonds = c("AGG", "TIP", "TLT", "LQD"),
        #   Commodities = "GSG",
        #   `Real Estate` = c("RWR", "RWX", "MBB")),
        selected = selected_tickers,
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      )
    ), 
    div(style = "display:inline-block; vertical-align: bottom;",
        actionButton(NS(id, "apply"), "Apply"))
  )
}

plotServer <- function(id, data, plot_title) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderEcharts4r({
      # Create dependency on actionButton
      input$apply
      
      #####
      # data <- prices_combined
      # input <- list(select = "AMZN")
      #####
      
      data %>% 
        rename(ticker = symbol,
               report_date = date,
               value = price) %>% 
        filter(ticker %in% isolate(input$select)) %>%
        group_by(ticker) %>% 
        e_chart(x = report_date) %>% 
        e_line(serie = value, symbol = 'emptyCircle', symbolSize = 5) %>% 
        e_tooltip(trigger="axis", backgroundColor = 'white') %>% 
        e_grid(right = "15%", top = "20%") %>% 
        e_title(text = plot_title,
                subtext = "",
                left = "center",
                top = "5%") %>% 
        e_legend(orient = "vertical",
                 type = "scroll",
                 top = "15%",
                 right = "5",
                 selector = list(
                   list(type = 'inverse', title = 'Invert'),
                   list(type = 'all', title = 'Reset')
                 )) %>% 
        e_datazoom(type = "slider", 
                   start = 90,
                   end = 100) %>%
        e_theme("shine") %>% 
        e_toolbox_feature("dataZoom") %>% 
        e_toolbox_feature(feature = "reset") %>% 
        e_toolbox_feature("dataView") %>% 
        e_toolbox_feature("saveAsImage")
    })
  })
}  


  
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel("hi"),
#   mainPanel(
#   plotUI(
#     "plot1",
#     ticker_choices = symbol_choices,
#     selected_tickers = c("SPY", "MDY", "IWM")
#   )
# )))
# 
# server <- function(input, output, session) {
#   plotServer("plot1", data = prices_combined)
# }
# 
# shinyApp(ui, server)



# UI ----------------------------------------------------------------------

mod_charts_securities_ui <-
  function(id, industry_choices, ratio_choices) {
    ns <- NS(id)

    tagList(
      waiter::use_waiter(),
      br(),
      br(),
      fluidRow(
        column(
          7,
          box(
            width = NULL,
            withSpinner(
            echarts4rOutput(ns("ratios_plot"),
                            height = "350px"),
                            # click = ns("plot_click")),
                            type = 8,
                            color = "darkgray"
            ),
            div(style = "display:inline-block; vertical-align: top;",
                selectInput(
                  ns("industry"),
                  "Industry:",
                  choices = sort(industry_choices),
                  selected = industry_choices[1],
                  width = "200px"
                )),
            div(style = "display:inline-block; vertical-align: top;",
                selectInput(
                  ns("ratio"),
                  "Field:",
                  choices = sort(ratio_choices),
                  selected = "cash_ratio",
                  width = "200px"
                )
            ),
            div(style = "display:inline-block; vertical-align: bottom;",
                checkboxInput(ns("remove_outliers"), "Remove outliers")
            ),
            div(
              div(style = "display:inline-block; vertical-align: top;",
                actionButton(ns("apply_ratios"), "Apply!")
              ),
              div(style = "display:inline-block; vertical-align: top;",
                actionButton(ns("showhide"), "Show/Hide table")
                )
            ),
            hidden(
              htmlOutput(ns("html_break")),
              shiny::dataTableOutput(ns("ratio_table"))
            )
            # tags$button("Show alert", onclick="alertGo()"),
            # tags$button("Show conf", onclick="confirmDial()")
          ),
          box(
            width = NULL,
            plotUI(
              ns("plot_indexes"),
              ticker_choices = prices_indexes %>% distinct(symbol) %>% pull(),
              selected_tickers = c("^DJI", "^SP500TR", "^TYX")
            )
          ),
          box(
            width = NULL,
            plotUI(
              ns("plot_stocks"),
              ticker_choices = prices_stocks %>% distinct(symbol) %>% pull(),
              selected_tickers = c("GOOG", "AMD", "LOW", "TREE", "ZM")
            )
        )
        ),
        column(
          width = 5,
          div(
            box(
              width = NULL,
              textInput(
                ns("ticker_lookup"),
                label = "Ticker Lookup:",
                placeholder = "(Enter ticker)"
              ),
              uiOutput(ns("ticker_profile"))
            ),
            box(
              width = NULL,
              strong(div("Field:", class = "text-muted")),
              textOutput(ns("formula_name")),
              textOutput(ns("formula")),
              textOutput(ns("formula_guide")),
              class = "text-muted"
            ),
            box(
              width = NULL,
              div(
                textAreaInput(
                ns("notes"),
                "Notes:",
                width = "95%", #"400px",
                height = "200px",
                value = notes,
                placeholder = "(Enter note)"
                )
                ),
              div(
                actionButton(
                ns("save_notes"),
                label = "Save to file",
                icon = icon("save"),
                class = "btn-primary"
                )
                )
            )
          )
        )
      )
    )
  }


mod_charts_securities_server <-
  function(id, ratios, ratio_functions, ratio_guide) {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- NS(id)

                   ratios_data <- reactive({
                     ######
                     # input <- list(industry = "retail_general",
                     #               ratio = "revenue_1Y")
                     ######
                     input$apply_ratios
                     
                     ratios %>%
                       filter(industry_damodaran == isolate(input$industry)) %>%
                       select(ticker, report_date, isolate(input$ratio)) %>%
                       pivot_longer(where(is.numeric),
                                    names_to = "ratio",
                                    values_to = "value") %>%
                       drop_na(value) %>%
                       mutate(value = signif(value, 2),
                              Median = signif(median(value,
                                                     na.rm = TRUE)),
                              2)
                   })

                   plot_data <- reactive({

                     if (!isolate(input$remove_outliers)) {
                       ratios_data()
                     } else {
                       ratios_data() %>%
                         filter(
                           value < quantile(value, 0.975,
                                            na.rm = TRUE),
                           value > quantile(value, 0.025,
                                            na.rm = TRUE)
                         )
                     }
                   })

                   max_value <- reactive({
                     plot_data() %>% pull(value) %>% max(na.rm = TRUE)
                   })

                   min_value <- reactive({
                     plot_data() %>% pull(value) %>% min(na.rm = TRUE) %>%
                       {
                         . - (max_value() * 1.2 - max_value())
                       }
                   })

                   output$ratios_plot <- renderEcharts4r({
                     # Create dependency on actionbButton
                     input$apply_ratios

                     plot_data() %>%
                       group_by(ticker) %>%
                       custom_echart(
                         title = isolate(input$ratio),
                         subtitle = paste("Industry: ", isolate(input$industry)))

                   })

                  #  Demonstrate on different blog article!
#                    output$ratio_table <- renderTable({
#                        # Don't print table outline until plot is clicked
#                        req(input$plot_click)
#                        data_to_print <- plot_data() %>%
#                            select(Ticker, report_date,
#                                   ratio, value)
#                        nearPoints(data_to_print, input$plot_click,
#                                   maxpoints = 1)
# 
                   # })


                   output$html_break <- renderUI({
                     HTML("<hr>")
                   })
                   
                   observeEvent(input$showhide, {
                     toggle("html_break")
                   })
                   
                   output$ratio_table <- renderDataTable({
                     input$apply_ratios
                     
                      plot_data()
                   }, options = list(pageLength = 10))

                   observeEvent(input$showhide, {
                     toggle("ratio_table")
                   })
                   
                   
                   

                   plotServer("plot_indexes", data = prices_indexes,
                              plot_title = "Index prices")
                   plotServer("plot_stocks", data = prices_stocks,
                              plot_title = "Stock prices")

                   # Ticker lookup
                   ticker_lookup <-
                     reactive({
                       input$ticker_lookup
                     })
                   ticker_lookup_data <- reactive({
                     profile_data %>%
                       filter(ticker == ticker_lookup())
                   })

                   output$ticker_profile <- renderUI({
                     req(input$ticker_lookup)
                     tagList(
                       br(),
                       strong("Ticker", style = "color: gray;"),
                       renderText({
                         req(input$ticker_lookup %in%
                               ticker_choices_sec)
                         input$ticker_lookup
                       }),
                       strong("Industry", style = "color: gray;"),
                       renderText(
                         ticker_lookup_data() %>%
                           distinct(industry_damodaran) %>%
                           pull()
                       ),
                       br(),
                       strong("Business Summary", style = "color: gray;"),
                       renderText(ticker_lookup_data() %>%
                                    distinct(name_damodaran) %>%
                                    pull()),
                       br(),
                       strong("Long Business Summary", style =
                                "color: gray;"),
                       renderText(
                         ticker_lookup_data() %>%
                           distinct(long_business_summary) %>%
                           pull()
                       )
                     )
                   })

                   output$formula_name <- renderText({
                     req(input$update)
                     isolate(input$ratio)
                   })

                   output$formula <- renderText(ratio_functions[[input$ratio]])

                   output$formula_guide <- renderText(ratio_guide[[input$ratio]])

                   observeEvent(input$save_notes, {
                     write_file(input$notes, "data/notes.txt")
                   })
                 })
  }



# plot_data <-
#   industry_data() %>%
#   filter(industry_yhoo == "credit_services") %>%
#   select(ticker, report_date, net_income_1Q) %>%
#   pivot_longer(where(is.numeric),
#                names_to = "ratio",
#                values_to = "value") %>%
#   drop_na(value) %>%
#   mutate(value = signif(value, 2),
#          Median = signif(median(
#            Value, na.rm = TRUE)), 2) %>%
#   rename(Ticker = ticker)
#
# plot_data %>%
#   ggplot(aes(x = report_date, y = value)) +
#   geom_point(aes(color = Ticker),
#              size = 1.1, alpha = 0.7,
#              na.rm = TRUE) +
#   theme_minimal() +
#   theme(axis.title.y = element_text(
#     color = "grey")) +
#   ?xkcdaxis(as.numeric(range(plot_data$report_date)), range(plot_data$value))
