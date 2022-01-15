

notes <- read_file("data/notes.txt")





mod_charts_securities_ui <-
  function(id, industry_choices, ratio_choices) {
    ns <- NS(id)

    # Reusable
    inputPanel_ts_plot <- function(id, selected_symbs) {
      inputPanel(
        selectInput(ns(id),
          "Variables:",
          multiple = TRUE,
          choices = sort(symbol_choices),
          selected = selected_symbs
        )
      )
    }

    tagList(
      waiter::use_waiter(),
      br(),
      br(),
      fluidRow(
        column(
          7,
          withSpinner(
          plotlyOutput(ns("ratios_plot"),
            height = "300px"#,
            # click = ns("plot_click")
          ), type = 8, color = "lightgreen"
          ),
          inputPanel(
            selectInput(ns("industry"),
              "Industry:",
              choices = sort(industry_choices),
              selected = "credit_services"
            ),
            selectInput(ns("ratio"), "Field:",
              choices = sort(ratio_choices),
              selected = "net_income_1Q"
            ),
            checkboxInput(ns("remove_outliers"), "Remove outliers"),
            actionButton(ns("update"), "Update!"),
            actionButton(ns("toggle_table"), "Show/Hide table")
          ),
          hidden(
            shiny::dataTableOutput(ns("ratio_table"))
          ),
            # tableOutput(ns("ratio_data")),
          inputPanel_ts_plot(
            id = "ts_plot_1_vars",
            selected_symbs = c("VIXY", "WOOD", "SOFI")
          ),
          withSpinner(
            plotOutput(ns("ts_plot_1"), height = "300px"),
            type = 8, color = "lightgreen"
          ),
          inputPanel_ts_plot(
            id = "ts_plot_2_vars",
            selected_symbs = c(
              "FEDFUNDS", "UNRATE",
              "U6RATE"
            )
          ),
          withSpinner(
            plotOutput(ns("ts_plot_2"), height = "300px"),
            type = 8, color = "lightgreen"
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
          ),
          inputPanel_ts_plot(
            id = "ts_plot_3_vars",
            selected_symbs = c("GLD", "SLV")
          ),
          withSpinner(
            plotOutput(ns("ts_plot_3"), height = "300px"),
            type = 8, color = "lightgreen"
          ),
          inputPanel_ts_plot(
            id = "ts_plot_4_vars",
            selected_symbs = c("M1", "M2", "M1V", "M2V")
          ),
          withSpinner(
            plotOutput(ns("ts_plot_4"), height = "300px"),
            type = 8, color = "lightgreen"
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
                     input$update
                     ratios %>% 
                       filter(industry_damodaran == input$industry)
                   })
                   
                   plot_data <- reactive({
                     # Don't create reactive data until actionButton is
                     #  clicked
                     input$update
                     
                     # waiter::Waiter$new(id = ns("ratios_plot"),
                     #                    html = spin_ripple())$show()
                     
                     if (!isolate(input$remove_outliers)) {
                       ratios_data() %>%
                         # filter(industry_yhoo == "discount_stores")
                         # filter(industry_yhoo ==
                         #   isolate(input$industry)) %>%
                         select(ticker, report_date, isolate(input$ratio)) %>%
                         pivot_longer(where(is.numeric),
                                      names_to = "ratio",
                                      values_to = "value") %>%
                         drop_na(value) %>%
                         mutate(value = signif(value, 2),
                                Median = signif(median(value,
                                                       na.rm = TRUE)),
                                2) %>%
                         rename(Ticker = ticker)
                     } else {
                       ratios_data() %>%
                         # filter(industry_yhoo ==
                         #   isolate(input$industry)) %>%
                         select(ticker, report_date, isolate(input$ratio)) %>%
                         pivot_longer(where(is.numeric),
                                      names_to = "ratio",
                                      values_to = "value") %>%
                         drop_na(value) %>%
                         mutate(value = signif(value, 2),
                                Median = signif(median(value,
                                                       na.rm = TRUE)),
                                2) %>%
                         filter(
                           value < quantile(value, 0.975,
                                            na.rm = TRUE),
                           value > quantile(value, 0.025,
                                            na.rm = TRUE)
                         ) %>%
                         rename(Ticker = ticker)
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
                   
                   
                   output$ratios_plot <- renderPlotly({
                     # Don't create plot outline until actionbButton is
                     #  clicked
                     req(input$update)
                     
                     isolate({
                       ratio <- input$ratio
                       industry <- input$industry
                       min_value <- min_value()
                       max_value <- max_value()
                     })
                     
                     plt <- plot_data() %>%
                       ggplot(aes(x = report_date, y = value)) +
                       geom_point(
                         aes(color = Ticker),
                         size = 1.1,
                         alpha = 0.7,
                         na.rm = TRUE
                       ) +
                       geom_hline(aes(yintercept = Median),
                                  color = "red",
                                  linetype = "dotted") +
                       expand_limits(y = c(min_value,
                                           max_value * 1.2)) +
                       labs(
                         title = snakecase::to_title_case(as.character(ratio)),
                         subtitle = industry,
                         x = "",
                         y = "%"
                       ) +
                       # theme_xkcd() +
                       theme_minimal() +
                       theme(axis.title.y = element_text(color = "grey")) #+
                     # xkcd::xkcdaxis(as.numeric(c(ymd("1999-12-31"),
                     #                  ymd("2021-12-31"))),
                     #                c(0, 1e10))
                     ggplotly(plt)
                     
                   })#, res = 96) #%>% bindCache(input$industry, input$ratio,
                   #input$remove_outliers)
                   
                   
                   # output$ratio_data <- renderTable({
                   #     # Don't print table outline until plot is clicked
                   #     req(input$plot_click)
                   #     data_to_print <- plot_data() %>%
                   #         select(Ticker, report_date,
                   #                ratio, value)
                   #     nearPoints(data_to_print, input$plot_click,
                   #                maxpoints = 1)
                   #
                   # })
                   
                   
                   
                   
                   output$ratio_table <- renderDataTable({
                     req(input$update)
                     # input$data_table
                     plot_data()
                   }, options = list(pageLength = 10))
                   
                   observeEvent(input$toggle_table, {
                     # hide(ns("ratio_table"))
                     toggle("ratio_table")
                   })
                   
                   
                   
                   output$ts_plot_1 <- renderPlot({
                     # waiter::Waiter$new(id = ns("ts_plot_1"),
                     #                    html = spin_ripple())$show()
                     prices_combined %>%
                       filter(symbol %in% input$ts_plot_1_vars) %>%
                       ggplot(aes(x = date, y = price, color = symbol)) +
                       geom_line(na.rm = TRUE)
                     
                   }, res = 96)
                   
                   output$ts_plot_2 <- renderPlot({
                     # waiter::Waiter$new(id = ns("ts_plot_2"),
                     #                    html = spin_ripple())$show()
                     prices_combined %>%
                       filter(symbol %in% input$ts_plot_2_vars) %>%
                       ggplot(aes(x = date, y = price, color = symbol)) +
                       geom_line(na.rm = TRUE)
                     
                   }, res = 96)
                   
                   output$ts_plot_3 <- renderPlot({
                     prices_combined %>%
                       filter(symbol %in% input$ts_plot_3_vars) %>%
                       ggplot(aes(x = date, y = price, color = symbol)) +
                       geom_line(na.rm = TRUE)
                     
                   }, res = 96)
                   
                   output$ts_plot_4 <- renderPlot({
                     prices_combined %>%
                       filter(symbol %in% input$ts_plot_4_vars) %>%
                       ggplot(aes(x = date, y = price, color = symbol)) +
                       geom_line(na.rm = TRUE)
                     
                   }, res = 96)
                   
                   
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
