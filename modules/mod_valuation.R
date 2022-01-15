
# mod_valuation.R


mod_valuation_ui <- function(id) { #, inflation) {
    
    ns <- NS(id)
    
    
    tagList(
        br(),
        br(),
        fluidRow(
            valueBox(
                width = 4,
                value = paste0(inflation %>% pull(level) %>% last() %>% round(1), "%"),
                subtitle = HTML("Inflation <br> YE: ", 
                                  inflation %>% pull(date) %>% last(), " (YEFPCPITOTLZGUSA)"),
                color = "aqua",
                icon = icon("money-bill-alt")#,
                # href = ""
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
                        ),
                        status = "secondary"
                    )
                )
            )
        )
    )
    )
}