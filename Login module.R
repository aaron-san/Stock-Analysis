 # Login module

library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

# UI

ui <- navbarPage(
    title = "R Shiny advanced tips",
    collapsible = TRUE,
    windowTitle = "R Shiny tips = TFI",
    theme = shinytheme("readable"),
    
    tabPanel(
        title = "Demo",
        useShinyjs()
    ),
    
    div(
        id = "login-basic",
        style = "width: 500px; max-width: 100%; margin: 0 auto;",
        
        div(
            class = "well",
            h4(class = "text-center", "Please login"),
            p(class = "text-center",
              tags$small("First approach login form")
              ),
            textInput(
                inputId = "ti_user_name_basic",
                label = tagList(icon("user"),
                                "User Name"),
                placeholder = "Enter user name"
            ),
            passwordInput(
                inputId = "ti_password_basic",
                label = tagList(icon("unlock-alt"),
                                "Password"),
                placeholder = "Enter password"
            ),
            
            div(
                class = "text-center",
                actionButton(
                    inputId = "ab_login_button_basic",
                    label = "Log in",
                    class = "btn-primary"
                )
            ),
            
            uiOutput(outputId = "display_content_basic")
        )
    )
)


server <- function(input, output, session) {
 
    # Create userbase
    user_base_basic_tbl <- tibble(
        user_name = "user_basic_1",
        password = "pass_basic_1"
    )
    
    
    # Check credentials vs tibble
    validate_password_basic <- 
        eventReactive(input$ab_login_button_basic, {
            
            validate = FALSE
            
            if(input$ti_user_name_basic == user_base_basic_tbl$user_name &&
               input$ti_password_basic == user_base_basic_tbl$password) {
                   validate <- TRUE
                   }
                })
    
    # Hide form
    observeEvent(validate_password_basic(), {
        shinyjs::hide(id = "login-basic")
    })
    
    # Show app
    
    output$display_content_basic <- renderUI({
        
        req(validate_password_basic())
        
        div(
            class = "bg-success",
            id = "success_basic",
            h4("Access confirmed!"),
            p("Welcome to your basically-secured application!")
            )
    })
    
    
    
    
    
    
    
       
}


shinyApp(ui = ui, server = server)