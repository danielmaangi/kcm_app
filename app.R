source("setup.R")
source("periods.R")
source("grants.R")
source("config/credentials.R")


thematic_shiny()

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "litera",
                   base_font = font_google("Space Mono"),
                   code_font = font_google("Space Mono")),
  title = "Global Fund Grants Dashboard",
  tags$style(HTML("
    .navbar-brand {
      background-color: #17A2B8 !important;
      color: white !important;
    }
    .navbar {
      background-color: #17A2B8 !important;
      color: white !important;
    }
    .nav-tabs > li > a:hover {
      background-color: #17A2B8 !important;
      color: white !important;
    }
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:focus, 
    .nav-tabs > li.active > a:hover {
      background-color: #17A2B8 !important;
      color: white !important;
    }
    .bold-header {
      font-weight: bold !important;
    }
  ")),
  
  sidebar = sidebar(
    selectInput("grant", "Select Grant",
                choices = grants,
                selected = "HIV, KRCS",
                multiple = FALSE),
    
    selectInput("gf_period", "Select Period",
                choices = gf_period,
                selected = max(gf_period),
                multiple = FALSE),
    
    dateInput("start_date", "Start Date", value = Sys.Date() - 1100),
    dateInput("end_date", "End Date", value = Sys.Date()),
    actionButton("refresh", "Refresh Data")
  ),
  
  tabsetPanel(
    tabPanel("Programmes", icon = icon("suitcase-medical"),
             tabsetPanel(
               tabPanel("Coverage", tags$div(style = "margin-top: 20px;"),
                        layout_columns(
                          card(card_header("Coverage indicators", class = "bold-header"),
                          DTOutput("data_table"),
                          verbatimTextOutput("error_message")
                          )
                        )
               ),
               tabPanel("Outcomes", tags$div(style = "margin-top: 20px;"),
                        layout_columns(
                          card(card_header("Outcome indicators", 
                                           class = "bold-header"))
                        )
               ),
               
               tabPanel("Impact", tags$div(style = "margin-top: 20px;"),
                        layout_columns(
                          card(card_header("Impact Indicators", 
                                           class = "bold-header"))
                        )
               )
             )
    ),
    
    tabPanel("Finance", icon = icon("sack-dollar"),
             tabsetPanel(
               tabPanel("Fund Absorption", tags$div(style = "margin-top: 20px;"),
                        layout_columns(
                          card(card_header("Fund Absorption", class = "bold-header"))
                        )
               )
             )
    ),
    
    tabPanel("Grant Rating", icon = icon("sack-dollar"),
             tabsetPanel(
               tabPanel("Overall", tags$div(style = "margin-top: 20px;"),
                        layout_columns(
                          card(card_header("Description", class = "bold-header"))
                        )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Create a reactive trigger for refreshing data
  refresh_trigger <- reactiveVal(FALSE)
  
  pull_data_from_dhis2 <- reactive({
    req(input$start_date, input$end_date)
    
    # Check refresh trigger
    refresh_trigger()
    
    api_url <- paste0(
      base_url, "api/dataValueSets?dataSet=", dataSet,
      "&&startDate=", input$start_date, 
      "&endDate=", input$end_date, 
      "&orgUnit=", orgunit, 
      "&children=true"
    )
    
    cat("API URL: ", api_url, "\n")
    
    response <- tryCatch({
      GET(
        url = api_url,
        authenticate(username, password),
        accept_json()
      )
    }, error = function(e) {
      return(e)
    })
    
    if (inherits(response, "error")) {
      output$error_message <- renderText({
        paste("Error in API request:", response$message)
      })
      return(NULL)
    }
    
    if (status_code(response) == 200 && http_type(response) == "application/json") {
      data <- fromJSON(content(response, as = "text"), flatten = TRUE)
      if (!is.null(data$dataValues)) {
        return(data$dataValues)
      } else {
        output$error_message <- renderText({
          "No 'dataValues' found in the response."
        })
        return(NULL)
      }
    } else {
      output$error_message <- renderText({
        paste("API request failed. Status code:", status_code(response), 
              "Content Type:", http_type(response))
      })
      return(NULL)
    }
  })
  
  observe({
    withProgress(message = 'Loading data, please wait...', value = 0, {
      incProgress(0.5)
      
      output$error_message <- renderText("")
      
      prog_data <- pull_data_from_dhis2() %>%
        as.data.frame() |>
        left_join(des, by = c("dataElement" = "id")) %>%
        left_join(all_periods, by = c("period" = "quarter")) %>%
        filter(grepl("/COVERAGE", code)) %>%
        mutate(
          data = case_when(str_detect(code, "TARGET") ~ "Target",
                           str_detect(code, "RESULT") ~ "Result",
                           str_detect(code, "COMMENT") ~ "Comment",
                           TRUE ~ NA_character_
          )
        ) %>%
        select(period, Indicator , data, value) %>%
        pivot_wider(names_from = "data",
                    values_from = "value") %>%
        mutate(
          `% Achieved` = round(as.numeric(Result)*100/ as.numeric(Target),0),
          .before = Comment
        ) %>%
        filter(gf_period %in% input$gf_period) %>%
        select(-period) %>%
        mutate(
          across(c(Target, Result, `% Achieved`), 
                 ~ prettyNum(as.numeric(.), big.mark = ","))
        ) 
      
      incProgress(1)
      
      if (!is.null(prog_data)) {
        output$data_table <- renderDT({
          datatable(prog_data, options = list(pageLength = 5)) %>%
            formatStyle(
              '% Achieved',
              backgroundColor = styleInterval(
                c(50, 75),  
                c('red', 'yellow', 'green')  
              ),
              color = styleInterval(
                c(50, 75),  
                c('white', 'black', 'white')  
              )
            )
        })
      } else {
        output$data_table <- renderDT({
          datatable(data.frame(message = "No data available or request failed."))
        })
      }
    })
  })
  
  # Observe the Refresh Data button click
  observeEvent(input$refresh, {
    # Update the refresh trigger to force data re-fetch
    refresh_trigger(!refresh_trigger())
  })
}





shinyApp(ui = ui, server = server)
