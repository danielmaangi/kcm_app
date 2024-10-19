source("setup.R")
source("periods.R")
source("grants.R")
source("config/credentials.R")


thematic_shiny()

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "litera",
                   base_font = font_google("Space Mono"),
                   code_font = font_google("Space Mono")),
  title = "GC7 Dashboard",
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
    selectInput("grant", "Grant",
                choices = grants,
                selected = "WFU6M2XN4W4",
                multiple = FALSE),
    
    selectInput("gf_period", "Period",
                choices = gf_period,
                selected = max(gf_period),
                multiple = FALSE),
    
    selectInput("programme_indicator", "Programme indicators",
                choices = programme_indicators,
                selected = "Coverage",
                multiple = FALSE),
    
    dateInput("start_date", "Start Date", value = Sys.Date() - 1100),
    dateInput("end_date", "End Date", value = Sys.Date()),
    actionButton("refresh", "Refresh", icon = icon("arrows-rotate"))
  ),
  
  tabsetPanel(
    tabPanel("Programmes", icon = icon("suitcase-medical"),
             tabsetPanel(
               tabPanel("Overview", tags$div(style = "margin-top: 20px;"),
                        layout_columns(
                          card(card_header("Current period", class = "bold-header"),
                          DTOutput("data_table"),
                          verbatimTextOutput("error_message")
                          )
                        )
               ),

               
               tabPanel("Trend", tags$div(style = "margin-top: 20px;"),
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

# ***************************************************************************************
# ---------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Create a reactive trigger for refreshing data
  refresh_trigger <- reactiveVal(FALSE)
  
  pull_program_data <- reactive({
    req(input$start_date, input$end_date)
    
    # Check refresh trigger
    refresh_trigger()
    
    api_url <- paste0(
      base_url, "api/dataValueSets?dataSet=", input$grant,
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
      
      prog_data <- prog_data <- pull_program_data() %>%
        as.data.frame() |>
        left_join(des, by = c("dataElement" = "id")) %>%
        left_join(all_periods, by = c("period" = "quarter")) %>%
        mutate(
          data = case_when(str_detect(name, "/TAR") ~ "Target",
                           str_detect(name, "/RES") ~ "Result",
                           str_detect(name, "/COM") ~ "Comment",
                           TRUE ~ NA_character_
          ),
          type = case_when(str_detect(name, "COVERAGE") ~ "Coverage",
                           str_detect(name, "OUTCOME") ~ "Outcome",
                           str_detect(name, "IMPACT") ~ "Impact",
                           str_detect(name, "PSEAH") ~ "PSEAH",
                           TRUE ~ NA_character_
          )
          
        ) %>%
        select(period, Indicator , fieldMask, type, data, value) %>%
        pivot_wider(names_from = "data",
                    values_from = "value",
                    values_fill = NA) %>%
        select(period, Indicator , fieldMask, type, Target, Result, Comment) %>%
          mutate(
            Percent = round(as.numeric(Result)*100/ as.numeric(Target),0),
            .before = Comment
          ) %>%
        mutate(Percent = ifelse(fieldMask == "Inverse", 
                                round(10000 / Percent,0), 
                                Percent)) %>%
        select(-fieldMask) %>%
          mutate(
            across(c(Target, Result, Percent), 
                   ~ prettyNum(as.numeric(.), big.mark = ","))
          ) %>%
        filter(type == input$programme_indicator)%>%
        select(-type) %>%
        filter(gf_period %in% input$gf_period) %>%
        select(-period)
      
      
      incProgress(1)
      
      if (!is.null(prog_data)) {
        output$data_table <- renderDT({
          datatable(prog_data,
                    options = list(
                      pageLength = 5,
                      columnDefs = list(
                        list(
                          targets = 5, 
                          visible = FALSE
                        ),
                        list(
                          targets = 4,
                          createdCell = JS(
                            "function(td, cellData, rowData, row, col) {
                              $(td).attr('title', rowData[5]); // Comment column
                             $(td).tooltip();
                             }"
                          )
                        )
                      )
                    )
                    ) %>%
            formatStyle(
              'Percent',
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
