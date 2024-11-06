source("setup.R")
source("periods.R")
source("grants.R")
source("config/credentials.R")

thematic_shiny()

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "cosmo", 
                   base_font = font_google("Space Mono"),
                   code_font = font_google("Space Mono")),
  tags$style(HTML("
    /* Remove top bar styling */
    .navbar, .navbar-brand {
      display: none !important;
    }
    .nav-tabs > li > a:hover {
      background-color: #FF5733 !important;
      color: white !important;
    }
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:focus, 
    .nav-tabs > li.active > a:hover {
      background-color: #FF5733 !important;
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
    tabPanel("Grant overview", icon = icon("circle-info"),
             tabsetPanel(
               layout_columns(
                 card(uiOutput("grant_info_cards"))
               )
             )
    ),
    
    tabPanel("Programmes", icon = icon("suitcase-medical"),
             uiOutput("prog_dynamic_tab")
    ),
    
    tabPanel("Finance", icon = icon("sack-dollar"),
             tabsetPanel(
               tabPanel("Global Funds", tags$div(style = "margin-top: 20px;"),
                        fluidRow(
                          column(7, 
                                 card(
                                   card_header("Figures in USD", class = "bold-header"),
                                   DTOutput("finance_table")
                                 )
                          ),
                          column(5,
                                 card(
                                   card_header("Context", class = "bold-header"),
                                   textOutput("finance_comments")
                                 )
                          )
                        )
               ),
               tabPanel("Counterpart Funds", tags$div(style = "margin-top: 20px;"),
                        fluidRow(
                          column(7, 
                                 card(
                                   card_header("Figures in KES", class = "bold-header"),
                                   DTOutput("cofin_table")
                                 )
                          ),
                          column(5,
                                 card(
                                   card_header("Context", class = "bold-header"),
                                   textOutput("cofin_comments")
                                 )
                          )
                        )
               )
             )
    ),
    
    tabPanel("Products", icon = icon("pills"),
             tabsetPanel(
               layout_columns(
                 card(DTOutput("stock_table"))
               )
             )
    )
  )
)


server <- function(input, output, session) {
  
  refresh_trigger <- reactiveVal(FALSE)
  
  output$prog_dynamic_tab <- renderUI({
    tabsetPanel(
      tags$div(style = "margin-top: 20px;"),
      layout_columns(
        card(card_header(paste0(input$programme_indicator, " indicators"), class = "bold-header"),
             DTOutput("data_table"),
             verbatimTextOutput("error_message")
        )
      )
    )
  })
  
  pull_program_data <- reactive({
    req(input$start_date, input$end_date)
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
      
      grant_data <- pull_program_data() %>%
        as.data.frame() |>
        left_join(des, by = c("dataElement" = "id")) %>%
        left_join(all_periods, by = c("period" = "quarter"))
      
      prog_data <- grant_data %>%
        filter(class %in% c("Program", "PSEAH")) %>%
        filter(!is.na(data)) %>%
        filter(!is.na(type)) %>%
        select(period, Indicator , fieldMask, type, data, value) %>%
        pivot_wider(names_from = "data",
                    values_from = "value",
                    values_fill = NA) %>%
        mutate(
          Target = if (!"Target" %in% names(.)) NA_real_ else Target,
          Result = if (!"Result" %in% names(.)) NA_real_ else Result,
          Comment = if (!"Comment" %in% names(.)) NA_character_ else Comment
        ) %>%
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
      
      fin_data <- grant_data %>%
        filter(class %in% c("Finance")) %>%
        select(period, Indicator , value)
      
      finance_comments <- fin_data %>%
        filter(Indicator == "Comments") %>%
        pull(value)
      
      fin_summary <- fin_data %>%
        filter(Indicator != "Comments") %>%
        filter(gf_period %in% input$gf_period) %>%
        select(-period) %>%
        mutate(value = as.numeric(value)) %>%
        pivot_wider(names_from = Indicator, values_from = value) %>%
        mutate(
          `Cumulative Budget` = if (!"Cumulative Budget" %in% names(.)) NA_real_ else `Cumulative Budget`,
          `Cumulative Funds Expensed by PR` = if (!"Cumulative Funds Expensed by PR" %in% names(.)) NA_real_ else `Cumulative Funds Expensed by PR`,
          Commitments = if (!"Commitments" %in% names(.)) NA_real_ else Commitments,
          Obligations = if (!"Obligations" %in% names(.)) NA_real_ else Obligations
        ) %>%
        transmute(
          `Cumulative Budget` = sum(`Cumulative Budget`, na.rm = TRUE),
          `Cumulative Expenditure` = sum(`Cumulative Funds Expensed by PR`, na.rm = TRUE),
          Commitments = sum(Commitments, na.rm = TRUE),
          Obligations = sum(Obligations, na.rm = TRUE),
          Variance = sum(`Cumulative Budget`, na.rm = TRUE) - sum(`Cumulative Funds Expensed by PR`, na.rm = TRUE),
          `Absorption Rate` = round(`Cumulative Funds Expensed by PR` / `Cumulative Budget` * 100, 1)
        )%>%
        pivot_longer(everything(),
                     names_to = "Indicator",
                     values_to = "Result") %>%
        mutate(res_col = case_when(Indicator == "Absorption Rate" ~ as.numeric(Result), 
                                   TRUE ~ NA_real_))
      
      cofin_data <- grant_data %>%
        filter(class %in% c("Co-financing")) %>%
        select(period, Indicator , value)
      
      
      cofin_comments <- cofin_data %>%
        filter(Indicator == "Comments") %>%
        pull(value)
      
      cofin_summary <- if(!input$grant %in% c("XEUXTIGkU8H","l5VURDJNlpx","Vg7RJh2mM35")){
        tibble(
          Indicator = c("Cumulative Budget","Cumulative Expenditure" ,"Commitments" ,"Obligations" ,"Variance","Absorption Rate"),
          Result = NA_real_,
          res_col = NA_real_
        )
      } else { cofin_data %>%
          filter(Indicator != "Comments") %>%
          filter(gf_period %in% input$gf_period) %>%
          select(-period) %>%
          mutate(value = as.numeric(value)) %>%
          pivot_wider(names_from = Indicator,
                      values_from = value) %>%
          transmute(
            `Cumulative Budget` = sum(`Cumulative budget`,na.rm = T),
            `Cumulative Expenditure` = sum(`Cumulative expenditure`, na.rm = T),
            Commitments = sum(Commitments, na.rm = T),
            Obligations = sum(Obligations, na.rm = T),
            Variance = sum(`Cumulative budget`,na.rm = T) - sum(`Cumulative expenditure`, na.rm = T),
            `Absorption Rate` = round(`Cumulative Expenditure` / `Cumulative Budget` * 100,1)
          ) %>%
          pivot_longer(everything(),
                       names_to = "Indicator",
                       values_to = "Result") %>%
          mutate(res_col = case_when(Indicator == "Absorption Rate" ~ as.numeric(Result), 
                                     TRUE ~ NA_real_))
      }
      
      
      rating_data <- grant_data %>%
        filter(class %in% c("Rating"))
      
      rating <- if(nrow(rating_data) > 0) { 
        rating_data %>%
          select(period, Indicator, value) %>%
          filter(gf_period %in% input$gf_period) %>%
          select(-period) %>%
          transmute(
            Category = case_when(
              str_detect(Indicator, "Financial") ~ "Financial",
              str_detect(Indicator, "Programmatic") ~ "Programmatic",
              TRUE ~ NA_character_
            ),
            type = case_when(
              str_detect(Indicator, "End Date") ~ "End Date",
              str_detect(Indicator, "Start Date") ~ "Start Date",
              str_detect(Indicator, "Rating Comments") ~ "Comments",
              str_detect(Indicator, "Financial Rating") ~ "Rating",
              str_detect(Indicator, "Programmatic Rating") ~ "Rating",
              TRUE ~ NA_character_
            ),
            value = value
          ) %>%
          pivot_wider(names_from = type, values_from = value) %>%
          mutate(
            Rating = if (!"Rating" %in% names(.)) NA_character_ else Rating,
            Comments = if (!"Comments" %in% names(.)) NA_character_ else Comments
          ) %>%
          select(c(Category, Rating, Comments))
      } else {
        tibble(
          Category = NA_character_,
          Comments = NA_character_,
          Rating = NA_character_,
          period = NA_character_
        ) 
      }
      
      
      prog_rate <- rating %>% filter(Category == "Programmatic") %>% pull(Rating) %>%
        substr(1,1)
      fin_rate <- rating %>% filter(Category == "Financial") %>% pull(Rating) %>%
        substr(1,1)
      
      overall_rating <- tibble(
        Category = "Overall",
        Rating = paste0(prog_rate, "-", fin_rate)
      )
      
      rating_all <- bind_rows(rating)
      
      # +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*++*+*+*+**++
      products_data <- grant_data %>%
        filter(gf_period %in% input$gf_period) %>%
        filter(class %in% c("Products")) %>%
        select(period, Indicator , value) %>%
        separate(Indicator, into = c("product", "metric"), sep = " (?=[^ ]+$)", extra = "merge", fill = "right") %>%
        mutate(
          product = str_replace_all(product, "(?i)\\b(Ending|Under|SOR)\\b", "")
        ) %>%
        mutate(
          product = str_trim(str_replace(product, " -$", ""))  
        ) %>%
        pivot_wider(names_from = metric,
                    values_from = value) %>%
        transmute(
          Product = product,
          Balance = if(!"Balance" %in% names(.)) NA_character_ else Balance,
          Consumption = if(!"AMC" %in% names(.)) NA_character_ else AMC,
          Procured = if(!"Procurement" %in% names(.)) NA_character_ else Procurement,
          `SOR Numerator` = if(!"Numerator" %in% names(.)) NA_character_ else Numerator,
          `SOR Denominator` = if(!"Denominator" %in% names(.)) NA_character_ else Denominator,
          `Stock out` = round(as.numeric(`SOR Numerator`) / as.numeric(`SOR Denominator`),1),
          Comments = if(!"Comments" %in% names(.)) NA_character_ else Comments
        ) %>%
        mutate(across(
          .cols = where(is.character) & !c("Product", "Comments"), 
          .fns = as.numeric,                                        
          .names = "{.col}"                                         
        )) %>%
        mutate(
          `MoS` = round(Balance / Consumption, 1),
          .before = Procured 
        ) %>%
        mutate(
          across(c(Balance, Consumption, Procured), 
                 ~ prettyNum(as.numeric(.), big.mark = ","))
        ) %>%
        select(-c(`SOR Numerator`, `SOR Denominator`))
      
      
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
      
      if (!is.null(fin_summary)) {
        output$finance_table <- renderDT({
          datatable(fin_summary,
                    options = list(
                      pageLength = nrow(fin_summary),
                      paging = FALSE,
                      searching = FALSE,
                      info = FALSE,
                      dom = 't',
                      autoWidth = TRUE,
                      columnDefs = list(list(
                        targets = 2,  
                        visible = FALSE
                      ))
                    ),
                    rownames = FALSE,
                    colnames = NULL,
                    caption = "Legend: Red < 50%, Yellow 50 - 75 %, Green > 75%"
          ) %>%
            formatCurrency(
              columns = 'Result',     
              currency = "USD ",         
              interval = 3,           
              mark = ",",             
              digits = 2,             
              dec.mark = ".",         
              before = TRUE,          
              rows = which(fin_summary$Indicator != "Absorption Rate") 
            ) %>%
            formatStyle(
              columns = 'Result',
              valueColumns = 'res_col',
              target = c("cell"),
              backgroundColor = styleInterval(
                c(50, 75),  
                c('red', 'yellow', 'green')  
              ),
              rows = which(fin_summary$Indicator == "Absorption Rate")
            ) %>%
            formatStyle(
              c('Indicator', 'Result'),   
              fontWeight = styleEqual("Absorption Rate", "bold") 
            )
        })
        
        
        output$finance_comments <- renderText({
          if (length(finance_comments) > 0) {
            finance_comments
          } else {
            "No comments available."
          }
        })
        
        output$finance_legend <- renderText({
          "Red: < 50%, Yellow: 50 - 75 %, Green: > 75%"
        })
      }
      
      output$cofin_legend <- renderUI({
        HTML("<span style='color: red;'>Red: < 50%</span>, 
              <span style='color: yellow;'>Yellow: 50% to 75%</span>, 
              <span style='color: green;'>Green: > 75%</span>")
      })
      
      
      output$cofin_table <- if (sum(cofin_summary$Result, na.rm = T) > 0) { 
        renderDT({
          datatable(cofin_summary,
                    options = list(
                      pageLength = nrow(cofin_summary),
                      paging = FALSE,
                      searching = FALSE,
                      info = FALSE,
                      dom = 't',
                      autoWidth = TRUE,
                      columnDefs = list(list(
                        targets = 2,  
                        visible = FALSE
                      ))
                    ),
                    rownames = FALSE,
                    colnames = NULL,
                    caption = "Legend: Red < 50%, Yellow 50 - 75 %, Green > 75%"
          ) %>%
            formatCurrency(
              columns = 'Result',     
              currency = "KES ",         
              interval = 3,           
              mark = ",",             
              digits = 2,             
              dec.mark = ".",         
              before = TRUE,          
              rows = which(cofin_summary$Indicator != "Absorption Rate") 
            ) %>%
            formatStyle(
              columns = 'Result',
              valueColumns = 'res_col',
              target = c("cell"),
              backgroundColor = styleInterval(
                c(50, 75),  
                c('red', 'yellow', 'green')  
              ),
              rows = which(fin_summary$Indicator == "Absorption Rate")
            ) %>%
            formatStyle(
              c('Indicator', 'Result'),   
              fontWeight = styleEqual("Absorption Rate", "bold") 
            )
        })
      } else {
        renderDT({
          datatable(
            data.frame(message = "Not applicable, data not available or request failed."),
            options = list(
              dom = 't',        
              paging = FALSE,   
              searching = FALSE
            ),
            colnames = NULL,
            rownames = FALSE    
          )
        })
      }
      
      
      output$cofin_comments <- if (length(cofin_comments) > 0) {
        renderText({
          cofin_comments
        }) 
      } else {
        renderText({"No comments available."})
      }
      
      output$cofin_legend <- renderText({
        "Legend: Red: < 50%, Yellow: 50 - 75 %, Green: > 75%"
      })
      
      
      
      
      
      output$grant_info_cards <- renderUI({
        grant_info <- grant_info %>%
          filter(dataset_id == input$grant)
        
        if (nrow(grant_info) == 0) {
          return(tags$p("No grant information available."))
        }
        
        percent_committed <- round((grant_info$committed_amount / grant_info$signed_amount) * 100, 1)
        percent_disbursed <- round((grant_info$disbursed_amount / grant_info$signed_amount) * 100, 1)
        
        fluidRow(
          card(
            card_header(
              paste0(grant_info$grant_yoy), 
              class = "bold-header"
            ),
            fluidRow(
              column(6, 
                     tags$div(
                       tags$p(tags$strong("Signed Amount (USD):"), prettyNum(grant_info$signed_amount, big.mark = ",")),
                       tags$p(tags$strong("Committed Amount (USD):"), prettyNum(grant_info$committed_amount, big.mark = ","), 
                              tags$span(paste0(" (", percent_committed, "%)"))),
                       tags$p(tags$strong("Disbursed Amount (USD):"), prettyNum(grant_info$disbursed_amount, big.mark = ","), 
                              tags$span(paste0(" (", percent_disbursed, "%)"))),
                       tags$p(tags$strong("Rating:"), paste0(prog_rate, "-", fin_rate))
                     )
              ),
              column(6, 
                     output$rating_table <- renderDT({
                       datatable(rating_all,
                                 options = list(
                                   dom = 't',
                                   paging = FALSE,
                                   searching = FALSE,
                                   columnDefs = list(
                                     list(
                                       targets = 2, 
                                       visible = FALSE
                                     ),
                                     list(
                                       targets = 1,
                                       createdCell = JS(
                                         "function(td, cellData, rowData, row, col) {
                      $(td).attr('title', rowData[2]); // Assuming Comment column is in index 2
                      $(td).tooltip();
                    }"
                                       )
                                     )
                                   )
                                 ),
                                 rownames = FALSE,
                                 colnames = NULL
                       ) %>%
                         formatStyle(
                           'Rating',
                           backgroundColor = styleEqual(
                             c("A - 100+ %", "B - 90-99 %", "C - 60-89 %", "D - 30-59 %", "E <30 %",
                               "1 - Excellent, 95+ %", "2 - Good, 85-94 %", "3 - Moderate, 75-84 %", "4 - Poor, 65-74 %", "5 - Very Poor, <65 %"),
                             c("darkgreen", "lightgreen", "yellow", "orange", "red",
                               "darkgreen", "lightgreen", "yellow", "orange", "red")
                           ),
                           color = styleEqual(
                             c("A - 100+ %", "B - 90-99 %", "C - 60-89 %", "D - 30-59 %", "E <30 %",
                               "1 - Excellent, 95+ %", "2 - Good, 85-94 %", "3 - Moderate, 75-84 %", "4 - Poor, 65-74 %", "5 - Very Poor, <65 %"),
                             c("white", "black", "black", "black", "white",
                               "white", "black", "black", "black", "white")
                           )
                         )
                       
                     })
              )
              
            )
          ),
          
          column(5, card(
            card_header("Goals", class = "bold-header"),
            tags$ul(
              lapply(grant_info$goal_bullets[[1]], function(item) {
                tags$li(item)
              })
            )
          )),
          column(7, card(
            card_header("Objectives", class = "bold-header"),
            tags$ul(
              lapply(grant_info$objectives_bullets[[1]], function(item) {
                tags$li(item)
              })
            )
          ))
        )
      })
      
      
      # Render products table
      if (!is.null(products_data)) {
        output$stock_table <- renderDT({
          datatable(products_data,
                    options = list(
                      pageLength = 5,
                      columnDefs = list(
                        list(
                          targets = 7, 
                          visible = FALSE
                        ),
                        list(
                          targets = 4,
                          createdCell = JS(
                            "function(td, cellData, rowData, row, col) {
                              $(td).attr('title', rowData[7]); // Comment column
                             $(td).tooltip();
                             }"
                          )
                        )
                      )
                    )
          ) %>%
            formatStyle(
              'MoS',
              backgroundColor = styleInterval(
                c(1, 4, 10),  
                c('red','green', 'yellow', 'red')  
              ),
              color = styleInterval(
                c(1, 4, 10),  
                c('black', 'white', 'black', 'white')  
              )
            ) %>%
            formatPercentage(
              columns = 6,
              digits = 0
            ) %>%
            formatStyle(
              columns = 6,
              backgroundColor = styleInterval(
                c(0.50, 0.75),  
                c('green', 'yellow','red')  
              ),
              color = styleInterval(
                c(0.50, 0.75),  
                c('white', 'black', 'white')  
              )
            )
        })
      } else {
        output$stock_table <- renderDT({
          datatable(data.frame(message = "No data available or request failed."))
        })
      }
      
      
      
    })
  })
  
  observeEvent(input$refresh, {
    refresh_trigger(!refresh_trigger())
  })
}

shinyApp(ui = ui, server = server)
