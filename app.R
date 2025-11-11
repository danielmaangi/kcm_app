source("setup.R")
source("periods.R")
source("grants.R")
source("config/credentials.R")

categoryoptioncombos <- fread("metadata/categoryoptioncombos.csv") 

thematic_shiny()

# Function to create grant mapping for dataset IDs
get_grant_name <- function(grant_dataset_id) {
  grant_mapping <- list(
    "WFU6M2XN4W4" = "KEN-H-KRCS",
    "XEUXTIGkU8H" = "KEN-H-TNT", 
    "SbX36Gmomkz" = "KEN-M-AMREF",
    "l5VURDJNlpx" = "KEN-M-TNT",
    "SgPSOoSZ8Iz" = "KEN-T-AMREF", 
    "Vg7RJh2mM35" = "KEN-T-TNT"
  )
  
  return(grant_mapping[[grant_dataset_id]])
}

ui <- bslib::page_sidebar(
  theme = bs_theme(bootswatch = "cosmo", 
                   base_font = font_google("Space Mono"),
                   code_font = font_google("Space Mono")),
  
  tags$style(HTML("
    /* General Background and Layout Styling */
    body {
      background-color: #f7f9fc !important;
    }
    
    /* Sidebar Styling */
    .bslib-sidebar {
      background-color: #F5F5DC !important;
      color: white !important;
      padding: 20px;
    }
    
    /* Sidebar Inputs */
    .bslib-sidebar .form-group label {
      color: red;
      font-weight: bold;
    }
    
    /* Button Styling */
    #refresh {
      background-color: #ff5733 !important;
      border-color: #ff5733 !important;
      color: white !important;
      margin-top: 15px;
    }
    
    /* Card Styling */
    .card {
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      border-radius: 8px;
      margin-bottom: 20px;
      background-color: white;
      padding: 15px;
    }
    
    /* Tab Panel Styling */
    .nav-tabs > li > a {
      color: #495057;
    }
    
    .nav-tabs > li > a:hover,
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #ff5733 !important;
      color: white !important;
      border-radius: 5px;
    }
    
    /* Bold Header Styling */
    .bold-header {
      font-weight: bold;
      color: #343a40;
      text-transform: uppercase;
    }
    
    /* Print Media Styling */
    @media print {
      .bslib-sidebar {
        display: none !important;
      }
      .main-content {
        width: 100% !important;
        overflow: visible !important;
      }
      #print {
        display: none !important;
      }
      body {
        margin: 1cm;
      }
      .dataTable {
        width: 100% !important;
        font-size: 10pt !important;
      }
      .card, .tabPanel {
        page-break-inside: avoid;
      }
      * {
        -webkit-print-color-adjust: exact !important;
        print-color-adjust: exact !important;
      }
    }
    
    /* Print Button Styling */
    .btn-print {
      background-color: gray !important;
      border-color: #ff5733 !important;
      color: white !important;
      font-weight: bold;
      padding: 10px 20px;
      margin: 10px 0;
      border-radius: 5px;
    }
    
    /* Grant Comparison Tables Styling */
    .grant-comparison-table {
      font-size: 11px !important;
    }
    
    .grant-comparison-table .dataTables_wrapper {
      font-size: 11px !important;
    }
    
    .grant-comparison-table table {
      font-size: 11px !important;
      width: 100% !important;
    }
    
    .grant-comparison-table th,
    .grant-comparison-table td {
      font-size: 11px !important;
      padding: 8px 10px !important;
      white-space: nowrap;
    }
    
    /* Sub-Recipients Table Styling */
    .subrec-table {
      font-size: 12px !important;
    }
    
    .subrec-table .dataTables_wrapper {
      font-size: 12px !important;
    }
    
    .subrec-table table {
      font-size: 12px !important;
      width: 100% !important;
    }
    
    .subrec-table th,
    .subrec-table td {
      font-size: 12px !important;
      padding: 8px 10px !important;
      white-space: nowrap;
    }
  ")),
  
  sidebar = sidebar(
    selectInput("grant", "Grant",
                choices = grants,
                selected = "WFU6M2XN4W4",
                multiple = FALSE),
    
    selectInput("select_period", "Period",
                choices = data_periods,
                selected = "P4",
                multiple = FALSE),
    
    selectInput("programme_indicator", "Indicator",
                choices = programme_indicators,
                selected = "Coverage",
                multiple = FALSE),
    
    dateInput("start_date", "Start Date", value = Sys.Date() - 1100),
    dateInput("end_date", "End Date", value = Sys.Date()),
    actionButton("refresh", "Refresh", icon = icon("arrows-rotate")),
    
  ),
  
  div(class = "main-content",
      
      actionButton("print", "Save as PDF", icon = icon("print"), class = "btn-print"),
      
      tabsetPanel(
        tabPanel("Grant overview", icon = icon("circle-info", style = "color: #ff5733;"),
                 tabsetPanel(
                   layout_columns(
                     card(uiOutput("grant_info_cards"))
                   )
                 )
        ),
        
        tabPanel("Programmes", icon = icon("suitcase-medical", style = "color: #ff5733;"),
                 uiOutput("prog_dynamic_tab")
        ),
        
        tabPanel("Finance", icon = icon("sack-dollar", style = "color: #ff5733;"),
                 tabsetPanel(
                   tabPanel("Main Grant", tags$div(style = "margin-top: 20px;"),
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
                   tabPanel("RSSH Grant", tags$div(style = "margin-top: 20px;"),
                            fluidRow(
                              column(7, 
                                     card(
                                       card_header("Figures in USD", class = "bold-header"),
                                       DTOutput("finance_table_rssh")
                                     )
                              ),
                              column(5,
                                     card(
                                       card_header("Context", class = "bold-header"),
                                       textOutput("finance_comments_rssh")
                                     )
                              )
                            )
                   ),
                   tabPanel("Sub-Recipients", tags$div(style = "margin-top: 20px;"),
                            fluidRow(
                              column(12,
                                     card(
                                       card_header("Sub-Recipient Financial Performance", class = "bold-header"),
                                       DTOutput("subrec_finance_table")
                                     )
                              )
                            )
                   ),
                   tabPanel("Co-financing", tags$div(style = "margin-top: 20px;"),
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
        
        tabPanel("Products", icon = icon("pills", style = "color: #ff5733;"),
                 tabsetPanel(
                   layout_columns(
                     card(DTOutput("stock_table"))
                   )
                 )
        ),
        
        tabPanel("Grant Comparison", icon = icon("chart-bar", style = "color: #ff5733;"),
                 tags$div(style = "margin-top: 20px;"),
                 fluidRow(
                   column(12,
                          card(
                            card_header("Financial Performance Comparison - All Grants", class = "bold-header"),
                            div(class = "grant-comparison-table", DTOutput("grants_comparison_table"))
                          )
                   )
                 )
        )
      )
  ),
  
  tags$script(HTML("
    $(document).on('click', '#print', function() {
      window.print();
    });
  "))
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
  
  # Corrected reactive function for sub-recipient data
  pull_subrec_data <- reactive({
    req(input$start_date, input$end_date, input$grant)
    refresh_trigger()
    
    # Get current grant name
    current_grant_name <- get_grant_name(input$grant)
    
    if (is.null(current_grant_name)) {
      return(NULL)
    }
    
    # Use the special dataset ID for sub-recipient data
    api_url <- paste0(
      base_url, "api/dataValueSets?dataSet=O7LYGHTAVmT",
      "&&startDate=", input$start_date, 
      "&endDate=", input$end_date, 
      "&orgUnit=", orgunit, 
      "&children=true"
    )
    
    cat("Sub-recipient API URL: ", api_url, "\n")
    
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
      return(NULL)
    }
    
    if (status_code(response) == 200 && http_type(response) == "application/json") {
      data <- fromJSON(content(response, as = "text"), flatten = TRUE)
      if (!is.null(data$dataValues)) {
        # Process the data step by step as requested
        processed_data <- data$dataValues %>%
          as.data.frame() %>%
          mutate(
            dataElement = if (!"dataElement" %in% names(.)) NA_character_ else dataElement,
            period = if (!"period" %in% names(.)) NA_character_ else period,
            value = if (!"value" %in% names(.)) NA_character_ else value,
            attributeOptionCombo = if (!"attributeOptionCombo" %in% names(.)) NA_character_ else attributeOptionCombo
          ) %>%
          # Step 1: Left join with categoryoptioncombos
          left_join(categoryoptioncombos, by = c("attributeOptionCombo" = "id")) %>%
          # Step 2: Split name column - anything before comma is grant, after is sub-recipient
          # filter(!is.na(name) & str_detect(name, ",")) %>%  # Only rows with comma (sub-recipients)
          separate(name, into = c("grant", "subrecipient"), sep = ",", extra = "merge") %>%
          mutate(
            grant = str_trim(grant),
            subrecipient = str_trim(subrecipient)
          ) %>%
          # Filter for current grant only
          filter(grant == current_grant_name) %>%
          # Join with data elements info
          left_join(des, by = c("dataElement" = "id")) %>%
          # Join with periods info
          left_join(all_periods, by = c("period" = "quarter")) %>%
          filter(grant_periods == input$select_period) %>%
          select(period, Indicator, value, grant, subrecipient, attributeOptionCombo)
        
        return(processed_data)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  observe({
    withProgress(message = 'Loading data, please wait...', value = 0, {
      incProgress(0.5)
      
      output$error_message <- renderText("")
      
      grant_data <- pull_program_data() %>%
        as.data.frame() %>%
        mutate(
          dataElement = if (!"dataElement" %in% names(.)) NA_character_ else dataElement,
          period = if (!"period" %in% names(.)) NA_character_ else period,
          value = if (!"value" %in% names(.)) NA_character_ else value
        ) %>%
        left_join(des, by = c("dataElement" = "id")) %>%
        left_join(all_periods, by = c("period" = "quarter")) %>%
        filter(grant_periods == input$select_period)
      
      # Process sub-recipient data with corrected logic
      subrec_data <- pull_subrec_data()
      
      if (!is.null(subrec_data) && nrow(subrec_data) > 0) {
        # Create sub-recipient summary table
        subrec_summary <- subrec_data %>%
          filter(Indicator != "Comments") %>%
          mutate(value = as.numeric(value)) %>% mutate(Indicator = str_replace(Indicator, "^SR ", "")) %>%
          group_by(subrecipient) %>%
          summarise(
            `Cumulative Budget` = sum(value[Indicator == "Cumulative budget"], na.rm = TRUE),
            `Cumulative Expenditure` = sum(value[Indicator == "Cumulative funds expensed by SR"], na.rm = TRUE),
            Commitments = sum(value[Indicator == "Commitments"], na.rm = TRUE),
            Obligations = sum(value[Indicator == "Obligations"], na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            Variance = `Cumulative Budget` - `Cumulative Expenditure`,
            `Absorption Rate` = round(`Cumulative Expenditure` / `Cumulative Budget` * 100, 1),
            `Total Absorption Rate` = round((`Cumulative Expenditure` + Commitments + Obligations) / `Cumulative Budget` * 100, 1)
          ) %>%
          rename(`Sub-Recipient` = subrecipient) %>%
          filter(`Cumulative Budget` > 0)  # Only show sub-recipients with budget data
        
        output$subrec_finance_table <- renderDT({
          if (nrow(subrec_summary) > 0) {
            datatable(subrec_summary,
                      options = list(
                        pageLength = 10,
                        paging = TRUE,
                        searching = TRUE,
                        info = TRUE,
                        autoWidth = TRUE,
                        scrollX = TRUE
                      ),
                      rownames = FALSE,
                      caption = "Sub-Recipient Financial Performance (USD) - Legend: Red < 50%, Yellow 50-75%, Green > 75%",
                      class = "subrec-table"  # Add this line
            ) %>%
              formatCurrency(
                columns = c('Cumulative Budget', 'Cumulative Expenditure', 'Commitments', 'Obligations', 'Variance'),
                currency = "USD ",
                interval = 3,
                mark = ",",
                digits = 0,
                dec.mark = ".",
                before = TRUE
              ) %>%
              formatStyle(
                columns = c('Absorption Rate', 'Total Absorption Rate'),
                backgroundColor = styleInterval(
                  c(50, 75),
                  c('red', 'yellow', 'green')
                ),
                color = styleInterval(
                  c(50, 75),
                  c('white', 'black', 'white')
                )
              ) %>%
              formatStyle(
                c('Absorption Rate', 'Total Absorption Rate'),
                fontWeight = 'bold'
              )
          } else {
            datatable(
              data.frame(message = "No sub-recipient financial data available for this grant."),
              options = list(
                dom = 't',
                paging = FALSE,
                searching = FALSE
              ),
              colnames = NULL,
              rownames = FALSE
            )
          }
        })
      } else {
        output$subrec_finance_table <- renderDT({
          datatable(
            data.frame(message = "No sub-recipient financial data available for this grant."),
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
        select(-period)
      
      ## Finance overall      
      fin_data <- grant_data %>%
        filter(class %in% c("Finance")) %>%
        filter(!grepl("RSSH", name)) %>%
        select(period, Indicator , value)
      
      finance_comments <- fin_data %>%
        filter(Indicator == "Comments") %>%
        pull(value)
      
      fin_summary <- fin_data %>%
        filter(Indicator != "Comments") %>%
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
          `Absorption Rate` = round(`Cumulative Funds Expensed by PR` / `Cumulative Budget` * 100, 1),
          `Total Absorption Rate` = round((`Cumulative Funds Expensed by PR` + Commitments + Obligations) / `Cumulative Budget` * 100, 1)
        )%>%
        pivot_longer(everything(),
                     names_to = "Indicator",
                     values_to = "Result") %>%
        mutate(res_col = case_when(
          Indicator %in% c("Absorption Rate", "Total Absorption Rate") ~ as.numeric(Result), 
          TRUE ~ NA_real_
        ))
      
      # RSSH Finance Data Processing
      fin_data_rssh <- grant_data %>%
        filter(class %in% c("Finance")) %>%
        filter(grepl("RSSH", name)) %>%
        select(period, Indicator, value)
      
      finance_comments_rssh <- fin_data_rssh %>%
        filter(Indicator == "Comments") %>%
        pull(value)
      
      fin_summary_rssh <- fin_data_rssh %>%
        filter(Indicator != "Comments") %>%
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
          `Absorption Rate` = round(`Cumulative Funds Expensed by PR` / `Cumulative Budget` * 100, 1),
          `Total Absorption Rate` = round((`Cumulative Funds Expensed by PR` + Commitments + Obligations) / `Cumulative Budget` * 100, 1)
        ) %>%
        pivot_longer(everything(),
                     names_to = "Indicator",
                     values_to = "Result") %>%
        mutate(res_col = case_when(
          Indicator %in% c("Absorption Rate", "Total Absorption Rate") ~ as.numeric(Result), 
          TRUE ~ NA_real_
        ))
      
      # Render RSSH Finance Table
      output$finance_table_rssh <- renderDT({
        if (sum(fin_summary_rssh$Result, na.rm = TRUE) > 0) {
          datatable(fin_summary_rssh,
                    options = list(
                      pageLength = nrow(fin_summary_rssh),
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
              rows = which(!fin_summary_rssh$Indicator %in% c("Absorption Rate", "Total Absorption Rate")) 
            ) %>%
            formatStyle(
              columns = 'Result',
              valueColumns = 'res_col',
              target = c("cell"),
              backgroundColor = styleInterval(
                c(50, 75),  
                c('red', 'yellow', 'green')  
              ),
              rows = which(fin_summary_rssh$Indicator == "Absorption Rate")
            ) %>%
            formatStyle(
              c('Indicator', 'Result'),   
              fontWeight = styleEqual(c("Absorption Rate", "Total Absorption Rate"), c("bold", "bold"))
            )
        } else {
          datatable(
            data.frame(message = "No RSSH finance data available."),
            options = list(
              dom = 't',        
              paging = FALSE,   
              searching = FALSE
            ),
            colnames = NULL,
            rownames = FALSE    
          )
        }
      })
      
      # Render RSSH Comments
      output$finance_comments_rssh <- renderText({
        if (length(finance_comments_rssh) > 0) {
          finance_comments_rssh
        } else {
          "No comments available."
        }
      })
      
      ## Cofinance data
      cofin_data <- grant_data %>%
        filter(class %in% c("Co-financing")) %>%
        select(period, Indicator , value)
      
      cofin_comments <- cofin_data %>%
        filter(Indicator == "Comments") %>%
        pull(value)
      
      cofin_summary <- if(!input$grant %in% c("XEUXTIGkU8H","l5VURDJNlpx","Vg7RJh2mM35")){
        tibble(
          Indicator = c("Total Budget Allocation", "Cumulative Budget","Cumulative Expenditure" ,"Commitments" ,"Obligations" ,"Variance","Absorption Rate", "Total Absorption Rate"),
          Result = NA_real_,
          res_col = NA_real_
        )
      } else { cofin_data %>%
          filter(Indicator != "Comments") %>%
          select(-period) %>%
          mutate(value = as.numeric(value)) %>%
          pivot_wider(names_from = Indicator,
                      values_from = value) %>%
          mutate(
            `Total Budget Allocation` = if (!"Total Budget Allocation" %in% names(.)) NA_real_ else `Total Budget Allocation`,
            `Cumulative budget` = if (!"Cumulative budget" %in% names(.)) NA_real_ else `Cumulative budget`,
            `Cumulative expenditure` = if (!"Cumulative expenditure" %in% names(.)) NA_real_ else `Cumulative expenditure`,
            Commitments = if (!"Commitments" %in% names(.)) NA_real_ else Commitments,
            Obligations = if (!"Obligations" %in% names(.)) NA_real_ else Obligations
          ) %>%
          transmute(
            `Total Budget Allocation` = sum(`Total Budget Allocation`,na.rm = T),
            `Cumulative Budget` = sum(`Cumulative budget`,na.rm = T),
            `Cumulative Expenditure` = sum(`Cumulative expenditure`, na.rm = T),
            Commitments = sum(Commitments, na.rm = T),
            Obligations = sum(Obligations, na.rm = T),
            Variance = sum(`Cumulative budget`,na.rm = T) - sum(`Cumulative expenditure`, na.rm = T),
            `Absorption Rate` = round(`Cumulative Expenditure` / `Cumulative Budget` * 100,1),
            `Total Absorption Rate` = round((`Cumulative Expenditure` + Commitments + Obligations) / `Cumulative Budget` * 100, 1)
          ) %>%
          pivot_longer(everything(),
                       names_to = "Indicator",
                       values_to = "Result") %>%
          mutate(res_col = case_when(
            Indicator %in% c("Absorption Rate", "Total Absorption Rate") ~ as.numeric(Result), 
            TRUE ~ NA_real_
          ))
      }
      
      rating_data <- grant_data %>%
        filter(class %in% c("Rating"))
      
      rating <- if(nrow(rating_data) > 0) { 
        rating_data %>%
          select(period, Indicator, value) %>%
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
      
      # Products data
      products_data <- grant_data %>%
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
                 ~ prettyNum(as.numeric(.), big.mark = ",", scientific = F))
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
              rows = which(!fin_summary$Indicator %in% c("Absorption Rate", "Total Absorption Rate")) 
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
              fontWeight = styleEqual(c("Absorption Rate", "Total Absorption Rate"), c("bold", "bold"))
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
              rows = which(!cofin_summary$Indicator %in% c("Absorption Rate", "Total Absorption Rate")) 
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
              fontWeight = styleEqual(c("Absorption Rate", "Total Absorption Rate"), c("bold", "bold"))
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
                      pageLength = 10,
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
                c(3, 6, 10),  
                c('red','yellow', 'green', 'red')  
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
  
  # Grant Comparison functionality
  pull_all_grants_data <- reactive({
    req(input$start_date, input$end_date, input$select_period)
    
    all_grants_data <- map_dfr(grants, function(grant_id) {
      api_url <- paste0(
        base_url, "api/dataValueSets?dataSet=", grant_id,
        "&&startDate=", input$start_date, 
        "&endDate=", input$end_date, 
        "&orgUnit=", orgunit, 
        "&children=true"
      )
      
      response <- tryCatch({
        GET(
          url = api_url,
          authenticate(username, password),
          accept_json()
        )
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(response) && status_code(response) == 200 && http_type(response) == "application/json") {
        data <- fromJSON(content(response, as = "text"), flatten = TRUE)
        if (!is.null(data$dataValues)) {
          data$dataValues %>%
            mutate(grant_id = grant_id)
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
    
    if (!is.null(all_grants_data) && nrow(all_grants_data) > 0) {
      all_grants_data %>%
        as.data.frame() %>%
        mutate(
          dataElement = if (!"dataElement" %in% names(.)) NA_character_ else dataElement,
          period = if (!"period" %in% names(.)) NA_character_ else period,
          value = if (!"value" %in% names(.)) NA_character_ else value
        ) %>%
        left_join(des, by = c("dataElement" = "id")) %>%
        left_join(all_periods, by = c("period" = "quarter")) %>%
        filter(grant_periods == input$select_period)
    } else {
      NULL
    }
  })
  
  # Financial Performance Comparison Table
  output$grants_comparison_table <- renderDT({
    all_grants_data <- pull_all_grants_data()
    
    if (is.null(all_grants_data)) {
      return(datatable(data.frame(message = "No data available for comparison.")))
    }
    
    # Process financial data for all grants
    financial_comparison <- all_grants_data %>%
      filter(class %in% c("Finance")) %>%
      filter(!grepl("RSSH", name)) %>%
      select(grant_id, Indicator, value) %>%
      filter(Indicator != "Comments") %>%
      mutate(value = as.numeric(value)) %>%
      group_by(grant_id) %>%
      summarise(
        `Cumulative Budget` = sum(value[Indicator == "Cumulative Budget"], na.rm = TRUE),
        `Cumulative Expenditure` = sum(value[Indicator == "Cumulative Funds Expensed by PR"], na.rm = TRUE),
        Commitments = sum(value[Indicator == "Commitments"], na.rm = TRUE),
        Obligations = sum(value[Indicator == "Obligations"], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        Variance = `Cumulative Budget` - `Cumulative Expenditure`,
        `Absorption Rate` = round(`Cumulative Expenditure` / `Cumulative Budget` * 100, 1),
        `Total Absorption Rate` = round((`Cumulative Expenditure` + Commitments + Obligations) / `Cumulative Budget` * 100, 1)
      ) %>%
      # Add grant names
      mutate(
        Grant = case_when(
          grant_id == "WFU6M2XN4W4" ~ "HIV, KRCS",
          grant_id == "XEUXTIGkU8H" ~ "HIV, TNT",
          grant_id == "SbX36Gmomkz" ~ "Malaria, AMREF",
          grant_id == "l5VURDJNlpx" ~ "Malaria, TNT",
          grant_id == "SgPSOoSZ8Iz" ~ "TB, AMREF",
          grant_id == "Vg7RJh2mM35" ~ "TB, TNT",
          TRUE ~ grant_id
        )
      ) %>%
      select(Grant, `Cumulative Budget`, `Cumulative Expenditure`, Commitments, Obligations, Variance, `Absorption Rate`, `Total Absorption Rate`) %>%
      filter(`Cumulative Budget` > 0)  # Only show grants with budget data
    
    if (nrow(financial_comparison) == 0) {
      return(datatable(data.frame(message = "No financial data available for comparison.")))
    }
    
    datatable(financial_comparison,
              options = list(
                pageLength = nrow(financial_comparison),
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                dom = 't',
                autoWidth = TRUE,
                scrollX = TRUE
              ),
              rownames = FALSE,
              caption = "Financial Performance Comparison - All Grants (USD)"
    ) %>%
      formatCurrency(
        columns = c('Cumulative Budget', 'Cumulative Expenditure', 'Commitments', 'Obligations', 'Variance'),
        currency = "USD ",
        interval = 3,
        mark = ",",
        digits = 0,
        dec.mark = ".",
        before = TRUE
      ) %>%
      formatStyle(
        columns = c('Absorption Rate', 'Total Absorption Rate'),
        backgroundColor = styleInterval(
          c(50, 75),
          c('red', 'yellow', 'green')
        ),
        color = styleInterval(
          c(50, 75),
          c('white', 'black', 'white')
        )
      ) %>%
      formatStyle(
        c('Absorption Rate', 'Total Absorption Rate'),
        fontWeight = 'bold'
      )
  })
  
}

shinyApp(ui = ui, server = server)