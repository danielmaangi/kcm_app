source("setup.R")
source("periods.R")
source("grants.R")
source("config/credentials.R")

pull_program_data <- function(start_date = NULL, end_date = NULL, grant = NULL, orgunit = NULL, 
                              base_url = NULL, username = NULL, password = NULL) {
  
  # Check if essential parameters are provided
  if (is.null(start_date) || is.null(end_date) || is.null(orgunit) || is.null(base_url) || 
      is.null(username) || is.null(password)) {
    stop("Missing required parameters: start_date, end_date, orgunit, base_url, username, or password.")
  }
  
  # Construct the API URL
  api_url <- paste0(
    base_url, "api/dataValueSets?dataSet=", grant,
    "&&startDate=", start_date, 
    "&endDate=", end_date, 
    "&orgUnit=", orgunit, 
    "&children=true"
  )
  
  cat("API URL: ", api_url, "\n")
  
  # Send the API request
  response <- tryCatch({
    GET(
      url = api_url,
      authenticate(username, password),
      accept_json()
    )
  }, error = function(e) {
    return(e)
  })
  
  # Check if the response encountered an error
  if (inherits(response, "error")) {
    message <- paste("Error in API request:", response$message)
    cat(message, "\n")
    return(NULL)
  }
  
  # Process the response if successful
  if (status_code(response) == 200 && http_type(response) == "application/json") {
    data <- fromJSON(content(response, as = "text"), flatten = TRUE)
    if (!is.null(data$dataValues)) {
      return(data$dataValues)
    } else {
      cat("No 'dataValues' found in the response.\n")
      return(NULL)
    }
  } else {
    message <- paste("API request failed. Status code:", status_code(response), 
                     "Content Type:", http_type(response))
    cat(message, "\n")
    return(NULL)
  }
}



#---------------------------------------------------------------
des <- fread("metadata/dataElements.txt") 



test_data <- pull_program_data(start_date = "2024-01-01", end_date = "2024-12-31",
                               grant = "l5VURDJNlpx",
                               orgunit = orgunit, 
                               base_url = base_url,
                               username = username, password = password) %>%
  as.data.frame() %>%
  mutate(
    dataElement = if (!"dataElement" %in% names(.)) NA_character_ else dataElement,
    period = if (!"period" %in% names(.)) NA_character_ else period,
    value = if (!"value" %in% names(.)) NA_character_ else value
  ) %>%
  left_join(des, by = c("dataElement" = "id")) %>%
  left_join(all_periods, by = c("period" = "quarter")) %>%
  filter(grant_periods == "P2") 

# RSSH Finance Data Processing
fin_data_rssh <- test_data %>%
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
    `Absorption Rate` = round(`Cumulative Funds Expensed by PR` / `Cumulative Budget` * 100, 1)
  ) %>%
  pivot_longer(everything(),
               names_to = "Indicator",
               values_to = "Result") %>%
  mutate(res_col = case_when(Indicator == "Absorption Rate" ~ as.numeric(Result), 
                             TRUE ~ NA_real_))


products_data <- test_data %>%
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




products_data <- test_data %>%
  filter(period == "2024Q3") %>%
  filter(class %in% c("Products")) %>%
  select(period, Indicator , value) %>%
  transmute(
    metric  = case_when(
      str_detect(str_to_lower(Indicator), "ending balance") ~ "Stock on hand",
      str_detect(str_to_lower(Indicator), "amc") ~ "Average Monthly Consumption",
      str_detect(str_to_lower(Indicator), "under procurement") ~ "Under Procurement",
      str_detect(str_to_lower(Indicator), "sor numerator") ~ "SOR Numerator",
      str_detect(str_to_lower(Indicator), "sor denominator") ~ "SOR Denominator",
      str_detect(str_to_lower(Indicator), "comments") ~ "Comments",
      TRUE ~ NA_character_
    ),
    value = value
  ) %>%
  pivot_wider(names_from = metric,
              values_from = value)

# Define the list of known metric keywords to aid separation
metric_keywords <- c("Ending Balance", "AMC", "Under Procurement", "SOR Numerator", "SOR Denominator", "Comments")

# Use regex to split based on the first appearance of any metric keyword
products_data <- test_data %>%
  filter(period == "2024Q3") %>%
  filter(class %in% c("Products")) %>%
  select(period, Indicator , value) %>%
  separate(Indicator, into = c("product", "metric"), 
           sep = paste0("(?i)\\b(", paste(metric_keywords, collapse = "|"), ")\\b"), 
           extra = "merge", fill = "right") %>%
  mutate(
    metric = str_trim(metric),
    product = str_trim(product)
  )





sub_recipient_finance <- pull_program_data(start_date = "2024-01-01", end_date = "2025-12-31",
                               grant = "O7LYGHTAVmT",
                               orgunit = orgunit, 
                               base_url = base_url,
                               username = username, password = password) %>%
  as.data.frame() %>%
  mutate(
    dataElement = if (!"dataElement" %in% names(.)) NA_character_ else dataElement,
    period = if (!"period" %in% names(.)) NA_character_ else period,
    value = if (!"value" %in% names(.)) NA_character_ else value
  ) %>%
  left_join(des, by = c("dataElement" = "id")) %>%
  left_join(all_periods, by = c("period" = "quarter")) 


processed_data <- pull_program_data(start_date = "2024-01-01", end_date = "2025-12-31",
                                    grant = "O7LYGHTAVmT",
                                    orgunit = orgunit, 
                                    base_url = base_url,
                                    username = username, password = password) %>%
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
  # Join with data elements info
  left_join(des, by = c("dataElement" = "id")) %>%
  # Join with periods info
  left_join(all_periods, by = c("period" = "quarter")) %>%
  select(period, Indicator, value, grant, subrecipient, attributeOptionCombo)



subrec_summary <- processed_data %>%
  filter(Indicator != "Comments") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(Indicator = str_replace(Indicator, "^SR ", "")) %>%
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
  filter(`Cumulative Budget` > 0)










