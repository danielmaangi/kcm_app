source("setup.R")
source("config/credentials.R")


tb_tnt_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:Vg7RJh2mM35", 
                                        fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) %>%
  select(name, id, code, shortName, description, formName,
         domainType, valueType)

library(openxlsx)
write.xlsx(tb_tnt_gc7, file = "metadata/tb_tnt_system.xlsx")

tb_amref_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:SgPSOoSZ8Iz", 
                                      fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) %>%
  select(name, id, code, shortName, description, formName,
         domainType, valueType)

library(openxlsx)
write.xlsx(tb_amref_gc7, file = "metadata/tb_amref_system.xlsx")







tb_amref_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:SgPSOoSZ8Iz", 
                                        fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) %>%
  select(name, id, code, shortName, description, formName,
         domainType, valueType)

library(openxlsx)
write.xlsx(tb_amref_gc7, file = "metadata/tb_amref_system.xlsx")

library(httr)

# Dataelements to delete in dhis2
library(httr)
base_url <- "https://kcmdashboard.org/api/"
username <- "danielmaangi@gmail.com"
password <- "TheOnlyLegend@25"

uids <- c("jpmu2XNKHvE")  # Actual UIDs

delete_data_elements <- function() {
  for (uid in uids) {
    url <- paste0(base_url, "dataElements/", uid)
    response <- DELETE(
      url,
      authenticate(username, password)
    )
    if (status_code(response) == 204) {
      cat(paste0("Successfully deleted ", uid, "\n"))
    } else {
      cat(paste0("Failed to delete ", uid, ": ", content(response, "text"), "\n"))
    }
  }
}

delete_data_elements()




library(httr)
library(jsonlite)

dataset_id <- "SgPSOoSZ8Iz"


# Function to delete data values for a given dataset
delete_dataset_data_values <- function() {
  # Step 1: Get the list of data elements in the dataset
  url <- paste0(base_url, "dataSets/", dataset_id, "?fields=dataSetElements[dataElement[id]]")
  response <- GET(url, authenticate(username, password))
  
  if (status_code(response) != 200) {
    stop("Failed to retrieve dataset: ", content(response, "text"))
  }
  
  content <- content(response, "text")
  json <- fromJSON(content)
  data_elements <- sapply(json$dataSetElements, function(x) x$dataElement$id)
  
  if (length(data_elements) == 0) {
    cat("No data elements found in dataset ", dataset_id, "\n")
    return()
  }
  
  cat("Found ", length(data_elements), " data elements in dataset ", dataset_id, "\n")
  
  # Step 2: Delete data values for each data element
  for (de in data_elements) {
    cat("Processing data element: ", de, "\n")
    page <- 1
    page_size <- 100
    has_more <- TRUE
    
    while (has_more) {
      # Construct URL for querying data values
      query_url <- paste0(base_url, "dataValues?dataElement=", de, "&page=", page, "&pageSize=", page_size)
      response <- GET(query_url, authenticate(username, password))
      
      if (status_code(response) != 200) {
        cat("Failed to retrieve data values for ", de, ": ", content(response, "text"), "\n")
        next
      }
      
      content <- content(response, "text")
      json <- fromJSON(content)
      data_values <- json$dataValues
      
      # If no data values are returned, move to the next data element
      if (is.null(data_values) || nrow(data_values) == 0) {
        has_more <- FALSE
        cat("No more data values for ", de, "\n")
        next
      }
      
      # Delete each data value
      for (i in 1:nrow(data_values)) {
        dv <- data_values[i, ]
        de_id <- dv$dataElement
        pe <- dv$period
        ou <- dv$orgUnit
        cc <- if ("categoryOptionCombo" %in% names(dv)) dv$categoryOptionCombo else NULL
        cp <- if ("attributeOptionCombo" %in% names(dv)) dv$attributeOptionCombo else NULL
        
        # Construct DELETE URL
        delete_url <- paste0(base_url, "dataValues?de=", de_id, "&pe=", pe, "&ou=", ou)
        if (!is.null(cc)) delete_url <- paste0(delete_url, "&cc=", cc)
        if (!is.null(cp)) delete_url <- paste0(delete_url, "&cp=", cp)
        
        # Send DELETE request
        delete_response <- DELETE(delete_url, authenticate(username, password))
        
        if (status_code(delete_response) %in% c(200, 204)) {
          cat("Deleted data value for ", de_id, " in period ", pe, " and org unit ", ou, "\n")
        } else {
          cat("Failed to delete data value: ", content(delete_response, "text"), "\n")
        }
      }
      
      # Check for more pages
      if ("pager" %in% names(json) && json$pager$page < json$pager$pageCount) {
        page <- page + 1
      } else {
        has_more <- FALSE
      }
    }
  }
  
  cat("Finished deleting data values for dataset ", dataset_id, "\n")
}

# Execute the function
delete_dataset_data_values()
