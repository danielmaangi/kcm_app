source("setup.R")
source("config/credentials.R")

# Metadata
# ==============================================================
# organisations <- datimutils::getMetadata("organisationUnits", 
#                                         fields = "name, id, code, shortName, parent, path") %>%
#   dplyr::arrange(name)



dataSets <- datimutils::getMetadata("dataSets", 
                                    fields = "name, id") |>
  dplyr::arrange(name) 

glimpse(dataSets)

fwrite(dataSets, "metadata/datasets.csv")

#==============================================
# HIV
#-------------------------------
# KRCS HIV
hiv_krcs_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:WFU6M2XN4W4", 
                                    fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = case_when(str_detect(name, "/FINANCE]") ~ "Finance",
                      str_detect(name, "/RATING]") ~ "Rating",
                      str_detect(name, "/COMMODITIES]") ~ "Products",
                      str_detect(name, "/COVERAGE]") ~ "Program",
                      str_detect(name, "/OUTCOME]") ~ "Program",
                      str_detect(name, "/IMPACT]") ~ "Program",
                      str_detect(name, "/PSEAH-") ~ "PSEAH",
                      str_detect(name, "CO-FINANCE]") ~ "Co-financing",
                      TRUE ~ NA_character_),
    data = case_when(str_detect(name, "/TARGET/") ~ "Target",
                     str_detect(name, "/TAR_PSEAH]") ~ "Target",
                     str_detect(name, "/RESULT/") ~ "Result",
                     str_detect(name, "/RES_PSEAH]") ~ "Result",
                     str_detect(name, "/COMMENT/") ~ "Comment",
                     str_detect(name, "/COMM_PSEAH]") ~ "Comment",
                     TRUE ~ NA_character_),
    type = case_when(str_detect(name, "/COVERAGE") ~ "Coverage",
                     str_detect(name, "/OUTCOME") ~ "Outcome",
                     str_detect(name, "/IMPACT") ~ "Impact",
                     str_detect(name, "/PSEAH") ~ "PSEAH",
                     TRUE ~ NA_character_
    ),
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

check_fin <- hiv_krcs_gc7 %>% filter(class == "Rating") |>
  select(name, valueType)

hiv_tnt_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:XEUXTIGkU8H", 
                                       fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = case_when(str_detect(name, "/FINANCE]") ~ "Finance",
                      str_detect(name, "/RATING]") ~ "Rating",
                      str_detect(name, "/COMMODITIES]") ~ "Products",
                      str_detect(name, "/COVERAGE]") ~ "Program",
                      str_detect(name, "/OUTCOME]") ~ "Program",
                      str_detect(name, "/IMPACT]") ~ "Program",
                      str_detect(name, "/PSEAH-") ~ "PSEAH",
                      str_detect(name, "CO-FINANCE]") ~ "Co-financing",
                      TRUE ~ NA_character_),
    data = case_when(str_detect(name, "/TARGET/") ~ "Target",
                     str_detect(name, "/TAR_PSEAH]") ~ "Target",
                     str_detect(name, "/RESULT/") ~ "Result",
                     str_detect(name, "/RES_PSEAH]") ~ "Result",
                     str_detect(name, "/COMMENT/") ~ "Comment",
                     str_detect(name, "/COMM_PSEAH]") ~ "Comment",
                     TRUE ~ NA_character_),
    type = case_when(str_detect(name, "/COVERAGE") ~ "Coverage",
                     str_detect(name, "/OUTCOME") ~ "Outcome",
                     str_detect(name, "/IMPACT") ~ "Impact",
                     str_detect(name, "/PSEAH") ~ "PSEAH",
                     TRUE ~ NA_character_
    ),
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

#===============================================
# Malaria
# -----------------------------------
malaria_amref_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:SbX36Gmomkz", 
                                        fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = case_when(str_detect(name, "/FINANCE]") ~ "Finance",
                      str_detect(name, "/RATING]") ~ "Rating",
                      str_detect(name, "/COMMODITIES]") ~ "Products",
                      str_detect(name, "/COVERAGE]") ~ "Program",
                      str_detect(name, "/OUTCOME]") ~ "Program",
                      str_detect(name, "/IMPACT]") ~ "Program",
                      str_detect(name, "/PSEAH-") ~ "PSEAH",
                      str_detect(name, "CO-FINANCE]") ~ "Co-financing",
                      TRUE ~ NA_character_),
    data = case_when(str_detect(name, "/TARGET/") ~ "Target",
                     str_detect(name, "/TAR_PSEAH]") ~ "Target",
                     str_detect(name, "/RESULT/") ~ "Result",
                     str_detect(name, "/RES_PSEAH]") ~ "Result",
                     str_detect(name, "/COMMENT/") ~ "Comment",
                     str_detect(name, "/COMM_PSEAH]") ~ "Comment",
                     TRUE ~ NA_character_),
    type = case_when(str_detect(name, "/COVERAGE") ~ "Coverage",
                     str_detect(name, "/OUTCOME") ~ "Outcome",
                     str_detect(name, "/IMPACT") ~ "Impact",
                     str_detect(name, "/PSEAH") ~ "PSEAH",
                     TRUE ~ NA_character_
    ),
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


malaria_tnt_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:l5VURDJNlpx", 
                                             fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(
    class = case_when(str_detect(name, "/FINANCE]") ~ "Finance",
                      str_detect(name, "/RATING]") ~ "Rating",
                      str_detect(name, "/COMMODITIES]") ~ "Products",
                      str_detect(name, "/COVERAGE]") ~ "Program",
                      str_detect(name, "/OUTCOME]") ~ "Program",
                      str_detect(name, "/IMPACT]") ~ "Program",
                      str_detect(name, "/PSEAH-") ~ "PSEAH",
                      str_detect(name, "CO-FINANCE]") ~ "Co-financing",
                      TRUE ~ NA_character_),
    data = case_when(str_detect(name, "/TARGET/") ~ "Target",
                     str_detect(name, "/TAR_PSEAH]") ~ "Target",
                     str_detect(name, "/RESULT/") ~ "Result",
                     str_detect(name, "/RES_PSEAH]") ~ "Result",
                     str_detect(name, "/COMMENT/") ~ "Comment",
                     str_detect(name, "/COMM_PSEAH]") ~ "Comment",
                     TRUE ~ NA_character_),
    type = case_when(str_detect(name, "/COVERAGE") ~ "Coverage",
                     str_detect(name, "/OUTCOME") ~ "Outcome",
                     str_detect(name, "/IMPACT") ~ "Impact",
                     str_detect(name, "/PSEAH") ~ "PSEAH",
                     TRUE ~ NA_character_
    ),
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


#======================================
# TB
#------------------------------------

tb_amref_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:SgPSOoSZ8Iz", 
                                       fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = case_when(str_detect(name, "/FINANCE]") ~ "Finance",
                      str_detect(name, "/RATING]") ~ "Rating",
                      str_detect(name, "/COMMODITIES]") ~ "Products",
                      str_detect(name, "/COVERAGE]") ~ "Program",
                      str_detect(name, "/OUTCOME]") ~ "Program",
                      str_detect(name, "/IMPACT]") ~ "Program",
                      str_detect(name, "/PSEAH-") ~ "PSEAH",
                      str_detect(name, "CO-FINANCE]") ~ "Co-financing",
                      TRUE ~ NA_character_),
    data = case_when(str_detect(name, "/TARGET/") ~ "Target",
                     str_detect(name, "/TAR_PSEAH]") ~ "Target",
                     str_detect(name, "/RESULT/") ~ "Result",
                     str_detect(name, "/RES_PSEAH]") ~ "Result",
                     str_detect(name, "/COMMENT/") ~ "Comment",
                     str_detect(name, "/COMM_PSEAH]") ~ "Comment",
                     TRUE ~ NA_character_),
    type = case_when(str_detect(name, "/COVERAGE") ~ "Coverage",
                     str_detect(name, "/OUTCOME") ~ "Outcome",
                     str_detect(name, "/IMPACT") ~ "Impact",
                     str_detect(name, "/PSEAH") ~ "PSEAH",
                     TRUE ~ NA_character_
    ),
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

tb_tnt_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:Vg7RJh2mM35", 
                                        fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = case_when(str_detect(name, "/FINANCE]") ~ "Finance",
                      str_detect(name, "/RATING]") ~ "Rating",
                      str_detect(name, "/COMMODITIES]") ~ "Products",
                      str_detect(name, "/COVERAGE]") ~ "Program",
                      str_detect(name, "/OUTCOME]") ~ "Program",
                      str_detect(name, "/IMPACT]") ~ "Program",
                      str_detect(name, "/PSEAH-") ~ "PSEAH",
                      str_detect(name, "CO-FINANCE]") ~ "Co-financing",
                      TRUE ~ NA_character_),
    data = case_when(str_detect(name, "/TARGET/") ~ "Target",
                     str_detect(name, "/TAR_PSEAH]") ~ "Target",
                     str_detect(name, "/RESULT/") ~ "Result",
                     str_detect(name, "/RES_PSEAH]") ~ "Result",
                     str_detect(name, "/COMMENT/") ~ "Comment",
                     str_detect(name, "/COMM_PSEAH]") ~ "Comment",
                     TRUE ~ NA_character_),
    type = case_when(str_detect(name, "/COVERAGE") ~ "Coverage",
                     str_detect(name, "/OUTCOME") ~ "Outcome",
                     str_detect(name, "/IMPACT") ~ "Impact",
                     str_detect(name, "/PSEAH") ~ "PSEAH",
                     TRUE ~ NA_character_
    ),
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


# Save all
# ------------------------
dataElements <- bind_rows(hiv_krcs_gc7, hiv_tnt_gc7, malaria_amref_gc7, malaria_tnt_gc7, tb_amref_gc7, tb_tnt_gc7) %>%
  rename(Indicator = description) %>%
  mutate(
    Indicator = case_when(class %in% c("Finance", "Rating") ~ str_replace(Indicator, "PR ", ""),
                          TRUE ~ Indicator)
  )

fwrite(dataElements, "metadata/dataElements.txt")
# -------------------------------------------------------------------


finance_d <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:WFU6M2XN4W4", 
                                       fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo,optionSet") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id, optionSet.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(finance_d)


fwrite(finance_d, "metadata/finance_rev.csv")


#---------------------------------------------------------------
startdate <- Sys.Date() - 1100
enddate <- Sys.Date()
test_One_url <- paste0(base_url,
                       "api/dataValueSets?dataSet=WFU6M2XN4W4&&startDate=", 
                       startdate, "&endDate=", enddate,"&orgUnit=",
                       orgunit, "&children=true")

test_One_01 <- content(GET(test_One_url), as = "parsed")

test_One <- rbindlist(test_One_01$dataValues, fill = TRUE) |>
  left_join(dataElements, by = c("dataElement" = "id")) %>%
  filter(class == "Finance")


fin_summary <- test_One %>%
  mutate(Indicator = str_replace(Indicator, "PR ", "")) %>%
  filter(period == "2024Q3") %>%
  select(Indicator, value) %>%
  pivot_wider(names_from = Indicator,
              values_from = value) %>%
  mutate(`Absorption Rate` = scales::label_percent()(as.numeric(`Cumulative Funds Expensed by PR`) / as.numeric(`Cumulative Budget`))) %>%
  pivot_longer(everything(),
               names_to = "Indicator",
               values_to = "Result") 


comments_text <- fin_summary %>%
  filter(Indicator == "Comments") %>%
  pull(Result)

















