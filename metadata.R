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
dataElements <- bind_rows(hiv_krcs_gc7, hiv_tnt_gc7, malaria_amref_gc7, malaria_tnt_gc7, 
                          tb_amref_gc7, 
                          tb_tnt_gc7) %>%
  rename(Indicator = description) %>%
  mutate(
    Indicator = case_when(class %in% c("Finance", "Rating") ~ str_replace(Indicator, "PR ", ""),
                          TRUE ~ Indicator)
  ) %>%
  mutate(
    type = case_when(str_detect(name, "RSSH") ~ "RSSH",
                     TRUE ~ type),
  )

fwrite(dataElements, "metadata/dataElements.txt")
# -------------------------------------------------------------------


# Grants: lvrhp60kkp1
#---------------------------------------------------------------
# Construct the URL with all necessary parameters
# Construct the full URL with all specified parameters
grants_url <- paste0(
  base_url,
  "api/40/tracker/events.csv?",
  "fields=dataValues,occurredAt,event,status,orgUnit,program,programType,updatedAt,createdAt,assignedUser,",
  "&program=lvrhp60kkp1&orgUnit=HfVjCurKxh2&programStage=GDaw1VVUL1i",
  "&ouMode=SELECTED&order=occurredAt:desc&skipPaging=true"
)

# Retrieve and parse the data
grants_01 <- content(GET(grants_url), "text")
grants_data <- read_csv(grants_01) |>
  select(event, status, dataElement, value) |>
  pivot_wider(names_from = dataElement,
              values_from = value) |>
  transmute(
    event = event,
    status = status,
    grant_number = JNNg3GI0aRQ,
    component = GzWft4oGtTX,
    principal_recipient = SRsN6vYnQgv,
    start_date = TToLBIWglqb,
    end_date = PTjOzPGQ4SH,
    signed_amount = B10phkwSM5n,
    committed_amount = fyKtOEGtPRS,
    disbursed_amount = JrgZEBmvQug,
    goal = gBvbTi6CZGF,
    objectives = rI5rhJSPdvx,
  ) %>%
  arrange(grant_number) |>
  mutate(
    dataset_id = case_when(grant_number == "KEN-H-KRCSPO6 2023-2025" ~ "WFU6M2XN4W4",
                           grant_number == "KEN-H-TNTPO6 2023-2025" ~ "XEUXTIGkU8H",
                           grant_number == "KEN-M-AMREFPO4 2023-2025" ~ "SbX36Gmomkz",
                           grant_number == "KEN-M-TNTPO4 2023-2025" ~ "l5VURDJNlpx",
                           grant_number == "KEN-T-AMREFPO6 2023-2025" ~ "SgPSOoSZ8Iz",
                           grant_number == "KEN-T-TNTPO6 2023-2025" ~ "Vg7RJh2mM35",
                           TRUE ~ NA_character_
                           ),
    grant_yoy = str_extract(grant_number, "^[^P]+")
  ) %>%
  filter(
    status == "ACTIVE"
  )  


fwrite(grants_data, "metadata/grant_info.txt")

finance_d <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:Vg7RJh2mM35", 
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



















