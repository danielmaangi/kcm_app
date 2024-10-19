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
    class = "Programmes PR",
    cycle = "GC7",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

hiv_tnt_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:XEUXTIGkU8H", 
                                       fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    cycle = "GC7",
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
    class = "Programmes PR",
    cycle = "GC7",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


malaria_tnt_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:l5VURDJNlpx", 
                                             fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    cycle = "GC7",
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
    class = "Programmes PR",
    cycle = "GC7",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

tb_tnt_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:Vg7RJh2mM35", 
                                        fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    cycle = "GC7",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


finance <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:hhqiQm61ezm", 
                                 fields = "name, id, code, shortName, description, formName,fieldMask,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator,fieldMask, categoryCombo.id)|>
  mutate(
    class = "Finance PR",
    cycle = "GC7",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

# Save all
# ------------------------
dataElements <- bind_rows(hiv_krcs_gc7, hiv_tnt_gc7, malaria_amref_gc7, malaria_tnt_gc7, tb_amref_gc7, tb_tnt_gc7, finance)
fwrite(dataElements, "metadata/dataElements.txt")
# -------------------------------------------------------------------


tnt_tb <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:Vg7RJh2mM35", 
                                       fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(tnt_tb)


fwrite(pseah, "metadata/pseah.csv")

