#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date 2023.12.25
rm(list=ls())
# ------ libraries ------
library(here)
library(tidyverse)
library(jsonlite)
library(testthat)
source(here("R", "wos-tools.R"))
# ------ constants ------
kInputPath <- here("tests", "inputForTest", "testGetOrganizationsFromRawJson.json")
# ------ functions ------
# Create a JSON file for testing
EditTestJson <- function(input_data){
  rec <- input_data[[1]]$Data$Records$records$REC %>% .[1:3] %>% map( ~ {
    target <- .
    target$dates <- NULL
    target$r_id_disclaimer <- NULL
    target$dynamic_data <- NULL
    target$static_data$summary <- NULL
    target$static_data$item <- NULL
    target$static_data$contributors <- NULL
    target$static_data$fullrecord_metadata$category_info <- NULL
    target$static_data$fullrecord_metadata$normalized_languages <- NULL
    target$static_data$fullrecord_metadata$languages <- NULL
    target$static_data$fullrecord_metadata$refs <- NULL
    target$static_data$fullrecord_metadata$reprint_addresses <- NULL
    target$static_data$fullrecord_metadata$abstracts <- NULL
    target$static_data$fullrecord_metadata$fund_ack <- NULL
    target$static_data$fullrecord_metadata$normalized_doctypes <- NULL
    for (i in 1:3){
      for (j in 1:target$static_data$fullrecord_metadata$addresses$count){
        target$static_data$fullrecord_metadata$addresses$address_name[[j]]$names <- NULL
        target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$suborganizations <- NULL
        target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$zip <- NULL
        target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$street <- NULL
        target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$city <- NULL
        target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$state <- NULL
        target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$full_address <- str_c("full_address", i, j, sep="_")
        for (k in 1:length(target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$organizations$organization)){
          target$static_data$fullrecord_metadata$addresses$address_name[[j]]$address_spec$organizations$organization[[k]]$content <- str_c("organization", i, j, k, sep="_")
        }
      }
    }
    return(target)
  })
  rec[[1]]["UID"] <- "WOS:000000000000001"
  rec[[2]]["UID"] <- "WOS:000000000000002"
  rec[[3]]["UID"] <- "WOS:000000000000003"
  testtest <<- rec
  input_data[[1]]$Data$Records$records$REC <- rec
  for (i in 38:2){
    input_data[[i]] <- NULL
  }
  return(input_data)
}
# Create a JSON file for testing
CreateTestJson <- function(){
  # Specify the path to raw.json, the source of the JSON file for testing.
  input_json <- "~/Library/CloudStorage/Box-Box/Projects/NHO 臨床研究支援部/英文論文/wos-tools/result/result_20231204085755/raw/raw.json"
  test <- input_json %>% read_json()
  test <- test %>% EditTestJson()
  test %>% write_json(kInputPath, auto_unbox=T)
}
ExecTestGetOrganizationsFromAllPapers <- function(){
  target <- GetOrganizationsFromRawJson(kInputPath)
  uid <- c("WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003")
  ad <- c("full_address_3_1", "full_address_3_2", "full_address_3_3", "full_address_3_4", "full_address_3_5", "full_address_3_5", "full_address_3_5", "full_address_3_1", "full_address_3_2", "full_address_3_3", "full_address_3_4", "full_address_3_5", "full_address_3_5", "full_address_3_5", "full_address_3_1", "full_address_3_1", "full_address_3_2", "full_address_3_3", "full_address_3_3", "full_address_3_4", "full_address_3_4", "full_address_3_5", "full_address_3_5", "full_address_3_5", "full_address_3_6", "full_address_3_6", "full_address_3_7", "full_address_3_7", "full_address_3_8", "full_address_3_8")
  oo <- c("organization_3_1_1", "organization_3_2_1", "organization_3_3_1", "organization_3_4_1", "organization_3_5_1", "organization_3_5_2", "organization_3_5_3", "organization_3_1_1", "organization_3_2_1", "organization_3_3_1", "organization_3_4_1", "organization_3_5_1", "organization_3_5_2", "organization_3_5_3", "organization_3_1_1", "organization_3_1_2", "organization_3_2_1", "organization_3_3_1", "organization_3_3_2", "organization_3_4_1", "organization_3_4_2", "organization_3_5_1", "organization_3_5_2", "organization_3_5_3", "organization_3_6_1", "organization_3_6_2", "organization_3_7_1", "organization_3_7_2", "organization_3_8_1", "organization_3_8_2")
  df_check <- data.frame(uid, ad, oo)
  result <- expect_true(all.equal(target, df_check), info="Data frames are not equal.")
  if (result){
    print("GetOrganizationsFromRawJson passed.")
  }
  return(result)
}
# ------ main ------
#CreateTestJson()
ExecTestGetOrganizationsFromAllPapers()


