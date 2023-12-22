#' testGetOrganizationsFromAllPapers.R
#'
#' @author Mariko Ohtsuka
#' @date 2023.12.22
rm(list=ls())
# ------ libraries ------
library(here)
library(tidyverse)
library(jsonlite)
library(wosTools)
library(testthat)
# ------ constants ------
kInputPath <- here("tests", "inputForTest", "testGetOrganizationsFromAllPapers.json")
# ------ functions ------
# Create a JSON file for testing
EditTestJson <- function(test){
  if (length(test) != 3){
    return(NULL)
  }
  test[[1]]["uid"] <- "WOS:000000000000001"
  test[[2]]["uid"] <- "WOS:000000000000002"
  test[[3]]["uid"] <- "WOS:000000000000003"
  target_names <- c("uid", "authors")
  authors_target_names <- c()
  for (i in 1:3){
    delete_target <- setdiff(names(test[[i]]), target_names)
    for (j in 1:length(delete_target)){
      test[[i]][[delete_target[j]]] <- NULL
    }
    for (j in 1:length(test[[i]]$authors)){
      test[[i]]$authors[[j]]$name <- paste("name", i, j, sep="_")
      organizations <- test[[i]]$authors[[j]]$organizations
      if (length(organizations) != 0){
        for (k in 1:length(organizations)){
          test[[i]]$authors[[j]]$organizations[[k]]$fullAddress <- paste("fullAddress", i, j, k, sep="_")
          for (m in 1:length(organizations[[k]]$content)){
            test[[i]]$authors[[j]]$organizations[[k]]$content[[m]] <- paste("content", i, j, k, m, sep="_")
          }
        }
      } else {
        org <- NULL
      }
    }
  }
  return(test)
}
# Create a JSON file for testing
CreateTestJson <- function(){
  # Specify the path to allpapers.json, the source of the JSON file for testing.
  input_json <- "~/Library/CloudStorage/Box-Box/Projects/NHO 臨床研究支援部/英文論文/wos-tools/result/result_20231204085755/raw/all_papers.json"
  test <- input_json %>% read_json()
  test <- test[1:3]
  test <- EditTestJson(test)
  test %>% write_json(kInputPath, auto_unbox=T)
}
ExecTestGetOrganizationsFromAllPapers <- function(){
  target <- GetOrganizationsFromAllPapers(kInputPath)
  uid <- c("WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000001", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000002", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003", "WOS:000000000000003")
  oo <- c("content_1_1_1_1", "content_1_2_1_1", "content_1_3_1_1", "content_1_3_2_1", "content_1_3_3_1", "content_1_3_3_2", "content_1_3_3_3", "content_1_4_1_1", "content_1_5_1_1", "content_2_1_1_1", "content_2_2_1_1", "content_2_3_1_1", "content_2_3_2_1", "content_2_3_3_1", "content_2_3_3_2", "content_2_3_3_3", "content_2_4_1_1", "content_2_5_1_1", "content_3_1_1_1", "content_3_1_1_2", "content_3_1_2_1", "content_3_2_1_1", "content_3_2_1_2", "content_3_3_1_1", "content_3_3_1_2", "content_3_4_1_1", "content_3_4_1_2", "content_3_4_2_1", "content_3_4_2_2", "content_3_5_1_1", "content_3_5_1_2", "content_3_6_1_1", "content_3_6_1_2", "content_3_6_1_3", "content_3_7_1_1", "content_3_7_1_2", "content_3_8_1_1", "content_3_8_1_2", "content_3_9_1_1", "content_3_9_1_2", "content_3_10_1_1", "content_3_10_1_2", "content_3_11_1_1", "content_3_11_1_2", "content_3_12_1_1", "content_3_12_1_2", "content_3_13_1_1", "content_3_13_1_2", "content_3_14_1_1", "content_3_14_1_2", "content_3_15_1_1", "content_3_15_1_2", "content_3_16_1_1", "content_3_16_1_2", "content_3_17_1_1", "content_3_17_1_2")
  ad <- c("fulladdress_1_1_1", "fulladdress_1_2_1", "fulladdress_1_3_1", "fulladdress_1_3_2", "fulladdress_1_3_3", "fulladdress_1_3_3", "fulladdress_1_3_3", "fulladdress_1_4_1", "fulladdress_1_5_1", "fulladdress_2_1_1", "fulladdress_2_2_1", "fulladdress_2_3_1", "fulladdress_2_3_2", "fulladdress_2_3_3", "fulladdress_2_3_3", "fulladdress_2_3_3", "fulladdress_2_4_1", "fulladdress_2_5_1", "fulladdress_3_1_1", "fulladdress_3_1_1", "fulladdress_3_1_2", "fulladdress_3_2_1", "fulladdress_3_2_1", "fulladdress_3_3_1", "fulladdress_3_3_1", "fulladdress_3_4_1", "fulladdress_3_4_1", "fulladdress_3_4_2", "fulladdress_3_4_2", "fulladdress_3_5_1", "fulladdress_3_5_1", "fulladdress_3_6_1", "fulladdress_3_6_1", "fulladdress_3_6_1", "fulladdress_3_7_1", "fulladdress_3_7_1", "fulladdress_3_8_1", "fulladdress_3_8_1", "fulladdress_3_9_1", "fulladdress_3_9_1", "fulladdress_3_10_1", "fulladdress_3_10_1", "fulladdress_3_11_1", "fulladdress_3_11_1", "fulladdress_3_12_1", "fulladdress_3_12_1", "fulladdress_3_13_1", "fulladdress_3_13_1", "fulladdress_3_14_1", "fulladdress_3_14_1", "fulladdress_3_15_1", "fulladdress_3_15_1", "fulladdress_3_16_1", "fulladdress_3_16_1", "fulladdress_3_17_1", "fulladdress_3_17_1")
  df_check <- data.frame(uid, ad, oo)
  result <- expect_true(all.equal(target, df_check), info="Data frames are not equal.")
  if (result){
    print("GetOrganizationsFromAllPapers passed.")
  }
  return(result)
}
# ------ main ------
#CreateTestJson()
test <- ExecTestGetOrganizationsFromAllPapers()

