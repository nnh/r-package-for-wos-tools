#' Get Organizations From All Papers
#'
#' This function reads JSON data from a file path containing information about papers and extracts
#' organizations from authors' information.
#'
#' @param allpapers_path Path to the JSON file containing information about papers.
#' @return A data frame with columns including 'uid', 'ad', and 'oo' representing paper UID,
#'   organization's full address, and organization's content, respectively.
#'
#' @examples
#' allpapers_path <- "path/to/allpapers.json"
#' organizations_data <- GetOrganizationsFromAllPapers(allpapers_path)
#'
#' @importFrom jsonlite read_json
#' @importFrom purrr map bind_rows
#' @importFrom stringr tolower
#' @importFrom tidyr list_c
#'
#' @export
GetOrganizationsFromAllPapers <- function(allpapers_path){
  allpapers <- allpapers_path %>% read_json()
  df_uid_ad_oo <- allpapers %>% map( ~ {
    allpaper <- .
    uid <- allpaper$uid
    authors <- allpaper$authors
    organizations <- authors %>% map( ~ {
      orgs <- .$organizations %>% map( ~ {
        org <- .
        ad <- org$fullAddress %>% tolower()
        oo <- org$content %>% list_c() %>% tolower()
        df_ad <- data.frame(ad=ad)
        df_oo <- data.frame(oo=oo)
        df_ad_oo <- if (nrow(df_oo) > 0) {
          data.frame(ad = df_ad$ad, oo = df_oo$oo)
        } else {
          data.frame(ad = df_ad$ad)
        }
        return(df_ad_oo)
      }) %>% bind_rows()
      return(orgs)
    }) %>% bind_rows()
    res <- data.frame(uid=uid) %>% base::cbind(organizations)
    return(res)
  }) %>% bind_rows()
  return(df_uid_ad_oo)
}
#' Get organizations data from raw JSON file.
#'
#' This function reads a raw JSON file and extracts organizations data.
#'
#' @param rawJson_path Path to the raw JSON file.
#' @return A data frame containing UID, address (ad), and organization (oo) information.
#' @export
#'
#' @examples
#' # Example usage:
#' organizations_data <- GetOrganizationsFromRawJson("/path/to/raw_json_file.json")
#'
#' @importFrom jsonlite read_json
#' @importFrom purrr map map_dfr
#'
#' @seealso \code{\link{GetAddressesFromRawJson}}
GetOrganizationsFromRawJson <- function(rawJson_path){
  raw_json <- rawJson_path %>% read_json()
  raw_json_data <- raw_json %>% map( ~ .$Data$Records$records$REC)
  data_list <- raw_json_data %>% map( ~ GetAddressesFromRawJson(.))
  data_df <- data_list %>% map( ~ {
    target <- .
    df <- target %>% map_dfr( ~ data.frame(
          uid = .$uid,
          ad = .$addresses$ad,
          oo = .$addresses$oo,
          stringsAsFactors = FALSE
    ))
    return(df)
  }) %>% bind_rows()
  return(data_df)
}
GetAddressesFromRawJson <- function(target){
  res <- target %>% map( ~ {
    uid <- .$UID
    address_name <- .$static_data$fullrecord_metadata$addresses$address_name
    addresses <- GetAdAndOoFromRawJson(address_name)
    return(list(uid=uid, addresses=addresses))
  })
  return(res)
}
GetOoFromRawJson <- function(organizations){
  res <- organizations$organization %>% map( ~ { .$content}) %>% unlist()
  if (length(res) == 0){
    res <- NA
  }
  #res <- ifelse(length(res) > 0, res, NA)
  return(res)
}
GetAdFromRawJson <- function(full_address){
  res <- ifelse(length(full_address) > 0, full_address, NA)
  return(res)
}
GetAdAndOoFromRawJson <- function(address_name){
  temp_addresses <- address_name %>% map( ~ {
    address_spec <- .$address_spec
    ad <<- GetAdFromRawJson(address_spec$full_address) %>% data.frame(ad=.)
    oo <<- GetOoFromRawJson(address_spec$organizations)
    ad_oo <<- ad %>% cbind(oo)
    return(ad_oo)
  }) %>% bind_rows()
  return(temp_addresses)
}
