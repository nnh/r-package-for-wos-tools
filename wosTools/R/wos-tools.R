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
