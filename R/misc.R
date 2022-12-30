#' Perform a dry run on the request on BoF-PSS 3
#'
#' @param ... query parameters
#' @param method request method
#' @param body JSON body for the request
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a list containing information on the request to BoF-PSS 3 object
#'
#' @examples
pss_dry_run <- function(...,
                        body = NULL,
                        method = "GET",
                        pss_domain = "http://localhost:8080") {
  pss(...) |>
    httr2::req_body_json(body) |>
    httr2::req_dry_run()
}


#' Get BoF-PSS3 Simulator Information
#'
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a list containing information about installed BoF-PSS 3 application
#' @export
#'
#' @examples
pss_info <- function(pss_domain = "http://localhost:8080") {
  pss("simulator", "info") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
