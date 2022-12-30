#' Title
#'
#' @param resp
#' @param resp_type
#'
#' @return
#' @export
#'
#' @examples
parse_response <- function(resp, resp_type = "json") {
  resp_type <- match.arg(tolower(resp_type), c("json", "html", "string"))

  if (is.null(resp)) return( resp )

  if (resp_type == "json") {
    r <- tryCatch(httr2::resp_body_json(resp),
                  error = function(e) "response is not a valid JSON")
  } else if (resp_type == "html") {
    r <- tryCatch(httr2::resp_body_html(resp),
                  error = function(e) "response is not a valid HTML")
  } else {
    r <- tryCatch(httr2::resp_body_string(resp),
                  error = function(e) "response is not valid")
  }

  if (typeof(r) != "list") stop(r)

  return( r )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
no_pss_api <- function() {
  resp <- tryCatch(pss_info(), error = function(e) NULL)

  is.null(resp)
}
