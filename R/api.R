#' Generate a request to BoF-PSS 3 object
#'
#' @param ... query parameters
#' @param method request method
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a request to BoF-PSS 3 object
#'
#' @examples
pss <- function(..., method = "GET", pss_domain = "http://localhost:8080") {
  req <- httr2::request(pss_domain) |>
    httr2::req_method(method)

  if (length(list(...)) > 0) {
    req <- req |> httr2::req_url_path_append(...)
  }

  req
}



#' Send a GET request to BoF-PSS 3
#'
#' @param ... query parameters
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a response from BoF-PSS
#'
#' @examples
pss_get <- function(...,
                    resp_type = "json",
                    pss_domain = "http://localhost:8080") {
  req <- pss(..., method = "GET", pss_domain = pss_domain)

  httr2::req_perform(req)
}



#' Send a POST request to BoF-PSS 3
#'
#' @param ... query parameters
#' @param body JSON body for the request
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a response from BoF-PSS
#'
#' @examples
pss_post <- function(...,
                     data = NULL,
                     path = NULL,
                     type = "json",
                     resp_type = "json",
                     pss_domain = "http://localhost:8080") {
  if (!is.null(data) & !is.null(path)) {
    stop("Can only a add request body from either `data` or `path` argument")
  }

  req <- pss(..., method = "POST", pss_domain = pss_domain)

  if (!is.null(data)) req <- httr2::req_body_json(req, data)
  if (!is.null(path)) req <- httr2::req_body_file(req, path, type)

  httr2::req_perform(req)
}



#' Send a PUT request to BoF-PSS 3
#'
#' @param ... query parameters
#' @param body JSON body for the request
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a response from BoF-PSS
#'
#' @examples
pss_put <- function(...,
                    data = NULL,
                    path = NULL,
                    type = "json",
                    resp_type = "json",
                    pss_domain = "http://localhost:8080") {
  if (!is.null(data) & !is.null(path)) {
    stop("Can only add a request body from either `data` or `path` argument")
  }

  req <- pss(..., method = "PUT", pss_domain = pss_domain)

  if (!is.null(data)) req <- httr2::req_body_json(req, data)
  if (!is.null(path)) req <- httr2::req_body_file(req, path, type)

  httr2::req_perform(req)
}



#' Send a DELETE request to BoF-PSS 3
#'
#' @param ... query parameters
#' @param body JSON body for the request
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a response from BoF-PSS
#'
#' @examples
pss_delete <- function(...,
                       data = NULL,
                       path = NULL,
                       type = "json",
                       resp_type = "json",
                       pss_domain = "http://localhost:8080") {
  if (!is.null(data) & !is.null(path)) {
    stop("Can only add a request body from either `data` or `path` argument")
  }

  req <- pss(..., method = "DELETE", pss_domain = pss_domain)

  if (!is.null(data)) req <- httr2::req_body_json(req, data)
  if (!is.null(path)) req <- httr2::req_body_file(req, path, type)

  httr2::req_perform(req)
}



#' Send a request to upload files to BoF-PSS 3
#'
#' @param ... query parameters
#' @param file file to upload
#' @param pss_domain domain of BoF-PSS 3 API, default https://localhost:8080
#'
#' @return a response from BoF-PSS
#'
#' @examples
pss_upload <- function(..., path, pss_domain = "http://localhost:8080") {
  req <- pss(..., method = "POST", pss_domain = pss_domain) |>
    httr2::req_body_multipart(file = curl::form_file(path))

  httr2::req_perform(req)
}
