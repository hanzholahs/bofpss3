#' List existing BoF-PSS 3 project(s)
#'
#' @param proj_name project name
#'
#' @return list of projects
#' @export
#'
#' @examples
pss_search_proj <- function(proj_name = NULL) {
  resp <- tryCatch(pss_get("projects", proj_name), error = function(e) NULL)

  parse_response(resp, "json")
}



#' Create a new BoF-PSS3 project
#'
#' @param proj_name project name
#'
#' @return a response with HTTP status code
#' @export
#'
#' @examples
pss_create_proj <- function(proj_name) {
  if (!is.null(pss_search_proj(proj_name))) {
    stop(paste0("The project `", proj_name, "` is already exists."),
         call. = FALSE)
  }

  resp <- pss_post("projects", data = list(name = proj_name))

  parse_response(resp, "json")
}



#' Delete a BoF-PSS 3project
#'
#' @param proj_name project name
#'
#' @return a response with HTTP status code
#' @export
#'
#' @examples
pss_delete_proj <- function(proj_name) {
  if (is.null(pss_search_proj(proj_name)) && proj_name != "all") {
    stop(paste0("The project `", proj_name, "` does not exist."),
         call. = FALSE)
  }

  if (proj_name == "all") {
    message("You are deleting all projects.")
  } else {
    message(paste0("You are deleting the `", proj_name, "` project."))
  }

  resp <- pss_delete("projects", proj_name)
  invisible(resp)
}
