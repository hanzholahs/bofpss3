stop_if_no_project <- function(proj_name) {
  if (is.null(pss_search_proj(proj_name))) {
    stop(paste0("The project `", proj_name, "` does not exist."),
         call. = FALSE)
  }
}

stop_if_no_system <- function(proj_name, system_name) {
  if (is.null(pss_search_system(proj_name, system_name))) {
    stop(paste0("The system `", system_name, "` does not exist."),
         call. = FALSE)
  }
}


stop_if_no_system_dataset <- function(proj_name, system_dataset_name) {
  if (is.null(pss_search_sysdata(proj_name, system_dataset_name))) {
    stop(paste0("The dataset `", system_dataset_name, "` does not exist."),
         call. = FALSE)
  }
}
