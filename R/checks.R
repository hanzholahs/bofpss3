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

stop_if_no_algorithm <- function(algorithm_name, algorithm_type) {
  algorithm_list <- pss_get("algorithm", algorithm_type) |>
    parse_response("json") |>
    purrr::map_chr("algoId")

  if (!algorithm_name %in% algorithm_list) {
    stop(paste0("The algorithm `", algorithm_name, "` with type of `",
                algorithm_type, "` does not exist."),
         call. = FALSE)
  }
}

stop_if_no_any_algorithms <- function(algorithm_list) {
  for (algorithm in algorithm_list) {
    stop_if_no_algorithm(algorithm$algorithmName, algorithm$type)
  }
}
