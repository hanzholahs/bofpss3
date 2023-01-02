pss_search_sysdata <- function(proj_name, system_dataset_name = NULL) {
  stop_if_no_project(proj_name)

  resp <- pss_get("projects", proj_name, "systemdatasets")
  system_datasets <- parse_response(resp, "json")

  if (is.null(system_dataset_name)) {
    return( system_datasets )
  }

  system_dataset_names <- system_datasets |> purrr::map_chr("name")
  system_dataset_ids   <- system_datasets |> purrr::map_chr("id")
  system_dataset_id    <- which(system_dataset_name == system_dataset_names)

  if (length(system_dataset_id) == 0) {
    return( NULL )
  }
  return( system_datasets[[ system_id ]] )
}



pss_create_sysdata <- function(proj_name, system_dataset) {
  stop_if_no_project(proj_name)

  json         <- pss_to_request_body(system_dataset)
  dataset_name <- system_dataset$dataset_name
  dataset_id   <- system_dataset$dataset_id

  if (!is.null(pss_search_sysdata(proj_name, dataset_name))) {
    stop(paste0("The dataset `", system_dataset_name, "` is already exists."),
         call. = FALSE)
  }

  resp <- pss_post("projects", proj_name, "systems", data = json)

  parse_response(resp, "json")
}



pss_modify_sysdata <- function(proj_name, system_dataset) {
  stop_if_no_project(proj_name)

  json         <- pss_to_request_body(system_dataset)
  dataset_name <- system_dataset$dataset_name
  dataset_id   <- system_dataset$dataset_id

  if (!is.null(pss_search_sysdata(proj_name, dataset_name))) {
    stop(paste0("The dataset `", system_dataset_name, "` is already exists."),
         call. = FALSE)
  }

  resp <- pss_post("projects", proj_name, "systems", dataset_id, data = json)

  parse_response(resp, "json")
}



pss_delete_sysdata <- function(proj_name, system_dataset) {
  stop_if_no_project(proj_name)
  stop_if_no_system_dataset(proj_name, system_dataset_name)


}
