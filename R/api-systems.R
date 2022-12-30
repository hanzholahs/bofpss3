pss_search_system <- function(proj_name, system_name = NULL) {
  stop_if_no_project(proj_name)

  resp    <- pss_get("projects", proj_name, "systems")
  systems <- parse_response(resp, "json")

  if (is.null(system_name)) {
    return( systems )
  }

  system_names <- systems |> purrr::map_chr("name")
  system_ids   <- systems |> purrr::map_chr("id")
  system_id    <- which(system_name == system_names)

  if (length(system_id) == 0) {
    return( NULL )
  }
  return( systems[[ system_id ]] )
}

pss_create_system <- function(proj_name, system_name) {
  stop_if_no_project(proj_name)

  if (!is.null(pss_search_system(proj_name, system_name))) {
    stop(paste0("The system `", system_name, "` is already exists."),
         call. = FALSE)
  }

  resp <- pss_post("projects",
                   proj_name,
                   "systems",
                   data = list(name = system_name))

  parse_response(resp, "json")
}

pss_delete_system <- function(proj_name, system_name) {
  stop_if_no_project(proj_name)

  system <- pss_search_system(proj_name, system_name)

  if (is.null(system) && proj_name != "all") {
    stop(paste0("The system `", system_name, "` does not exist."),
         call. = FALSE)
  }

  if (proj_name == "all") {
    message("You are deleting all systems")
    resp <- pss_delete("projects", proj_name, "systems", "all")
  } else {
    message(paste0("You are deleting system `", system$name, "`"))
    resp <- pss_delete("projects", proj_name, "systems", system$id)
  }

  invisible(resp)
}
