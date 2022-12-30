test_that("system search function works", {
  expect_true(exists("pss_search_system"))

  skip_if(no_pss_api())

  rand_proj_name <- paste0(letters[as.integer(runif(8, 1, 26))], collapse = "")
  rand_sys_name  <- paste0(letters[as.integer(runif(4, 1, 26))], collapse = "")

  pss_create_proj(rand_proj_name)

  resp <- pss_search_system(rand_proj_name)
  expect_type(resp, "list")
  expect_length(resp, 0)

  pss_post("projects", rand_proj_name, "systems",
           data = list(name = rand_sys_name))

  resp <- pss_search_system(rand_proj_name)
  expect_type(resp, "list")
  expect_length(resp, 1)
  expect_length(resp[[1]], 5)
  expect_equal(resp[[1]]$name, rand_sys_name)

  system_id <- resp[[1]]$id

  pss_delete("projects", rand_proj_name, "systems", system_id)
  pss_delete_proj(rand_proj_name)
})



test_that("system create function works", {
  expect_true(exists("pss_create_proj"))

  skip_if(no_pss_api())

  rand_proj_name <- paste0(letters[as.integer(runif(8, 1, 26))], collapse = "")
  rand_sys_name  <- paste0(letters[as.integer(runif(4, 1, 26))], collapse = "")

  pss_create_proj(rand_proj_name)

  resp <- pss_create_system(rand_proj_name, rand_sys_name)
  expect_type(resp, "list")
  expect_length(resp, 5)
  expect_equal(resp$name, rand_sys_name)

  system_id <- resp$id

  resp <- pss_search_system(rand_proj_name, rand_sys_name)
  expect_type(resp, "list")
  expect_length(resp, 5)
  expect_equal(resp$id, system_id)
  expect_equal(resp$name, rand_sys_name)

  pss_delete("projects", rand_proj_name, "systems", system_id)
  pss_delete_proj(rand_proj_name)
})



test_that("system delete function works", {
  expect_true(exists("pss_create_proj"))

  skip_if(no_pss_api())

  rand_proj_name <- paste0(letters[as.integer(runif(8, 1, 26))], collapse = "")
  rand_sys_name  <- paste0(letters[as.integer(runif(4, 1, 26))], collapse = "")

  pss_create_proj(rand_proj_name)
  pss_create_system(rand_proj_name, rand_sys_name)

  system_exists <- pss_search_system(rand_proj_name, rand_sys_name)

  pss_delete_system(rand_proj_name, rand_sys_name)

  system_does_not_exists <- pss_search_system(rand_proj_name, rand_sys_name)

  expect_type(system_exists, "list")
  expect_null(system_does_not_exists)
})
