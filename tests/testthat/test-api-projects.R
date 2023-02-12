test_that("search function works", {
  expect_true(exists("pss_search_proj"))

  skip_if(no_pss_api())

  resp <- pss_search_proj()
  expect_true(is.null(resp) | is.list(resp))

  rand_proj_name <- paste0(letters[as.integer(runif(8, 1, 26))], collapse = "")
  expect_null(pss_search_proj(rand_proj_name))

  pss_post("projects", data = list(name = rand_proj_name))

  resp <- pss_search_proj(rand_proj_name)
  expect_type(resp, "list")
  expect_length(resp, 14)
  expect_equal(resp$name, rand_proj_name)

  pss_delete("projects", rand_proj_name)
})



test_that("create function works", {
  expect_true(exists("pss_create_proj"))

  skip_if(no_pss_api())

  rand_proj_name <- paste0(letters[as.integer(runif(8, 1, 26))], collapse = "")

  resp <- pss_create_proj(rand_proj_name)
  expect_type(resp, "list")
  expect_length(resp, 14)
  expect_equal(resp$name, rand_proj_name)

  resp <- pss_search_proj(rand_proj_name)
  expect_type(resp, "list")
  expect_length(resp, 14)
  expect_equal(resp$name, rand_proj_name)

  pss_delete("projects", rand_proj_name)
})



test_that("delete function works", {
  expect_true(exists("pss_delete_proj"))

  skip_if(no_pss_api())

  rand_proj_name <- paste0(letters[as.integer(runif(8, 1, 26))], collapse = "")

  pss_create_proj(rand_proj_name)

  project_exists <- pss_search_proj(rand_proj_name)

  pss_delete_proj(rand_proj_name)

  project_does_not_exists <- pss_search_proj(rand_proj_name)

  expect_type(project_exists, "list")
  expect_null(project_does_not_exists)
})
