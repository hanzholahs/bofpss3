test_that("function to create an api object works", {
  expect_true(exists("pss"))

  skip_if(no_pss_api())

  req <- pss()
  expect_equal(class(req), "httr2_request")
  expect_equal(req$url, "http://localhost:8080")
  expect_equal(req$method, "GET")

  req <- pss("path", method = "POST", pss_domain = "http://localhost:123")
  expect_equal(req$url, "http://localhost:123/path")
  expect_equal(req$method, "POST")
})


test_that("basic api call functions works", {
  expect_true(exists("pss_get"))
  expect_true(exists("pss_post"))
  expect_true(exists("pss_put"))
  expect_true(exists("pss_delete"))

  skip_if(no_pss_api())

  proj_name <- "sample_abcijkxyz432"

  resp1 <- pss_post("projects", data = list(name = proj_name))
  expect_type(resp1, "list")

  resp2 <- pss_get("projects")
  expect_type(resp2, "list")

  resp3 <- pss_put("projects", data = list(name = proj_name))
  expect_type(resp3, "list")

  resp4 <- pss_delete("projects", proj_name)
  expect_type(resp4, "list")
})
