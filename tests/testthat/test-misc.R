test_that("api dry run exists", {
  expect_true(exists("pss_dry_run"))

  skip_if(no_pss_api())

  printed <- capture.output(
    dry_run <- pss_dry_run("simulator", "info", body = list(hello = "world"))
  )

  expect_equal(printed[2], "Host: localhost:8080")
  expect_equal(printed[9], "{\"hello\":\"world\"}")

  expect_type(dry_run, "list")
  expect_equal(dry_run$path, "/simulator/info")
  expect_equal(dry_run$headers$`content-type`, "application/json")
})



test_that("able to check simulator info", {
  expect_true(exists("pss_info"))

  skip_if(no_pss_api())

  resp <- pss_info()
  expect_type(resp, "list")
  expect_type(resp$company, "character")
  expect_type(resp$applicationName, "character")
  expect_type(resp$version, "character")
  expect_type(resp$info, "character")
})
