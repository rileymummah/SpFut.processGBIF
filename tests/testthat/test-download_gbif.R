test_that("output should be a list", {
  # Download raw and then clean it
  raw <- download_gbif(scientificName = "Anaxyrus macroscaphus",
                       startyear = 1990,
                       country = "US",
                       source = "all",
                       user = "clanescher",
                       pwd = "0xjFyIlxJoKrNh",
                       email = "clanescher@gmail.com")

  expect_equal(class(raw), 'list')
})
