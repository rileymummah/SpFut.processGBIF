test_that("output should be a list", {
  # Download raw and then clean it
  raw <- download_gbif(scientificName = "Eurycea bislineata",
                       user = "clanescher",
                       pwd = "0xjFyIlxJoKrNh",
                       email = "clanescher@gmail.com")

  expect_equal(class(raw), 'list')
})
