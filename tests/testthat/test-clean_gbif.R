# Download raw and then clean it
raw <- download_gbif(scientificName = "Anaxyrus macroscaphus",
                     user = "clanescher",
                     pwd = "0xjFyIlxJoKrNh",
                     email = "clanescher@gmail.com")

test_that("clean_GBIF creates a dataframe", {

  clean <- clean_gbif(raw = raw$dat,
                      startYear = 1980)

  expect_equal(class(clean), c('tbl_df','tbl','data.frame'))
})
