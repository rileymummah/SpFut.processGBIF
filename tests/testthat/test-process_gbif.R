test_that("process_gbif only saves iNat data", {

  process_gbif(scientificName = "Eurycea bislineata",
               sp.code = "EBIS",
               keep = "iNat",
               user = "clanescher",
               pwd = "0xjFyIlxJoKrNh",
               email = "clanescher@gmail.com")

  expect_equal(file.exists('./.EBIS_iNat_PO.csv'), TRUE)

})

test_that("process_gbif only saves Museum data", {

  process_gbif(scientificName = "Eurycea bislineata",
               sp.code = "EBIS",
               keep = "museum",
               user = "clanescher",
               pwd = "0xjFyIlxJoKrNh",
               email = "clanescher@gmail.com")

  expect_equal(file.exists('./.EBIS_museum_PO.csv'), TRUE)

})
