test_that("assignGroupToTaxa() assigns a group variable to each taxa", {
  expect_equal(assignGroupToTaxa(ListOfTaxa = c("Pinus", "Betula", "Poaceae", "Picea", "Artemisia"),
                                 AssignedGroup = c("Trees", "Shrubs", "Grasses", "Trees", "Herbs")),
               c("Pinus" = "Trees", "Betula" = "Shrubs", "Poaceae" = "Grasses", "Picea" = "Trees", "Artemisia" = "Herbs"))

  expect_warning(assignGroupToTaxa(ListOfTaxa = c("Pinus", "Betula", "Poaceae"),
                                 AssignedGroup = c("Trees", "Shrubs", "Grasses", "Trees", "Herbs")))

  expect_equal(assignGroupToTaxa(ListOfTaxa = c("Pinus", "Betula", "Poaceae")),
               c("Pinus" = "Pinus", "Betula" = "Betula", "Poaceae" = "Poaceae"))
})
