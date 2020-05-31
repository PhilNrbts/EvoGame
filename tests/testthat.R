library(testthat)
library(EvoGame)

test_check("EvoGame")

context("evo_game functionality")

test_that("evo_game returns meta_data that always has two more rows than max of Agent_Detail (- Start, -0", {
  tick <- 200
  game <- evo_game(rowsField = 20, columnsField = 20, nFood = 50, nAgent = 20,
                   tick = tick, foodGeneratingType = "linear",
                   foodGeneratingFactor = 1)
  expect_equal(nrow(game$Meta_Data), max(as.numeric(game$Agent_Details[["t"]])[(!is.na(as.numeric(game$Agent_Details[["t"]])))]) + 2)
})
