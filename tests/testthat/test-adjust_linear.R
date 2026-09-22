rlang::local_options(lifecycle_verbosity = "quiet")

test_that("step_adjust_linear basic behavior and drop options", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    z = c(5, 6, 7, 8, 9, 10),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec_remove <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "remove"
    ) |>
    prep(training = dat)

  baked_remove <- bake(rec_remove, new_data = dat)
  expect_false("batch" %in% names(baked_remove))
  expect_true("group" %in% names(baked_remove))
  expect_false(isTRUE(all.equal(baked_remove$y, dat$y)))

  rec_both <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "both"
    ) |>
    prep(training = dat)

  baked_both <- bake(rec_both, new_data = dat)
  expect_false("batch" %in% names(baked_both))
  expect_false("group" %in% names(baked_both))

  rec_none <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "none"
    ) |>
    prep(training = dat)

  baked_none <- bake(rec_none, new_data = dat)
  expect_true(all(c("batch", "group") %in% names(baked_none)))
})

test_that("step_adjust_linear can adjust multiple outcomes", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    z = c(20, 21, 22, 23, 24, 25),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec <- recipe(~., data = dat) |>
    step_adjust_linear(
      y,
      z,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "none"
    ) |>
    prep(training = dat)

  baked <- bake(rec, new_data = dat)
  expect_false(isTRUE(all.equal(baked$y, dat$y)))
  expect_false(isTRUE(all.equal(baked$z, dat$z)))
})

test_that("step_adjust_linear validates arguments", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16),
    batch = c(0, 0, 1, 1),
    group = factor(c("a", "a", "b", "b")),
    bad = as.Date("2020-01-01") + 0:3
  )

  expect_error(
    recipe(y ~ ., data = dat) |>
      step_adjust_linear(y) |>
      prep(training = dat),
    "remove_vars"
  )

  expect_error(
    recipe(y ~ ., data = dat) |>
      step_adjust_linear(
        y,
        remove_vars = vars(batch, group),
        keep_vars = vars(group)
      ) |>
      prep(training = dat),
    "disjoint"
  )

  expect_error(
    recipe(y ~ ., data = dat) |>
      step_adjust_linear(y, remove_vars = vars(bad)) |>
      prep(training = dat),
    "either factors or numeric"
  )
})

test_that("step_adjust_linear tidy works before and after prep", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec_untrained <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      id = "adj"
    )

  td_untrained <- tidy(rec_untrained, number = 1)
  expect_true(all(
    c("variables", "term", "type", "value", "id") %in% names(td_untrained)
  ))
  expect_true(all(td_untrained$id == "adj"))

  rec_trained <- prep(rec_untrained, training = dat)
  td_trained <- tidy(rec_trained, number = 1)
  expect_true(nrow(td_trained) > 0)
  expect_true(all(c("remove", "keep") %in% unique(td_trained$type)))
})

test_that("step_adjust_linear bake errors when required columns are missing", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "none"
    )

  rec_trained <- prep(rec, training = dat, verbose = FALSE)

  expect_error(
    bake(rec_trained, new_data = dplyr::select(dat, -batch)),
    "required"
  )
})

test_that("step_adjust_linear can use case weights", {
  skip_if_not_installed("hardhat")

  dat <- tibble::tibble(
    y = c(1, 2, 3, 6, 9, 30),
    batch = c(0, 0, 1, 1, 2, 2),
    wts = hardhat::importance_weights(c(1, 1, 1, 1, 1, 20))
  )

  rec_weighted <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(y, remove_vars = vars(batch), drop = "none") |>
    prep(training = dat)

  rec_unweighted <- recipe(y ~ ., data = dplyr::select(dat, -wts)) |>
    step_adjust_linear(y, remove_vars = vars(batch), drop = "none") |>
    prep(training = dplyr::select(dat, -wts))

  baked_weighted <- bake(rec_weighted, new_data = dplyr::select(dat, -wts))
  baked_unweighted <- bake(rec_unweighted, new_data = dplyr::select(dat, -wts))

  expect_false(isTRUE(all.equal(baked_weighted$y, baked_unweighted$y)))
})
