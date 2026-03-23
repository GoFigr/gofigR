# Unit tests that do not require GoFigr credentials

test_that("base62_encode handles edge cases", {
  expect_equal(base62_encode(0), "0")
  expect_equal(base62_encode(1), "1")
  expect_equal(base62_encode(9), "9")
  expect_equal(base62_encode(10), "a")
  expect_equal(base62_encode(35), "z")
  expect_equal(base62_encode(36), "A")
  expect_equal(base62_encode(61), "Z")
  expect_error(base62_encode(-1), "Cannot encode negative numbers")
})

test_that("base62_encode handles multi-character values", {
  expect_equal(base62_encode(62), "10")
  expect_equal(base62_encode(63), "11")
  expect_equal(base62_encode(124), "20")
  expect_equal(base62_encode(3844), "100")  # 62^2
})

test_that("make_short_id combines prefix and encoded index", {
  expect_equal(make_short_id("abcd1234", 0), "abcd12340")
  expect_equal(make_short_id("abcd1234", 62), "abcd123410")
  expect_equal(make_short_id("XXXXXXXX", 999), "XXXXXXXXg7")
})

test_that("get_revision_url uses short_id when available", {
  rev_with_short <- local({
    api_id <- "full-uuid-here"
    short_id <- "abc12340"
    environment()
  })
  url <- get_revision_url(rev_with_short)
  expect_true(grepl("abc12340$", url))
  expect_false(grepl("full-uuid-here", url))
})

test_that("get_revision_url falls back to api_id without short_id", {
  rev_no_short <- local({
    api_id <- "full-uuid-here"
    environment()
  })
  url <- get_revision_url(rev_no_short)
  expect_true(grepl("full-uuid-here$", url))
})

test_that("get_revision_url returns NULL for NULL revision", {
  expect_null(get_revision_url(NULL))
})

test_that("next_short_id returns NULL without prefix", {
  opts <- local({
    short_id_prefix <- NULL
    short_id_counter <- 0L
    environment()
  })
  expect_null(next_short_id(opts))
})

test_that("next_short_id generates sequential IDs and increments counter", {
  opts <- local({
    short_id_prefix <- "testpfx0"
    short_id_counter <- 0L
    environment()
  })
  expect_equal(next_short_id(opts), "testpfx00")
  expect_equal(opts$short_id_counter, 1L)
  expect_equal(next_short_id(opts), "testpfx01")
  expect_equal(opts$short_id_counter, 2L)
})

test_that("annotate_git returns valid structure in a git repo", {
  # This test suite runs inside a git repo
  result <- gofigR:::annotate_git()
  if (!is.null(result)) {
    expect_type(result, "list")
    expect_named(result, c("branch", "hash", "remote_url", "commit_link"),
                 ignore.order = TRUE)
    expect_true(nchar(result$hash) == 40)  # full SHA
    expect_true(nchar(result$branch) > 0)
  }
})

test_that("annotate_system returns a non-empty string", {
  result <- gofigR:::annotate_system()
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("annotate_packages returns a non-empty string", {
  result <- gofigR:::annotate_packages()
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  # Should contain at least one package==version pair
  expect_true(grepl("==", result))
})

test_that("annotate_packages reads renv.lock when present", {
  skip_if_not_installed("withr")
  tmp <- withr::local_tempdir()
  writeLines('{"R":{"Version":"4.3.0"}}', file.path(tmp, "renv.lock"))
  withr::local_dir(tmp)
  result <- gofigR:::annotate_packages()
  expect_true(grepl("4.3.0", result))
})

test_that("annotate_history returns NULL in non-interactive context", {
  # savehistory() fails outside interactive sessions
  result <- gofigR:::annotate_history()
  expect_null(result)
})
