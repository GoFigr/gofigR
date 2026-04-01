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

# ---- Clean room: capture_function_source ----

test_that("capture_function_source extracts multi-statement body", {
  fn <- function(x, y) {
    z <- x + y
    z * 2
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("z <- x \\+ y", src))
  expect_true(grepl("z \\* 2", src))
  # Should NOT contain "function" or the outer braces as standalone lines
  expect_false(grepl("^function", src))
  expect_false(grepl("^\\{$", src))
})

test_that("capture_function_source extracts single-expression body", {
  fn <- function(x) x + 1
  src <- gofigR:::capture_function_source(fn)
  expect_equal(src, "x + 1")
})

test_that("capture_function_source handles empty body", {
  fn <- function() NULL
  src <- gofigR:::capture_function_source(fn)
  expect_equal(src, "NULL")
})

test_that("capture_function_source handles body with only one statement in braces", {
  fn <- function(x) {
    x + 1
  }
  src <- gofigR:::capture_function_source(fn)
  expect_equal(trimws(src), "x + 1")
  expect_false(grepl("\\{", src))
})

test_that("capture_function_source handles nested braces", {
  fn <- function(x) {
    if (x > 0) {
      x + 1
    } else {
      x - 1
    }
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("if \\(x > 0\\)", src))
  expect_true(grepl("x \\+ 1", src))
  expect_true(grepl("x - 1", src))
  # Should NOT start with the outer {
  expect_false(startsWith(trimws(src), "{"))
})

test_that("capture_function_source handles for loops", {
  fn <- function(n) {
    total <- 0
    for (i in 1:n) {
      total <- total + i
    }
    total
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("total <- 0", src))
  expect_true(grepl("for \\(i in 1:n\\)", src))
  expect_true(grepl("total$", src))
})

test_that("capture_function_source handles string literals with special chars", {
  fn <- function() {
    x <- "hello\nworld"
    y <- 'single "quoted"'
    paste(x, y)
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("hello", src))
  expect_true(grepl("paste\\(x, y\\)", src))
})

test_that("capture_function_source handles function definitions inside body", {
  fn <- function() {
    inner <- function(a, b) a + b
    inner(1, 2)
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("inner <- function\\(a, b\\)", src))
  expect_true(grepl("inner\\(1, 2\\)", src))
})

test_that("capture_function_source handles pipe operators", {
  # R's AST converts |> to nested calls, so deparse won't preserve pipe syntax.
  # We verify the semantics are preserved.
  fn <- function(data) {
    data |> subset(x > 0) |> nrow()
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("nrow", src))
  expect_true(grepl("subset", src))
})

test_that("capture_function_source handles long expressions that deparse to multiple lines", {
  fn <- function(data) {
    result <- list(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9, j = 10, k = 11)
    result
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("result <- list\\(", src))
  expect_true(grepl("result$", src))
})

test_that("capture_function_source handles tryCatch", {
  fn <- function(x) {
    tryCatch({
      log(x)
    }, error = function(e) {
      NA
    })
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("tryCatch", src))
  expect_true(grepl("log\\(x\\)", src))
  expect_true(grepl("error = function\\(e\\)", src))
})

test_that("capture_function_source handles assignments with complex RHS", {
  fn <- function() {
    x <- if (TRUE) 1 else 2
    x
  }
  src <- gofigR:::capture_function_source(fn)
  expect_true(grepl("x <- if \\(TRUE\\)", src))
})

test_that("capture_function_source strips param defaults (not included)", {
  fn <- function(bins = slider(20, min = 1, max = 100),
                 color = dropdown("blue", choices = c("red", "blue"))) {
    plot(1:bins, col = color)
  }
  src <- gofigR:::capture_function_source(fn)
  # Parameter defaults should NOT appear
  expect_false(grepl("slider", src))
  expect_false(grepl("dropdown", src))
  # Body should appear
  expect_true(grepl("plot\\(1:bins", src))
})

# ---- Clean room: parameter constructors ----

test_that("slider creates correct gf_param for integer", {
  p <- slider(20L, min = 1L, max = 100L, step = 5L)
  expect_s3_class(p, "gf_param")
  expect_equal(p$type, "integer")
  expect_equal(p$widget, "slider")
  expect_equal(p$default, 20L)
  expect_equal(p$min, 1L)
  expect_equal(p$max, 100L)
  expect_equal(p$step, 5L)
})

test_that("slider creates correct gf_param for numeric", {
  p <- slider(0.5, min = 0, max = 1, step = 0.1)
  expect_s3_class(p, "gf_param")
  expect_equal(p$type, "number")
  expect_equal(p$default, 0.5)
})

test_that("dropdown creates correct gf_param", {
  p <- dropdown("blue", choices = c("red", "blue", "green"))
  expect_s3_class(p, "gf_param")
  expect_equal(p$type, "string")
  expect_equal(p$widget, "dropdown")
  expect_equal(p$default, "blue")
  expect_equal(p$choices, c("red", "blue", "green"))
})

test_that("checkbox creates correct gf_param", {
  p <- checkbox(TRUE)
  expect_s3_class(p, "gf_param")
  expect_equal(p$type, "boolean")
  expect_equal(p$widget, "checkbox")
  expect_equal(p$default, TRUE)
})

test_that("text_input creates correct gf_param", {
  p <- text_input("hello")
  expect_s3_class(p, "gf_param")
  expect_equal(p$type, "string")
  expect_equal(p$widget, "text")
  expect_equal(p$default, "hello")
})

test_that("static infers type for data.frame", {
  p <- static(iris)
  expect_s3_class(p, "gf_param")
  expect_equal(p$type, "dataframe")
  expect_null(p$widget)
})

test_that("static infers type for numeric", {
  p <- static(42)
  expect_equal(p$type, "number")
})

test_that("static infers type for integer", {
  p <- static(1L)
  expect_equal(p$type, "integer")
})

test_that("static infers type for logical", {
  p <- static(FALSE)
  expect_equal(p$type, "boolean")
})

test_that("static infers type for character", {
  p <- static("hello")
  expect_equal(p$type, "string")
})

test_that("static infers type for unknown", {
  p <- static(list(a = 1))
  expect_equal(p$type, "none")
})

# ---- Clean room: manifest building ----

test_that("build_manifest produces correct structure", {
  descs <- list(
    bins = slider(20L, min = 5L, max = 50L),
    color = dropdown("blue", choices = c("red", "blue")),
    flag = checkbox(TRUE),
    title = text_input("test"),
    data = static(iris)
  )

  m <- gofigR:::build_manifest(descs, c("ggplot2"), list(), "my_func")

  expect_equal(m$language, jsonlite::unbox("r"))
  expect_equal(m$function_name, jsonlite::unbox("my_func"))

  # Check packages resolved
  expect_true("ggplot2" %in% names(m$packages))

  # Check parameter entries (values are jsonlite::unbox scalars, compare with ignore_attr)
  expect_equal(m$parameters$bins$type, jsonlite::unbox("integer"))
  expect_equal(m$parameters$bins$widget, jsonlite::unbox("slider"))
  expect_equal(m$parameters$color$widget, jsonlite::unbox("dropdown"))
  expect_equal(m$parameters$color$choices, c("red", "blue"))
  expect_equal(m$parameters$flag$type, jsonlite::unbox("boolean"))
  expect_equal(m$parameters$title$widget, jsonlite::unbox("text"))
  expect_equal(m$parameters$data$type, jsonlite::unbox("dataframe"))

  # dataframe params should not have a value field
  expect_null(m$parameters$data$value)
})

test_that("build_manifest handles NULL name", {
  descs <- list(x = slider(1, min = 0, max = 10))
  m <- gofigR:::build_manifest(descs, character(0), list(), NULL)
  # function_name should be unboxed NA (serializes to null in JSON)
  expect_true(is.na(m$function_name))
})

test_that("build_manifest handles named package list with versions", {
  descs <- list(x = static(1))
  m <- gofigR:::build_manifest(descs, list(base = "4.3.0"), list(), NULL)
  expect_equal(m$packages$base, jsonlite::unbox("4.3.0"))
})

test_that("manifest JSON roundtrips correctly", {
  descs <- list(
    n = slider(10L, min = 1L, max = 100L, step = 1L),
    label = text_input("test")
  )
  m <- gofigR:::build_manifest(descs, character(0), list(), "test_fn")
  json <- jsonlite::toJSON(m, auto_unbox = FALSE, null = "null", pretty = FALSE)
  parsed <- jsonlite::fromJSON(as.character(json))

  expect_equal(parsed$language, "r")
  expect_equal(parsed$function_name, "test_fn")
  expect_equal(parsed$parameters$n$type, "integer")
  expect_equal(parsed$parameters$n$widget, "slider")
  expect_equal(parsed$parameters$label$type, "string")
})
