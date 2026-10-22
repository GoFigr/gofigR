# Jupyter (IRkernel) integration: hooks and rich output. No kernel needed --
# the Executor is stood in for by a plain environment with the two methods.

fake_executor <- function() {
  env <- new.env()
  env$calls <- list()
  env$send_plot <- function(plotobj) env$calls <- append(env$calls, list(list(what = "send_plot", obj = plotobj)))
  env$handle_value <- function(obj, visible) env$calls <- append(env$calls, list(list(what = "handle_value", obj = obj, visible = visible)))
  env
}

with_jupyter_options <- function(auto_publish, code) {
  old <- get_options()
  on.exit({
    set_options(old)
    jupyter_state$hooks_installed <- FALSE
    jupyter_state$publishing <- FALSE
    intercept_off()
  })
  set_options(local({
    auto_publish <- auto_publish
    environment()
  }))
  intercept_on()
  jupyter_state$hooks_installed <- FALSE
  force(code)
}

fake_recordedplot <- function() {
  structure(list(), class = "recordedplot", .irkernel_width = 5, .irkernel_height = 4, .irkernel_ppi = 96)
}

test_that("install_jupyter_hooks publishes visible plot objects and defers the rest", {
  with_jupyter_options(TRUE, {
    published <- list()
    local_mocked_bindings(publish = function(plot_obj, ...) {
      published <<- append(published, list(list(obj = plot_obj, args = list(...))))
      invisible(NULL)
    })
    ex <- fake_executor()
    expect_true(install_jupyter_hooks(ex))

    p <- ggplot2::ggplot()
    ex$handle_value(p, visible = TRUE)
    expect_length(published, 1)
    expect_identical(published[[1]]$obj, p)
    expect_equal(published[[1]]$args$units, "in")
    expect_length(ex$calls, 0)

    # Invisible values and non-plots go to the kernel's own handler.
    ex$handle_value(p, visible = FALSE)
    ex$handle_value(data.frame(x = 1), visible = TRUE)
    expect_length(published, 1)
    expect_equal(sapply(ex$calls, `[[`, "what"), c("handle_value", "handle_value"))
  })
})

test_that("install_jupyter_hooks publishes completed base graphics at the kernel's size", {
  with_jupyter_options(TRUE, {
    published <- list()
    local_mocked_bindings(
      publish = function(plot_obj, ...) {
        published <<- append(published, list(list(obj = plot_obj, args = list(...))))
        invisible(NULL)
      }
    )
    local_mocked_bindings(as.ggplot = function(x) structure(list(replayed = x), class = "ggplot"), .package = "ggplotify")
    ex <- fake_executor()
    install_jupyter_hooks(ex)

    ex$send_plot(fake_recordedplot())
    expect_length(published, 1)
    expect_s3_class(published[[1]]$obj, "ggplot")
    expect_equal(published[[1]]$args$width, 5)
    expect_equal(published[[1]]$args$height, 4)
    expect_equal(published[[1]]$args$dpi, 96)
    expect_false(published[[1]]$args$base_convert)
    expect_length(ex$calls, 0)
  })
})

test_that("hooks fall through to the kernel when auto-publish is off or publish fails", {
  with_jupyter_options(FALSE, {
    local_mocked_bindings(publish = function(...) stop("should not be called"))
    ex <- fake_executor()
    install_jupyter_hooks(ex)
    ex$handle_value(ggplot2::ggplot(), visible = TRUE)
    ex$send_plot(fake_recordedplot())
    expect_equal(sapply(ex$calls, `[[`, "what"), c("handle_value", "send_plot"))
  })

  with_jupyter_options(TRUE, {
    local_mocked_bindings(publish = function(...) stop("server down"))
    ex <- fake_executor()
    install_jupyter_hooks(ex)
    expect_warning(ex$handle_value(ggplot2::ggplot(), visible = TRUE), "server down")
    expect_equal(sapply(ex$calls, `[[`, "what"), "handle_value")
  })
})

test_that("hooks are not re-entered while a figure is being published", {
  with_jupyter_options(TRUE, {
    ex <- fake_executor()
    install_jupyter_hooks(ex)
    local_mocked_bindings(publish = function(plot_obj, ...) {
      # publish() drawing on the device during a publish must not publish again
      ex$send_plot(fake_recordedplot())
      invisible(NULL)
    })
    ex$handle_value(ggplot2::ggplot(), visible = TRUE)
    expect_equal(sapply(ex$calls, `[[`, "what"), "send_plot")
  })
})

test_that("install_jupyter_hooks refuses an executor without the expected methods", {
  with_jupyter_options(TRUE, {
    expect_warning(res <- install_jupyter_hooks(new.env()), "unsupported IRkernel")
    expect_false(res)
    expect_warning(res <- install_jupyter_hooks(NULL), "inside a Jupyter cell")
    expect_false(res)
  })
})

fake_revision <- function() {
  list(api_id = "rev-api-id", short_id = "abcd12340", figure = list(api_id = "fig-api-id"),
       created_on = "2026-10-22T10:00:00Z", size_bytes = 1234L)
}

temp_png <- function() {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 40, height = 30)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  path
}

test_that("figure_payload follows the shared mime contract", {
  raw <- temp_png()
  on.exit(unlink(raw))
  payload <- figure_payload(fake_revision(), "Volcano", raw, watermark_enabled = TRUE, image_formats = c("eps"))
  expect_equal(payload$version, 1L)
  expect_equal(payload$revision$api_id, "rev-api-id")
  expect_true(grepl("abcd12340$", payload$revision$url))
  expect_equal(payload$figure, list(api_id = "fig-api-id", name = "Volcano"))
  expect_equal(payload$watermark$url, payload$revision$url)
  expect_equal(payload$image$width, 40L)
  expect_equal(payload$image$height, 30L)
  expect_true(payload$image$embedded)
  expect_equal(base64enc::base64decode(payload$image$b64), read_raw_bytes(raw))
  expect_equal(sapply(payload$formats, `[[`, "format"), c("png", "png", "eps"))
  expect_equal(sapply(payload$formats, `[[`, "watermarked"), c(FALSE, TRUE, FALSE))

  json <- jsonlite::fromJSON(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))
  expect_null(json$interactive)
  expect_equal(json$watermark$margin_px, c(10, 10))

  plain <- figure_payload(fake_revision(), "Volcano", raw, watermark_enabled = FALSE)
  expect_null(plain$watermark)
})

test_that("jupyter_display_revision sends one bundle with png, html and the GoFigr mime", {
  raw <- temp_png()
  wm <- temp_png()
  on.exit(unlink(c(raw, wm)))
  sent <- NULL
  withr::local_options(jupyter.base_display_func = function(data, metadata = NULL) sent <<- data)

  expect_true(jupyter_display_revision(fake_revision(), "Volcano", raw, wm))
  expect_setequal(names(sent), c("image/png", "text/html", "text/plain", GOFIGR_FIGURE_MIME))
  expect_equal(sent[["image/png"]], base64enc::base64encode(read_raw_bytes(wm)))
  expect_true(grepl("View on GoFigr", sent[["text/html"]]))
  expect_true(grepl("<b>Volcano</b>", sent[["text/html"]]))
  payload <- jsonlite::fromJSON(sent[[GOFIGR_FIGURE_MIME]])
  expect_equal(payload$image$b64, base64enc::base64encode(read_raw_bytes(raw)))
  expect_equal(payload$figure$name, "Volcano")

  # Nothing to show without a frontend to send to.
  withr::local_options(jupyter.base_display_func = NULL)
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_false(jupyter_display_revision(fake_revision(), "Volcano", raw, NULL))
})

test_that("startup_widget_html links the workspace and analysis", {
  html <- startup_widget_html(list(api_id = "ws1", name = "Lab <A>"), list(api_id = "an1", name = "Study"), TRUE)
  expect_true(grepl("/workspace/ws1", html))
  expect_true(grepl("/analysis/an1", html))
  expect_true(grepl("Lab &lt;A&gt;", html))
  expect_true(grepl("Auto-publish: TRUE", html))
})
