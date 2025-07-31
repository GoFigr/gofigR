get_image_embedding <- function(model, image_path) {
  # Define the image pre-processing steps
  transform <- function(img) {
    img |>
      magick::image_background("white") |>
      magick::image_flatten() |>
      torchvision::transform_to_tensor() |>
      torchvision::transform_resize(size = c(224, 224)) |>
      torchvision::transform_normalize(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225)) |>
      torch::torch_unsqueeze(1)
  }

  # Load and process the image
  image <- magick::image_read(image_path)
  image_tensor <- transform(image)

  # Generate the embedding (disabling gradient calculation for efficiency)
  torch::with_no_grad({
    embedding_tensor <- model(image_tensor)
  })

  # Return the embedding as a standard R vector
  as.numeric(embedding_tensor)
}


cosine_similarity <- function(x, y) {
  # The dot product of vectors x and y
  dot_product <- x %*% y

  # The product of the vector magnitudes
  magnitude_product <- sqrt(sum(x^2)) * sqrt(sum(y^2))

  # Return the final similarity value
  return(dot_product / magnitude_product)
}


image_similarity_score <- function(model, path1, path2) {
  vec1 <- get_image_embedding(model, path1)
  vec2 <- get_image_embedding(model, path2)

  return(cosine_similarity(vec1, vec2))
}


cleanup <- function(analysis_name) {
  gf <- gofigr_client()
  sapply(gofigR::list_analyses(gf), function(ana) {
    if(ana$name == analysis_name) {
      delete_analysis(gf, ana$api_id)
      message(paste0("Deleted ", ana$name))
    }
  })
  return()
}

download_images <- function(analysis_name, out_dir) {
  gf <- gofigr_client()
  ana <- purrr::detect(list_analyses(gf), function(ana) {ana$name == analysis_name})
  if(is.null(ana)) {
    stop(paste0("Test analysis not found: ", analysis_name))
  }

  ana <- get_analysis(gf, ana$api_id) # fetch full information
  images <- list()

  sapply(ana$figures, function(fig) {
    fig <- get_figure(gf, fig$api_id)

    sapply(tail(fig$revisions, n=1), function(rev) {
      rev <- get_revision(gf, rev$api_id) # fetch data listing

      sapply(rev$data, function(datum) {
        if(datum$type == "image" && !datum$metadata$is_watermarked && tolower(datum$metadata$format) == "png") {
          datum <- get_data(gf, datum$api_id) # fetch data content

          contents <- base64enc::base64decode(datum$data)
          img_path <- file.path(out_dir, paste0(fig$name, ".png"))
          writeBin(contents, img_path)
          images <<- c(images, img_path)
        }
      })
    })
  })
  return(images)
}


check_revisions <- function(analysis_name) {
  gf <- gofigr_client()
  ana <- purrr::detect(list_analyses(gf), function(ana) {ana$name == analysis_name})
  if(is.null(ana)) {
    stop("Test analysis not found")
  }

  ana <- get_analysis(gf, ana$api_id) # fetch full information
  testthat::expect_gte(length(ana$figures), 2)

  sapply(ana$figures, function(fig) {
    fig <- get_figure(gf, fig$api_id)

    testthat::expect_gte(length(fig$revisions), 1)

    sapply(fig$revisions, function(rev) {
      rev <- get_revision(gf, rev$api_id) # fetch data listing

      testthat::expect_gte(length(rev$data), 1)

      dtypes <- unique(lapply(rev$data, function(datum) {datum$type}))
      testthat::expect_contains(dtypes, c("image", "file", "code", "text"))

      lapply(rev$data, function(datum) {
        datum <- get_data(gf, datum$api_id)
        testthat::expect_gte(str_length(datum$data), 10)
        testthat::expect_equal(length(base64enc::base64decode(datum$data)), datum$size_bytes)
      })

      testthat::expect_equal(length(rev$assets), 1)
      lapply(rev$assets, function(asset_link) {
        testthat::expect_equal(asset_link$figure_revision, rev$api_id)
        asset_rev <- get_asset_revision(gf, asset_link$asset_revision)
        asset <- get_asset(gf, asset_rev$asset)

        testthat::expect_equal(asset$name, "test_data.txt")

        sapply(asset_rev$data, function(datum) {
          datum <- get_data(gf, datum$api_id)
          contents <- rawToChar(base64enc::base64decode(datum$data))
          testthat::expect_equal(trimws(contents), "GoFigr!")
        })
      })
    })
  })
}


compare_images <- function(model, reference_path, actual_path) {
  list_images <- function(dir) {
    sort(list.files(dir, pattern=".*\\.png$"))
  }

  ref_images <- list_images(reference_path)
  actual_images <- list_images(actual_path)

  common <- intersect(ref_images, actual_images)
  diff <- c(setdiff(ref_images, actual_images), setdiff(actual_images, ref_images))

  if(length(diff) > 0) {
    testthat::fail(diff)
  }
  testthat::expect_gte(length(common), 2)

  lapply(common, function(name) {
    score <- image_similarity_score(model,
                                    file.path(reference_path, name),
                                    file.path(actual_path, name))
    if(score > 0.90) {
      testthat::succeed()
    } else {
      testthat::fail(paste0("Image comparison failed for ", name, ". Distance: ", dist,
                            ": \n",
                            "  - Reference: ", file.path(reference_path, name),
                            "\n  - Actual: ", file.path(actual_path, name)))
    }
  })
}


compare_html_images <- function(model, reference_path, html_path, tmp_dir) {
  list_images <- function(dir) {
    sort(list.files(dir, pattern=".*\\.png$"))
  }

  ref_images <- list_images(reference_path)
  html_doc <- rvest::read_html(html_path)
  image_nodes <- rvest::html_elements(html_doc, "img")
  image_sources <- rvest::html_attr(image_nodes, "src")

  # Grab all images out of the HTML and write them to the temporary dir
  img_idx <- 1
  lapply(image_sources, function(src_attribute) {
    base64_data <- str_split(src_attribute, ",", simplify = TRUE)[2]
    decoded_data <- base64enc::base64decode(base64_data)
    img_path <- file.path(tmp_dir, paste0("image_", img_idx, ".png"))
    writeBin(decoded_data, img_path)

    # Crop out the watermark
    img <- magick::image_read(img_path)
    img %>% magick::image_crop(paste0(magick::image_info(.)$width,
                                      "x",
                                      magick::image_info(.)$height - 200, "+0+0")) %>%
    magick::image_write(path = img_path)
    magick::image_destroy(img)

    img_idx <<- img_idx + 1
  })

  actual_images <- list_images(tmp_dir)

  testthat::expect_gte(length(actual_images), length(ref_images))

  # Generate a similarity matrix. Make sure at least one image
  # in the HTML report is sufficiently similar to the reference.
  lapply(ref_images, function(ref_name) {
    sims <- unlist(lapply(actual_images, function(act_name) {
      return(image_similarity_score(model,
                                    file.path(reference_path, ref_name),
                                    file.path(tmp_dir, act_name)))
    }))


    testthat::expect_gte(max(sims), 0.90)
    testthat::expect_gte(sd(sims), 0.01) # make sure there's some variability
  })
  return(NULL)
}


get_model <- function() {
  # Load the pre-trained ResNet-18 model
  model <- torchvision::model_resnet18(pretrained = TRUE)

  # Modify the model to output the 512-dimension feature vector
  model$fc <- torch::nn_identity()

  # Set the model to evaluation mode
  model$eval()

  return(model)
}


test_that("We correctly capture plots from knitr", {
  skip_on_cran()

  model <- get_model()

  lapply(c("html", "pdf"), function(fmt) {
    analysis_name <- paste0("Markdown test ", uuid::UUIDgenerate())

    withr::defer({cleanup(analysis_name)})

    tmp_dir <- withr::local_tempdir()

    # 1. Define paths
    input_rmd <- test_path("testdata", "markdown.Rmd")
    testthat::expect_true(file.exists(input_rmd))

    # Output files in a temporary directory
    output_report <- tempfile(fileext = paste0(".", fmt))

    # 2. Render to HTML and check for success
    testthat::expect_no_error(
      rmarkdown::render(
        input = input_rmd,
        output_format = paste0(fmt, "_document"),
        output_file = output_report,
        quiet = TRUE,
        params = list(analysis_name = analysis_name)
      )
    )
    # Assert that the report was created
    testthat::expect_true(file.exists(output_report))

    download_images(analysis_name, tmp_dir)

    check_revisions(analysis_name)
    compare_images(model,
                   test_path("testdata", "expected_images", "markdown"),
                   tmp_dir)

    if(fmt == "html") {
      compare_html_images(model,
                          test_path("testdata", "expected_images", "markdown"),
                          output_report, withr::local_tempdir())
    }
  })
})


replace_in_file <- function(filepath, old_string, new_string) {
  # Read the content of the file
  content <- readLines(filepath)

  # Replace all occurrences of the old string with the new string
  modified_content <- gsub(old_string, new_string, content, fixed = TRUE)

  # Write the modified content back to the file
  writeLines(modified_content, filepath)

  # Optionally, return a message
  message(paste("Replaced", old_string, "with", new_string, "in", filepath))
}


test_that("We correctly capture plots from scripts", {
  skip_on_cran()

  model <- get_model()

  analysis_name <- paste0("Script test ", uuid::UUIDgenerate())

  withr::defer({cleanup(analysis_name)})

  tmp_dir <- withr::local_tempdir()

  input_rmd <- test_path("testdata", "markdown.Rmd")
  script_path <- test_path(tmp_dir, "script.R")
  testthat::expect_true(file.exists(input_rmd))

  # Convert to a plain ol' R script
  knitr::purl(input_rmd, output = script_path)
  testthat::expect_true(file.exists(script_path))

  rscript_path <- file.path(R.home("bin"), "Rscript")
  testthat::expect_true(file.exists(rscript_path))

  # Gross but knitr::purl doesn't support params
  replace_in_file(script_path, "Automated markdown test", analysis_name)

  system2(rscript_path, args = script_path)

  download_images(analysis_name, tmp_dir)
  check_revisions(analysis_name)
  compare_images(model,
                 test_path("testdata", "expected_images", "markdown"),
                 tmp_dir)
})

