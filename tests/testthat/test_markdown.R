library(testthat)
library(withr)
library(magick)
library(dtt)
library(rvest)
library(stringr)


# Function to generate the Perceptual Hash (p-hash)
image_phash <- function(image_path, hash_size = 8) {
  # 1. Read and resize the image to a fixed size (e.g., 32x32)
  img <- image_read(image_path) |> image_background("white") |> image_flatten()

  img_resized <- image_resize(img, paste0(hash_size * 4, "x", hash_size * 4, "!")) |>
    image_convert(colorspace = "gray")

  # 2. Get pixel values and compute the 2D DCT
  pixels <- as.matrix(image_data(img_resized, "gray")[1, , ])
  storage.mode(pixels) <- "numeric"

  image_destroy(img_resized)
  image_destroy(img)

  # CORRECTED LINE: Use dtt::dct() instead of dct_2d()
  dct_matrix <- dtt::dct(pixels)

  # 3. Reduce the DCT to the top-left corner (hash_size x hash_size)
  dct_subset <- dct_matrix[1:hash_size, 1:hash_size]

  # 4. Compute the median of the DCT values (excluding the first term)
  median_val <- median(dct_subset[-1])

  # 5. Create the hash: 1 if the DCT value is > median, otherwise 0
  phash <- as.vector(t(dct_subset > median_val))
  return(phash)
}


# Function to calculate the Hamming distance between two hashes
hamming_distance <- function(hash1, hash2) {
  sum(hash1 != hash2)
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
    stop("Test analysis not found")
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
  expect_gte(length(ana$figures), 2)

  sapply(ana$figures, function(fig) {
    fig <- get_figure(gf, fig$api_id)

    expect_gte(length(fig$revisions), 1)

    sapply(fig$revisions, function(rev) {
      rev <- get_revision(gf, rev$api_id) # fetch data listing

      expect_gte(length(rev$data), 1)

      dtypes <- unique(lapply(rev$data, function(datum) {datum$type}))
      expect_contains(dtypes, c("image", "file", "code", "text"))

      lapply(rev$data, function(datum) {
        datum <- get_data(gf, datum$api_id)
        expect_gte(str_length(datum$data), 10)
        expect_equal(length(base64enc::base64decode(datum$data)), datum$size_bytes)
      })
    })
  })
}


compare_images <- function(reference_path, actual_path) {
  list_images <- function(dir) {
    sort(list.files(dir, pattern=".*\\.png$"))
  }

  ref_images <- list_images(reference_path)
  actual_images <- list_images(actual_path)

  common <- intersect(ref_images, actual_images)
  diff <- c(setdiff(ref_images, actual_images), setdiff(actual_images, ref_images))

  expect_length(diff, 0)
  expect_gte(length(common), 2)

  lapply(common, function(name) {
    ref_hash <- image_phash(file.path(reference_path, name))
    actual_hash <- image_phash(file.path(actual_path, name))
    dist <- hamming_distance(ref_hash, actual_hash)
    if(dist < 5) {
      testthat::succeed()
    } else {
      file.copy(file.path(reference_path, name), gsub("\\.png", "_ref.png", name))
      file.copy(file.path(actual_path, name), gsub("\\.png", "_act.png", name))
      testthat::fail(paste0("Image comparison failed for ", name, ". Distance: ", dist))
    }
  })
}


compare_html_images <- function(reference_path, html_path, tmp_dir) {
  list_images <- function(dir) {
    sort(list.files(dir, pattern=".*\\.png$"))
  }

  ref_images <- list_images(reference_path)
  html_doc <- read_html(html_path)
  image_nodes <- html_elements(html_doc, "img")
  image_sources <- html_attr(image_nodes, "src")

  # Grab all images out of the HTML and write them to the temporary dir
  img_idx <- 1
  lapply(image_sources, function(src_attribute) {
    base64_data <- str_split(src_attribute, ",", simplify = TRUE)[2]
    decoded_data <- base64enc::base64decode(base64_data)
    img_path <- file.path(tmp_dir, paste0("image_", img_idx, ".png"))
    writeBin(decoded_data, img_path)

    # Crop out the watermark
    img <- image_read(img_path)
    img %>% image_crop(paste0(image_info(.)$width, "x", image_info(.)$height - 200, "+0+0")) %>%
    image_write(path = img_path)
    image_destroy(img)

    img_idx <<- img_idx + 1
  })

  actual_images <- list_images(tmp_dir)

  expect_gte(length(actual_images), length(ref_images))

  # Generate a similarity matrix. Make sure at least one image
  # in the HTML report is sufficiently similar to the reference.
  lapply(ref_images, function(ref_name) {
    sims <- unlist(lapply(actual_images, function(act_name) {
      ref_hash <- image_phash(file.path(reference_path, ref_name))
      actual_hash <- image_phash(file.path(tmp_dir, act_name))
      return(hamming_distance(ref_hash, actual_hash))
    }))

    if(grepl("lattice", ref_name, ignore.case=TRUE)) {
      # phash works really poorly on lattice even though the images
      # are visually almost identical. TODO for later.
      expect_lt(min(sims), 30)
      expect_gte(max(sims), 40) # make sure there are poor matches, too
    } else {
      expect_lt(min(sims), 10)
      expect_gt(max(sims), 20) # make sure there are poor matches, too
    }
  })
  return(NULL)
}


test_that("We correctly capture plots from knitr", {
  skip_on_cran()

  lapply(c("html", "pdf"), function(fmt) {
    analysis_name <- paste0("Markdown test ", uuid::UUIDgenerate())

    defer({cleanup(analysis_name)})

    tmp_dir <- withr::local_tempdir()

    # 1. Define paths
    input_rmd <- test_path("testdata", "markdown.Rmd")
    expect_true(file.exists(input_rmd))

    # Output files in a temporary directory
    output_report <- tempfile(fileext = paste0(".", fmt))

    # 2. Render to HTML and check for success
    expect_no_error(
      rmarkdown::render(
        input = input_rmd,
        output_format = paste0(fmt, "_document"),
        output_file = output_report,
        quiet = TRUE,
        params = list(analysis_name = analysis_name)
      )
    )
    # Assert that the report was created
    expect_true(file.exists(output_report))

    download_images(analysis_name, tmp_dir)

    check_revisions(analysis_name)
    compare_images(test_path("testdata", "expected_images", "markdown"),
                   tmp_dir)

    if(fmt == "html") {
      compare_html_images(test_path("testdata", "expected_images", "markdown"),
                          output_report, withr::local_tempdir())
    }
  })
})
