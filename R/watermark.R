#' Stacks images vertically, centering them horizontally.
#'
#' @param images vector of images to stack
#'
#' @return composite image
stack_vertically <- function(images) {
  images <- as.list(images)
  infos <- lapply(images, magick::image_info)

  total_width <- do.call(max, lapply(infos, function(x) {x$width}))
  total_height <- do.call(sum, lapply(infos, function(x) {x$height}))

  res <- magick::image_blank(width=total_width, height=total_height, color="#fff")

  y_offset <- 0
  for(img in images) {
    img_info <- magick::image_info(img)
    xpos <- (total_width - img_info$width) / 2
    res <- magick::image_composite(res, img, offset=magick::geometry_point(xpos, y_offset), operator="Copy")

    y_offset <- y_offset + img_info$height
  }

  return(res)
}

#' Stacks images horizontally, centering them vertically.
#'
#' @param images vector of images to stack
#'
#' @return composite image
stack_horizontally <- function(images) {
  images <- as.list(images)
  infos <- lapply(images, magick::image_info)

  total_width <- do.call(sum, lapply(infos, function(x) {x$width}))
  total_height <- do.call(max, lapply(infos, function(x) {x$height}))

  res <- magick::image_blank(width=total_width, height=total_height, color="#fff")

  x_offset <- 0
  for(img in images) {
    img_info <- magick::image_info(img)
    ypos <- (total_height - img_info$height) / 2
    res <- magick::image_composite(res, img, offset=magick::geometry_point(x_offset, ypos), operator="Copy")

    x_offset <- x_offset + img_info$width
  }

  return(res)
}


#' Makes a watermark generator. You can use the result with enable(watermark=...).
#'
#' @param show_qr show QR code
#' @param qr_size_px two-element vector specifying the width, height of the QR code
#' @param link_size_px two-element vector specifying the width, height of the link
#' @param link_bg background color for the link
#' @param font_color font color for the link
#' @param font_size font size for the link
#' @param font font name or family, e.g. "mono"
#' @param dynamic_size whether to automatically adjust the watermark size
#'  depending on the size of the current graphics device
#'
#' @return a function which you can pass to enable_knitr(watermark)
#' @export
watermark_generator <- function(show_qr=TRUE,
                                qr_size_px=c(100, 100),
                                link_size_px=c(500, 100),
                                link_bg="#ffffff",
                                font_color="#000000",
                                font_size=14,
                                font="mono",
                                dynamic_size=TRUE) {
  function(revision, image) {
    if(dynamic_size) {
      if(!is.null(grDevices::dev.list())) {
        dimensions <- grDevices::dev.size("px")
        w <- max(dimensions[1], 1920)
        h <- as.integer(1920.0 * dimensions[2] / dimensions[1])

      } else {
        w <- 1920
        h <- 1080
      }

      qr_size_px <- c(as.integer(w * 0.2), as.integer(w * 0.2))
      link_size_px <- c(as.integer(w * 0.8), as.integer(h * 0.2))
      font_size <- as.integer(1.0 * font_size * w / 700)
    }

    url <- file.path(APP_URL, "r", get_api_id(revision))

    # Link
    mark <- magick::image_blank(width=link_size_px[[1]],
                                            height=link_size_px[[2]],
                                            color=link_bg)
    link_img <- magick::image_annotate(mark, url,
                                       color=font_color, gravity="Center", font=font,
                                       size=font_size)

    # QR code
    if(show_qr) {
      qr <- qrcode::qr_code(url)
      svg_path <- tempfile(fileext = ".svg")
      qrcode::generate_svg(qr, svg_path, show=FALSE)

      qr_img <- force(magick::image_read_svg(svg_path, width=qr_size_px[[1]],
                                             height=qr_size_px[[2]]))
      file.remove(svg_path)

      link_img <- stack_horizontally(list(link_img, qr_img))

      magick::image_destroy(qr_img)
    }

    # Composite
    if(!is.null(image)) {
      res <- stack_vertically(list(image, link_img))
      magick::image_destroy(link_img)
      return(res)
    } else {
      return(link_img)
    }
  }
}


#' Applies a watermark to a plot object/function.
#'
#' @param qr pre-generated QR code, as an image
#' @param plot_obj plot object
#'
#' @return ggplot object with the watermark applied
#' @export
ggwatermark <- function(qr, plot_obj) {
  p <- cowplot::ggdraw()

  if(is_ggplot(plot_obj)) {
    p <- p + cowplot::draw_plot(plot_obj,
                                height=0.8, x=0.0, y=0.2)
  } else {
    warning("Provided object is not a ggplot. Output will be empty.")
  }

  p <- p + cowplot::draw_image(qr,
                               x=0, y=0,
                               height=0.2)
  return(p)
}

#' Draws a watermark with a GoFigr link and a QR code
#'
#' @param revision GoFigr revision object for which to generate a watermark
#' @param image Magick image to which to add the watermark
#'
#' @return a function which you can pass to enable_knitr(watermark)
#' @export
QR_WATERMARK = watermark_generator()

#' Draws a watermark with just a GoFigr link
#'
#' @param revision GoFigr revision object for which to generate a watermark
#' @param image Magick image to which to add the watermark
#'
#' @return a function which you can pass to enable_knitr(watermark)
#' @export
LINK_WATERMARK = watermark_generator(show_qr = FALSE)

#' Does not draw any watermarks.
#'
#' @return does not return anything (NULL)
#' @export
NO_WATERMARK = NULL
