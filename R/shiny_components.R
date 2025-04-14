#' Generates a QR code and converts it into an img element.
#'
#' @param url URL to generate the code for
#' @param xres QR width, in pixels
#' @param yres QR height, in pixels
#' @param width width of the HTML img element
#' @param height height of the HTML img element
#'
#' @returns HTML string
#' @export
get_qr_png <- function(url, xres=400, yres=400, width=100, height=100) {
  qr <- qrcode::qr_code(url)

  path <- tempfile(fileext=paste0(".png"))
  grDevices::png(path, width=xres, height=yres)
  plot(qr)
  grDevices::dev.off()

  data <- readr::read_file_raw(path)
  datab64 <- base64enc::base64encode(data)
  return(paste0("<img style=\"width: ", width, "px; height: ", height, "px\" src=\"",
                "data:image/png;base64,",
                datab64,
                "\"/>"))
}

#' Generates a div container for the GoFigr widget.
#'
#' @param ... passed to the div
#' @param jc justify-content CSS value
#'
#' @returns styled div element
gfContainer <- function(..., jc="center") {
  shiny::div(style=paste0("display: flex; justify-content: ", jc, "; align-items: center"),
             ...)
}

#' Defines a GoFigr plot area.
#'
#' @param id ID of this plot area
#' @param ... same as plotOutput
#'
#' @returns HTML elements
#' @export
gfPlot <- function(id, ...) {
  ns <- shiny::NS(id)

  return(shiny::div(
    shinyjs::useShinyjs(),
    shiny::fluidRow(shiny::plotOutput(ns("plot", ...))),
    shinyjs::hidden(gfContainer(jc="right",
                                id=ns("loader"),
                                style="margin: 20px; color: rgb(255, 87, 87)",
                                "Please wait...")),
    shiny::fluidRow(id=ns("watermark-container"),
                    shiny::uiOutput(ns("watermark")))))
}

#' Creates a Shiny component to handle plotting and publishing. Has to be
#' paired with a gfPlot element in the UI.
#'
#' @param id id of the gfPlot element
#' @param expr expression generating a plot
#' @param metadata metadata to publish with the figure. You can pass the shiny input object
#'   to capture input values.
#' @param env environment in which to evaluate the expression
#' @param figure_name name of the figure to publish under. Inferred from figure's title if NULL.
#' @param quoted whether the passed expression is quoted
#' @param base_graphics whether the passed expression uses base graphics
#'
#' @returns moduleServer
#' @export
gfPlotServer <- function(id, expr, metadata=NULL,
                         env=parent.frame(),
                         figure_name = NULL,
                         quoted = FALSE,
                         base_graphics = FALSE) {
  plot_func <- shiny::installExprFunction(expr,
                                          "func", env,
                                          quoted,
                                          label = "gfServer",
                                          ..stacktraceon = TRUE)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- shiny::reactiveValues(revision=NULL)

    output$plot <- shiny::renderPlot({
      values$revision <- NULL
      plot_func()
    })

    output$watermark <- shiny::renderUI({
      if (is.null(values$revision)) {
        return(gfContainer(
          jc = "right",
          shiny::div(
            style = "margin: 10px",
            shiny::actionButton(
              ns("publish"),
              "Publish to GoFigr.io",
              icon = shiny::icon("cloud-arrow-up", style =
                                   "color: rgb(255, 87, 87)")
            )
          )
        ))
      }

      url <- file.path(APP_URL, "r", get_api_id(values$revision))

      return(gfContainer(
        shiny::div(style = "margin: 10px", shiny::tags$a(
          paste0(
            "View on GoFigr: ",
            values$revision$figure_metadata$name,
            " #",
            values$revision$revision_index + 1
          ),
          href = url,
          target = "_blank"
        )),
        shiny::div(style = "margin: 10px", shiny::div(shiny::HTML(get_qr_png(
          url
        ))))
      ))
    })

    shiny::observeEvent(input$publish, {
      shinyjs::show(id="loader")
      shinyjs::hide(id="watermark-container")

      if(shiny::is.reactivevalues(metadata)) {
        metadata <- shiny::reactiveValuesToList(metadata)
      }

      if(base_graphics) {
        pub <- gofigR::publish_base
      } else {
        pub <- gofigR::publish
      }

      values$revision = pub(plot_func(),
                            metadata=list(`Shiny inputs`=metadata),
                            figure_name=figure_name)
      shinyjs::hide(id="loader")
      shinyjs::show(id="watermark-container")
    })
  })
}
