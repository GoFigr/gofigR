#
# TCGA-LUAD Clinical Compliance Shiny App
#

library(shiny)
library(gofigR)
library(DT)
library(SummarizedExperiment)

gofigR::enable(url = "https://api.gofigr.io")

# Load the SummarizedExperiment object once at startup
se <- readRDS("TCGA-LUAD_TPM_SE_subset.rds")

# Derive patient (sample) choices from the colData row names or column names
clinical_df <- as.data.frame(colData(se))

if (is.null(rownames(clinical_df)) || any(rownames(clinical_df) == "")) {
  rownames(clinical_df) <- colnames(se)
}

patient_choices <- rownames(clinical_df)

# Ensure we have some identifier to display
stopifnot(length(patient_choices) > 0)

ui <- fluidPage(
  titlePanel("TCGA Compliance Check"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_patients",
        label = "Select Patient(s):",
        choices = patient_choices,
        selected = patient_choices[1],
        multiple = TRUE
      )
    ),
    mainPanel(
      DT::DTOutput("clinical_table"),
      br(),
      actionButton("find_assets", "Find patient assets", class="btn btn-primary"),
      br(),
      uiOutput("revision_results")
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(all_revisions=NULL,
                           matching_revisions=NULL,
                           assets_valid=FALSE)

  observeEvent(input$find_assets, {
    withProgress({
      setProgress(message="Searching revisions...")
      client <- gofigR::get_client()
      ana <- gofigR::find_analysis(client, "shiny_tcga.R")
      ana <- gofigR::get_analysis(client, ana$api_id)

      all_revisions <- NULL
      lapply(ana$figures, function(fig) {
        fig <- gofigR::get_figure(client, fig$api_id)
        all_revisions <<- append(all_revisions, fig$revisions)
      })

      values$all_revisions <- all_revisions

      setProgress(message="Searching patient IDs...")
      matches <- NULL
      lapply(all_revisions, function(rev) {
        meta <- rev$metadata
        if(!is.null(meta) && "patients" %in% names(meta) && any(input$selected_patients %in% unlist(meta$patients))) {
          matches <<- append(matches, list(rev))
        }
      })
      values$matching_revisions <- matches
      values$assets_valid <- TRUE
    })
  })

  observeEvent(input$selected_patients, {
    values$all_revisions <- NULL
    values$matching_revisions <- NULL
    values$assets_valid <- FALSE
  })

  output$clinical_table <- renderDT({
    req(input$selected_patients)

    patient_ids <- input$selected_patients

    # Retrieve the clinical information for the selected patients
    patient_rows <- clinical_df[patient_ids, , drop = FALSE]

    DT::datatable(
      data.frame(
        Patient = rownames(patient_rows),
        patient_rows,
        check.names = FALSE,
        row.names = NULL
      ),
      options = list(scrollX = TRUE),
      selection = "none"
    )
  })

  output$revision_results <- renderUI({
    if (!isTRUE(values$assets_valid)) {
      return(NULL)
    }

    tagList(
      h4("Assets Referencing Patients of Interest"),
      DT::DTOutput("revision_table")
    )
  })

  output$revision_table <- renderDT({
    req(values$assets_valid)

    revs <- values$matching_revisions

    if (is.null(revs) || length(revs) == 0) {
      data <- data.frame(
        `Revision ID` = character(0),
        `GoFigr Link` = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      ids <- vapply(
        revs,
        function(rev) {
          if (!is.null(rev$api_id)) rev$api_id else NA_character_
        },
        character(1)
      )

      n_patients <- sapply(
        revs,
        function(rev) {
          if (!is.null(rev$metadata$patients)) length(rev$metadata$patients) else 0
        }
      )

      data_ver <- sapply(
        revs,
        function(rev) {
          if (!is.null(rev$metadata$data_cube)) rev$metadata$data_cube$hash else ""
        }
      )

      matching_patients <- sapply(
        revs,
        function(rev) {
          if(!is.null(rev$metadata$patients)) {
            patient_ids <- input$selected_patients
            return(paste0(patient_ids[patient_ids %in% rev$metada$patients], collapse=", "))
          } else {
            return("")
          }
        }
      )

      links <- ifelse(
        is.na(ids),
        NA_character_,
        sprintf(
          '<a href="%s" target="_blank">Open in GoFigr</a>',
          file.path("https://app.gofigr.io", "r", ids)
        )
      )

      data <- data.frame(
        `Revision ID` = ids,
        `Data cube` = data_ver,
        `Matching patients` = matching_patients,
        `#total patients` = n_patients,
        `GoFigr Link` = links,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    DT::datatable(
      data,
      escape = FALSE,
      options = list(scrollX = TRUE),
      selection = "none"
    )
  })
}

shinyApp(ui = ui, server = server)

