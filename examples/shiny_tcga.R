#
# TCGA-LUAD Gene Expression Shiny App
#

library(shiny)
library(ggplot2)
library(gofigR)
library(digest)
library(SummarizedExperiment)

gofigR::enable(url="https://api.gofigr.io",
               analysis_name = "TCGA")

DATA_PATH <- "TCGA-LUAD_TPM_SE_subset.rds"
se <- readRDS(DATA_PATH)

# Get gene symbols (e.g., "EGFR", "TP53", ...) for the dropdown
gene_choices <- rowData(se)$gene_name

# Create the TPM matrix and assign gene symbols as the rownames
tpm_matrix <- assay(se, "tpm_unstrand")
rownames(tpm_matrix) <- gene_choices

# Combine metadata (colData) and (transposed) TPM data into one data.frame
# t() transposes the matrix from (genes x samples) to (samples x genes)
full_plot_data <- cbind(as.data.frame(colData(se)), t(tpm_matrix))

# Define choices for the UI inputs
strat_choices <- c(
  "definition", "tumor_descriptor", "sample_type",
  "preservation_method", "classification_of_tumor"
)

# Get unique, non-NA smoking statuses for the filter
smoking_choices <- unique(full_plot_data$tobacco_smoking_status)
smoking_choices <- na.omit(smoking_choices)


ui <- fluidPage(
  titlePanel("TCGA-LUAD Gene Expression Explorer"),

  sidebarLayout(

    # --- Sidebar Panel (Inputs) ---
    sidebarPanel(
      h4("Plot Controls"),

      # Input 1: Gene selection (uses gene symbols)
      selectInput(
        inputId = "selected_gene",
        label = "Select Gene:",
        choices = gene_choices,
        selected = gene_choices[1] # Default to the first gene
      ),

      # Input 2: Stratification variable selection
      selectInput(
        inputId = "strat_var",
        label = "Select Stratification Variable:",
        choices = strat_choices,
        selected = "sample_type" # Default to sample_type
      ),

      # Input 3: Smoking status filter (multiple choice)
      checkboxGroupInput(
        inputId = "smoking_filter",
        label = "Filter by Smoking Status:",
        choices = smoking_choices,
        selected = smoking_choices # Default to all selected
      )
    ),

    # --- Main Panel (Output) ---
    mainPanel(
      # The plot will be rendered here
      gfPlot("distPlot")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({

    validate(
      need(input$smoking_filter, "Please select at least one smoking status.")
    )

    full_plot_data[full_plot_data$tobacco_smoking_status %in% input$smoking_filter,
                   c("tobacco_smoking_status", input$strat_var, input$selected_gene)]
  })

  metadata <- reactive({
    frame <- filtered_data()
    res <- reactiveValuesToList(input)
    res$patients <- row.names(frame)
    res$data_cube <- list(version="1.2.3-prod",
                          hash=digest(
                            object = DATA_PATH,
                            algo = "sha256",  # Specify the algorithm
                            file = TRUE       # Tell the function to treat 'object' as a file path
                          ))
    return(res)
  })

  gfPlotServer("distPlot", {

    # Get the reactive data (already filtered by smoking status)
    data_to_plot <- filtered_data()

    # Ensure data is not empty after filtering
    validate(
      need(nrow(data_to_plot) > 0, "No data available for the selected filters.")
    )

    # Create the ggplot
    # We use the .data[[]] syntax to use string inputs (from the dropdowns)
    # inside aes()
    ggplot(data_to_plot,
           aes(x = log2(.data[[input$selected_gene]] + 1),
               color = .data[[input$strat_var]])) +

      # Create overlayed density plots with transparency
      geom_density(alpha = 0.7, na.rm = TRUE) +

      # Add informative labels
      labs(
        title = paste("TPM Distribution of", input$selected_gene),
        subtitle = paste("Stratified by", input$strat_var, "& filtered by smoking status"),
        x = paste("log2(TPM + 1) of", input$selected_gene),
        y = "Density",
        fill = input$strat_var # Label for the legend
      ) +

      # Use a clean theme
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  }, metadata, figure_name="TPM Distribution in LUAD")
}


shinyApp(ui = ui, server = server)
