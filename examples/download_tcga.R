if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install(c("TCGAbiolinks", "SummarizedExperiment"))

library(TCGAbiolinks)
library(SummarizedExperiment)

query_luad_exp <- GDCquery(
  project = "TCGA-LUAD",
  data.category = "Transcriptome Profiling",
  data.type = "Gene Expression Quantification",
  workflow.type = "STAR - Counts",
  experimental.strategy = "RNA-Seq"
)

GDCdownload(query_luad_exp)

luad_se <- GDCprepare(
  query = query_luad_exp,
  save = FALSE
)

saveRDS(luad_se, "TCGA-LUAD_TPM_SE.rds", compress = FALSE)

target_genes <- c("EGFR", "TP53", "ALK", "ROS1", "BRAF", "MET")
rows_to_keep <- rowData(luad_se)$gene_name %in% target_genes
luad_se_subset <- luad_se[rows_to_keep, ]

saveRDS(luad_se_subset, "TCGA-LUAD_TPM_SE_subset.rds")
