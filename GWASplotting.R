list.files("/Users/ryantraband1/Documents/Citrus/assoc-files/GWAS-177-transformed")

library(qqman)
library(dplyr)
library(stringr)

# Set input and output directories
assoc_dir <- "/Users/ryantraband1/Documents/Citrus/assoc-files/qualitative_multivariate/"
plot_output_dir <- "/Users/ryantraband1/Documents/Citrus/Manahttan_plots/qualitative_multivariate"



dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# List of .assoc.txt files
assoc_files <- list.files(assoc_dir, pattern = "\\.assoc\\.txt$", full.names = TRUE)

# Initialize list to collect top SNPs
top_snps_by_trait <- list()

for (file in assoc_files) {
  trait_name <- str_replace(basename(file), "\\.assoc\\.txt$", "")
  if (trait_name %in% c("GEMMA_asp.petiole", "GEMMA_asp.leaf")) next
  
  # Remove "GEMMA_" prefix from trait_name for plot title
  plot_title <- str_remove(trait_name, "^GEMMA_")
  
  gwas_data <- tryCatch(read.table(file, header = TRUE), error = function(e) NULL)
  if (is.null(gwas_data)) next
  
  if (!all(c("chr", "ps", "p_wald") %in% colnames(gwas_data))) next
  if (!"rs" %in% colnames(gwas_data)) {
    gwas_data$rs <- paste0("SNP_", seq_len(nrow(gwas_data)))
  }
  
  gwas_data <- gwas_data %>% filter(!is.na(p_wald) & p_wald > 0)
  if (nrow(gwas_data) < 10) next
  
  # Thresholds
  bonf_thresh <- 0.05 / nrow(gwas_data)
  bonf_y <- -log10(bonf_thresh)
  
  gwas_data <- gwas_data %>%
    arrange(p_wald) %>%
    mutate(fdr = p.adjust(p_wald, method = "fdr"))
  
  fdr_line_val <- if (any(gwas_data$fdr <= 0.05, na.rm = TRUE)) {
    max(gwas_data$p_wald[gwas_data$fdr <= 0.05], na.rm = TRUE)
  } else {
    min(gwas_data$p_wald, na.rm = TRUE)
  }
  
  #fdr_y <- -log10(fdr_line_val)
  fdr_y<- 5
  ylim_max <- ceiling(max(bonf_y, fdr_y, na.rm = TRUE) * 1.1)
  
  # === PLOT MANHATTAN ===
  out_png <- file.path(plot_output_dir, paste0(trait_name, ".png"))
  png(filename = out_png, width = 1200, height = 500, res = 150)
  
  tryCatch({
    manhattan(gwas_data,
              chr = "chr", bp = "ps", p = "p_wald", snp = "rs",
              main = plot_title,  # Use modified title without GEMMA_,
              ylim = c(0, 8),
              genomewideline = bonf_y,
              suggestiveline = fdr_y,
              col = c("orange", "steelblue"),
              cex = 0.6,
              cex.axis = 0.9,
              cex.main = 1.4,
              ylab = expression(-log[10](italic(P))),
              xlab = "Chromosome")
  }, error = function(e) {
    message(sprintf("Failed to plot %s: %s", trait_name, e$message))
  })
  
  dev.off()  # === FORCE plot to save
  message(sprintf("Saved plot: %s", out_png))
  
  # Top SNPs
  top_snps <- gwas_data %>%
    filter(p_wald <= -log10(1e-5)) %>%
    arrange(p_wald) %>%
    slice_head(n = 5)
  
  top_snps_by_trait[[trait_name]] <- top_snps
}

# === SAVE CSV ===
combined_top_snps <- do.call(rbind, lapply(names(top_snps_by_trait), function(trait) {
  df <- top_snps_by_trait[[trait]]
  if (nrow(df) == 0) return(NULL)
  df$trait <- trait
  df[, c("trait", "rs", "chr", "ps", "p_wald", "fdr")]
}))

write.csv(combined_top_snps,
          file = file.path(plot_output_dir, "top_snps_by_trait_qulaitative-multivariate-test.csv"),
          row.names = FALSE)


#========================================================save at 50% of the width

library(qqman)
library(dplyr)
library(stringr)

# Set input and output directories
assoc_dir <- "/Users/ryantraband1/Documents/Citrus/assoc-files/177all"
plot_output_dir <- "/Users/ryantraband1/Documents/Citrus/Manahttan_plots/177all2"

dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# List of .assoc.txt files
assoc_files <- list.files(assoc_dir, pattern = "\\.assoc\\.txt$", full.names = TRUE)

# Initialize list to collect top SNPs
top_snps_by_trait <- list()

for (file in assoc_files) {
  trait_name <- str_replace(basename(file), "\\.assoc\\.txt$", "")
  if (trait_name %in% c("GEMMA_asp.petiole", "GEMMA_asp.leaf")) next
  
  gwas_data <- tryCatch(read.table(file, header = TRUE), error = function(e) NULL)
  if (is.null(gwas_data)) next
  
  if (!all(c("chr", "ps", "p_wald") %in% colnames(gwas_data))) next
  if (!"rs" %in% colnames(gwas_data)) {
    gwas_data$rs <- paste0("SNP_", seq_len(nrow(gwas_data)))
  }
  
  gwas_data <- gwas_data %>% filter(!is.na(p_wald) & p_wald > 0)
  if (nrow(gwas_data) < 10) next
  
  # Thresholds
  bonf_thresh <- 0.05 / nrow(gwas_data)
  bonf_y <- -log10(bonf_thresh)
  
  gwas_data <- gwas_data %>%
    arrange(p_wald) %>%
    mutate(fdr = p.adjust(p_wald, method = "fdr"))
  
  fdr_line_val <- if (any(gwas_data$fdr <= 0.05, na.rm = TRUE)) {
    max(gwas_data$p_wald[gwas_data$fdr <= 0.05], na.rm = TRUE)
  } else {
    min(gwas_data$p_wald, na.rm = TRUE)
  }
  
  fdr_y <- 5
  ylim_max <- ceiling(max(bonf_y, fdr_y, na.rm = TRUE) * 1.1)
  
  # === PLOT MANHATTAN ===
  out_png <- file.path(plot_output_dir, paste0(trait_name, ".png"))
  png(filename = out_png, width = 1200 * 0.5, height = 800, res = 150)
  
  tryCatch({
    manhattan(gwas_data,
              chr = "chr", bp = "ps", p = "p_wald", snp = "rs",
              main = trait_name,
              ylim = c(0, ylim_max),
              genomewideline = bonf_y,
              suggestiveline = fdr_y,
              col = c("orange", "steelblue"),
              cex = 0.6,
              cex.axis = 0.9,
              cex.main = 1.4,
              ylab = expression(-log[10](italic(P))),
              xlab = "Chromosome",
              font.main = 2,  # Bold title
              font.lab = 2,   # Bold axis labels
              font.axis = 2)  # Bold axis text
  }, error = function(e) {
    message(sprintf("Failed to plot %s: %s", trait_name, e$message))
  })
  
  dev.off()  # === FORCE plot to save
  message(sprintf("Saved plot: %s", out_png))
  
  # Top SNPs
  top_snps <- gwas_data %>%
    filter(p_wald <= -log10(1e-5)) %>%
    arrange(p_wald) %>%
    slice_head(n = 5)
  
  top_snps_by_trait[[trait_name]] <- top_snps
}

# === SAVE CSV ===
combined_top_snps <- do.call(rbind, lapply(names(top_snps_by_trait), function(trait) {
  df <- top_snps_by_trait[[trait]]
  if (nrow(df) == 0) return(NULL)
  df$trait <- trait
  df[, c("trait", "rs", "chr", "ps", "p_wald", "fdr")]
}))

write.csv(combined_top_snps,
          file = file.path(plot_output_dir, "top_snps_by_leaf_07-9.csv"),
          row.names = FALSE)









#==============================================================================

library(qqman)
library(dplyr)
library(stringr)

# Set input and output directories
assoc_dir <- "/Users/ryantraband1/Documents/Citrus/assoc-files/PCv1"
plot_output_dir <- "/Users/ryantraband1/Documents/Citrus/Manahttan_plots/PCv1"

dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# List of .assoc.txt files
assoc_files <- list.files(assoc_dir, pattern = "\\.assoc\\.txt$", full.names = TRUE)

# Initialize list to collect top SNPs
top_snps_by_trait <- list()

for (file in assoc_files) {
  trait_name <- str_replace(basename(file), "\\.assoc\\.txt$", "")
  if (trait_name %in% c("GEMMA_asp.petiole", "GEMMA_asp.leaf")) next
  
  # Remove "GEMMA_" prefix from trait_name for plot title
  plot_title <- str_remove(trait_name, "^GEMMA_")
  
  gwas_data <- tryCatch(read.table(file, header = TRUE), error = function(e) NULL)
  if (is.null(gwas_data)) next
  
  if (!all(c("chr", "ps", "p_wald") %in% colnames(gwas_data))) next
  if (!"rs" %in% colnames(gwas_data)) {
    gwas_data$rs <- paste0("SNP_", seq_len(nrow(gwas_data)))
  }
  
  gwas_data <- gwas_data %>% filter(!is.na(p_wald) & p_wald > 0)
  if (nrow(gwas_data) < 10) next
  
  # Thresholds
  bonf_thresh <- 0.05 / nrow(gwas_data)
  bonf_y <- -log10(bonf_thresh)
  
  gwas_data <- gwas_data %>%
    arrange(p_wald) %>%
    mutate(fdr = p.adjust(p_wald, method = "fdr"))
  
  fdr_line_val <- if (any(gwas_data$fdr <= 0.05, na.rm = TRUE)) {
    max(gwas_data$p_wald[gwas_data$fdr <= 0.05], na.rm = TRUE)
  } else {
    min(gwas_data$p_wald, na.rm = TRUE)
  }
  
  fdr_y <- 5
  ylim_max <- ceiling(max(bonf_y, fdr_y, na.rm = TRUE) * 1.1)
  
  # === PLOT MANHATTAN ===
  out_png <- file.path(plot_output_dir, paste0(trait_name, ".png"))
  png(filename = out_png, width = 1200 * 0.5, height = 800, res = 150)
  
  tryCatch({
    manhattan(gwas_data,
              chr = "chr", bp = "ps", p = "p_wald", snp = "rs",
              main = plot_title,  # Use modified title without GEMMA_
              ylim = c(0, ylim_max),
              genomewideline = bonf_y,
              suggestiveline = fdr_y,
              col = c("orange", "steelblue"),
              cex = 0.6,
              cex.axis = 0.9,
              cex.main = 1.4,
              ylab = expression(-log[10](italic(P))),
              xlab = "Chromosome",
              font.main = 2,  # Bold title
              font.lab = 2,   # Bold axis labels
              font.axis = 2)  # Bold axis text
  }, error = function(e) {
    message(sprintf("Failed to plot %s: %s", trait_name, e$message))
  })
  
  dev.off()  # === FORCE plot to save
  message(sprintf("Saved plot: %s", out_png))
  
  # Top SNPs
  top_snps <- gwas_data %>%
    filter(p_wald <= -log10(1e-5)) %>%
    arrange(p_wald) %>%
    slice_head(n = 5)
  
  top_snps_by_trait[[trait_name]] <- top_snps
}

# === SAVE CSV ===
combined_top_snps <- do.call(rbind, lapply(names(top_snps_by_trait), function(trait) {
  df <- top_snps_by_trait[[trait]]
  if (nrow(df) == 0) return(NULL)
  df$trait <- trait
  df[, c("trait", "rs", "chr", "ps", "p_wald", "fdr")]
}))

write.csv(combined_top_snps,
          file = file.path(plot_output_dir, "top_snps_by_traitqualitative_multivariate_test.csv"),
          row.names = FALSE)
