#' Plot plant phenotypes and perform statistical tests
#'
#' This function plots the specified phenotype from the prepared data.
#' It can create different plot types (e.g., "boxplot") and optionally perform ANOVA and a post-hoc test.
#' The statistical results are displayed on the plot.
#'
#' @param data A data frame resulting from \code{prepare_data}.
#' @param phenotype Character. The name of the column containing the phenotype to plot.
#' @param plot_type Character. Type of plot to create. Default is "boxplot". Other options could be "violin", etc.
#' @param do_stats Logical. If TRUE, perform ANOVA and post-hoc tests comparing treatments.
#' @param line_col Character. The name of the column that indicates the line/genotype.
#' @param treatment_col Character. The name of the column that indicates the treatment groups.
#' @return A ggplot object.
#' @export
#'
plot_phenotype <- function(data, phenotype, line_col = "genotype", treatment_col = "treatment") {
  library(dplyr)
  library(ggplot2)
  library(ggbeeswarm)

  # Step 1: Calculate ANOVA and FDR correction by line
  stat_test <- data %>%
    group_by_at(line_col) %>%
    do({
      formula <- as.formula(paste(phenotype, "~", treatment_col))
      model <- aov(formula, data = .)
      anova_results <- summary(model)[[1]]
      p_value <- anova_results[["Pr(>F)"]][1]  # Get the p-value for the treatment effect
      data.frame(p_value = p_value)
    }) %>%
    ungroup() %>%
    mutate(p.adj = p.adjust(p_value, method = "fdr")) %>%
    mutate(significance = cut(p.adj,
                              breaks = c(0, 1e-04, 0.001, 0.01, 0.05, 1),
                              labels = c("****", "***", "**", "*", "ns")))

  # Print the statistical summary for debugging or confirmation
  print(stat_test)

  # Step 2: Determine max Y for placing annotations
  y_max <- max(data[[phenotype]], na.rm = TRUE)
  # A little offset above the max value for placing significance labels
  y_position <- y_max * 1.05

  # Step 3: Create the boxplot
  p <- ggplot(data, aes_string(x = line_col, y = phenotype, fill = treatment_col)) +
    geom_boxplot(position = position_dodge(width = 0.85),
                 alpha = 0.6, color = "grey35",
                 outlier.colour = NA, size = 0.5) +
    ggbeeswarm::geom_quasirandom(dodge.width = 0.85, size = 1, shape = 21, color = "black",
                                 width = 0.1, alpha = 0.4, show.legend = FALSE) +
    stat_summary(fun.data = mean_se, geom = "errorbar", linewidth = 0.5, width = 0.4,
                 position = position_dodge(width = 0.85)) +
    theme_bw() +
    scale_fill_manual(name = "Conditioning", values = c("seagreen", "gold2")) +
    ylab(phenotype) +
    theme(legend.position = "bottom")

  # Step 4: Add significance annotations
  # For each line, extract its significance code from stat_test and place it above that line
  lines <- unique(data[[line_col]])
  for (i in seq_along(lines)) {
    line_name <- lines[i]
    sig_info <- stat_test %>% filter(!!sym(line_col) == line_name)
    if (nrow(sig_info) > 0) {
      p <- p +
        annotate("text",
                 x = i,
                 y = y_position,
                 label = sig_info$significance,
                 size = 3,
                 color = "black",
                 fontface = "bold")
    }
  }

  return(p)
}

# Example usage:
# prepared_data <- prepare_data("path_to_your_data.csv", new_colnames = c("genotype", "treatment", "condition", "replicate"))
# p <- plot_phenotype(data = prepared_data, phenotype = "fresh_weight")
# print(p)

