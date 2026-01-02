# ------------
# STEP 0: Load packages 
# ------------
library(lubridate) # for time-date formats
library(ggplot2)
library(tidyr)
library(dplyr) # for data management
library(patchwork) # for combined plots

rm(list = ls())

# set working directory
setwd("/Users/lotta/Library/Mobile Documents/com~apple~CloudDocs/Documents/Promotion/# 3 Clustering Inference/Code")

# ------------
# STEP 1: Load data
# ------------
load("results/hac_object_D.RData")
load("results/hac_object_y_y_hat_errors_errors_squared.RData")

data_to_plot <- c(
  "y",
  "y_hat_pred_linear_intraday",
  "y_hat_pred_pvar_intraday",
  "y_hat_pred_rf_full_countries",
  "y_hat_pred_xgb_full_countries"
)

cov_to_plot1 <- hac_objects[data_to_plot]

pos_M_selected <- c(
  "d_pred_linear_intraday_pred_pvar_intraday",
  "d_pred_linear_intraday_pred_rf_full_countries",
  "d_pred_linear_intraday_pred_xgb_full_countries",
  "d_pred_pvar_intraday_pred_rf_full_countries",
  "d_pred_rf_full_countries_pred_xgb_full_countries"
)

cov_to_plot2 <- hac_object_D[pos_M_selected]

hac_objects_all <- c(cov_to_plot1, cov_to_plot2)

# for (i in seq_along(hac_objects_all)) {
#   write.csv(hac_objects_all[[i]]$S_hat,
#             file = paste0("covmat_", i, ".csv"))
# }

# for (i in seq_along(hac_objects_all)) {
#   write.csv(hac_objects_all[[i]]$mask,
#             file = paste0("mask_", i, ".csv"))
# }

plot_titles <- list(
  expression(y),
  expression(hat(y)^"Linear"),
  expression(hat(y)^"PVAR-X"),
  expression(hat(y)^"Random Forest"),
  expression(hat(y)^"XGBoost"),
  expression(bar(d)^{"Linear, PVAR-X"}),
  expression(bar(d)^{"Linear, Random Forest"}),
  expression(bar(d)^{"Linear, XGBoost"}),
  expression(bar(d)^{"PVAR-X, Random Forest"}),
  expression(bar(d)^{"Random Forest, XGBoost"})
)

# ------------
# STEP 2: Create covariance plots
# ------------

# ------------------------------------------------------------
# GLOBAL signed-log color scale
# ------------------------------------------------------------
signed_log <- function(x) sign(x) * log10(1 + abs(x))

global_vals <- unlist(lapply(hac_objects_all, function(x) signed_log(as.numeric(x$S_hat))))
global_min  <- min(global_vals, na.rm = TRUE)
global_max  <- max(global_vals, na.rm = TRUE)

# ------------------------------------------------------------
# Build list of plots using your original plotting logic
# ------------------------------------------------------------
covplot_list <- list()

for(i in seq_along(hac_objects_all)) {
  
  mat  <- hac_objects_all[[i]]$S_hat
  mask <- hac_objects_all[[i]]$mask
  
  # --- Signed-log transform (as in original code) ---
  signed_log <- function(x) sign(x) * log10(1 + abs(x))
  
  # --- Reorder matrix (AT first, others alphabetical) ---
  countries <- rownames(mat)
  new_order <- c("AT", sort(countries[countries != "AT"]))
  
  mat_reordered  <- mat[new_order, new_order]
  mask_reordered <- mask[new_order, new_order]
  
  # Lookup tables
  row_order <- tibble(Var1 = new_order, row_id = seq_along(new_order))
  col_order <- tibble(Var2 = new_order, col_id = seq_along(new_order))
  
  # --- Convert to long format ---
  df_mat <- as.data.frame(mat_reordered) %>%
    mutate(Var1 = rownames(.)) %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
    left_join(row_order, by = "Var1") %>%
    left_join(col_order, by = "Var2") %>%
    mutate(value_trans = signed_log(value))
  
  df_mask <- as.data.frame(mask_reordered) %>%
    mutate(Var1 = rownames(.)) %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "keep")
  
  df <- left_join(df_mat, df_mask, by = c("Var1","Var2"))
  
  # --- Factors for plotting: identical to your original ---
  df <- df %>%
    mutate(
      Var2 = factor(Var2, levels = new_order),
      Var1 = factor(Var1, levels = rev(new_order))   # y reversed (your original)
    )
  
  # --- Keep only upper triangle (same as original code) ---
  df <- df %>%
    mutate(value_trans = ifelse(row_id >= col_id, value_trans, NA_real_))
  
  # --- Your exact ggplot code ---
  covplot_list[[i]] <- ggplot(df, aes(Var2, Var1, fill = value_trans)) +
    geom_tile(color = "grey70") +
    scale_fill_gradient2(
      low  = "blue",
      mid  = "white",
      high = "red",
      midpoint = 0,
      limits = c(global_min, global_max),
      name = expression("Signed " * log[10] * "(" * "|" * hat(s)[ij] * "|" + 1 * ")")
    ) +
    scale_x_discrete(position = "bottom") +
    geom_point(
      data = df %>% filter(!keep & row_id >= col_id),
      aes(x = Var2, y = Var1),
      shape = 4,
      size  = 10,
      stroke = 0.25,
      colour = "black",
      inherit.aes = FALSE
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(size = 15),
      legend.position = "bottom"
    ) +
    coord_fixed() +
    labs(
      x = "",
      y = "",
      title = plot_titles[[i]]
    )
}

# ------------------------------------------------------------
# 2 × 5 grid (your appearance preserved)
# ------------------------------------------------------------
final_plot <- wrap_plots(covplot_list, nrow = 2, guides = "collect") &
  theme(legend.position = "bottom")

final_plot

# Save
ggsave(
  filename = "plots/covplot_2x5.pdf",
  plot = final_plot,
  width = 22,
  height = 11,
  units = "in"
)


