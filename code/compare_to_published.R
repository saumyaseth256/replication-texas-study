library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

source("code/published_values.R")
dir.create("tables/formatted",
           showWarnings = FALSE,
           recursive = TRUE)
read_output <- function(path) {
  read_csv(path, show_col_types = FALSE, progress = FALSE)
}

###############################################################################
# Replications of tables compared to paper
###############################################################################

# Table 1 --------------------------------------------------------------------
replicated_table1 <- read_output("tables/table1_rating_distributions.csv") |>
  mutate(term = row) |>
  select(-year, -row) |>
  pivot_longer(-term, names_to = "column", values_to = "estimate") |>
  mutate(table = "Table 1", panel = "", se = NA_real_)

# Table 2 --------------------------------------------------------------------
replicated_table2 <- bind_rows(
  read_output("tables/table2a_taas_transitions.csv") |> mutate(panel = "A_TAAS"),
  read_output("tables/table2b_transition.csv") |> mutate(panel = "B_TRANSITION"),
  read_output("tables/table2c_taks_transitions.csv") |> mutate(panel = "C_TAKS")
) |>
  pivot_longer(c(L, A, R, E), names_to = "column", values_to = "estimate") |>
  transmute(table = "Table 2",
            panel,
            term = previous_rating,
            column,
            estimate,
            se = NA_real_)

# Tables 3 and 4 -------------------------------------------------------------
descriptive_long <- function(path, row_column, table_name) {
  read_output(path) |>
    rename(term = all_of(row_column)) |>
    pivot_longer(-term,
                 names_to = c("column", ".value"),
                 names_pattern = "^(TA[AK]S_[LARE])_(mean|sd)$") |>
    transmute(
      table = table_name,
      panel = "",
      term,
      column,
      estimate = mean,
      se = sd
    )
}
observation_rows <- function(path, table_name) {
  read_output(path) |>
    transmute(
      table = table_name,
      panel = "",
      term = "observations",
      column = paste0(regime, "_", campus_rating),
      estimate = observations,
      se = NA_real_
    )
}
replicated_table3 <- bind_rows(
  descriptive_long(
    "tables/table3_school_characteristics.csv",
    "characteristic",
    "Table 3"
  ),
  observation_rows("tables/table3_observations.csv", "Table 3")
)
replicated_table4 <- descriptive_long("tables/table4_mean_resources.csv", "resource", "Table 4")

# Regression tables ----------------------------------------------------------
clean_term <- function(term) {
  term |>
    str_replace("^factor\\(rating_(\\d{4})\\)([LAR])$", "\\2_\\1")
}
clean_outcome <- function(outcome) {
  str_remove(outcome, "^change_\\d{4}_")
}
regression_long <- function(models) {
  coefficients <- models |>
    transmute(
      table,
      panel = coalesce(panel, ""),
      term = clean_term(term),
      column = clean_outcome(outcome),
      estimate,
      se = standard_error,
      p_value
    )
  observations <- models |>
    distinct(table, panel = coalesce(panel, ""), outcome, observations) |>
    transmute(
      table,
      panel,
      term = "observations",
      column = clean_outcome(outcome),
      estimate = observations,
      se = NA_real_,
      p_value = NA_real_
    )
  bind_rows(coefficients, observations)
}
regression_files <- c(
  "Table 5" = "tables/table5_models.csv",
  "Table 6" = "tables/table6_models.csv",
  "Table 7" = "tables/table7_models.csv",
  "Table 8" = "tables/table8_models.csv",
  "Table 9" = "tables/table9_models.csv",
  "Table 10" = "tables/table10_models.csv"
)
replicated_regressions <- map_dfr(regression_files, function(path) {
  models <- read_output(path)
  if (!"panel" %in% names(models))
    models$panel <- ""
  regression_long(models)
})
joint_f <- read_output("tables/table5_joint_f.csv") |>
  transmute(
    table,
    panel = "",
    term = "F_any_drop",
    column = clean_outcome(outcome),
    estimate = f_statistic,
    se = NA_real_,
    p_value
  )
replicated_values <- bind_rows(
  replicated_table1,
  replicated_table2,
  replicated_table3,
  replicated_table4,
  replicated_regressions,
  joint_f
)

# Keep the cells reported in the paper, in the paper's row and column order.
stars <- function(p) {
  case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}
table_order <- paste("Table", 1:10)
replicated_cells <- published_values |>
  select(table, panel, term, column) |>
  left_join(
    replicated_values |>
      rename(replicated = estimate, replicated_se = se),
    by = c("table", "panel", "term", "column")
  ) |>
  mutate(replicated_stars = stars(p_value))

###############################################################################
# Formatted replication
###############################################################################

format_number <- function(x, digits) {
  digits <- rep_len(digits, length(x))
  out <- vapply(seq_along(x), function(i) {
    formatC(x[i],
            format = "f",
            digits = digits[i],
            big.mark = "")
  }, character(1))
  str_replace(out, "^-(0\\.?0*)$", "\\1")
}
digits_for <- function(table_name, column) {
  case_when(
    table_name == "Table 7" ~ 3L,
    column %in% c("student_teacher") ~ 2L,
    table_name %in% c("Table 1", "Table 2", "Table 3", "Table 8") ~ 1L,
    table_name == "Table 4" &
      column != "student_teacher_ratio" ~ 0L,
    TRUE ~ 1L
  )
}
walk(table_order, function(table_name) {
  cells <- replicated_cells |>
    filter(table == table_name, !is.na(replicated)) |>
    mutate(
      digits = if_else(
        term %in% c("observations"),
        0L,
        digits_for(table_name, column)
      ),
      digits = if_else(
        table_name == "Table 4" & term == "student_teacher_ratio",
        1L,
        digits
      ),
      digits = if_else(term == "F_any_drop", 2L, digits),
      value = paste0(format_number(replicated, digits), replicated_stars),
      se_value = if_else(
        is.na(replicated_se),
        NA_character_,
        paste0("(", format_number(replicated_se, digits), ")")
      )
    )
  column_order <- unique(cells$column)
  term_order <- unique(cells$term)
  formatted <- bind_rows(
    cells |> transmute(panel, term, line = 1L, column, text = value),
    cells |> filter(!is.na(se_value)) |>
      transmute(panel, term, line = 2L, column, text = se_value)
  ) |>
    mutate(
      term = factor(term, levels = term_order),
      column = factor(column, levels = column_order)
    ) |>
    arrange(panel, term, line, column) |>
    pivot_wider(names_from = column, values_from = text) |>
    mutate(term = if_else(line == 1L, as.character(term), "")) |>
    select(-line)
  write_csv(formatted,
            paste0("tables/formatted/", str_replace(tolower(table_name), " ", ""), ".csv"),
            na = "")
})
