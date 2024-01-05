library(data.table)
library(brms)
library(flextable)
library(bayestestR)

source("R/bda/_functions.R")

load("R/bda/bm/ftp_emg.RData")
load("R/bda/bm/ftp_jump.RData")
load("R/bda/bm/ftp_full_adj.RData")

load("docs/manuscript/tables/summary_ftp_jump.RData")
load("docs/manuscript/tables/summary_ftp_emg.RData")
load("docs/manuscript/tables/summary_ftp_full_adj.RData")


# -------------------------------------------------------------------------

tbl_a2 <- prep_table(tbl_a2, m_a2, rope_ci = c(-.1, .1))
tbl_b2 <- prep_table(tbl_b2, m_b2, rope_ci = c(-.1, .1))
tbl_c2 <- prep_table(tbl_c2, m_c2, rope_ci = c(-.1, .1))

tbl_data <- list(Jump = tbl_a2, EMG = tbl_b2, Adjusted = tbl_c2) |>
  rbindlist(idcol = "Model")

tbl_data[, Outcome := fcase(
  grepl("ftpz", Parameter), "Power",
  grepl("wkgz", Parameter), "WAP"
)]

tbl_data[, Parameter := fcase(
  grepl("Intercept", Parameter), "Intercept",
  grepl("aba_altura", Parameter), "Abalakov Height",
  grepl("cmj_altura", Parameter), "CMJ Height",
  grepl("sj_altura", Parameter), "SJ Height",
  grepl("median_emg", Parameter), "Median EMG activity"
)]

tbl_data[, Parameter := factor(
  x = Parameter,
  levels = c("Intercept", "Abalakov Height", "CMJ Height", "SJ Height", "Median EMG activity")
)]

setkey(tbl_data, Model, Outcome, Parameter)
setcolorder(tbl_data)

tbl_data[, `:=`(
  BF = round(exp(log_BF), 3),
  log_BF = NULL, CI = NULL,
  Rhat = format(Rhat, digits = 4, nsmall = 4),
  ESS = round(ESS)
)]

ind <- c("Median", "CI_low", "CI_high", "pd", "ps", "BF", "p_ROPE")
tbl_data[, (ind) := lapply(.SD, round, digits = 3), .SDcols = ind]

names(tbl_data) <- c("Model", "Outcome", "Parameter", "Estimate", "Low", "High",
                     "P (direction)", "R-hat", "ESS", "P (significance)", "ROPE", "BF")
data.table::setcolorder(x = tbl_data,
                        neworder = c("Model", "Outcome", "Parameter", "Estimate", "Low", "High",
                                     "P (direction)", "P (significance)", "ROPE", "BF", "R-hat", "ESS"))

tbl_3 <-
  tbl_data |>
  flextable() |>
  line_spacing(space = .6) |>
  fontsize(size = 8, part = "all") |>
  autofit() |>
  merge_at(1:10, 1) |>
  merge_at(1:5, 2) |>
  merge_at(6:10, 2) |>
  merge_at(11:14, 1) |>
  merge_at(11:12, 2) |>
  merge_at(13:14, 2) |>
  merge_at(15:22, 1) |>
  merge_at(15:18, 2) |>
  merge_at(19:22, 2) |>
  add_header_row(
    values = c("","95% CI", "Effect Existence and Significance", "Diagnostics"),
    colwidths = c(4, 2, 4, 2)
  ) |>
  theme_vanilla() |>
  align(align = "center", part = "all") |>
  align(j = 1:3, align = "left", part = "all") |>
  border(i = 1, border.top = fp_border_default(width = 2), part = "all") |>
  border_inner_h(part = "all", border = fp_border_default(width = .1, color = "gray")) |>
  border(i = 1, j = 1:4, border.bottom = fp_border_default(color = "white"), part = "header") |>
  border(i = 1:2, j = c(1,2,3,6,10), border.right = fp_border_default(color = "white",width = 4), part = "header") |>
  border(i = c(11,15), border.top = fp_border_default(width = 1.5), part = "body") |>
  border(j = 1, border.bottom = fp_border_default(width = 1.5), part = "body") |>
  border(i = 19:22, j = 2, border.bottom = fp_border_default(width = 1.5), part = "body")

save(tbl_3, file = "docs/manuscript/tables/table-3.RData")
