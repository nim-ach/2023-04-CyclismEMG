library(data.table)
library(brms)
library(flextable)
library(bayestestR)

source("R/bda/_functions.R")

load("R/bda/bm/emg_jump.RData")
load("R/bda/bm/emg_ftp.RData")
load("R/bda/bm/emg_aui.RData")
load("R/bda/bm/emg_full_adj.RData")

load("docs/manuscript/tables/summary_emg_jump.RData")
load("docs/manuscript/tables/summary_emg_ftp.RData")
load("docs/manuscript/tables/summary_emg_aui.RData")
load("docs/manuscript/tables/summary_emg_full_adj.RData")

tbl_a <- prep_table(tbl_a, m_a1)
tbl_b <- prep_table(tbl_b, m_b1)
tbl_c <- prep_table(tbl_c, m_c1)
tbl_d <- prep_table(tbl_d, m_d1)

tbl_data <- rbind(
  cbind(Type = "Jump height", tbl_a),
  cbind(Type = "FTP", tbl_b),
  cbind(Type = "AUI", tbl_c),
  cbind(Type = "Adjusted", tbl_d)
) |> as.data.table()

tbl_data[, `:=`(
  CI = NULL,
  BF = exp(log_BF), log_BF = NULL,
  Rhat = format(Rhat, digits = 4, nsmall = 4)
)]

ind <- c("Median", "CI_low", "CI_high", "pd", "ps", "p_ROPE", "BF")

tbl_data[, (ind) := lapply(.SD, round, digits = 3), .SDcols = ind]

names(tbl_data) <- c("Model", "Parameter", "Estimate", "Low", "High", "P (direction)", "R-hat", "ESS",
                  "P (significance)", "ROPE", "BF")

data.table::setcolorder(x = tbl_data,
                        neworder = c("Model", "Parameter", "Estimate", "Low", "High", "P (direction)",
                                     "P (significance)", "ROPE", "BF", "R-hat", "ESS"))
tbl_data[,Parameter := factor(
  Parameter,
  levels = c("b_Intercept", "b_sj_altura_mt_z", "b_cmj_altura_mt_z", "b_aba_altura_mt_z", "b_ind_util_braz_perc_z", "b_ftp_z", "b_w_kg_z"),
  labels = c("Intercept", "SJ height", "CMJ height", "Abalakov height", "Arm usage index", "Power", "WAP")
  )]

tbl_data[,Model := factor(
  Model,
  levels = c("Adjusted", "Jump height", "AUI", "FTP")
)]

setkey(tbl_data, Model, Parameter)


tbl_2 <- tbl_data |>
  ## Convertimos en Flextable
  flextable() |>
  ## Tamaño de letra e interlineado
  line_spacing(space = .6) |>
  fontsize(size = 8, part = "all") |>
  autofit() |>
  ## Unir celdas
  merge_at(1:7, 1) |>
  merge_at(8:11, 1) |>
  merge_at(12:13, 1) |>
  merge_at(14:16, 1) |>
  ## Crear encabezados
  add_header_row(
    values = c("","95% CI", "Effect Existence and Significance", "Diagnostics"),
    colwidths = c(3, 2, 4, 2)
  ) |>
  ## Tema vainilla
  theme_vanilla() |>
  ## Alineación de celdas
  align(align = "center", part = "all") |>
  align(j = 1:2, align = "left", part = "all") |>
  ## Creamos bordes
  border(i = 1, border.top = fp_border_default(width = 2), part = "all") |>
  border_inner_h(part = "all", border = fp_border_default(width = .1, color = "gray")) |>
  border(i = 1, j = 1:3, border.bottom = fp_border_default(color = "white"), part = "header") |>
  border(i = 1:2, j = c(1,2,3,5,9), border.right = fp_border_default(color = "white",width = 4), part = "header") |>
  border(i = c(8,12,14), border.top = fp_border_default(width = 1.5), part = "body") |>
  border(i = c(14:16), j = 1, border.bottom = fp_border_default(width = 1.5), part = "body")

save(tbl_2, file = "docs/manuscript/tables/table-2.RData")
