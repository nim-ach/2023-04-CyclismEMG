library(data.table)
library(brms)
library(flextable)
library(bayestestR)

load("R/bda/bm/emg_jump.RData")
load("R/bda/bm/emg_ftp.RData")
load("R/bda/bm/emg_aui.RData")
load("R/bda/bm/emg_full_adj.RData")

load("docs/manuscript/tables/summary_emg_jump.RData")
load("docs/manuscript/tables/summary_emg_ftp.RData")
load("docs/manuscript/tables/summary_emg_aui.RData")
load("docs/manuscript/tables/summary_emg_full_adj.RData")

as.data.table(tbl_a)
as.data.table(tbl_b)
as.data.table(tbl_c)
as.data.table(tbl_d)

tbl_2 <- rbind(
  cbind(Type = "Univariate", rbind(tbl_a[-1,], tbl_b[-1,], tbl_c[-1,])),
  cbind(Type = "Adjusted", tbl_d)
) |> as.data.table()
tbl_2[, `:=`(
  CI = NULL, ROPE_CI = NULL,
  ROPE_low = NULL, ROPE_high = NULL,
  BF = exp(log_BF), log_BF = NULL,
  Rhat = format(Rhat, digits = 4, nsmall = 4)
)]
ind <- c("Median", "CI_low", "CI_high", "pd", "ps", "ROPE_Percentage", "BF")
tbl_2[, (ind) := lapply(.SD, round, digits = 3), .SDcols = ind]
tbl_2[which.max(BF), BF := NA]
names(tbl_2) <- c("Model", "Parameter", "Estimate", "Low", "High", "P (direction)", "P (significance)",
                  "ROPE (%)", "R-hat", "ESS", "BF")
data.table::setcolorder(x = tbl_2,
                        neworder = c("Model", "Parameter", "Estimate", "Low", "High", "P (direction)",
                                     "P (significance)", "ROPE (%)", "BF", "R-hat", "ESS"))
tbl_2[, Parameter := factor(
  x = Parameter,
  levels = c("b_aba_altura_mt_z", "b_cmj_altura_mt_z", "b_ftp_z", "b_ind_util_braz_perc_z",
             "b_Intercept", "b_sj_altura_mt_z", "b_w_kg_z"),
  labels = c("Abalakov Height (m)", "CMJ Height (m)", "Power (watts)", "Arm usage index (%)",
             "Adjusted mean EMG", "SJ Height (m)", "WAP (watts per kg)"))]


tbl_2 <- tbl_2 |>
  flextable() |>
  line_spacing(space = .6) |>
  fontsize(size = 8, part = "all") |>
  autofit() |>
  merge_at(7:13, 1) |>
  merge_at(1:6, 1) |>
  add_header_row(
    values = c("","95% CI", "Effect Existence and Significance", "Diagnostics"),
    colwidths = c(3, 2, 4, 2)
  ) |>
  theme_vanilla() |>
  align(align = "center", part = "all") |>
  align(j = 1:2, align = "left", part = "all") |>
  border(i = 1, border.top = fp_border_default(width = 2), part = "all") |>
  border_inner_h(part = "all", border = fp_border_default(width = .1, color = "gray")) |>
  border(i = 1, j = 1:3, border.bottom = fp_border_default(color = "white"), part = "header") |>
  border(i = 1:2, j = c(1,2,3,5,9), border.right = fp_border_default(color = "white",width = 4), part = "header") |>
  border(i = 7, border.top = fp_border_default(width = 1.5), part = "body") |>
  border(i = c(7:12), j = 1, border.bottom = fp_border_default(width = 1.5), part = "body")

save(tbl_2, file = "docs/manuscript/tables/table-2.RData")
