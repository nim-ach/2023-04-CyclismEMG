## Load libraries
library(data.table)
library(brms)
## Load dataset
data("cyclist")

## Load custom functions
source(file = "R/bda/_functions.R")

sensitivity <- function(model, seed = 1234) {
  set.seed(seed)
  model <- add_criterion(model, "loo")
  m_01 <- update(model, prior = prior(normal(0, 1), class = "b"), refresh = 0) |> add_criterion("loo")
  m_05 <- update(model, prior = prior(normal(0, 5), class = "b"), refresh = 0) |> add_criterion("loo")
  m_100 <- update(model, prior = prior(normal(0, 100), class = "b"), refresh = 0) |> add_criterion("loo")
  out <- loo(model, m_01, m_05, m_100)
  out <- out$ic_diffs__ |>
    as.data.table(keep.rownames = "Comparison") |>
    transform(ELPD = LOOIC / -2, ELPD_SE = SE / 2) |>
    transform(std_md = ELPD/ELPD_SE) |>
    subset(subset = grepl("model", Comparison),
           select = c("Comparison", "ELPD", "ELPD_SE", "std_md"))
}

# neuromuscular efficiency ------------------------------------------------

## Jump - model m_a1
load("R/bda/bm/emg_jump.RData")
s_emg_jump <- sensitivity(m_a1)
save(s_emg_jump, file = "R/bda/bm/sensitivity/s_emg_jump.RData")

load("R/bda/bm/emg_ftp.RData")
s_emg_ftp <- sensitivity(m_b1)
save(s_emg_ftp, file = "R/bda/bm/sensitivity/s_emg_ftp.RData")

load("R/bda/bm/emg_aui.RData")
s_emg_aui <- sensitivity(m_c1)
save(s_emg_aui, file = "R/bda/bm/sensitivity/s_emg_aui.RData")

load("R/bda/bm/emg_full_adj.RData")
s_emg_full <- sensitivity(m_d1)
save(s_emg_full, file = "R/bda/bm/sensitivity/s_emg_full.RData")


# Muscle performance ------------------------------------------------------

load("R/bda/bm/ftp_jump.RData")
s_ftp_jump <- sensitivity(m_a2)
save(s_ftp_jump, file = "R/bda/bm/sensitivity/s_ftp_jump.RData")

load("R/bda/bm/ftp_emg.RData")
s_ftp_emg <- sensitivity(m_b2)
save(s_ftp_emg, file = "R/bda/bm/sensitivity/s_ftp_emg.RData")

load("R/bda/bm/ftp_full_adj.RData")
s_ftp_full <- sensitivity(m_c2)
save(s_ftp_full, file = "R/bda/bm/sensitivity/s_ftp_full.RData")
