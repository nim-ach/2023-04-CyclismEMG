
# Prepare workspace -------------------------------------------------------

## Load packages
library(data.table) # For fast data-processing
library(brms) # For bayesian regression models
library(bayestestR) # For bayesian regression models
library(flextable) # For bayesian regression models

## Load dataset
data("cyclist")

## Load custom functions
source(file = "R/bda/_functions.R")

# Lets prepare de data ----------------------------------------------------

## Estimate the overall emg median for the three windows
cyclist[, median_emg := mean(c(emg_mediana_1, emg_mediana_2, emg_mediana_3)), id][]

# Lets specify the priors -------------------------------------------------

## Prior for betas
b_prior <- prior(normal(0, 10), class = b)

# EMG ~ Jump height -------------------------------------------------------

if (file.exists("R/bda/bm/emg_jump.RData")) {
  load("R/bda/bm/emg_jump.RData")
} else {
  m_a1 <- brm(
    formula = median_emg ~ sj_altura_mt_z + cmj_altura_mt_z + aba_altura_mt_z,
    data = cyclist,
    family = gaussian(),
    prior = b_prior,
    chains = 5, iter = 12e3,
    warmup = 2e3, cores = 5,
    seed = 1234)
  save(m_a1, file = "R/bda/bm/emg_jump.RData")
}

tbl_a <- describe_posterior(
  posteriors = m_a1,
  ci_method = "hdi",
  test = c("pd", "p_significance", "p_rope", "bf"))
save(tbl_a, file = "docs/manuscript/tables/summary_emg_jump.RData")

# EMG ~ FTP ---------------------------------------------------------------

if (file.exists("R/bda/bm/emg_ftp.RData")) {
  load("R/bda/bm/emg_ftp.RData")
} else {
  m_b1 <- brm(
    formula = median_emg ~ ftp_z + w_kg_z,
    data = cyclist,
    family = gaussian(),
    prior = b_prior,
    chains = 5, iter = 12e3,
    warmup = 2e3, cores = 5,
    seed = 1234)
  save(m_b1, file = "R/bda/bm/emg_ftp.RData")
}

tbl_b <- describe_posterior(
  posteriors = m_b1,
  ci_method = "hdi",
  test = c("pd", "p_significance", "p_rope", "bf"))
save(tbl_b, file = "docs/manuscript/tables/summary_emg_ftp.RData")

# EMG ~ AUI ---------------------------------------------------------------

if (file.exists("R/bda/bm/emg_aui.RData")) {
  load("R/bda/bm/emg_aui.RData")
} else {
  m_c1 <- brm(
    formula = median_emg ~ ind_util_braz_perc_z,
    data = cyclist,
    family = gaussian(),
    prior = b_prior,
    chains = 5, iter = 12e3,
    warmup = 2e3, cores = 5,
    seed = 1234)
  save(m_c1, file = "R/bda/bm/emg_aui.RData")
}

tbl_c <- describe_posterior(
  posteriors = m_c1,
  ci_method = "hdi",
  test = c("pd", "p_significance", "p_rope", "bf"))
save(tbl_c, file = "docs/manuscript/tables/summary_emg_aui.RData")

# EMG ~ Jump + FTP + AUI --------------------------------------------------

if (file.exists("R/bda/bm/emg_full_adj.RData")) {
  load("R/bda/bm/emg_full_adj.RData")
} else {
  m_d1 <- brm(
    formula = median_emg ~
      sj_altura_mt_z + cmj_altura_mt_z + aba_altura_mt_z +
      ftp_z + w_kg_z + ind_util_braz_perc_z,
    data = cyclist,
    family = gaussian(),
    prior = b_prior,
    control = list(adapt_delta = .999),
    chains = 5, iter = 12e3,
    warmup = 2e3, cores = 5,
    seed = 1234)
  save(m_d1, file = "R/bda/bm/emg_full_adj.RData")
}

tbl_d <- describe_posterior(
  posteriors = m_d1,
  ci_method = "hdi",
  test = c("pd", "p_significance", "p_rope", "bf"))
save(tbl_d, file = "docs/manuscript/tables/summary_emg_full_adj.RData")
