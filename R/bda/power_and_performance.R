
# Prepare workspace -------------------------------------------------------

## Load packages
library(data.table) # For fast data-processing
library(brms) # For bayesian regression models
library(bayestestR) # For bayesian regression models

## Load dataset
data("cyclist")

## Load custom functions
source(file = "R/bda/_functions.R")

# Lets prepare de data ----------------------------------------------------

## Estimate the overall emg median for the three windows
cyclist[, median_emg := mean(c(emg_mediana_1, emg_mediana_2, emg_mediana_3)), id][]

# Lets specify the priors -------------------------------------------------

## Prior for betas and intercepts
b_prior <-
  prior(normal(0, 10), class = b, resp = ftpz) +
  prior(normal(0, 10), class = b, resp = wkgz) +
  prior(student_t(3, 0, 2.5), class = Intercept, resp = ftpz) +
  prior(student_t(3, 0, 2.5), class = Intercept, resp = wkgz)

# EMG ~ Jump height -------------------------------------------------------

if (file.exists("R/bda/bm/ftp_jump.RData")) {
  load("R/bda/bm/ftp_jump.RData")
} else {
  m_a2 <- brm(
    formula = bf(mvbind(ftp_z, w_kg_z) ~ sj_altura_mt_z + cmj_altura_mt_z + aba_altura_mt_z) + set_rescor(TRUE),
    data = cyclist,
    family = gaussian(),
    prior = b_prior,
    chains = 5, iter = 12e3,
    warmup = 2e3, cores = 5,
    seed = 1234)
  save(m_a2, file = "R/bda/bm/ftp_jump.RData")
}

tbl_a2 <- describe_posterior(
  posteriors = m_a2,
  ci_method = "hdi",
  test = c("pd", "p_significance", "rope", "bf"),
  rope_range = c(-.1, .1))
save(tbl_a2, file = "docs/manuscript/tables/summary_ftp_jump.RData")

# EMG ~ FTP ---------------------------------------------------------------

if (file.exists("R/bda/bm/ftp_emg.RData")) {
  load("R/bda/bm/ftp_emg.RData")
} else {
  m_b2 <- brm(
    formula = bf(mvbind(ftp_z, w_kg_z) ~ median_emg) + set_rescor(TRUE),
    data = cyclist,
    family = gaussian(),
    prior = b_prior,
    chains = 5, iter = 12e3,
    warmup = 2e3, cores = 5,
    seed = 1234)
  save(m_b2, file = "R/bda/bm/ftp_emg.RData")
}

tbl_b2 <- describe_posterior(
  posteriors = m_b2,
  ci_method = "hdi",
  test = c("pd", "p_significance", "rope", "bf"),
  rope_range = c(-.1, .1))
save(tbl_b2, file = "docs/manuscript/tables/summary_ftp_emg.RData")

# EMG ~ Jump + FTP + AUI --------------------------------------------------

if (file.exists("R/bda/bm/ftp_full_adj.RData")) {
  load("R/bda/bm/ftp_full_adj.RData")
} else {
  m_c2 <- brm(
    formula = bf(mvbind(ftp_z, w_kg_z) ~ sj_altura_mt_z + cmj_altura_mt_z + aba_altura_mt_z + median_emg) + set_rescor(TRUE),
    data = cyclist,
    family = gaussian(),
    prior = b_prior,
    control = list(adapt_delta = .999, max_treedepth = 100),
    chains = 5, iter = 12e3,
    warmup = 2e3, cores = 5,
    seed = 1234)
  save(m_c2, file = "R/bda/bm/ftp_full_adj.RData")
}

tbl_c2 <- describe_posterior(
  posteriors = m_c2,
  ci_method = "hdi",
  test = c("pd", "p_significance", "rope", "bf"),
  rope_range = c(-.1, .1))
save(tbl_c2, file = "docs/manuscript/tables/summary_ftp_full_adj.RData")
