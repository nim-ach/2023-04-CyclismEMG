library(data.table)
library(bayesplot)
library(patchwork)
library(brms)

color_scheme_set("red")
load("R/bda/bm/emg_full_adj.RData")

posterior <- as.data.frame(m_d1)[, 2:7]
names(posterior) <- c("SJ Heigth", "CMJ Height", "Abalakov Height",
                      "Power", "WAP", "Arm usage index")
fig_1a <- mcmc_areas(posterior) +
  ggplot2::labs(x = "Linear effect on median EMG activity by one\nSD increase in the independent variable")


ggplot2::ggsave(filename = "docs/manuscript/figures/fig-1a.pdf", plot = fig_1a,
                device = "pdf", width = 4, height = 6)

set.seed(123)
fig_1b <- ppc_dens_overlay(
  y = m_d1$data$median_emg,
  yrep = posterior_predict(m_d1, ndraws = 100)
) +
  ggplot2::theme(legend.position = "top") +
  ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::labs(x = "Median EMG activity", y = "Density")

ggplot2::ggsave(filename = "docs/manuscript/figures/fig-1b.pdf", plot = fig_1b,
                device = "pdf", width = 4, height = 6)

fig_1 <- fig_1a + fig_1b +
  plot_annotation(tag_levels = "A")

ggplot2::ggsave(filename = "docs/manuscript/figures/fig-1.pdf", plot = fig_1,
                device = "pdf", width = 8, height = 6)
