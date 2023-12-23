library(data.table)
library(bayesplot)
library(patchwork)
library(brms)

color_scheme_set("red")
load("R/bda/bm/ftp_full_adj.RData")

# Height or Heigth
# Weight or Weigth
posterior <- as.data.frame(m_c2)[, 3:10]
names(posterior) <- c("SJ Height [Power]", "CMJ Height [Power]", "Abalakov Height [Power]", "Median EMG [Power]",
                      "SJ Height [WAP]", "CMJ Height [WAP]", "Abalakov Height [WAP]", "Median EMG [WAP]")
fig_2a <- mcmc_areas(posterior, area_method = "equal height", prob_outer = .99) +
  ggplot2::labs(x = "Linear effect on Power and WAP\nZ-score by one SD increase in the\nindependent variable")


ggplot2::ggsave(filename = "docs/manuscript/figures/fig-2a.pdf", plot = fig_2a,
                device = "pdf", width = 4, height = 6)

set.seed(123)
fig_2b <- ppc_dens_overlay(
  y = as.numeric(m_c2$data$ftp_z),
  yrep = posterior_predict(m_c2, ndraws = 90)[,,"ftpz"]
) +
  ggplot2::theme(legend.position = "top") +
  ggplot2::scale_x_continuous() +
  ggplot2::labs(x = "Power [Z-score]", y = "Density")

ggplot2::ggsave(filename = "docs/manuscript/figures/fig-2b.pdf", plot = fig_2b,
                device = "pdf", width = 4, height = 6)

set.seed(123)
fig_2c <- ppc_dens_overlay(
  y = as.numeric(m_c2$data$w_kg_z),
  yrep = posterior_predict(m_c2, ndraws = 90)[,,"wkgz"]
) +
  ggplot2::theme(legend.position = "top") +
  ggplot2::scale_x_continuous() +
  ggplot2::labs(x = "WAP [Z-score]", y = "Density")

ggplot2::ggsave(filename = "docs/manuscript/figures/fig-2c.pdf", plot = fig_2c,
                device = "pdf", width = 4, height = 6)

fig_2 <- fig_2a + (fig_2b / fig_2c) +
  plot_annotation(tag_levels = "A")

ggplot2::ggsave(filename = "docs/manuscript/figures/fig-2.pdf", plot = fig_2,
                device = "pdf", width = 8, height = 8)
