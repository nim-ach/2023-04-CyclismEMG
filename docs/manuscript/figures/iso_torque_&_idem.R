library(data.table)
library(ggplot2)
library(see)
library(brms)

data("cyclist")

anonymize = function(x) {
  as.factor(x = as.integer(x = factor(x = x)))
}

cyclist[, sujetos := anonymize(sujetos)]

plot_data <- melt.data.table(cyclist,
                id.vars = "sujetos",
                measure.vars = list(
                  mean = c(
                    quad_der = "iso_mean_torque_quad_der_raw",
                    quad_izq = "iso_mean_torque_quad_izq_raw",
                    isqt_der = "iso_mean_torque_isquio_der_raw",
                    isqt_izq = "iso_mean_torque_isquio_izq_raw"),
                  sd = c(
                    quad_der = "iso_sd_torque_quad_der_raw",
                    quad_izq = "iso_sd_torque_quad_izq_raw",
                    isqt_der = "iso_sd_torque_isquio_der_raw",
                    isqt_izq = "iso_sd_torque_isquio_izq_raw")
                ))

plot_data[, variable := fcase(variable == 1, "quad_der",
                              variable == 2, "quad_izq",
                              variable == 3, "isqt_der",
                              variable == 4, "isqt_izq")]
plot_data[, side := fcase(grepl("der", variable), "Right",
                          grepl("izq", variable), "Left")]
plot_data[, variable := fcase(grepl("quad", variable), "Quadriceps",
                              grepl("isqt", variable), "Hamstrings")]
plot_data <- droplevels(plot_data[complete.cases(mean, sd)])
plot_data[, ci := sd * qnorm(.975)]

m <- brms::brm(
  bf(mean | se(sd, sigma = TRUE) ~ 1 + side + variable + (1|sujetos)),
  data = plot_data,
  family = gaussian(),
  prior = prior(student_t(3, 0, 5), class = b),
  backend = "cmdstanr",
  control = list(adapt_delta = .99,
                 max_treedepth = 100),
  iter = 12000, chains = 5, warmup = 2000,
  seed = 12345, cores = 5
)

saveRDS(m, file = "R/bda/bm/iso_torque_&_others.RDS")

# lme4::lmer(formula = mean ~ side + variable + (1|sujetos), data = plot_data, REML = T) |>
#   equatiomatic::extract_eq(intercept = T,
#                            mean_separate = T,
#                            index_factors = T,
#                            swap_var_names = c(variable = "Muscle",
#                                               side = "Leg",
#                                               sujetos = "Subject",
#                                               mean = "Torque"),
#                            return_variances = TRUE)


plot_data <- predict(object = m) |>
  cbind(plot_data)

p1 <- ggplot(plot_data, aes(Estimate, reorder(sujetos, mean))) +
  facet_grid(side ~ variable, scales = "free_x") +
  geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5, col = variable),
                  size = 1/3,
                  position = position_nudge(y = .2),
                  show.legend = FALSE) +
  geom_pointrange(aes(xmin = mean - ci, xmax = mean + ci, x = mean, col = variable),
                  size = 1/3, pch = 4,
                  position = position_nudge(y = -.2),
                  show.legend = FALSE) +
  see::theme_modern() +
  scale_x_continuous(n.breaks = 5) +
  scale_y_discrete(labels = function(x) LETTERS[rev(seq_along(x))]) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(col = "Leg", x = "Mean torque [Nm]", y = "Subject ID",
       caption = "o = predicted mean torque\nx = observed mean torque") +
  theme(legend.position = "top",
        axis.title = element_text(size = 8),
        axis.title.x = element_text(margin = margin(5,0,5,0)),
        axis.title.y = element_text(margin = margin(0,5,0,5)),
        axis.text = element_text(size = 8),
        plot.tag = element_text(size = 10))

ggsave("docs/manuscript/figures/iso_torque_&_idem.pdf", p1, "pdf", width = 5, height = 5, units = "in")
ggsave("docs/manuscript/figures/iso_torque_&_idem.jpeg", p1, "jpeg", width = 5, height = 5, units = "in", dpi = 400)
