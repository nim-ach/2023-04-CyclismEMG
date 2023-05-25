
# Preparamos espacio de trabajo -------------------------------------------

## Cargamos paquetes ----

## Manipulación de datos
library(data.table)

## Modelos bayesianos
library(brms) ## Paquete principal

## Cargamos los datos
data("cyclist")


########################################################
############## Estimación con iso_quad #################
########################################################

# Ajustamos los modelos ---------------------------------------------------

quad_iso_data <- cyclist[!is.na(iso_mean_torque_quad_der_raw), list(
  mean_torque_quad = mean(x = c(iso_mean_torque_quad_der_raw, iso_mean_torque_quad_izq_raw)),
  sd_torque_quad = mean(x = c(iso_sd_torque_quad_der_raw, iso_sd_torque_quad_izq_raw)),
  emg_mean_1
  ), sujetos]

## Vemos los priors por defecto
get_prior(formula = mean_torque_quad | se(sd_torque_quad) ~ emg_mean_1, data = quad_iso_data)

quad_model <- brm(
  ## Especificamos el modelo
  formula = mean_torque_quad | se(sd_torque_quad) ~ emg_mean_1,

  ## Escogemos la distribución a priori de los coeficientes
  prior = prior(prior = student_t(3, 0, 5), class = b),

  ## Datos a usar
  data = quad_iso_data,

  ## Familia de distribución a usar
  family = gaussian(),

  ## Hiperparámetros del modelo
  chains = 5L, iter = 12e3, warmup = 2e3,
  cores = 5L, backend = "cmdstanr",
  save_pars = save_pars(all = TRUE),
  seed = 1234
)

saveRDS(quad_model, file = "R/Bayesian models/models/quad_torque_&_emg.RDS")

## Valoración del ajuste del modelo
pp_check(quad_model, ndraws = 200)  # shows dens_overlay plot by default
pp_check(quad_model, type = "error_hist", ndraws = 12)
pp_check(quad_model, type = "scatter_avg")
pp_check(quad_model, type = "stat_2d")
pp_check(quad_model, type = "loo_ribbon")
pp_check(quad_model, type = "stat_freqpoly", ndraws = 1e4, binwidth = 2)


# Visualización de los resultados -----------------------------------------

## Primer gráfico -----

## Simulación de predictores a intervalos regulares
sim_emg <- seq(from = min(cyclist$emg_mean_1),
               to = max(cyclist$emg_mean_1),
               length.out = 10)

## Simulación de predictores a intervalos regulares
sim_emg <- data.frame(emg_mean_1 = seq(from = min(cyclist$emg_mean_1),
                                       to = max(cyclist$emg_mean_1),
                                       length.out = 10),
                      sd_torque_quad = median(quad_iso_data$sd_torque_quad))


## Usamos el modelos con los predictores simulados
pred_data <- predict(object = model,
                     newdata = sim_emg,
                     probs = c(0.025, .1, .25, .75, .9, .975),
                     robust = TRUE,
                     cores = 5) |>
  cbind(sim_emg) |>
  as.data.table()

## Generamos el primer gráfico
p1 <- ggplot(pred_data, aes(emg_mean_1, Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = "95%")) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = "80%")) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = "50%")) +
  scale_fill_manual(values = `names<-`(RColorBrewer::brewer.pal(3, "Blues"), c("95%", "80%", "50%"))) +
  geom_line() +
  geom_point(data = quad_iso_data, aes(emg_mean_1, mean_torque_quad)) +
  scale_y_continuous(n.breaks = 6, limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(x = "Mean EMG", y = "Mean quadriceps torque", fill = "CI") +
  see::theme_modern()

print(p1)

## Segundo gráfico -----

## Obtenemos los muestreos de las simulaciones de las Cadenas de Markov
post_dist <- tidy_draws(model) |>
  as.data.table()

## Calculamos la media acumulada por cadena para ilustrar la convergencia a la
## solución del modelo
post_dist[j = cumulative_emg_mean_1 := cumsum(b_emg_mean_1) / seq_along(b_emg_mean_1),
          by = list(.chain)]

ribbon_data <- posterior_summary(model, variable = "b_emg_mean_1")

## Generamos el segundo gráfico
p2 <- ggplot(post_dist, aes(.iteration, b_emg_mean_1, col = as.factor(.chain))) +
  geom_line() +
  labs(x = "Iterations",
       y = "Estimated linear effect of\nEMG on quadriceps Torque",
       col = "Chain") +
  geom_ysidehistogram(aes(fill = as.factor(.chain)), bins = 100,
                      show.legend = FALSE) +
  geom_hline(yintercept = ribbon_data[,"Estimate"]) +
  geom_hline(yintercept = ribbon_data[,c("Q2.5","Q97.5")], lty = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(n.breaks = 6) +
  scale_ysidex_continuous(expand = c(0,0), breaks = NULL, labels = NULL) +
  scale_color_brewer(aesthetics = c("colour", "fill")) +
  see::theme_modern() +
  theme(ggside.panel.scale = 1/3)

print(p2)

## Tercer gráfico ----

## Preparamos los datos a usar
plot_chains_data <- post_dist[, list(
    emg_mean = cumulative_emg_mean_1,
    iter = .iteration,
    chain = as.factor(.chain)
  )]

## Estimación del valor central del parámetro modelado
estimated_value <- bayestestR::point_estimate(model, centrality = "median", parameters = "emg")[[2]]

p3 <- ggplot(plot_chains_data, aes(iter, emg_mean)) +
  geom_line(aes(col = chain)) +
  labs(x = "Iterations", y = "Estimated linear effect of\nEMG on quadriceps Torque",
       col = "Chain") +
  scale_color_brewer() +
  scale_x_log10() +
  geom_hline(yintercept = estimated_value, lty = 2) +
  see::theme_modern()

print(p3)

## Unimos los gráficos en uno solo ----

p_plots <- (guide_area() / ((p1 + p3) / p2)) +
  plot_layout(guides = "collect", heights = c(1,9)) &
  theme(legend.position = "top")


########################################################
############# Estimación con iso_isquio ################
########################################################


# Ajustamos los modelos ---------------------------------------------------

isquio_iso_data <- cyclist[!is.na(iso_mean_torque_isquio_der_raw), list(
  mean_torque_isquio = mean(x = c(iso_mean_torque_isquio_der_raw, iso_mean_torque_isquio_izq_raw)),
  sd_torque_isquio = mean(x = c(iso_sd_torque_isquio_der_raw, iso_sd_torque_isquio_izq_raw)),
  emg_mean_1
), sujetos]

## Vemos los priors por defecto
get_prior(formula = mean_torque_isquio | se(sd_torque_isquio) ~ emg_mean_1, data = isquio_iso_data)

model_2 <- brm(
  ## Especificamos el modelo
  formula = mean_torque_isquio | se(sd_torque_isquio) ~ emg_mean_1,

  ## Escogemos la distribución a priori de los coeficientes
  prior = prior(prior = student_t(3, 0, 5), class = b),

  ## Datos a usar
  data = isquio_iso_data,

  ## Familia de distribución a usar
  family = gaussian(),

  ## Hiperparámetros del modelo
  chains = 5L, iter = 12e3, warmup = 2e3,
  cores = 5L, backend = "cmdstanr",
  save_pars = save_pars(all = TRUE),
  seed = 1234
)

saveRDS(model_2, file = "R/Bayesian models/models/isquios_torque_&_emg.RDS")

## Valoración del ajuste del modelo
# pp_check(model_2, ndraws = 200)  # shows dens_overlay plot by default
# pp_check(model_2, type = "error_hist", ndraws = 12)
# pp_check(model_2, type = "scatter_avg")
# pp_check(model_2, type = "stat_2d")
# pp_check(model_2, type = "loo_ribbon")
# pp_check(model_2, type = "stat_freqpoly", ndraws = 1e4, binwidth = 2)


# Visualización de los resultados -----------------------------------------

## Primer gráfico -----

## Simulación de predictores a intervalos regulares
sim_emg <- data.frame(emg_mean_1 = seq(from = min(cyclist$emg_mean_1),
                                       to = max(cyclist$emg_mean_1),
                                       length.out = 10),
                      sd_torque_isquio = median(isquio_iso_data$sd_torque_isquio))

## Usamos el modelos con los predictores simulados
pred_data <- predict(object = model_2,
                     newdata = sim_emg,
                     probs = c(0.025, .1, .25, .75, .9, .975),
                     robust = TRUE,
                     cores = 5) |>
  cbind(sim_emg) |>
  as.data.table()

## Generamos el primer gráfico
q1 <- ggplot(pred_data, aes(emg_mean_1, Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = "95%")) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = "80%")) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = "50%")) +
  scale_fill_manual(values = `names<-`(RColorBrewer::brewer.pal(3, "Blues"), c("95%", "80%", "50%"))) +
  geom_line() +
  geom_point(data = isquio_iso_data, aes(emg_mean_1, mean_torque_isquio)) +
  scale_y_continuous(n.breaks = 6, limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(x = "Mean EMG", y = "Mean Hamstring Torque", fill = "CI") +
  see::theme_modern()

print(q1)

## Segundo gráfico -----

## Obtenemos los muestreos de las simulaciones de las Cadenas de Markov
post_dist <- tidy_draws(model_2) |>
  as.data.table()

## Calculamos la media acumulada por cadena para ilustrar la convergencia a la
## solución del modelo
post_dist[j = cumulative_emg_mean_1 := cumsum(b_emg_mean_1) / seq_along(b_emg_mean_1),
          by = list(.chain)]

ribbon_data <- posterior_summary(model_2, variable = "b_emg_mean_1")

## Generamos el segundo gráfico
q2 <- ggplot(post_dist, aes(.iteration, b_emg_mean_1, col = as.factor(.chain))) +
  geom_line() +
  labs(x = "Iterations",
       y = "Estimated linear effect of\nEMG on hamstring Torque",
       col = "Chain") +
  geom_ysidehistogram(aes(fill = as.factor(.chain)), bins = 100,
                      show.legend = FALSE) +
  geom_hline(yintercept = ribbon_data[,"Estimate"]) +
  geom_hline(yintercept = ribbon_data[,c("Q2.5","Q97.5")], lty = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(n.breaks = 6) +
  scale_ysidex_continuous(expand = c(0,0), breaks = NULL, labels = NULL) +
  scale_color_brewer(aesthetics = c("colour", "fill")) +
  see::theme_modern() +
  theme(ggside.panel.scale = 1/3)

print(q2)

## Tercer gráfico ----

## Preparamos los datos a usar
plot_chains_data <- post_dist[, list(
    emg_mean = cumulative_emg_mean_1,
    iter = .iteration,
    chain = as.factor(.chain)
  )]

## Estimación del valor central del parámetro modelado
estimated_value <- bayestestR::point_estimate(model_2, centrality = "median", parameters = "emg")[[2]]

q3 <- ggplot(plot_chains_data, aes(iter, emg_mean)) +
  geom_line(aes(col = chain)) +
  labs(x = "Iterations", y = "Estimated linear effect of\nEMG on hamstring Torque",
       col = "Chain") +
  scale_color_brewer() +
  scale_x_log10() +
  geom_hline(yintercept = estimated_value, lty = 2) +
  see::theme_modern()

print(q3)

## Unimos los gráficos en uno solo ----

q_plots <- (guide_area() / ((q1 + q3) / q2)) +
  plot_layout(guides = "collect", heights = c(1,9)) &
  theme(legend.position = "top")

(p_plots | q_plots) + plot_layout(guides = "collect")

plots <- (guide_area() / (((p1 + p3) / p2) | ((q1 + q3) / q2)))

plots <- plots +
  plot_layout(guides = "collect", heights = c(1,9)) +
  plot_annotation(tag_levels = list(c("A"), "1")) &
  theme(legend.position = "top")

pdf("")

