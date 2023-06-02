
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

quad_data <- cyclist[i = !is.na(iso_mean_torque_quad_der_raw),
                     j = list(
                       mean_torque_quad = mean(x = c(iso_mean_torque_quad_der_raw, iso_mean_torque_quad_izq_raw)),
                       sd_torque_quad = mean(x = c(iso_sd_torque_quad_der_raw, iso_sd_torque_quad_izq_raw)),
                       emg_mean_1
                     ),
                     by = sujetos]

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


########################################################
############# Estimación con iso_isquio ################
########################################################


# Ajustamos los modelos ---------------------------------------------------

isqt_data <- cyclist[i = !is.na(iso_mean_torque_isquio_der_raw),
                     j = list(
                       mean_torque_isquio = mean(x = c(iso_mean_torque_isquio_der_raw, iso_mean_torque_isquio_izq_raw)),
                       sd_torque_isquio = mean(x = c(iso_sd_torque_isquio_der_raw, iso_sd_torque_isquio_izq_raw)),
                       emg_mean_1
                     ),
                     by = sujetos]

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
pp_check(model_2, ndraws = 200)  # shows dens_overlay plot by default
pp_check(model_2, type = "error_hist", ndraws = 12)
pp_check(model_2, type = "scatter_avg")
pp_check(model_2, type = "stat_2d")
pp_check(model_2, type = "loo_ribbon")
pp_check(model_2, type = "stat_freqpoly", ndraws = 1e4, binwidth = 2)
