
# Preparamos espacio de trabajo -------------------------------------------

## Cargamos paquetes ----

## Manipulación de datos
library(data.table)

## Modelos bayesianos
library(brms) ## Paquete principal
library(bayestestR) ## Obtener parámetros estimados de los modelos
library(tidybayes) ## Extraer iteraciones de las cadenas de markov

## Visualización de los modelos
library(ggplot2) ## Paquete principal
library(ggside) ## Gráficos rotado en el costado de la figura
library(patchwork) ## Para unir los gráficos en uno solo

## Cargamos los datos
data("cyclist")

## Cargamos los modelos ajustados
isqt_mod <- readRDS("R/bda/bm/isquios_torque_&_emg.RDS")
quad_mod <- readRDS("R/bda/bm/quad_torque_&_emg.RDS")

# Creamos objetos y datos intermedios -------------------------------------

## Datos usados para ajustar los modelos ----

quad_data <- ## Cuádriceps
  cyclist[i = !is.na(iso_mean_torque_quad_der_raw),
          j = list(
            mean_torque_quad = mean(x = c(iso_mean_torque_quad_der_raw, iso_mean_torque_quad_izq_raw)),
            sd_torque_quad = mean(x = c(iso_sd_torque_quad_der_raw, iso_sd_torque_quad_izq_raw)),
            emg_mean_1
          ),
          by = sujetos]

isqt_data <- ## Isquiotibiales
  cyclist[i = !is.na(iso_mean_torque_isquio_der_raw),
          j = list(
            mean_torque_isquio = mean(x = c(iso_mean_torque_isquio_der_raw, iso_mean_torque_isquio_izq_raw)),
            sd_torque_isquio = mean(x = c(iso_sd_torque_isquio_der_raw, iso_sd_torque_isquio_izq_raw)),
            emg_mean_1
          ),
          by = sujetos]

## Datos y predicciones generados de los predictores a intervalos regulares ----

## Cuádriceps
quad_pred <- data.frame(
  emg_mean_1 = seq(from = min(cyclist$emg_mean_1),
                   to = max(cyclist$emg_mean_1),
                   length.out = 10),
  sd_torque_quad = median(quad_data$sd_torque_quad)
)

quad_pred <- predict(
  object = model,
  newdata = quad_pred,
  probs = c(0.025, .1, .25, .75, .9, .975),
  robust = TRUE,
  cores = 5
) |>
  cbind(quad_pred) |>
  as.data.table()

## Isquiotibiales


# -------------------------------------------------------------------------



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
