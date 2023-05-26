
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
if (any(!file.exists("R/bda/bm/isquios_torque_&_emg.RDS",
                     "R/bda/bm/quad_torque_&_emg.RDS"))) {
  stop("Uno o más modelos no han sido generados")
}

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
  sd_torque_quad = median(quad_data$sd_torque_quad))

quad_pred <- predict(
  object = quad_mod,
  newdata = quad_pred,
  probs = c(.005, .025, .1, .25, .75, .9, .975, .995),
  robust = TRUE,
  cores = 5) |>
  cbind(quad_pred) |>
  as.data.table()

## Isquiotibiales
isqt_pred <- data.frame(
  emg_mean_1 = seq(from = min(cyclist$emg_mean_1),
                   to = max(cyclist$emg_mean_1),
                   length.out = 10),
  sd_torque_isquio = median(isqt_data$sd_torque_isquio))

isqt_pred <- predict(
  object = isqt_mod,
  newdata = isqt_pred,
  probs = c(.005, .025, .1, .25, .75, .9, .975, .995),
  robust = TRUE,
  cores = 5) |>
  cbind(isqt_pred) |>
  as.data.table()

# -------------------------------------------------------------------------

scatter_plot = function(pred_data, raw_data, muscle = NULL) {
  if (is.null(muscle)) {
    stop("`muscle` must be one of \"quad\" or \"isquio\"")
  }
  if (grepl("quad|cuad", muscle, TRUE)) {
    ylab = "quadriceps"; point_var = "mean_torque_quad"
  } else if (grepl("hams|isquio", muscle, TRUE)) {
    ylab = "hamstrings"; point_var = "mean_torque_isquio"
  } else stop("`muscle` must be one of \"quad\" or \"isquio\"")

  ggplot(pred_data, aes(emg_mean_1, Estimate)) +
    geom_ribbon(aes(ymin = Q0.5, ymax = Q99.5, fill = "99%")) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = "95%")) +
    geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = "80%")) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = "50%")) +
    scale_fill_manual(values = `names<-`(RColorBrewer::brewer.pal(4, "Blues"), c("99%", "95%", "80%", "50%"))) +
    geom_line() +
    geom_point(data = raw_data, aes(emg_mean_1, .data[[point_var]])) +
    #scale_y_continuous(n.breaks = 6, limits = c(0, NA)) +
    scale_x_continuous(#limits = c(0, NA),
                       labels = function(x) scales::percent(x/100),
                       n.breaks = 4) +
    labs(x = "Mean EMG [%]", y = paste("Mean", ylab, "torque [Nm]"), fill = "CI") +
    see::theme_modern()
}

p1 <- scatter_plot(quad_pred, quad_data, "quad")
q1 <- scatter_plot(isqt_pred, isqt_data, "isquio")

## Segundo gráfico -----

trace_plot = function(model, muscle = NULL) {
  if (is.null(muscle)) {
    stop("`muscle` must be one of \"quad\" or \"isquio\"")
  }
  ylab <- ifelse(test = grepl("quad|cuad", muscle, TRUE),
                 yes = "quadriceps", no = "hamstrings")

  post_dist <- as.data.table(x = tidy_draws(model))
  hline_data <- posterior_summary(x = model,
                                  variable = "b_emg_mean_1")

  ggplot(post_dist, aes(.iteration, b_emg_mean_1, col = as.factor(.chain))) +
    geom_line() +
    labs(x = "Iterations (thousands)",
         y = paste("Estimated linear effect of\nEMG on", ylab, "torque"),
         col = "Chain") +
    geom_ysidehistogram(aes(fill = as.factor(.chain)), bins = 100,
                        show.legend = FALSE) +
    geom_hline(yintercept = hline_data[,"Estimate"]) +
    geom_hline(yintercept = hline_data[,c("Q2.5","Q97.5")], lty = 2) +
    scale_x_continuous(expand = c(0,0),
                       labels = function(i) i/1000) +
    scale_y_continuous(n.breaks = 6) +
    scale_ysidex_continuous(expand = c(0,0), breaks = NULL, labels = NULL) +
    scale_color_brewer(aesthetics = c("colour", "fill")) +
    see::theme_modern() +
    theme(ggside.panel.scale = 1/3)
}

p2 <- trace_plot(quad_mod, "quad")
q2 <- trace_plot(isqt_mod, "isquios")

## Tercer gráfico ----

convergence_plots = function(model, muscle) {
  if (is.null(muscle)) {
    stop("`muscle` must be one of \"quad\" or \"isquio\"")
  }
  ylab <- ifelse(test = grepl("quad|cuad", muscle, TRUE),
                 yes = "quadriceps", no = "hamstrings")

  post_dist <- as.data.table(x = tidy_draws(model))
  post_dist[j = cumulative_emg_mean_1 := cumsum(b_emg_mean_1) / seq_along(b_emg_mean_1),
            by = list(.chain)]
  post_dist <- post_dist[, list(
    emg_mean = cumulative_emg_mean_1,
    iter = .iteration,
    chain = as.factor(.chain)
  )]

  hline_data <- posterior_summary(x = model,
                                  variable = "b_emg_mean_1")

  ggplot(post_dist, aes(iter, emg_mean)) +
    geom_line(aes(col = chain)) +
    labs(x = "Iterations (log scale)", y = paste("Estimated linear effect of\nEMG on", ylab, "torque"),
         col = "Chain") +
    scale_color_brewer() +
    scale_x_continuous(trans = "log10",
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    geom_hline(yintercept = hline_data[,"Estimate"], lty = 2) +
    see::theme_modern()
}

p3 <- convergence_plots(quad_mod, "quad")
q3 <- convergence_plots(isqt_mod, "isquios")

plots <- (guide_area() / (((p1 + p3) / p2) | ((q1 + q3) / q2)))

plots <- plots +
  plot_layout(guides = "collect", heights = c(1,9)) +
  plot_annotation(tag_levels = list(c("A"), "1")) &
  theme(legend.position = "top")

ggsave("manuscript/figures/torque_&_emg.pdf", plots, "pdf", width = 12, height = 8, units = "in")
ggsave("manuscript/figures/torque_&_emg.jpeg", plots, "jpeg", width = 12, height = 8, units = "in", dpi = 300)
