
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
if (any(!file.exists("R/bda/bm/quad_iso_torque_&_abkv_fuerza.RDS",
                     "R/bda/bm/quad_iso_torque_&_abkv_fuerza.RDS",
                     "R/bda/bm/quad_iso_torque_&_cmj_fuerza.RDS",
                     "R/bda/bm/quad_iso_torque_&_cmj_fuerza.RDS",
                     "R/bda/bm/quad_iso_torque_&_sj_fuerza.RDS",
                     "R/bda/bm/quad_iso_torque_&_sj_fuerza.RDS"))) {
  stop("Uno o más modelos no han sido generados")
}

abkv_isqt_mod <- readRDS("R/bda/bm/isquios_iso_torque_&_abkv_fuerza.RDS")
abkv_quad_mod <- readRDS("R/bda/bm/quad_iso_torque_&_abkv_fuerza.RDS")
cmj_isqt_mod <- readRDS("R/bda/bm/isquios_iso_torque_&_cmj_fuerza.RDS")
cmj_quad_mod <- readRDS("R/bda/bm/quad_iso_torque_&_cmj_fuerza.RDS")
sj_isqt_mod <- readRDS("R/bda/bm/isquios_iso_torque_&_sj_fuerza.RDS")
sj_quad_mod <- readRDS("R/bda/bm/quad_iso_torque_&_sj_fuerza.RDS")

# -------------------------------------------------------------------------

scatter_plot = function(model, muscle = NULL) {
  if (is.null(muscle)) {
    stop("`muscle` must be one of \"quad\" or \"isquio\"")
  }
  if (grepl("quad|cuad", muscle, TRUE)) {
    ylab = "quadriceps"; point_var = "mean_torque_quad"; sd_var = "sd_torque_quad"
  } else if (grepl("hams|isquio", muscle, TRUE)) {
    ylab = "hamstrings"; point_var = "mean_torque_isquio"; sd_var = "sd_torque_isquio"
  } else stop("`muscle` must be one of \"quad\" or \"isquio\"")

  if (grepl("abkv", names(model$data[3L]), TRUE)) {
    xlab = "Abalakov peak force [Z score]"
  } else if (grepl("cmj", names(model$data[3L]), TRUE)) {
    xlab = "CMJ peak force [Z score]"
  } else if (grepl("sj", names(model$data[3L]), TRUE)) {
    xlab = "SJ peak force [Z score]"
  }

  pred_data <- data.frame(
    sd_torque = median(model$data[[2L]]),
    x_var = seq(from = min(model$data[[3L]]),
                     to = max(model$data[[3L]]),
                     length.out = 10))

  names(pred_data) <- names(model$data[2:3])

  pred_data <- predict(
    object = model,
    newdata = pred_data,
    probs = c(.005, .025, .1, .25, .75, .9, .975, .995),
    robust = TRUE,
    cores = 5) |>
    cbind(pred_data) |>
    as.data.table()

  x_var = names(pred_data)[ncol(pred_data)]

  ggplot(pred_data, aes(.data[[x_var]], Estimate)) +
    geom_ribbon(aes(ymin = Q0.5, ymax = Q99.5, fill = "99%")) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = "95%")) +
    geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = "80%")) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = "50%")) +
    scale_fill_manual(values = `names<-`(RColorBrewer::brewer.pal(4, "Blues"), c("99%", "95%", "80%", "50%"))) +
    geom_line() +
    geom_point(data = model$data, aes(.data[[x_var]], .data[[point_var]]), inherit.aes = FALSE) +
    geom_linerange(data = model$data, aes(x = .data[[x_var]],
                                        ymin = .data[[point_var]] + .data[[sd_var]] * qnorm(.025),
                                        ymax = .data[[point_var]] + .data[[sd_var]] * qnorm(.975)),
                  inherit.aes = FALSE) +
    #scale_y_continuous(n.breaks = 6, limits = c(0, NA)) +
    #scale_x_continuous(#limits = c(0, NA),
    #                   labels = function(x) scales::percent(x/100),
    #                   n.breaks = 4) +
    labs(x = xlab, y = paste("Mean", ylab, "torque [Nm]"), fill = "CI") +
    see::theme_modern()
}

p1_sj <- scatter_plot(sj_quad_mod, "quad")
q1_sj <- scatter_plot(sj_isqt_mod, "isquio")
p1_cmj <- scatter_plot(cmj_quad_mod, "quad")
q1_cmj <- scatter_plot(cmj_isqt_mod, "isquio")
p1_abkv <- scatter_plot(abkv_quad_mod, "quad")
q1_abkv <- scatter_plot(abkv_isqt_mod, "isquio")

## Segundo gráfico -----

trace_plot = function(model, muscle = NULL) {
  if (is.null(muscle)) {
    stop("`muscle` must be one of \"quad\" or \"isquio\"")
  }
  if (grepl("abkv", names(model$data[3L]), TRUE)) {
    ylab_jump = "Abalakov"
  } else if (grepl("cmj", names(model$data[3L]), TRUE)) {
    ylab_jump = "CMJ"
  } else if (grepl("sj", names(model$data[3L]), TRUE)) {
    ylab_jump = "SJ"
  }
  ylab <- ifelse(test = grepl("quad|cuad", muscle, TRUE),
                 yes = "quadriceps", no = "hamstrings")
  y_var <- paste0("b_", names(model$data)[[3L]])
  post_dist <- as.data.table(x = tidy_draws(model))

  hline_data <- posterior_summary(x = model,
                                  variable = y_var)

  ggplot(post_dist, aes(.iteration, .data[[y_var]], col = as.factor(.chain))) +
    geom_line() +
    labs(x = "Iterations (thousands)",
         y = paste("Estimated linear effect of one\n",ylab_jump,"SD on", ylab, "torque"),
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


p2_sj <- trace_plot(sj_isqt_mod, "isquios")
q2_sj <- trace_plot(sj_quad_mod, "quad")
p2_cmj <- trace_plot(cmj_isqt_mod, "isquios")
q2_cmj <- trace_plot(cmj_quad_mod, "quad")
p2_abkv <- trace_plot(abkv_isqt_mod, "isquios")
q2_abkv <- trace_plot(abkv_quad_mod, "quad")


## Tercer gráfico ----

convergence_plots = function(model, muscle) {
  if (is.null(muscle)) {
    stop("`muscle` must be one of \"quad\" or \"isquio\"")
  }
  if (grepl("abkv", names(model$data[3L]), TRUE)) {
    ylab_jump = "Abalakov"
  } else if (grepl("cmj", names(model$data[3L]), TRUE)) {
    ylab_jump = "CMJ"
  } else if (grepl("sj", names(model$data[3L]), TRUE)) {
    ylab_jump = "SJ"
  }
  ylab <- ifelse(test = grepl("quad|cuad", muscle, TRUE),
                 yes = "quadriceps", no = "hamstrings")
  y_var <- paste0("b_", names(model$data)[[3L]])
  post_dist <- as.data.table(x = tidy_draws(model))
  post_dist[j = cumulative := cumsum(.SD[[y_var]]) / seq_along(.SD[[y_var]]),
            by = list(.chain)]
  post_dist <- post_dist[, list(
    cumulative = cumulative,
    iter = .iteration,
    chain = as.factor(.chain)
  )]

  hline_data <- posterior_summary(x = model,
                                  variable = y_var)

  ggplot(post_dist, aes(iter, cumulative)) +
    geom_line(aes(col = chain)) +
    labs(x = "Iterations (log scale)",
         y = paste("Estimated linear effect of one\n",ylab_jump,"SD on", ylab, "torque"),
         col = "Chain") +
    scale_color_brewer() +
    scale_x_continuous(trans = "log10",
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    geom_hline(yintercept = hline_data[,"Estimate"], lty = 2) +
    see::theme_modern()
}

p3_sj <- convergence_plots(sj_isqt_mod, "isquios")
q3_sj <- convergence_plots(sj_quad_mod, "quad")
p3_cmj <- convergence_plots(cmj_isqt_mod, "isquios")
q3_cmj <- convergence_plots(cmj_quad_mod, "quad")
p3_abkv <- convergence_plots(abkv_isqt_mod, "isquios")
q3_abkv <- convergence_plots(abkv_quad_mod, "quad")

plots <- (
  guide_area() / (
    (((q1_sj + p3_sj) / p2_sj) | ((p1_sj + q3_sj) / q2_sj)) /
    (((q1_cmj + p3_cmj) / p2_cmj) | ((p1_cmj + q3_cmj) / q2_cmj)) /
    (((q1_abkv + p3_abkv) / p2_abkv) | ((p1_abkv + q3_abkv) / q2_abkv))
  )
)

plots <- plots +
  plot_layout(guides = "collect", heights = c(1,9)) +
  plot_annotation(tag_levels = list("I"), tag_suffix = ".") &
  theme(legend.position = "top",
        axis.title = element_text(size = 8),
        axis.title.x = element_text(margin = margin(5,0,5,0)),
        axis.title.y = element_text(margin = margin(0,5,0,5)),
        axis.text = element_text(size = 8),
        plot.tag = element_text(size = 10))

ggsave("docs/manuscript/figures/iso_torque_&_jump.pdf", plots, "pdf", width = 12, height = 15, units = "in")
ggsave("docs/manuscript/figures/iso_torque_&_jump.jpeg", plots, "jpeg", width = 12, height = 15, units = "in", dpi = 400)
