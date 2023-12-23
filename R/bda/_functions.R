
# Convergence graphics ----------------------------------------------------


convergence_plots = function(model, var, varlab = NULL, ylab = NULL) {
  if (missing(var)) {
    stop("`var` must be one of variables present in the model")
  }
  cm <- function(i) cumsum(i) / seq.int(nrow(i))
  post_dist <- data.table::as.data.table(x = brms::as_draws_df(model))
  var <- grep(pattern = var, x = names(post_dist), value = TRUE)
  post_dist[j = cummean := cm(.SD),
            by = list(.chain),
            .SDcols = var]

  hline_data <- brms::posterior_summary(x = model, variable = var)

  ggplot2::ggplot(post_dist, ggplot2::aes(.iteration, cummean)) +
    ggplot2::geom_line(ggplot2::aes(col = as.factor(.chain))) +
    ggplot2::labs(x = "Iterations (log scale)", y = paste("Estimated linear effect of one\n",varlab,"SD on",ylab),
         col = "Chain") +
    ggplot2::scale_color_brewer() +
    ggplot2::scale_x_continuous(trans = "log10",
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    ggplot2::geom_hline(yintercept = hline_data[,"Estimate"], lty = 2) +
    see::theme_modern()
}


# Traceplots --------------------------------------------------------------

trace_plot = function(model, var = NULL, varlab = NULL, ylab = NULL) {
  if (missing(var)) {
    stop("`var` must be one of variables present in the model")
  }

  post_dist <- data.table::as.data.table(x = brms::as_draws_df(model))
  var <- grep(pattern = var, x = names(post_dist), value = TRUE)
  hline_data <- posterior_summary(x = model, variable = var)

  ggplot2::ggplot(post_dist, ggplot2::aes(.iteration, .data[[var]], col = as.factor(.chain))) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Iterations (thousands)",
         y = paste("Estimated linear effect of one\n",varlab,"SD on",ylab),
         col = "Chain") +
    ggside::geom_ysidehistogram(ggplot2::aes(fill = as.factor(.chain)), bins = 100,
                        show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = hline_data[,"Estimate"]) +
    ggplot2::geom_hline(yintercept = hline_data[,c("Q2.5","Q97.5")], lty = 2) +
    ggplot2::scale_x_continuous(expand = c(0,0),
                       labels = function(i) i/1000) +
    ggplot2::scale_y_continuous(n.breaks = 6) +
    ggside::scale_ysidex_continuous(expand = c(0,0), breaks = NULL, labels = NULL) +
    ggplot2::scale_color_brewer(aesthetics = c("colour", "fill")) +
    see::theme_modern() +
    ggplot2::theme(ggside.panel.scale = 1/3)
}

scatter_plot = function(pred_data, raw_data, muscle = NULL) {
  if (is.null(muscle)) {
    stop("`muscle` must be one of \"quad\" or \"isquio\"")
  }
  if (grepl("quad|cuad", muscle, TRUE)) {
    ylab = "quadriceps"; point_var = "mean_torque_quad"; sd_var = "sd_torque_quad"
  } else if (grepl("hams|isquio", muscle, TRUE)) {
    ylab = "hamstrings"; point_var = "mean_torque_isquio"; sd_var = "sd_torque_isquio"
  } else stop("`muscle` must be one of \"quad\" or \"isquio\"")

  ggplot(pred_data, aes(emg_mean_1, Estimate)) +
    geom_ribbon(aes(ymin = Q0.5, ymax = Q99.5, fill = "99%")) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = "95%")) +
    geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = "80%")) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = "50%")) +
    scale_fill_manual(values = `names<-`(RColorBrewer::brewer.pal(4, "Blues"), c("99%", "95%", "80%", "50%"))) +
    geom_line() +
    geom_point(data = raw_data, aes(emg_mean_1, .data[[point_var]]), inherit.aes = FALSE) +
    geom_linerange(data = raw_data, aes(x = emg_mean_1,
                                        ymin = .data[[point_var]] + .data[[sd_var]] * qnorm(.025),
                                        ymax = .data[[point_var]] + .data[[sd_var]] * qnorm(.975)),
                   inherit.aes = FALSE) +
    #scale_y_continuous(n.breaks = 6, limits = c(0, NA)) +
    scale_x_continuous(#limits = c(0, NA),
      labels = function(x) scales::percent(x/100),
      n.breaks = 4) +
    labs(x = "Mean EMG [Z score]", y = paste("Mean", ylab, "torque [Nm]"), fill = "CI") +
    see::theme_modern()
}

# Summary methods ---------------------------------------------------------


report_model = function(x, se = NULL, seed = 1234) {
  p_summary = function(i, se) {
    if (is.null(se)) {
      se = sd(i)
    } else {
      if (length(se) > 1) {
        se = mean(se, na.rm = TRUE)
      }
    }
    ci = bayestestR::hdi(i)
    out = list(Median = median(i),
               SD = sd(i),
               CI = ci$CI,
               CI_Lower = ci$CI_low,
               CI_Upper = ci$CI_high,
               ROPE = bayestestR::rope(i, range = se * c(-1,1))$ROPE_Percentage,
               P_Existence = as.numeric(bayestestR::pd(i)),
               P_Significant = as.numeric(bayestestR::p_significance(i, se)))
    lapply(out, round, 2)
  }

  set.seed(seed)

  b = data.table::as.data.table(x = bayestestR::bayesfactor(x))

  b$BF = round(exp(b$log_BF), 2)
  b$Interpretation = effectsize::interpret_bf(b$BF)
  b = b[,c("Parameter", "BF", "Interpretation")]
  v = b$Parameter
  p = data.table::as.data.table(x)
  p = p[, .SD, .SDcols = c(v, "sigma")]
  p = rbindlist(l = lapply(p, p_summary, se = p$sigma),
                idcol = "Parameter")
  merge(p, b, by = "Parameter")
}

report_text = function(x) {
  out = list()
  out$param = x$Parameter
  out$mean_sd = paste0("$\\beta$ = ", x$Median, ", SE = ", x$SD)
  out$ci = paste0("CI~",x$CI*100,"%~[", x$CI_Lower, ", ", x$CI_Upper, "]")
  out$sexit = paste0("ROPE = ", x$ROPE * 100, "%, p~direction~ = ", x$P_Existence * 100, "%, p~signif~ = ", x$P_Significant * 100, "%")
  out$bf = paste0("BF~10~ = ", x$BF, " (", gsub(" evidence.*", "", x$Interpretation),")")
  out = data.table::as.data.table(out)
  data.table::setkeyv(out, "param")
  out
}


ev.ratio <- function(posterior = NULL, prior = NULL, parameter = NULL, model = NULL, n = 300, seed = 1234) {
  if (is.null(posterior)) posterior <- as.data.frame(model)
  if (is.null(prior)) prior <- as.data.frame(update(m_a1, sample_prior = "only", refresh = 0, seed = seed))
  x_prior <- prior[[parameter]]
  x_post <- posterior[[parameter]]
  f_prior <- approxfun(density(x_prior))
  f_post <- approxfun(density(x_post))
  x_range <- range(x_prior, x_post)
  x_i <- seq(x_range[1], x_range[2], length.out = n)
  er <- data.frame(x = x_i,
                   y = f_post(x_i) / f_prior(x_i))
  na.omit(er)
}

evr.plot <- function(evr) {
  x <- evr$x; y <- evr$y
  plot(x, y, type = "l",
       ylab = "Evidence Ratio (Posterior / Prior)",
       xlab = "Linear effect")
  f_cond <- function(x, y, bf) {
    i_x <- which(y >= bf); i_x_l <- length(i_x)
    data.frame(x = x[i_x],
               y = c(0, y[i_x[-c(1,i_x_l)]], 0))
  }
  polygon(f_cond(x, y, 0), col = RColorBrewer::brewer.pal(4, "Blues")[1])
  polygon(f_cond(x, y, 1/10), col = RColorBrewer::brewer.pal(4, "Blues")[2])
  polygon(f_cond(x, y, 1/3), col = RColorBrewer::brewer.pal(4, "Blues")[3])
  polygon(f_cond(x, y, 1), col = RColorBrewer::brewer.pal(4, "Blues")[4])
  legend(max(x), max(y), legend = c("BF ≥ 0", "BF ≥ 1/10", "BF ≥ 1/3", "BF ≥ 1"),
         fill = RColorBrewer::brewer.pal(4, "Blues"), box.lwd = NA,
         xjust = .95, yjust = .9)
}

evr.ci <- function(evr, bf = 1, k = 2) {
  x <- evr$x; y <- evr$y
  x_r <- round(range(x[y > bf]), digits = k)
  data.frame(estimate = round(x[which.max(y)], digits = k),
             conf.low = x_r[1],
             conf.high = x_r[2],
             BF = bf)
}
