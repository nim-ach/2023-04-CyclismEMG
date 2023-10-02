library(data.table)
data("cyclist")

cyclist[, sapply(cyclist, is.numeric), with = FALSE] |>
  vapply(function(i) shapiro.test(i)$p.value > 0.05, FUN.VALUE = 1)


corr <- correlation::correlation(
  data = cyclist[, sapply(cyclist, is.numeric), with = FALSE],
  p_adjust = "none",
  method = "spearman"
)

cor_names <- (
  signif_cor <- dplyr::filter(.data = corr, p < 0.05) |>
    dplyr::arrange(rho)
  ) |>
  dplyr::select(Parameter1, Parameter2) |>
  as.vector() |>  unlist() |> unname() |> unique()

plot(signif_cor)
