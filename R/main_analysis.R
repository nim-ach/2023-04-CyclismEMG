library(data.table)
data("cyclist")

cyclist[, sapply(cyclist, is.numeric), with = FALSE] |>
  vapply(function(i) shapiro.test(i)$p.value > 0.05, FUN.VALUE = 1) |>
  table()


corr <- correlation::correlation(cyclist[, sapply(cyclist, is.numeric), with = FALSE], p_adjust = "none", method = "spearman")

dplyr::filter(.data = corr, p < 0.05,
              !((Parameter1 %like% "^emg") & (Parameter2 %like% "^emg")),
              !((Parameter1 %like% "^jump_") & (Parameter2 %like% "^jump_")),
              !((Parameter1 %like% "^iso_") & (Parameter2 %like% "^iso_"))) |>
  dplyr::arrange(rho)
