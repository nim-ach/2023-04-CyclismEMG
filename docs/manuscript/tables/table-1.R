# Preparamos entorno de trabajo -------------------------------------------

## Cargamos paquetes
library(data.table)
library(gtsummary)

## Cargamos datos
data("cyclist")

## Funciones auxiliares
set_label = function(x, label) {
  attr(x, "label") <- label
  return(x)
}

# Preparamos los datos ----------------------------------------------------


## Datos de EMG
emg_data <- melt.data.table(data = cyclist,
                id.vars = "id",
                measure.vars = c("emg_media_1", "emg_mediana_1", "emg_peak_1",
                                 "emg_media_2", "emg_mediana_2", "emg_peak_2",
                                 "emg_media_3", "emg_mediana_3", "emg_peak_3"))
emg_data[, emg := fcase(
  grepl("emg_mediana", variable), "Median",
  grepl("emg_media", variable), "Mean",
  grepl("emg_peak", variable), "Peak"
)]
emg_data[, variable := fcase(
  grepl("_1$", variable), "1/3",
  grepl("_2$", variable), "2/3",
  grepl("_3$", variable), "3/3"
)]
emg_data <- dcast.data.table(emg_data, id + emg ~ variable)
tbl_1_emg <- emg_data[, lapply(.SD[,-1], function(i) {
  k <- ifelse(nchar(round(mean(i))) > 3, 1, 2)
  mu <- round(mean(i), k); sigma <- round(sd(i), k)
  paste0(mu, " (", sigma, ")")
}), keyby = list("EMG" = emg)] |>
  melt(id.vars = 1) |>
  setkey(EMG)
tbl_1_emg <- dcast.data.table(tbl_1_emg, EMG ~ variable)


## Datos de salto
jump_data <- melt.data.table(data = cyclist,
                             id.vars = "id",
                             measure.vars = c("sj_peak_fuerza_n", "sj_altura_mt", "sj_peak_potencia_w",
                                              "cmj_peak_fuerza_n", "cmj_altura_mt", "cmj_peak_potencia_w",
                                              "aba_peak_fuerza_n", "aba_altura_mt", "aba_peak_potencia_w"))
jump_data[, jump := fcase(grepl("sj", variable), "SJ",
                          grepl("cmj", variable), "CMJ",
                          grepl("aba", variable), "Abalakov")]
jump_data[, variable := gsub("^sj_|^cmj_|^aba_", "", variable)]
jump_data <- dcast.data.table(jump_data, id + jump ~ variable)

tbl_1_jump <- jump_data[, lapply(.SD[,-1], function(i) {
  k <- ifelse(nchar(round(mean(i))) > 3, 1, 2)
  mu <- round(mean(i), k); sigma <- round(sd(i), k)
  paste0(mu, " (", sigma, ")")
}), keyby = list("Jump type" = jump)] |>
  melt(id.vars = 1) |>
  transform(
    variable = rep(c("Height (m)", "Peak force (N)", "Peak power (Watts)"), each = 3)
  ) |> setkey(`Jump type`)
names(tbl_1_jump) <- c("Jump type", "Characteristic", "Mean (SD)")
tbl_1_jump <- dcast.data.table(tbl_1_jump, Characteristic ~ `Jump type`)

tbl_merged <- cbind(tbl_1_emg, tbl_1_jump)


# Generamos las tablas ----------------------------------------------------

tbl_1 <- tbl_merged |>
  flextable() |>
  line_spacing(space = .6) |>
  fontsize(size = 8, part = "all") |>
  autofit() |>
  add_header_row(
    values = c("", "FTP Window Measurement", "", "Jump type"),
    colwidths = c(1,3,1,3)
  ) |>
  theme_vanilla() |>
  border_inner_h(border = fp_border_default(color = "gray", width = 0.01), part = "all") |>
  border(j = 4, border.right = fp_border_default(color = "white", width = 4), part = "all") |>
  align(align = "center", part = "all")
save(tbl_1, file = "docs/manuscript/tables/table-1.RData")
