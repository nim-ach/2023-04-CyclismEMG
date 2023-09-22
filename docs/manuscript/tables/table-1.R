# Preparamos entorno de trabajo -------------------------------------------

## Cargamos paquetes
library(data.table)
library(gtsummary)

## Cargamos datos
data("cyclist")

## Funciones auxiliares
anonymize = function(x) {
  as.factor(x = as.integer(x = factor(x = x)))
}

set_label = function(x, label) {
  attr(x, "label") <- label
  return(x)
}

# Preparamos los datos ----------------------------------------------------

## Anonimizamos a los sujetos
cyclist[, sujetos := anonymize(sujetos)][]

## Datos de torque
torque_data <- melt.data.table(cyclist,
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
torque_data[, variable := fcase(variable == 1, "quad_der",
                              variable == 2, "quad_izq",
                              variable == 3, "isqt_der",
                              variable == 4, "isqt_izq")]
torque_data[, side := fcase(grepl("der", variable), "Right",
                          grepl("izq", variable), "Left")]
torque_data[, variable := fcase(grepl("quad", variable), "Quadriceps",
                              grepl("isqt", variable), "Hamstrings")]
torque_data[, `:=`(
  variable = set_label(variable, "Muscle"),
  mean = set_label(mean, "Mean torque"),
  sd = set_label(sd, "SD of torque"),
  side = set_label(side, "Leg")
)]

torque_data <- torque_data[complete.cases(mean, sd), droplevels(.SD)]

## Datos de EMG
emg_data <- cyclist[, c("emg_mean_1", "emg_median_1", "emg_peak_1")]

emg_data[, `:=`(
  emg_mean_1 = set_label(emg_mean_1, "Mean EMG (%)"),
  emg_median_1 = set_label(emg_median_1, "Median EMG (%)"),
  emg_peak_1 = set_label(emg_peak_1, "Peak EMG (%)")
)]

emg_data[which.max(emg_peak_1), emg_peak_1 := emg_peak_1/10][]

## Datos de salto
jump_data <- melt.data.table(data = cyclist,
                             id.vars = "sujetos",
                             measure.vars = c("jump_sj_fuerza_peak", "jump_sj_altura_m", "jump_sj_power_peak",
                                              "jump_cmj_fuerza_peak", "jump_cmj_altura_m", "jump_cmj_power_peak",
                                              "jump_abkv_fuerza_peak", "jump_abkv_altura_m", "jump_abkv_power_peak"))
jump_data[, jump := fcase(grepl("sj", variable), "SJ",
                          grepl("cmj", variable), "CMJ",
                          grepl("abkv", variable), "Abalakov")]
jump_data[, variable := gsub("_sj|_cmj|_abkv", "", variable)]
jump_data <- dcast.data.table(jump_data, sujetos + jump ~ variable)
jump_data[, `:=`(
  jump = set_label(jump, "Jump"),
  jump_altura_m = set_label(jump_altura_m, "Jump height (m)"),
  jump_fuerza_peak = set_label(jump_fuerza_peak, "Jump peak force (N)"),
  jump_power_peak = set_label(jump_power_peak, "Jump power peak (W)")
)]

# Generamos las tablas ----------------------------------------------------

tbl_1_emg <- tbl_summary(data = emg_data,
                         statistic = ~ "{mean} ({sd})",
                         type = ~ "continuous")

## Tabla de torque
tbl_1_torque <- tbl_strata(
  data = torque_data[, -1L],
  strata = variable,
  .tbl_fun =
    ~ .x %>%
    tbl_summary(by = side, missing = "no"),
  .header = "**{strata}**"
)

## Tabla de salto
tbl_1_jump <- tbl_summary(data = jump_data[, -1L],
                          by = "jump",
                          statistic = ~ "{mean} ({sd})",
                          type = ~ "continuous") |>
  modify_spanning_header(all_stat_cols() ~ "**Jump type**")

