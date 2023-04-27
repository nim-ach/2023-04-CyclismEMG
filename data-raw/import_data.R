library(readxl)
library(data.table)

iso <- as.data.table(
  x = read_excel(
    path = "data-raw/data-raw.xlsx",
    sheet = "ISOCINETICA",
    range = "A4:AU14",
    col_names = readLines("data-raw/aux/iso_titles")
  )
)

salto <- as.data.table(
  x = read_excel(
    path = "data-raw/data-raw.xlsx",
    sheet = "SALTO",
    range = "A4:X14",
    col_names = readLines("data-raw/aux/jump_titles")
  )
)

emg <- as.data.table(
  x = read_excel(
    path = "data-raw/data-raw.xlsx",
    sheet = "EMG",
    range = "A4:J14",
    col_names = readLines("data-raw/aux/emg_titles")
  )
)

emg[sujetos == "Fernando Mallada", sujetos := "Fernando Mellada"]

if (all(identical(iso$sujetos, salto$sujetos),
    identical(emg$sujetos, salto$sujetos),
    identical(iso$sujetos, emg$sujetos))) {
  cyclist <- merge(merge(iso, salto, by = "sujetos"), emg, by = "sujetos")
  rm(emg, iso, salto)
} else stop("Los nombres no coinciden entre almenos un conjunto de datos.\n¡Comprobarlo!")

save(cyclist, file = "data/cyclist.RData")
