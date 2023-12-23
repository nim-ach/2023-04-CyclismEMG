library(readxl)
library(data.table)
library(datawizard)

cyclist <- as.data.table(
  x = read_excel(
    path = "data-raw/data-raw.xlsx",
    sheet = "con salto v2",
    range = "A3:AM13",
    col_names = readLines("data-raw/misc/colnames.txt")
  )
)

str(cyclist)

cyclist[,id := rleid(id)][]
cyclist <- standardize(cyclist, append = "_z")

save(cyclist, file = "data/cyclist.RData")
save(cyclist, file = "docs/manuscript/misc/cyclist.RData")
