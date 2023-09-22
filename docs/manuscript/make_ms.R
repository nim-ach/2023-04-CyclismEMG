library(officer)
library(quarto)

quarto_render(input = "docs/manuscript/manuscript.qmd")

read_docx("docs/manuscript/manuscript.docx") |>
  cursor_bookmark(id = "tbl-1") |>
  body_add_docx(src = "docs/manuscript/tables/tbl-1.docx") |>
  print(target = "docs/manuscript/manuscript.docx")

zip::zip(
  zipfile = "docs/manuscript/MS-cyclism-emg.zip",
  files = c(
    list.files("docs/manuscript/figures/",
               pattern = "pdf|jpeg",
               full.names = TRUE),
    "docs/manuscript/tables/tbl-1.docx",
    "docs/manuscript/manuscript.docx"
  ),
  mode = "cherry-pick"
)
