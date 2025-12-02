library(readxl)
parse_connect_xlsx = function(file) {
  type = tools::file_ext(file)
  if (type == "csv") {
    body = read.csv(input$file1$datapath, skip = 1) |>
      select(-contains("%"))
  } else {
    body = readxl::read_excel(input$file1$datapath, skip = 1) |>
      select(-contains("%"))
  }
  header = names(body)
  ellipses = grepl("[0-9]", header)

  if (sum(ellipses) > 2) {
    header2 = c(
      paste0(
        "crosstab",
        seq(from = 1, by = 1, length.out = (sum(ellipses) - 2))
      ),
      "source",
      header[!ellipses],
      "Total People"
    )
  } else {
    header2 = c(
      "source",
      header[!ellipses],
      "Total People"
    )
  }
  names(body) = header2
  return(body)
}
