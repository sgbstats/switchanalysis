library(XML)
parse_connect_crap = function(file) {
  t2 = readHTMLTable(file)

  table_df <- as.data.frame(t2["NULL"])

  if (ncol(table_df) == 0) {
    stop("The parsed HTML table has no columns.")
  }

  header = as.character(table_df[1, ])
  body = as.data.frame(table_df[-1, ])

  if (sum(is.na(header)) > 2) {
    header2 = c(
      paste0(
        "crosstab",
        seq(from = 1, by = 1, length.out = (sum(is.na(header)) - 2))
      ),
      "source",
      header[!is.na(header)],
      "Total People"
    )
  } else {
    header2 = c(
      "source",
      header[!is.na(header)],
      "Total People"
    )
  }

  names(body) = header2
  if (ncol(body) > 1) {
    body[, -1] <- lapply(body[, -1], function(x) as.numeric(as.character(x)))
  }

  return(body)
}
