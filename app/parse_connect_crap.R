library(XML)
parse_connect_crap = function(file) {
  t2 = readHTMLTable(file)

  header = as.character(as.data.frame(t2["NULL"])[1, ])

  header2 = c("source", header[!is.na(header)], "Total People")

  body = as.data.frame(as.data.frame(t2["NULL"])[-1, ])
  names(body) = header2
  body[, -1] <- lapply(body[, -1], function(x) as.numeric(as.character(x)))

  return(body)
}
