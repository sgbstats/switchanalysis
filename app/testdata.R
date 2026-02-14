input = list()
input$file1$datapath = "examples/DW_2022_2025MPID.xls"
input$remove_unknowns = T
input$source_party = c(
  "Lib Dem",
  "Labour",
  "Conservative",
  "Green",
  "RefUK",
  "Independent",
  "Unaligned and No Data"
)
assumptions_val = function() {
  data.frame(
    source = c(
      "Lib Dem",
      "Labour",
      "Conservative",
      "Green",
      "Reform",
      "Independent",
      "Unaligned and No Data"
    ),
    weight = c(1, 1, 1, 1, 1, 1, 1)
  )
}
input$weight = T
