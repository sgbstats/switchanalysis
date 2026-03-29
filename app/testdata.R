input = list()
input$file1$datapath = "C:\\Users\\80010008\\Downloads\\DemographicsNew (1).xls"

#"examples/DW_2022_2025MPID.xls"
input$remove_unknowns = T
input$source_party = c(
  "Lib Dem",
  "Labour",
  "Conservative",
  "Green",
  "Reform",
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
input$pc_label = T
input$crosstab_filter = "Didsbury East"
