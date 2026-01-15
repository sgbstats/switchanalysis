library(shiny)
ui <- navbarPage(
  "Switch Analysis",
  theme = shinytheme("united"),
  header = tags$head(
    tags$style(HTML(
      "
    .inline-numeric-inputs .form-group.shiny-input-container {
      display: flex;
      align-items: center;
      margin-bottom: 5px;
    }
    .inline-numeric-inputs .control-label {
      flex: 0 0 100px;
      margin-right: 5px;
    }
    .inline-numeric-inputs input[type='number'] {
      flex: 1 1 auto;
      width: auto;
      padding: 2px 6px;
      height: 28px;
    }
  "
    )),
    tags$script(HTML(
      ' 
      $(document).ready(function() {
        var header = $(".navbar > .container-fluid");
        header.append("<div style=\"position: absolute; top: 5px; right: 20px;\"><img src=\"images/Libby_Black.png\" height=\"40px\" style=\"padding: 5px;\"></div>");
      });
    '
    ))
  ),
  tabPanel(
    "Home",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "file1",
          "Choose switch analysis file",
          accept = c(".xlsx", ".xls")
        ),

        checkboxInput(
          "remove_unknowns",
          "Remove Unknowns from Recent MPID",
          value = T
        ),

        pickerInput(
          "source_party",
          "Parties",
          choices = c(
            "Lib Dem",
            "Labour",
            "Conservative",
            "Green",
            "RefUK",
            "Independent",
            "Unaligned and No Data"
          ),
          selected = c(
            "Lib Dem",
            "Labour",
            "Conservative",
            "Green",
            "RefUK",
            "Independent",
            "Unaligned and No Data",
            "Not Voting",
            "Not Lib Dem"
          ),
          multiple = TRUE
        ),
        switchInput(
          "exclude_all",
          "Remove all parties",
          value = FALSE,
          width = "100%"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Tabular",
            checkboxInput("raw_pc", "Show %", value = TRUE),
            checkboxInput("expand_columns", "Expand Columns", value = FALSE),
            reactableOutput("table")
          ),
          tabPanel(
            "Sankey Plot",
            tags$div(
              class = "alert alert-warning",
              style = "margin:0; padding:2px 6px;", # tight alert
              HTML(
                paste0(
                  "Make sure you have read or watched the user guide before interpreting this diagram.",
                  "<button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>"
                )
              )
            ),
            uiOutput("crosstab_filter_ui"),
            checkboxInput("weight", "Weighted diagram", value = TRUE),
            conditionalPanel(
              "input.weight == true",
              tags$h4("Assumptions"),
              div(
                class = "inline-numeric-inputs",
                numericInput("lib_dem_assumption", "Lib Dem", 1),
                numericInput("labour_assumption", "Labour", 1),
                numericInput("conservative_assumption", "Conservatives", 1),
                numericInput("green_assumption", "Green", 1),
                numericInput("reform_assumption", "Reform", 1),
                numericInput("independent_assumption", "Independent", 1),
                numericInput("unknown_assumption", "New voters", 1)
              ),
              actionButton("update_assumptions", "Update Assumptions")
            ),
            plotOutput("sankeyPlot", height = "600px", width = "800px")
          )
        )
      )
    )
  ),
  tabPanel(
    "User Guide",
    tabsetPanel(
      tabPanel(
        "How to use the tool",
        div(
          HTML(
            '<iframe width="560" height="315" src="https://www.youtube.com/embed/vBdH8lQQ4P0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'
          ),
          includeMarkdown("userguide.qmd"),
        )
      ),
      tabPanel(
        "Intro to switch analysis",
        div(
          HTML(
            '<iframe width="560" height="315" src="https://www.youtube.com/embed/eT5ddMXNMFs" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'
          ),
          includeMarkdown("switch-guide.qmd"),
        )
      )
    )
  ),
  tabPanel(
    "Report a bug",
    div(
      p(
        "If you have found a bug either raise an ",
        a(
          href = "https://github.com/sgbstats/switchanalysis",
          "issue on Github"
        ),
        " or fill out the form below."
      ),
      HTML(
        '<div data-tf-live="01KBJ8CNNC7J04THJAX5QH7592"></div><script src="//embed.typeform.com/next/embed.js"></script>'
      )
    )
  ),
  footer = list(
    hr(),
    p(
      "Hosted by Posit. Published and promoted by S Bate on behalf of ALDC all at Unit 2KLM, Beehive Mill, Jersey Street, Manchester, M4 6JG"
    ),
    p(
      "Disclaimer: this tool serves to help you interpret a switch analysis. The Sankey plot does not (currently) constitute a prediction. The responsibility to interpret the data aggregated here is your own. This is currently only valid in England."
    ),
    p(a(
      "Licenced under CC BY-NC-SA 4.0",
      href = "https://github.com/sgbstats/switchanalysis/blob/main/LICENSE"
    ))
  )
)
