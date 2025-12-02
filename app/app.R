library(shiny)
library(tidyverse)
library(readxl)
library(ggsankey)
library(shinyWidgets)
library(flextable)
library(shinythemes)
library(shinyjs)
library(reactable)
options(shiny.autoreload.legacy_warning = FALSE)
source("parse_connect_crap.R")
source("parse_connect_xlsx.R")
ui <- fluidPage(
  tags$head(tags$style(HTML(
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
  ))),
  theme = shinytheme("united"),
  titlePanel("Switch Analysis Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file1",
        "Choose switch analysis file",
        accept = c(".xlsx", ".xls")
      ),
      checkboxInput("raw_pc", "Show %/Weighted diagram", value = F),

      checkboxInput(
        "remove_unknowns",
        "Remove Unknowns from Recent MPID",
        value = F
      ),
      # switchInput(
      #   "exclude_all",
      #   "Remove all parties",
      #   value = FALSE,
      #   width = "100%"
      # ),
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
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Tabular",
          checkboxInput("expand_columns", "Expand Columns", value = FALSE),
          reactableOutput("table")
        ),
        tabPanel(
          "Plot",
          conditionalPanel(
            "input.raw_pc == true",
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
        ),
        tabPanel("Debug", verbatimTextOutput("debug_text"))
      )
    )
  ),
  hr(),
  p(
    "Hosted by Posit. Published and promoted by S Bate on behalf of ALDC all at Unit 2KLM, Beehive Mill, Jersey Street, Manchester, M4 6JG"
  )
)

server <- function(input, output, session) {
  # observe(input$exclude_all, {
  #   current_parties <- input$source_party

  #   if (input$exclude_all) {
  #     prs = c()
  #   } else {
  #     prs = c(
  #       "Lib Dem",
  #       "Labour",
  #       "Conservative",
  #       "Green",
  #       "RefUK",
  #       "Independent",
  #       "Unaligned and No Data"
  #     )
  #   }

  #   updatePickerInput(session, "source_party", selected = prs)
  # })
  base_data_raw <- reactive({
    req(input$file1) # require file to be uploaded

    sa_raw <- tryCatch(
      {
        parse_connect_xlsx(input$file1$datapath)
      },
      error = function(e) {
        tryCatch(
          {
            parse_connect_crap(input$file1$datapath)
          },
          error = function(f) {
            showNotification(
              paste("Error reading Excel file:", f$message),
              type = "error"
            )
            return(NULL)
          }
        )
      }
    )
    return(sa_raw)
  })
  base_data <- reactive({
    sa_raw <- base_data_raw()
    if (is.null(sa_raw)) {
      return(NULL)
    }

    if (input$remove_unknowns) {
      target_remove = c("Unknown")
    } else {
      target_remove = c()
    }
    crosstabs_names = c(
      names(sa_raw)[grepl("crosstab", names(sa_raw))],
      "source"
    )

    sa <- sa_raw |>
      select(-contains("%")) |>
      select(-contains("...")) |>
      pivot_longer(
        cols = -any_of(crosstabs_names),
        names_to = "target",
        values_to = "value"
      ) |>
      filter(
        if_all(everything(), ~ !grepl("Total People", as.character(.)))
      ) |>
      mutate(
        source1 = source,
        target1 = target,
        source = case_when(
          grepl("Not Lib Dem", source) ~ "Not Lib Dem",
          grepl("Lib|Prob", source) ~ "Lib Dem",
          grepl("Lab", source) ~ "Labour",
          grepl("Con", source) ~ "Conservative",
          grepl("Green", source) ~ "Green",
          grepl("Ref|UKIP|BNP|Nat", source) ~ "RefUK",
          grepl("Ind", source) ~ "Independent",
          TRUE ~ "Unaligned and No Data"
        ),
        target = case_when(
          grepl("Not Lib Dem", target) ~ "Not Lib Dem",
          grepl("Lib|Prob", target) ~ "Lib Dem",
          grepl("Lab", target) ~ "Labour",
          grepl("Con", target) ~ "Conservative",
          grepl("Green", target) ~ "Green",
          grepl("Reform|UKIP|BNP|Nat", target) ~ "RefUK",
          grepl("Ind", target) ~ "Independent",
          grepl("Not Voting", target) ~ "Not Voting",
          TRUE ~ "Unknown"
        )
      ) |>
      filter(source %in% input$source_party) |>
      filter(!target %in% target_remove) |>
      mutate(pc = round(1000 * value / sum(value)), .by = crosstabs_names)

    return(sa)
  })

  assumptions_val <- reactiveVal({
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
  })

  observeEvent(input$update_assumptions, {
    req(
      input$lib_dem_assumption,
      input$labour_assumption,
      input$conservative_assumption,
      input$green_assumption,
      input$reform_assumption,
      input$independent_assumption,
      input$unknown_assumption
    )

    df <- data.frame(
      source = c(
        "Lib Dem",
        "Labour",
        "Conservative",
        "Green",
        "Reform",
        "Independent",
        "Unaligned and No Data"
      ),
      weight = c(
        input$lib_dem_assumption,
        input$labour_assumption,
        input$conservative_assumption,
        input$green_assumption,
        input$reform_assumption,
        input$independent_assumption,
        input$unknown_assumption
      )
    )
    assumptions_val(df)
  })

  plot_data <- reactive({
    sa <- base_data() |>
      summarise(value = sum(value), pc = sum(pc), .by = c("source", "target"))

    if (is.null(sa)) {
      return(NULL)
    }

    if (input$raw_pc) {
      sa_long <- sa |>
        merge(assumptions_val()) |>
        mutate(pc = pc * weight) |>
        select(-weight) |>
        uncount(weights = round(pc)) |>
        make_long(source, target)
    } else {
      sa_long <- sa |>
        uncount(weights = round(value)) |>
        make_long(source, target)
    }

    if (nrow(sa_long) == 0) {
      return(NULL)
    }

    desired_order <- c(
      "Lib Dem",
      "Labour",
      "Conservative",
      "Green",
      "RefUK",
      "Independent",
      "Unaligned and No Data",
      "Unknown",
      "Not Voting",
      "Not Lib Dem"
    )

    all_nodes <- unique(sa_long$node)
    other_nodes <- setdiff(all_nodes, desired_order)
    final_order <- rev(c(desired_order, other_nodes))

    df = sa_long |>
      mutate(
        node = factor(node, levels = final_order),
        next_node = factor(next_node, levels = final_order)
      )
    return(df)
  })

  output$sankeyPlot <- renderPlot(
    {
      df <- plot_data()

      if (is.null(df)) {
        return(NULL)
      }

      p <- ggplot(
        df,
        aes(
          x = x,
          next_x = next_x,
          node = node,
          next_node = next_node,
          fill = factor(node),
          label = node
        )
      ) +
        geom_sankey(flow.alpha = 0.5, node.color = 1) +
        geom_sankey_label(linewidth = 0, color = "white") +
        theme_sankey(base_size = 16) +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank()
        ) +
        scale_fill_manual(
          values = c(
            "Lib Dem" = "#ff6400",
            "Labour" = "#E4003B",
            "Conservative" = "#0087DC",
            "Green" = "#00a85a",
            "RefUK" = "#00bed6",
            "Unaligned and No Data" = "#888888",
            "Unknown" = "#888888",
            "Not Voting" = "#444444",
            "Not Lib Dem" = "#444444"
          )
        )

      p
    },
    bg = "transparent"
  )
  table_data <- reactive({
    sa <- base_data()
    if (is.null(sa)) {
      return(NULL)
    }

    sa <- sa |>
      filter(source %in% input$source_party)
    sa <- base_data()
    if (is.null(sa)) {
      return(NULL)
    }

    sa <- sa |>
      filter(source %in% input$source_party)

    col_indicator = case_when(
      isTRUE(input$expand_columns) & input$raw_pc ~ "TT",
      isTRUE(input$expand_columns) ~ "TF",
      input$raw_pc ~ "FT",
      TRUE ~ "FF"
    )

    if (col_indicator == "TT") {
      sa_wide <- sa |>
        select(-value, -target) |>
        mutate(pc = pc / 10) |>
        pivot_wider(names_from = "target1", values_from = "pc")
    } else if (col_indicator == "TF") {
      sa_wide <- sa |>
        select(-pc, -target) |>
        pivot_wider(names_from = "target1", values_from = "value")
    } else if (col_indicator == "FT") {
      sa_wide <- sa |>
        summarise(
          value = sum(value),
          pc = sum(pc),
          .by = c("source", "source1", "target")
        ) |>
        select(-value) |>
        mutate(pc = pc / 10) |>
        pivot_wider(names_from = "target", values_from = "pc")
    } else if (col_indicator == "FF") {
      sa_wide <- sa |>
        summarise(
          value = sum(value),
          pc = sum(pc),
          .by = c("source", "source1", "target")
        ) |>
        select(-pc) |>
        pivot_wider(names_from = "target", values_from = "value")
    }
    return(sa_wide)
  })
  output$table <- renderReactable({
    sa_wide <- table_data()

    desired_order <- c(
      "Lib Dem",
      "Labour",
      "Conservative",
      "Green",
      "RefUK",
      "Independent",
      "Others",
      "Unaligned and No Data",
      "Unknown",
      "Not Voting",
      "Not Lib Dem"
    )
    crosstabs_names = c(
      names(sa_wide)[grepl("crosstab", names(sa_wide))],
      "source",
      "source1"
    )
    all_nodes <- unique(sa_wide$source)
    all_nodes2 <- names(sa_wide)[!sa_wide %in% crosstabs_names]
    other_nodes <- setdiff(all_nodes, desired_order)
    other_nodes2 <- setdiff(all_nodes2, desired_order)
    final_order <- unique(c(other_nodes2, desired_order, other_nodes))

    cols_list <- lapply(crosstabs_names, function(col) {
      new_name <- col
      agg_func <- "unique"

      if (col == "source") {
        new_name <- "Party"
        agg_func <- JS("function() { return '' }")
      } else if (col == "source1") {
        new_name <- "Subgroup"
        agg_func <- JS("function() { return '' }")
      } else if (grepl("crosstab", col)) {
        # e.g. "crosstabx" becomes "Crosstab X"
        new_name <- str_to_title(sub("crosstab", "Crosstab ", col))
      }
      colDef(aggregate = agg_func, name = new_name)
    })

    names(cols_list) = crosstabs_names

    sa_wide |>
      mutate(source = factor(source, levels = final_order)) |>
      select(all_of(c(
        crosstabs_names,
        intersect(final_order, names(sa_wide))
      ))) |>
      reactable(
        groupBy = crosstabs_names[crosstabs_names != "source1"],
        columns = cols_list,
        defaultColDef = colDef(
          aggregate = "sum"
        )
      )
    # flextable() |>
    # font(fontname = "Arial", part = "all") |>
    # set_header_labels(source = "Recent MPID \u2192\nOld MPID \u2193") |>
    # htmltools_value()
  })

  output$debug_text <- renderPrint({
    return(table_data())
  })
}

# Run the app
shinyApp(ui = ui, server = server)
