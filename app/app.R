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
addResourcePath("images", "images")
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
    "Analysis",
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
            checkboxInput("raw_pc", "Show %", value = F),
            checkboxInput("expand_columns", "Expand Columns", value = FALSE),
            reactableOutput("table")
          ),
          tabPanel(
            "Sankey Plot",
            uiOutput("crosstab_filter_ui"),
            checkboxInput("weight", "Weighted diagram", value = T),
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
        includeMarkdown("switch-guide.qmd"),
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
      "Disclaimer: this tool serves to help you interpret a switch analysis. The Sankey plot does not (currently) constitute a prediction. The responsibility to interpret the data aggregated here is your own."
    ),
    p(a(
      "Licenced under CC BY-NC-SA 4.0",
      href = "https://github.com/sgbstats/switchanalysis/blob/main/LICENSE"
    ))
  )
)


server <- function(input, output, session) {
  output$crosstab_filter_ui <- renderUI({
    sa_raw <- base_data()
    if (!is.null(sa_raw) && "crosstab1" %in% names(sa_raw)) {
      choices <- c("All", unique(sa_raw$crosstab1))
      selectInput("crosstab_filter", "Crosstab 1", choices = choices)
    }
  })
  observeEvent(input$exclude_all, {
    if (input$exclude_all) {
      prs = character(0)
    } else {
      prs = c(
        "Lib Dem",
        "Labour",
        "Conservative",
        "Green",
        "RefUK",
        "Independent",
        "Unaligned and No Data",
        "Not Voting",
        "Not Lib Dem"
      )
    }
    updatePickerInput(session, "source_party", selected = prs)
  })

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
          grepl("Refused", source) ~ "Unaligned and No Data",
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
          grepl("Refused", target) ~ "Unknown",
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
    ) |>
      filter(weight > 0) |>
      mutate(weight = weight / min(weight))

    assumptions_val(df)
  })

  plot_data <- reactive({
    sa_data <- base_data()

    if (!is.null(input$crosstab_filter) && input$crosstab_filter != "All") {
      sa_data <- sa_data |>
        filter(crosstab1 == input$crosstab_filter)
    }

    sa <- sa_data |>
      summarise(
        value = sum(value, na.rm = TRUE),
        pc = sum(pc, na.rm = TRUE),
        .by = c("source", "target")
      ) |>
      drop_na(pc)

    if (is.null(sa)) {
      return(NULL)
    }

    if (input$weight) {
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

    crosstab_cols <- names(sa)[grepl("crosstab", names(sa))]

    if (isTRUE(input$expand_columns)) {
      sa_wide <- sa |>
        select(-pc, -target) |>
        pivot_wider(
          names_from = "target1",
          values_from = "value",
          values_fill = 0
        )
    } else {
      sa_wide <- sa |>
        summarise(
          value = sum(value),
          .by = c("source", "source1", "target", crosstab_cols)
        ) |>
        pivot_wider(
          names_from = "target",
          values_from = "value",
          values_fill = 0
        )
    }

    crosstabs_names_local <- c(crosstab_cols, "source", "source1")
    numeric_cols <-
      setdiff(names(sa_wide), crosstabs_names_local)
    numeric_cols_exist <-
      intersect(numeric_cols, names(sa_wide))

    if (length(numeric_cols_exist) > 0) {
      sa_wide$Total <-
        rowSums(sa_wide[numeric_cols_exist], na.rm = TRUE)
    } else {
      sa_wide$Total <- 0
    }

    return(sa_wide)
  })
  output$table <- renderReactable({
    sa_wide <- table_data()

    if (is.null(sa_wide) || nrow(sa_wide) == 0) {
      return()
    }

    # Define grouping and numeric columns
    crosstabs_names <-
      names(sa_wide)[grepl("crosstab", names(sa_wide))]
    grouping_cols <- c(crosstabs_names, "source", "source1")
    numeric_cols <-
      setdiff(names(sa_wide), c(grouping_cols, "Total"))

    # Define column order
    desired_order <-
      c(
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
    all_nodes <- unique(sa_wide$source)
    other_nodes <- setdiff(all_nodes, desired_order)
    final_order <- unique(c(numeric_cols, desired_order, other_nodes))

    # Base column definitions for grouping columns
    cols_list <- list(
      source = colDef(name = "Party"),
      source1 = colDef(name = "Subgroup")
    )
    for (col in crosstabs_names) {
      cols_list[[col]] <-
        colDef(name = str_to_title(sub("crosstab", "Crosstab ", col)))
    }

    # Define numeric columns
    if (isTRUE(input$raw_pc)) {
      # JS function for percentage cells
      percent_js_func <- JS(
        "function(cellInfo) {
            var total = cellInfo.row.Total;
            if (total === 0 || !total) return '0.0%';
            var pct = (cellInfo.value / total) * 100;
            return pct.toFixed(1) + '%';
        }"
      )

      numeric_col_defs <- lapply(numeric_cols, function(col) {
        colDef(
          cell = percent_js_func,
          aggregated = percent_js_func,
          aggregate = "sum"
        )
      })
      names(numeric_col_defs) <- numeric_cols
      cols_list <- c(cols_list, numeric_col_defs)
      # Hide Total column in percent view
      cols_list$Total <- colDef(show = FALSE, aggregate = "sum")
    } else {
      # For raw view, just ensure they are summed
      numeric_col_defs <-
        lapply(numeric_cols, function(col) {
          colDef(aggregate = "sum")
        })
      names(numeric_col_defs) <- numeric_cols
      cols_list <- c(cols_list, numeric_col_defs)
      # Show Total column in raw view
      cols_list$Total <-
        colDef(show = FALSE, aggregate = "sum")
    }

    # Set up grouping
    group_by_cols <-
      if (isTRUE(input$expand_columns)) {
        setdiff(grouping_cols, "source1")
      } else {
        c(crosstabs_names, "source")
      }

    sa_wide_ordered <- sa_wide |>
      mutate(source = factor(source, levels = final_order)) |>
      select(
        all_of(crosstabs_names),
        source,
        source1,
        Total,
        all_of(numeric_cols)
      )

    reactable(
      sa_wide_ordered,
      groupBy = group_by_cols,
      columns = cols_list,
      columnGroups = list(
        colGroup(name = "Previous Voter ID", columns = c("source", "source1")),
        colGroup(name = "Recent Voter ID", columns = numeric_cols)
      ),
      defaultColDef = colDef(aggregate = "sum")
    )
  })

  output$debug_text <- renderPrint({
    sa <- base_data() |>
      summarise(value = sum(value), pc = sum(pc), .by = c("source", "target"))

    if (is.null(sa)) {
      return(NULL)
    }

    if (input$weight) {
      sa_long <- sa |>
        merge(assumptions_val()) |>
        mutate(pc = pc * weight)
    } else {
      sa_long <- sa |>
        uncount(weights = round(value))
    }

    return(
      base_data()
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
