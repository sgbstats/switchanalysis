library(shiny)
library(tidyverse)
library(readxl)
library(ggsankey)
library(shinyWidgets)
library(flextable)
library(shinythemes)
options(shiny.autoreload.legacy_warning = FALSE)
source("parse_connect_crap.R")
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Switch Analysis Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file1",
        "Choose switch analysis file",
        accept = c(".xlsx", ".csv", ".xls")
      ),
      checkboxInput("raw_pc", "Equalise groups as %", value = F),
      checkboxInput(
        "remove_unknowns",
        "Remove Unknowns from Recent MPID",
        value = F
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
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabular", uiOutput("table")),
        tabPanel(
          "Plot",
          plotOutput("sankeyPlot", height = "600px", width = "800px")
        )
      )
    )
  ),
  hr(),
  p(
    "Hosted by Posit. Published and promoted by S Bate on behalf of ALDC all at Unit 2KLM, Beehive Mill, Jersey Street, Manchester, M4 6JG"
  )
)

server <- function(input, output) {
  base_data <- reactive({
    req(input$file1) # require file to be uploaded

    sa_raw <- tryCatch(
      {
        readxl::read_excel(input$file1$datapath, skip = 1) |>
          rename(source = 1)
      },
      error = function(e) {
        tryCatch(
          {
            parse_connect_crap(input$file1$datapath)
          },
          error = function(e) {
            showNotification(
              paste("Error reading Excel file:", e.message),
              type = "error"
            )
            return(NULL)
          }
        )
      }
    )

    if (is.null(sa_raw)) {
      return(NULL)
    }

    if (input$remove_unknowns) {
      target_remove = c("Unknown")
    } else {
      target_remove = c()
    }
    sa <- sa_raw |>
      select(-contains("%")) |>
      select(-contains("...")) |>
      pivot_longer(
        cols = -"source",
        names_to = "target",
        values_to = "value"
      ) |>
      filter(source != "Total People", target != "Total People") |>
      mutate(
        source1 = source,
        target1 = target,
        source = case_when(
          grepl("Lib", source) ~ "Lib Dem",
          grepl("Lab", source) ~ "Labour",
          grepl("Con", source) ~ "Conservative",
          grepl("Green", source) ~ "Green",
          grepl("Ref|UKIP|BNP|Nat", source) ~ "RefUK",
          grepl("Ind", source) ~ "Independent",
          TRUE ~ "Unaligned and No Data"
        ),
        target = case_when(
          grepl("Lib", target) ~ "Lib Dem",
          grepl("Lab", target) ~ "Labour",
          grepl("Con", target) ~ "Conservative",
          grepl("Green", target) ~ "Green",
          grepl("Reform|UKIP|BNP|Nat", target) ~ "RefUK",
          grepl("Ind", target) ~ "Independent",
          grepl("Not Voting", target) ~ "Not Voting",
          grepl("Not Lib Dem", target) ~ "Not Lib Dem",
          TRUE ~ "Unknown"
        )
      ) |>
      summarise(value = sum(value), .by = c("source", "target")) |>
      mutate(pc = round(1000 * value / sum(value)), .by = "source") |>
      filter(!target %in% target_remove)

    return(sa)
  })

  plot_data <- reactive({
    sa <- base_data()

    if (is.null(sa)) {
      return(NULL)
    }
    sa <- sa |>
      filter(source %in% input$source_party)

    if (input$raw_pc) {
      sa_long <- sa |>
        uncount(weights = as.integer(pc)) |>
        make_long(source, target)
    } else {
      sa_long <- sa |>
        uncount(weights = as.integer(value)) |>
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

    sa_long |>
      mutate(
        node = factor(node, levels = final_order),
        next_node = factor(next_node, levels = final_order)
      )
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
  output$table <- renderUI({
    sa <- base_data()
    if (is.null(sa)) {
      return(NULL)
    }

    sa <- sa |>
      filter(source %in% input$source_party)

    if (input$raw_pc) {
      sa_wide <- sa |>
        select(-value) |>
        mutate(pc = pc / 10) |>
        pivot_wider(names_from = "target", values_from = "pc")
    } else {
      sa_wide <- sa |>
        select(-pc) |>
        pivot_wider(names_from = "target", values_from = "value")
    }

    if (nrow(sa_wide) == 0) {
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

    all_nodes <- unique(sa_wide$source)
    other_nodes <- setdiff(all_nodes, desired_order)
    final_order <- rev(c(desired_order, other_nodes))

    sa_wide |>
      mutate(source = factor(source, levels = final_order)) |>
      select(all_of(c(
        "source",
        intersect(rev(final_order), names(sa_wide))
      ))) |>
      flextable() |>
      font(fontname = "Arial", part = "all") |>
      set_header_labels(source = "Recent MPID \u2192\nOld MPID \u2193") |>
      htmltools_value()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
