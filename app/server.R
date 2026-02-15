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

    sa_data <- sa_raw |>
      select(-any_of(contains("%"))) |>
      select(-any_of(contains("..."))) |>
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

    return(sa_data)
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
        .by = c("source", "target")
      ) |>
      drop_na(value) |>
      mutate(pc = round(1000 * value / sum(value)), .by = c("source"))

    if (is.null(sa)) {
      return(NULL)
    }
    sa_weights <- sa |>
      merge(assumptions_val()) |>
      mutate(pc = pc * weight) |>
      select(-weight) |>
      mutate(pc = replace_na(pc, 0))

    if (input$weight) {
      sa_weight <- sa_weights |> uncount(weights = round(pc))
    } else {
      sa_weight <- sa |>
        uncount(weights = round(value))
    }
    sa_long = sa_weight |> make_long(source, target)
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
    hold = list("df" = df, "sa_weights" = sa_weights)
    return(hold)
  })

  output$sankeyPlot <- renderPlot(
    {
      hold <- plot_data()
      sa_labels = hold[["sa_weights"]] |>
        summarise(pc = sum(pc), .by = target) |>
        filter(pc > 0) |>
        mutate(
          pc = paste0(target, sprintf(" %.1f", pc / sum(pc) * 100), "%")
        ) |>
        mutate(x = "target") |>
        rename(display_label = pc, node = target) |>
        select(node, display_label, x)

      if (is.null(df)) {
        return(NULL)
      }
      av_pc = assumptions_val() |>
        mutate(
          pc = weight / sum(weight),
          # was going to add percentage labels to the source nodes but it looked weird, so just showing the source name instead
          # display_label = paste0(
          #   source,
          #   sprintf(" %.1f", pc / sum(pc) * 100),
          #   "%"
          # )
          display_label = source
        ) |>
        mutate(x = "source") |>
        select("node" = source, display_label, x)

      if (input$pc_label) {
        df <- hold[["df"]] |>
          merge(rbind(av_pc, sa_labels), by = c("node", "x"), sort = F)
      } else {
        df <- hold[["df"]] |>
          mutate(
            display_label = node
          )
      }
      p <- ggplot(
        df,
        aes(
          x = x,
          next_x = next_x,
          node = node,
          next_node = next_node,
          fill = factor(node),
          label = display_label
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
