# Dependency map — app/server.R

Purpose
- Document which reactive expressions, reactiveVal, observers and outputs depend on which inputs and on each other for app/server.R.
- Use this to trace reactive invalidation and to find where to edit when behavior or UI doesn't update.

High-level flow (text)
- input\$file1 → base_data_raw() → base_data()
- base_data() → { crosstab_filter_ui, plot_data(), table_data(), debug_text }
- input\$exclude_all → observeEvent → updatePickerInput("source_party") → changes input\$source_party → base_data()
- input\$update_assumptions → observeEvent → assumptions_val() → influences plot_data() when input\$weight is TRUE → sankeyPlot
- input\$expand_columns → table_data() → table
- input\$raw_pc → changes cell formatting in table (renderReactable)

Reactives / reactiveVal
- base_data_raw()
  - Depends on: input\$file1
  - Returns: parsed sheet or NULL; uses parse_connect_xlsx() or parse_connect_crap()
- base_data()
  - Depends on: base_data_raw(), input\$remove_unknowns, input\$source_party
  - Returns: cleaned long-format data frame with pc column; used by plotting and tables
- assumptions_val (reactiveVal)
  - Initial default weights (1s)
  - Updated by: observeEvent(input\$update_assumptions)
- plot_data()
  - Depends on: base_data(), input\$crosstab_filter, input\$weight, assumptions_val()
  - Produces: sankey-ready long frame (with node/next_node, x positions)
- table_data()
  - Depends on: base_data(), input\$expand_columns
  - Produces: wide table used by reactable with Total column

Observers
- observeEvent(input\$exclude_all)
  - Updates input\$source_party via updatePickerInput(session, "source_party", selected = ...)
- observeEvent(input\$update_assumptions)
  - Reads assumption inputs, normalizes weights and sets assumptions_val(df)

Outputs / renderers
- output\$crosstab_filter_ui (renderUI)
  - Depends on: base_data(); creates selectInput("crosstab_filter", ...) when crosstab1 exists
- output\$sankeyPlot (renderPlot)
  - Depends on: plot_data(); builds sankey using geom_sankey and scale_fill_manual
- output\$table (renderReactable)
  - Depends on: table_data(); also reads input\$raw_pc and input\$expand_columns to set column defs and percent formatting
- output\$debug_text (renderPrint)
  - Depends on: base_data(), input\$weight, assumptions_val(); returns base_data() for debugging

Inputs referenced in server.R (ensure UI supplies these)
- file1 (fileInput)
- remove_unknowns (checkboxInput)
- source_party (pickerInput / selectInput)
- exclude_all (checkboxInput)
- crosstab_filter (selectInput created dynamically by crosstab_filter_ui)
- weight (checkboxInput)
- expand_columns (checkboxInput)
- raw_pc (checkboxInput)
- lib_dem_assumption, labour_assumption, conservative_assumption, green_assumption, reform_assumption, independent_assumption, unknown_assumption (numeric inputs)
- update_assumptions (actionButton)

Helper functions / external deps
- parse_connect_xlsx(), parse_connect_crap() — file parsing helpers
- make_long(source, target) — converts counts into sankey long format
- geom_sankey, geom_sankey_label, theme_sankey — plotting helpers (sankey)
- reactable, colDef, colGroup — table rendering
- updatePickerInput() — from shinyWidgets

Quick dependency arrows
- input\$file1 → base_data_raw() → base_data() → { output\$crosstab_filter_ui, plot_data(), table_data(), output\$debug_text }
- input\$exclude_all → observeEvent → input\$source_party → base_data()
- input\$update_assumptions → assumptions_val() → plot_data() (if input\$weight) → sankeyPlot
- input\$expand_columns → table_data() → table
- input\$raw_pc → renderReactable cell formatting

Where to edit
- Sankey ordering/colors: edit plot_data() final_order and scale_fill_manual in output\$sankeyPlot.
- Table columns/pivoting: edit table_data() and columns configuration inside output\$table.
- UI inputs: add missing inputs to ui.R if server references them.

Mermaid dependency diagram
```mermaid
flowchart LR
  subgraph Inputs
    F[file1]
    RN[remove_unknowns]
    SP[source_party]
    EX[exclude_all]
    CF[crosstab_filter]
    W[weight]
    EC[expand_columns]
    RP[raw_pc]
    U[update_assumptions]
    AssInputs[assumption inputs<br/>(lib_dem, labour, conservative,<br/>green, reform, independent, unknown)]
  end

  subgraph Reactives
    BDR[base_data_raw()]
    BD[base_data()]
    AV[assumptions_val()]
    PD[plot_data()]
    TD[table_data()]
  end

  subgraph Observers
    O1[(observeEvent: exclude_all → updatePickerInput)]
    O2[(observeEvent: update_assumptions)]
  end

  subgraph Outputs
    UI[crosstab_filter_ui (renderUI)]
    Sankey[sankeyPlot (renderPlot)]
    TableOut[table (renderReactable)]
    Debug[debug_text (renderPrint)]
  end

  %% input → reactives
  F --> BDR
  BDR --> BD
  RN --> BD
  SP --> BD

  %% observers
  EX --> O1
  O1 --> SP

  AssInputs --> O2
  U --> O2
  O2 --> AV

  %% reactives → other reactives / outputs
  BD --> UI
  BD --> PD
  BD --> TD
  BD --> Debug

  CF --> PD
  W --> PD
  AV --> PD

  PD --> Sankey
  TD --> TableOut
  RP --> TableOut
  EC --> TD

  %% notes
  classDef ext fill:#f8f9fa,stroke:#bbb;
  class Inputs ext;
  class Reactives ext;
  class Observers ext;
  class Outputs ext;