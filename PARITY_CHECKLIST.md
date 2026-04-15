# Parity Checklist

| Shiny feature | TypeScript replacement | Status |
| --- | --- | --- |
| `navbarPage()` with Home / User Guide / Report a bug | Root tabs in `src/components/SwitchAnalysisApp.tsx` | Matched |
| `fileInput("file1")` for `.xlsx` / `.xls` | Browser file input + `parseSwitchFile()` | Matched |
| `checkboxInput("remove_unknowns")` | Controlled checkbox in the sidebar | Matched |
| `pickerInput("source_party", multiple = TRUE)` | Multi-checkbox party selector | Equivalent UX |
| `switchInput("exclude_all")` | Remove-all checkbox that clears/restores selections | Matched |
| `checkboxInput("raw_pc")` | `Show %` toggle for table formatting | Matched |
| `checkboxInput("expand_columns")` | `Expand Columns` toggle for raw target columns | Matched |
| `renderReactable("table")` | Expandable grouped HTML table | Equivalent UX |
| Dynamic `crosstab_filter_ui` | Conditional Crosstab 1 select | Matched |
| `checkboxInput("weight")` | `Weighted diagram` checkbox | Matched |
| `checkboxInput("pc_label")` | `Show % on diagram` checkbox | Matched |
| Assumption numeric inputs + `actionButton("update_assumptions")` | Draft numeric inputs + Update Assumptions button | Matched |
| `renderPlot("sankeyPlot")` with `ggsankey` | Client-side Nivo Sankey chart | Equivalent UX |
| `downloadHandler("download_sankey_plot")` | Browser SVG-to-PNG export | Equivalent UX |
| `includeMarkdown("userguide.qmd")` / `includeMarkdown("switch-guide.qmd")` | Markdown loaded from `app/*.qmd` and rendered with `react-markdown` | Matched |
| YouTube embeds in guide tabs | Same embeds in the Next.js app | Matched |
| GitHub issue link + Typeform embed | Same link + iframe embed | Matched |
| `parse_connect_xlsx()` | `read-excel-file/browser` + `matrixToWorkbook()` | Matched |
| `parse_connect_crap()` | HTML `.xls` parsing with `DOMParser` | Matched |
| `base_data()` | `buildBaseRows()` | Matched |
| `table_data()` | `buildTable()` | Matched |
| `plot_data()` / `sankey_plot()` | `buildSankey()` | Matched |
| `validate()` / `req()` / notifications | Inline validation and error notices | Equivalent UX |
| Shiny server reactivity | React controlled state + memoized derived state | Equivalent architecture |

## Known limitations

1. The Shiny `reactable` widget is replaced with a custom grouped HTML table, so built-in column sorting/resizing are not included.
2. `ggsankey` and `@nivo/sankey` differ slightly in node placement and label layout, so the diagram is close rather than pixel-identical.
3. Plot downloads are generated entirely in the browser instead of a server `downloadHandler`.
4. Optional Next.js API routes are included for parity, but the UI does not require a backend and runs fully in the browser.
