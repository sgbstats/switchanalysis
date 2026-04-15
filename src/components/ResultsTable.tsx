"use client";

import { useState } from "react";
import { titleCase } from "@/lib/utils";
import type { TableGroupRow } from "@/shared/types";

interface ResultsTableProps {
  crosstabColumns: string[];
  tableColumns: string[];
  rows: TableGroupRow[];
  showPercentages: boolean;
}

function formatValue(value: number, total: number, showPercentages: boolean) {
  if (!showPercentages) return value.toLocaleString();
  if (!total) return "0.0%";
  return `${((value / total) * 100).toFixed(1)}%`;
}

export function ResultsTable({ crosstabColumns, tableColumns, rows, showPercentages }: ResultsTableProps) {
  const [expanded, setExpanded] = useState<Record<string, boolean>>({});

  if (!rows.length) {
    return <p className="muted">Upload a file to see the table.</p>;
  }

  return (
    <div className="table-shell">
      <div className="mini-actions">
        <button
          type="button"
          className="link-button"
          onClick={() => setExpanded(Object.fromEntries(rows.map((row) => [row.id, true])))}
        >
          Expand all
        </button>
        <button type="button" className="link-button" onClick={() => setExpanded({})}>
          Collapse all
        </button>
      </div>
      <div className="table-scroll">
        <table className="results-table">
          <thead>
            <tr>
              {crosstabColumns.map((column) => (
                <th key={column}>{titleCase(column.replace("crosstab", "Crosstab "))}</th>
              ))}
              <th>Party</th>
              <th>Subgroup</th>
              {tableColumns.map((column) => (
                <th key={column}>{column}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map((group) => {
              const isExpanded = Boolean(expanded[group.id]);
              return (
                <>
                  <tr key={group.id} className="group-row">
                    {crosstabColumns.map((column) => (
                      <td key={column}>{group.crosstabs[column] || "—"}</td>
                    ))}
                    <td>
                      <button
                        type="button"
                        className="toggle-button"
                        onClick={() => setExpanded((current) => ({ ...current, [group.id]: !isExpanded }))}
                      >
                        {isExpanded ? "▾" : "▸"} {group.source}
                      </button>
                    </td>
                    <td>{group.source}</td>
                    {tableColumns.map((column) => (
                      <td key={column}>{formatValue(group.values[column] ?? 0, group.total, showPercentages)}</td>
                    ))}
                  </tr>
                  {isExpanded
                    ? group.children.map((child) => (
                        <tr key={child.id} className="child-row">
                          {crosstabColumns.map((column) => (
                            <td key={column}></td>
                          ))}
                          <td></td>
                          <td>{child.source1}</td>
                          {tableColumns.map((column) => (
                            <td key={column}>{formatValue(child.values[column] ?? 0, child.total, showPercentages)}</td>
                          ))}
                        </tr>
                      ))
                    : null}
                </>
              );
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
}
