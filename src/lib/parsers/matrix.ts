import type { ParsedCell, ParsedWorkbook } from "@/shared/types";

function normalizeCell(value: ParsedCell | undefined): ParsedCell {
  if (value === undefined || value === null) return null;
  if (typeof value === "string") {
    const trimmed = value.replace(/\u0000/g, "").trim();
    return trimmed === "" ? null : trimmed;
  }
  return value;
}

function isEmpty(value: ParsedCell) {
  return value === null || value === "";
}

function pad(values: ParsedCell[], size: number) {
  if (values.length >= size) return values.slice(0, size);
  return [...values, ...Array.from({ length: size - values.length }, () => null)];
}

export function matrixToWorkbook(matrix: ParsedCell[][]): ParsedWorkbook {
  const rows = matrix
    .map((row) => row.map((cell) => normalizeCell(cell)))
    .filter((row) => row.some((cell) => !isEmpty(cell)));

  if (rows.length < 3) {
    throw new Error("The uploaded file does not contain enough rows to parse.");
  }

  const headerRow = rows[1];
  const blankCount = headerRow.filter((cell) => isEmpty(cell)).length;
  const crosstabCount = blankCount > 2 ? blankCount - 2 : 0;
  const orderedHeaders = [
    ...Array.from({ length: crosstabCount }, (_, index) => `crosstab${index + 1}`),
    "source",
    ...headerRow.filter((cell) => !isEmpty(cell)).map((cell) => String(cell)),
    "Total People",
  ];

  const columns = orderedHeaders.filter((header) => header !== "%");
  const rowsMapped = rows.slice(2).map((row) => {
    const padded = pad(row, orderedHeaders.length);
    const pairs = orderedHeaders.flatMap((header, index) =>
      header === "%" ? [] : [[header, padded[index] ?? null] as const],
    );
    return Object.fromEntries(pairs);
  });

  return {
    columns,
    crosstabColumns: Array.from({ length: crosstabCount }, (_, index) => `crosstab${index + 1}`),
    targetColumns: columns.filter(
      (column) => !column.startsWith("crosstab") && column !== "source" && column !== "Total People",
    ),
    rows: rowsMapped,
  };
}
