import readXlsxFile from "read-excel-file/browser";
import { matrixToWorkbook } from "@/lib/parsers/matrix";
import type { ParsedCell } from "@/shared/types";

function normalizeSheetCell(value: unknown): ParsedCell {
  if (value === null || value === undefined) return null;
  if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
    return value;
  }
  if (value instanceof Date) return value.toISOString();
  return String(value);
}

function stripTags(value: string) {
  return value
    .replace(/&nbsp;/gi, " ")
    .replace(/<br\s*\/?>/gi, "\n")
    .replace(/<[^>]+>/g, "")
    .trim();
}

function decodeHtml(buffer: ArrayBuffer) {
  // Raw Connect .xls exports are really HTML files; these encodings cover the
  // UTF-8/UTF-16LE and legacy Windows code-page variants we found in sample files.
  for (const encoding of ["utf-8", "utf-16le", "windows-1252"]) {
    try {
      const text = new TextDecoder(encoding).decode(buffer);
      if (/<table|<html/i.test(text.replace(/\u0000/g, ""))) return text;
    } catch {
      continue;
    }
  }
  return new TextDecoder().decode(buffer);
}

export function htmlTableToMatrix(html: string): ParsedCell[][] {
  const normalized = html.replace(/\u0000/g, "");
  const tableMatch = normalized.match(/<table\b[\s\S]*?<\/table>/i);
  if (!tableMatch) throw new Error("Could not find a table in the uploaded .xls file.");

  const rows = tableMatch[0].match(/<tr\b[\s\S]*?<\/tr>/gi);
  if (!rows?.length) throw new Error("The uploaded .xls table does not contain any rows.");

  return rows.map((rowHtml) => {
    const cells = Array.from(rowHtml.matchAll(/<(td|th)\b[^>]*>([\s\S]*?)<\/\1>/gi));
    return cells.map((cell) => stripTags(cell[2]) || null);
  });
}

export async function parseSwitchFile(file: File) {
  const extension = file.name.split(".").pop()?.toLowerCase();

  if (extension === "xlsx") {
    const workbook = await readXlsxFile(file);
    const firstSheet = workbook[0]?.data;
    if (!firstSheet) throw new Error("The uploaded workbook does not contain any sheets.");
    return matrixToWorkbook(firstSheet.map((row) => row.map((cell) => normalizeSheetCell(cell))));
  }

  if (extension === "xls") {
    const buffer = await file.arrayBuffer();
    return matrixToWorkbook(htmlTableToMatrix(decodeHtml(buffer)));
  }

  throw new Error("Please upload an .xlsx or raw Connect .xls export.");
}
