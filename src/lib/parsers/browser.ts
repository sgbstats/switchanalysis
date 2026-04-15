import readXlsxFile from "read-excel-file/browser";
import { matrixToWorkbook } from "@/lib/parsers/matrix";
import type { ParsedCell } from "@/shared/types";

function decodeHtml(buffer: ArrayBuffer) {
  const decoders = ["utf-8", "utf-16le", "windows-1252"];
  for (const encoding of decoders) {
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
  const doc = new DOMParser().parseFromString(html.replace(/\u0000/g, ""), "text/html");
  const table = doc.querySelector("table");
  if (!table) throw new Error("Could not find a table in the uploaded .xls file.");
  return Array.from(table.querySelectorAll("tr")).map((row) =>
    Array.from(row.querySelectorAll("th, td")).map((cell) => cell.textContent?.trim() ?? null),
  );
}

export async function parseSwitchFile(file: File) {
  const extension = file.name.split(".").pop()?.toLowerCase();

  if (extension === "xlsx") {
    const workbook = await readXlsxFile(file);
    const firstSheet = workbook[0]?.data;
    if (!firstSheet) throw new Error("The uploaded workbook does not contain any sheets.");
    return matrixToWorkbook(firstSheet);
  }

  if (extension === "xls") {
    const buffer = await file.arrayBuffer();
    return matrixToWorkbook(htmlTableToMatrix(decodeHtml(buffer)));
  }

  throw new Error("Please upload an .xlsx or raw Connect .xls export.");
}
