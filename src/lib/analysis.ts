import { DEFAULT_ASSUMPTIONS, PARTY_COLORS, PARTY_ORDER } from "@/shared/constants";
import type {
  AnalysisResult,
  AnalysisSettings,
  AssumptionWeights,
  BaseRow,
  ParsedWorkbook,
  TableGroupRow,
  TableLeafRow,
  WeightedParty,
} from "@/shared/types";
import { toNumber, unique } from "@/lib/utils";

function keyOf(parts: Array<string | number | undefined>) {
  return parts.map((part) => String(part ?? "")).join("||");
}

function ordered(values: string[]) {
  const extras = values.filter((value) => !PARTY_ORDER.includes(value as never)).sort();
  return [...PARTY_ORDER.filter((value) => values.includes(value)), ...extras];
}

export function normalizeSource(value: string) {
  if (/Refused/i.test(value)) return "Unaligned and No Data";
  if (/Not Lib Dem/i.test(value)) return "Not Lib Dem";
  if (/Lib|Prob/i.test(value)) return "Lib Dem";
  if (/Lab/i.test(value)) return "Labour";
  if (/Con/i.test(value)) return "Conservative";
  if (/Green/i.test(value)) return "Green";
  if (/Ref|UKIP|BNP|Nat/i.test(value)) return "Reform";
  if (/Ind/i.test(value)) return "Independent";
  return "Unaligned and No Data";
}

export function normalizeTarget(value: string) {
  if (/Refused/i.test(value)) return "Unknown";
  if (/Not Lib Dem/i.test(value)) return "Not Lib Dem";
  if (/Lib|Prob/i.test(value)) return "Lib Dem";
  if (/Lab/i.test(value)) return "Labour";
  if (/Con/i.test(value)) return "Conservative";
  if (/Green/i.test(value)) return "Green";
  if (/Reform|UKIP|BNP|Nat/i.test(value)) return "Reform";
  if (/Ind/i.test(value)) return "Independent";
  if (/Not Voting/i.test(value)) return "Not Voting";
  return "Unknown";
}

export function normalizeAssumptions(weights: AssumptionWeights) {
  const positive = Object.entries(weights)
    .filter((entry): entry is [WeightedParty, number] => Boolean(entry[1] && entry[1] > 0));

  if (!positive.length) {
    return { ...DEFAULT_ASSUMPTIONS };
  }

  const min = Math.min(...positive.map(([, weight]) => weight));
  return Object.fromEntries(positive.map(([party, weight]) => [party, weight / min]));
}

export function buildBaseRows(workbook: ParsedWorkbook, settings: AnalysisSettings): BaseRow[] {
  const groupTotals = new Map<string, number>();
  const rows: Omit<BaseRow, "id" | "pc">[] = [];

  workbook.rows.forEach((row) => {
    const source1 = String(row.source ?? "");
    const crosstabs = Object.fromEntries(
      workbook.crosstabColumns.map((column) => [column, String(row[column] ?? "")]),
    );

    if ([source1, ...Object.values(crosstabs)].some((value) => /Total People/i.test(value))) return;

    workbook.targetColumns.forEach((target1) => {
      if (/Total People/i.test(target1)) return;
      const source = normalizeSource(source1);
      const target = normalizeTarget(target1);
      if (!settings.sourceParties.includes(source as never)) return;
      if (settings.removeUnknowns && target === "Unknown") return;
      rows.push({
        crosstabs,
        source,
        source1,
        target,
        target1,
        value: toNumber(row[target1]),
      });
    });
  });

  rows.forEach((row) => {
    const key = keyOf([...workbook.crosstabColumns.map((column) => row.crosstabs[column]), row.source]);
    groupTotals.set(key, (groupTotals.get(key) ?? 0) + row.value);
  });

  return rows.map((row, index) => {
    const key = keyOf([...workbook.crosstabColumns.map((column) => row.crosstabs[column]), row.source]);
    const total = groupTotals.get(key) ?? 0;
    return {
      ...row,
      id: `${key}::${row.target1}::${index}`,
      pc: total ? Math.round((1000 * row.value) / total) : 0,
    };
  });
}

export function buildTable(baseRows: BaseRow[], settings: AnalysisSettings) {
  const crosstabColumns = unique(baseRows.flatMap((row) => Object.keys(row.crosstabs)));
  const columnValues = unique(baseRows.map((row) => (settings.expandColumns ? row.target1 : row.target)));
  const tableColumns = settings.expandColumns ? columnValues : ordered(columnValues);
  const leafMap = new Map<string, TableLeafRow>();

  baseRows.forEach((row) => {
    const key = keyOf([...crosstabColumns.map((column) => row.crosstabs[column]), row.source, row.source1]);
    const existing = leafMap.get(key) ?? {
      id: key,
      crosstabs: row.crosstabs,
      source: row.source,
      source1: row.source1,
      values: {},
      total: 0,
    };
    const column = settings.expandColumns ? row.target1 : row.target;
    existing.values[column] = (existing.values[column] ?? 0) + row.value;
    leafMap.set(key, existing);
  });

  const leaves = Array.from(leafMap.values()).map((leaf) => ({
    ...leaf,
    total: tableColumns.reduce((sum, column) => sum + (leaf.values[column] ?? 0), 0),
  }));

  const groupMap = new Map<string, TableGroupRow>();
  leaves.forEach((leaf) => {
    const key = keyOf([...crosstabColumns.map((column) => leaf.crosstabs[column]), leaf.source]);
    const existing = groupMap.get(key) ?? {
      id: key,
      crosstabs: leaf.crosstabs,
      source: leaf.source,
      source1: leaf.source,
      values: {},
      total: 0,
      children: [],
    };
    tableColumns.forEach((column) => {
      existing.values[column] = (existing.values[column] ?? 0) + (leaf.values[column] ?? 0);
    });
    existing.children.push(leaf);
    groupMap.set(key, existing);
  });

  const sourceOrder = ordered(unique(leaves.map((leaf) => leaf.source)));
  const compare = (left: TableLeafRow | TableGroupRow, right: TableLeafRow | TableGroupRow) => {
    for (const column of crosstabColumns) {
      const delta = (left.crosstabs[column] ?? "").localeCompare(right.crosstabs[column] ?? "");
      if (delta !== 0) return delta;
    }
    const leftIndex = sourceOrder.indexOf(left.source);
    const rightIndex = sourceOrder.indexOf(right.source);
    if (leftIndex !== rightIndex) return leftIndex - rightIndex;
    return left.source1.localeCompare(right.source1);
  };

  leaves.sort(compare);
  const groups = Array.from(groupMap.values())
    .map((group) => ({
      ...group,
      children: group.children.sort(compare),
      total: tableColumns.reduce((sum, column) => sum + (group.values[column] ?? 0), 0),
    }))
    .sort(compare);

  return { crosstabColumns, tableColumns, rows: groups };
}

export function buildSankey(baseRows: BaseRow[], settings: AnalysisSettings) {
  const filtered = settings.crosstabFilter
    ? baseRows.filter((row) => row.crosstabs.crosstab1 === settings.crosstabFilter)
    : baseRows;

  const summary = new Map<string, { source: string; target: string; value: number }>();
  filtered.forEach((row) => {
    const key = `${row.source}::${row.target}`;
    const existing = summary.get(key) ?? { source: row.source, target: row.target, value: 0 };
    existing.value += row.value;
    summary.set(key, existing);
  });

  const pairs = Array.from(summary.values());
  if (!pairs.length) return { nodes: [], links: [], hasData: false };

  const sourceTotals = new Map<string, number>();
  pairs.forEach((pair) => sourceTotals.set(pair.source, (sourceTotals.get(pair.source) ?? 0) + pair.value));

  const assumptions = normalizeAssumptions(settings.assumptions);
  const weightedPairs = pairs.map((pair) => {
    const pc = sourceTotals.get(pair.source) ? Math.round((1000 * pair.value) / (sourceTotals.get(pair.source) ?? 1)) : 0;
    return {
      ...pair,
      weighted: pc * (assumptions[pair.source as WeightedParty] ?? 0),
    };
  });

  const links = weightedPairs
    .map((pair) => ({
      source: `source:${pair.source}`,
      target: `target:${pair.target}`,
      value: Math.round(settings.weight ? pair.weighted : pair.value),
      color: PARTY_COLORS[pair.source] ?? "#999999",
    }))
    .filter((link) => link.value > 0);

  if (!links.length) return { nodes: [], links: [], hasData: false };

  const targetTotals = new Map<string, number>();
  weightedPairs.forEach((pair) => {
    targetTotals.set(pair.target, (targetTotals.get(pair.target) ?? 0) + pair.weighted);
  });
  const totalWeighted = Array.from(targetTotals.values()).reduce((sum, value) => sum + value, 0);

  const sourceParties = ordered(unique(links.map((link) => link.source.replace(/^source:/, ""))));
  const targetParties = ordered(unique(links.map((link) => link.target.replace(/^target:/, ""))));

  return {
    hasData: true,
    links,
    nodes: [
      ...sourceParties.map((party) => ({
        id: `source:${party}`,
        side: "source" as const,
        label: party,
        party,
        color: PARTY_COLORS[party] ?? "#999999",
      })),
      ...targetParties.map((party) => ({
        id: `target:${party}`,
        side: "target" as const,
        label:
          settings.showPercentageLabels && totalWeighted > 0
            ? `${party} ${(((targetTotals.get(party) ?? 0) / totalWeighted) * 100).toFixed(1)}%`
            : party,
        party,
        color: PARTY_COLORS[party] ?? "#999999",
      })),
    ],
  };
}

export function analyzeWorkbook(workbook: ParsedWorkbook, settings: AnalysisSettings): AnalysisResult {
  const baseRows = buildBaseRows(workbook, settings);
  const table = buildTable(baseRows, settings);
  const crosstabOptions = unique(baseRows.map((row) => row.crosstabs.crosstab1).filter(Boolean)).sort();
  return {
    crosstabColumns: table.crosstabColumns,
    crosstabOptions,
    tableColumns: table.tableColumns,
    tableRows: table.rows,
    sankey: buildSankey(baseRows, settings),
    baseRows,
  };
}
