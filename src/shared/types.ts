import type { DEFAULT_ASSUMPTIONS, PARTY_OPTIONS, WEIGHTED_PARTIES } from "@/shared/constants";

export type PartyOption = (typeof PARTY_OPTIONS)[number];
export type WeightedParty = (typeof WEIGHTED_PARTIES)[number];
export type AssumptionWeights = Partial<Record<WeightedParty, number>>;
export type ParsedCell = string | number | boolean | Date | typeof Date | null;

export interface ParsedWorkbookRow {
  [key: string]: ParsedCell;
}

export interface ParsedWorkbook {
  columns: string[];
  crosstabColumns: string[];
  targetColumns: string[];
  rows: ParsedWorkbookRow[];
}

export interface AnalysisSettings {
  removeUnknowns: boolean;
  sourceParties: PartyOption[];
  crosstabFilter: string | null;
  weight: boolean;
  rawPercentages: boolean;
  expandColumns: boolean;
  showPercentageLabels: boolean;
  assumptions: AssumptionWeights;
}

export interface BaseRow {
  id: string;
  crosstabs: Record<string, string>;
  source: string;
  source1: string;
  target: string;
  target1: string;
  value: number;
  pc: number;
}

export interface TableLeafRow {
  id: string;
  crosstabs: Record<string, string>;
  source: string;
  source1: string;
  values: Record<string, number>;
  total: number;
}

export interface TableGroupRow {
  id: string;
  crosstabs: Record<string, string>;
  source: string;
  source1: string;
  values: Record<string, number>;
  total: number;
  children: TableLeafRow[];
}

export interface SankeyNodeRecord {
  id: string;
  side: "source" | "target";
  label: string;
  party: string;
  color: string;
}

export interface SankeyLinkRecord {
  source: string;
  target: string;
  value: number;
  color: string;
}

export interface SankeyModel {
  nodes: SankeyNodeRecord[];
  links: SankeyLinkRecord[];
  hasData: boolean;
}

export interface AnalysisResult {
  crosstabColumns: string[];
  crosstabOptions: string[];
  tableColumns: string[];
  tableRows: TableGroupRow[];
  sankey: SankeyModel;
  baseRows: BaseRow[];
}

export type DefaultAssumptions = typeof DEFAULT_ASSUMPTIONS;
