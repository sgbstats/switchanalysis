import { analyzeWorkbook, normalizeAssumptions } from "@/lib/analysis";
import { DEFAULT_ASSUMPTIONS, PARTY_OPTIONS } from "@/shared/constants";
import type { AnalysisSettings, ParsedWorkbook } from "@/shared/types";

const workbook: ParsedWorkbook = {
  columns: ["source", "Lib Dem Def", "Labour", "Refused", "Total People"],
  crosstabColumns: [],
  targetColumns: ["Lib Dem Def", "Labour", "Refused"],
  rows: [
    {
      source: "Lib Dem Group",
      "Lib Dem Def": 30,
      Labour: 10,
      Refused: 5,
      "Total People": 45,
    },
    {
      source: "Labour Group",
      "Lib Dem Def": 12,
      Labour: 18,
      Refused: 0,
      "Total People": 30,
    },
  ],
};

const settings: AnalysisSettings = {
  removeUnknowns: true,
  sourceParties: [...PARTY_OPTIONS],
  crosstabFilter: null,
  weight: true,
  rawPercentages: true,
  expandColumns: false,
  showPercentageLabels: true,
  assumptions: { ...DEFAULT_ASSUMPTIONS },
};

describe("analyzeWorkbook", () => {
  it("builds table and sankey models that mirror the Shiny transformations", () => {
    const result = analyzeWorkbook(workbook, settings);

    expect(result.baseRows).toHaveLength(4);
    expect(result.tableColumns).toEqual(["Lib Dem", "Labour"]);
    expect(result.tableRows).toHaveLength(2);
    expect(result.sankey.hasData).toBe(true);
    expect(result.sankey.links.map((link) => link.value)).toEqual([750, 250, 400, 600]);
    expect(result.sankey.nodes.find((node) => node.id === "target:Lib Dem")?.label).toContain("57.5%");
  });

  it("normalizes assumptions by the smallest positive value", () => {
    expect(
      normalizeAssumptions({
        ...DEFAULT_ASSUMPTIONS,
        Labour: 4,
        Green: 0,
      }),
    ).toEqual({
      "Lib Dem": 1,
      Labour: 4,
      Conservative: 1,
      Reform: 1,
      Independent: 1,
      "Unaligned and No Data": 1,
    });
  });
});
