import { matrixToWorkbook } from "@/lib/parsers/matrix";

describe("matrixToWorkbook", () => {
  it("creates crosstab, source, target, and total columns from the Connect matrix", () => {
    const workbook = matrixToWorkbook([
      ["Title", null, null, null, null],
      [null, null, "Lib Dem Def", "%", null],
      ["Didsbury East", "Lib Dem Group", 42, 100, 42],
    ]);

    expect(workbook.crosstabColumns).toEqual(["crosstab1"]);
    expect(workbook.targetColumns).toEqual(["Lib Dem Def"]);
    expect(workbook.rows[0]).toEqual({
      crosstab1: "Didsbury East",
      source: "Lib Dem Group",
      "Lib Dem Def": 42,
      "Total People": 42,
    });
  });
});
