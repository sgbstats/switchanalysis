import { render, screen } from "@testing-library/react";
import { vi } from "vitest";

vi.mock("@/lib/parsers/browser", () => ({
  parseSwitchFile: vi.fn(),
}));

import { SwitchAnalysisApp } from "@/components/SwitchAnalysisApp";

describe("SwitchAnalysisApp", () => {
  it("renders the upload prompt and top-level tabs", () => {
    render(<SwitchAnalysisApp userGuide="# Guide" switchGuide="# Switch" />);

    expect(screen.getByRole("heading", { name: /switch analysis/i })).toBeInTheDocument();
    expect(screen.getByLabelText(/choose switch analysis file/i)).toBeInTheDocument();
    expect(screen.getByRole("tab", { name: /user guide/i })).toBeInTheDocument();
    expect(screen.getByText(/upload a connect switch analysis export/i)).toBeInTheDocument();
  });
});
