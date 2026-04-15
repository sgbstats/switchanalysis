"use client";

import { WEIGHTED_PARTIES } from "@/shared/constants";
import type { AssumptionWeights } from "@/shared/types";

interface AssumptionsPanelProps {
  draft: AssumptionWeights;
  active: AssumptionWeights;
  onDraftChange: (value: AssumptionWeights) => void;
  onApply: () => void;
  onDownload: () => void;
}

export function AssumptionsPanel({ draft, active, onDraftChange, onApply, onDownload }: AssumptionsPanelProps) {
  return (
    <section className="assumptions-card">
      <h3>Previous result</h3>
      <p className="muted">
        Add your weights/previous results here. Use percentages from the last election and approximately the proportion of new voters in the last column. Weights do not need to sum to 100%.
      </p>
      <div className="assumptions-grid">
        {WEIGHTED_PARTIES.map((party) => (
          <label key={party} className="number-field">
            <span>{party === "Unaligned and No Data" ? "New voters" : party}</span>
            <input
              type="number"
              min="0"
              step="0.1"
              value={draft[party] ?? 0}
              onChange={(event) =>
                onDraftChange({
                  ...draft,
                  [party]: Number(event.target.value),
                })
              }
            />
            <small>Active weight: {(active[party] ?? 0).toFixed(2)}</small>
          </label>
        ))}
      </div>
      <div className="actions-row">
        <button type="button" className="primary-button" onClick={onApply}>
          Update Assumptions
        </button>
        <button type="button" className="secondary-button" onClick={onDownload}>
          Download plot
        </button>
      </div>
    </section>
  );
}
