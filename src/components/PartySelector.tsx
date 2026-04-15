"use client";

import { PARTY_OPTIONS } from "@/shared/constants";
import type { PartyOption } from "@/shared/types";

interface PartySelectorProps {
  value: PartyOption[];
  onChange: (value: PartyOption[]) => void;
}

export function PartySelector({ value, onChange }: PartySelectorProps) {
  const toggle = (party: PartyOption) => {
    onChange(value.includes(party) ? value.filter((entry) => entry !== party) : [...value, party]);
  };

  return (
    <section className="panel-block">
      <div className="label-row">
        <label className="field-label">Parties</label>
        <div className="mini-actions">
          <button type="button" className="link-button" onClick={() => onChange([...PARTY_OPTIONS])}>
            Select all
          </button>
          <button type="button" className="link-button" onClick={() => onChange([])}>
            Clear all
          </button>
        </div>
      </div>
      <div className="stack-sm">
        {PARTY_OPTIONS.map((party) => (
          <label key={party} className="checkbox-row">
            <input type="checkbox" checked={value.includes(party)} onChange={() => toggle(party)} />
            <span>{party}</span>
          </label>
        ))}
      </div>
    </section>
  );
}
