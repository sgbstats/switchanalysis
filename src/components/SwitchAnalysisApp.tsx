"use client";

import Image from "next/image";
import { useMemo, useState } from "react";
import { analyzeWorkbook, normalizeAssumptions } from "@/lib/analysis";
import { parseSwitchFile } from "@/lib/parsers/browser";
import { AssumptionsPanel } from "@/components/AssumptionsPanel";
import { GuideMarkdown } from "@/components/GuideMarkdown";
import { PartySelector } from "@/components/PartySelector";
import { ResultsTable } from "@/components/ResultsTable";
import { SankeyDiagram } from "@/components/SankeyDiagram";
import { Tabs } from "@/components/Tabs";
import { env } from "@/lib/env";
import { DEFAULT_ASSUMPTIONS, GUIDE_TABS, HOME_TABS, PARTY_OPTIONS, ROOT_TABS } from "@/shared/constants";
import type { AnalysisSettings, AssumptionWeights, ParsedWorkbook } from "@/shared/types";

interface SwitchAnalysisAppProps {
  userGuide: string;
  switchGuide: string;
}

export function SwitchAnalysisApp({ userGuide, switchGuide }: SwitchAnalysisAppProps) {
  const [rootTab, setRootTab] = useState<(typeof ROOT_TABS)[number]>("Home");
  const [homeTab, setHomeTab] = useState<(typeof HOME_TABS)[number]>("Tabular");
  const [guideTab, setGuideTab] = useState<(typeof GUIDE_TABS)[number]>("How to use the tool");
  const [workbook, setWorkbook] = useState<ParsedWorkbook | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [dismissWarning, setDismissWarning] = useState(false);
  const [downloadPlot, setDownloadPlot] = useState<() => Promise<void>>(async () => {});
  const [settings, setSettings] = useState<AnalysisSettings>({
    removeUnknowns: true,
    sourceParties: [...PARTY_OPTIONS],
    crosstabFilter: null,
    weight: true,
    rawPercentages: true,
    expandColumns: false,
    showPercentageLabels: false,
    assumptions: { ...DEFAULT_ASSUMPTIONS },
  });
  const [draftAssumptions, setDraftAssumptions] = useState<AssumptionWeights>({ ...DEFAULT_ASSUMPTIONS });

  const analysis = useMemo(() => {
    if (!workbook) return null;
    return analyzeWorkbook(workbook, settings);
  }, [settings, workbook]);

  const validationMessage = useMemo(() => {
    if (!workbook) return "Upload a Connect switch analysis export to get started.";
    if (!settings.sourceParties.length) return "Select at least one party to show the table and diagram.";
    if (!analysis?.baseRows.length) return "The current filters returned no results.";
    return null;
  }, [analysis?.baseRows.length, settings.sourceParties.length, workbook]);

  async function onFileSelected(file: File | null) {
    if (!file) {
      setWorkbook(null);
      setError(null);
      return;
    }

    setLoading(true);
    setError(null);
    try {
      const parsed = await parseSwitchFile(file);
      setWorkbook(parsed);
      setSettings((current) => ({ ...current, crosstabFilter: null }));
    } catch (caught) {
      setWorkbook(null);
      setError(caught instanceof Error ? caught.message : "Could not read the uploaded file.");
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="page-shell">
      <header className="page-card header-card">
        <div>
          <h1>{env.NEXT_PUBLIC_APP_NAME}</h1>
          <p className="muted">A browser-first TypeScript port of the original R Shiny app.</p>
        </div>
        <Image src="/images/Libby_Black.png" alt="Libby logo" width={82} height={40} priority />
      </header>

      <Tabs tabs={[...ROOT_TABS]} active={rootTab} onChange={(tab) => setRootTab(tab as (typeof ROOT_TABS)[number])} />

      {rootTab === "Home" ? (
        <div className="home-layout">
          <aside className="page-card sidebar-card">
            <section className="panel-block">
              <label className="field-label" htmlFor="switch-file">Choose switch analysis file</label>
              <input
                id="switch-file"
                type="file"
                accept=".xlsx,.xls"
                onChange={(event) => void onFileSelected(event.target.files?.[0] ?? null)}
              />
            </section>

            <label className="checkbox-row">
              <input
                type="checkbox"
                checked={settings.removeUnknowns}
                onChange={(event) => setSettings((current) => ({ ...current, removeUnknowns: event.target.checked }))}
              />
              <span>Remove Unknowns from Recent MPID</span>
            </label>

            <PartySelector
              value={settings.sourceParties}
              onChange={(sourceParties) => setSettings((current) => ({ ...current, sourceParties }))}
            />

            <label className="split-row">
              <span>Remove all parties</span>
              <input
                type="checkbox"
                checked={settings.sourceParties.length === 0}
                onChange={(event) =>
                  setSettings((current) => ({
                    ...current,
                    sourceParties: event.target.checked ? [] : [...PARTY_OPTIONS],
                  }))
                }
              />
            </label>
          </aside>

          <main className="page-card content-card">
            <Tabs tabs={[...HOME_TABS]} active={homeTab} onChange={(tab) => setHomeTab(tab as (typeof HOME_TABS)[number])} />

            {loading ? <div className="notice notice-info">Reading file…</div> : null}
            {error ? <div className="notice notice-error">{error}</div> : null}
            {validationMessage ? <div className="notice notice-warning">{validationMessage}</div> : null}

            {homeTab === "Tabular" ? (
              <div className="stack-lg">
                <div className="inline-controls">
                  <label className="checkbox-row">
                    <input
                      type="checkbox"
                      checked={settings.rawPercentages}
                      onChange={(event) => setSettings((current) => ({ ...current, rawPercentages: event.target.checked }))}
                    />
                    <span>Show %</span>
                  </label>
                  <label className="checkbox-row">
                    <input
                      type="checkbox"
                      checked={settings.expandColumns}
                      onChange={(event) => setSettings((current) => ({ ...current, expandColumns: event.target.checked }))}
                    />
                    <span>Expand Columns</span>
                  </label>
                </div>
                <ResultsTable
                  key={`${settings.expandColumns}-${analysis?.tableRows.length ?? 0}`}
                  crosstabColumns={analysis?.crosstabColumns ?? []}
                  tableColumns={analysis?.tableColumns ?? []}
                  rows={analysis?.tableRows ?? []}
                  showPercentages={settings.rawPercentages}
                />
              </div>
            ) : (
              <div className="stack-lg">
                {!dismissWarning ? (
                  <div className="notice notice-warning notice-dismissible">
                    <span>Make sure you have read or watched the user guide before interpreting this diagram.</span>
                    <button type="button" className="link-button" onClick={() => setDismissWarning(true)}>
                      ×
                    </button>
                  </div>
                ) : null}

                {analysis?.crosstabOptions.length ? (
                  <label className="select-field">
                    <span>Crosstab 1</span>
                    <select
                      value={settings.crosstabFilter ?? "All"}
                      onChange={(event) =>
                        setSettings((current) => ({
                          ...current,
                          crosstabFilter: event.target.value === "All" ? null : event.target.value,
                        }))
                      }
                    >
                      <option value="All">All</option>
                      {analysis.crosstabOptions.map((option) => (
                        <option key={option} value={option}>
                          {option}
                        </option>
                      ))}
                    </select>
                  </label>
                ) : null}

                <div className="inline-controls">
                  <label className="checkbox-row">
                    <input
                      type="checkbox"
                      checked={settings.weight}
                      onChange={(event) => setSettings((current) => ({ ...current, weight: event.target.checked }))}
                    />
                    <span>Weighted diagram</span>
                  </label>
                  <label className="checkbox-row">
                    <input
                      type="checkbox"
                      checked={settings.showPercentageLabels}
                      onChange={(event) =>
                        setSettings((current) => ({ ...current, showPercentageLabels: event.target.checked }))
                      }
                    />
                    <span>Show % on diagram</span>
                  </label>
                </div>

                {settings.weight ? (
                  <AssumptionsPanel
                    draft={draftAssumptions}
                    active={normalizeAssumptions(settings.assumptions)}
                    onDraftChange={setDraftAssumptions}
                    onApply={() =>
                      setSettings((current) => ({
                        ...current,
                        assumptions: normalizeAssumptions(draftAssumptions),
                      }))
                    }
                    onDownload={() => {
                      void downloadPlot();
                    }}
                  />
                ) : null}

                <SankeyDiagram data={analysis?.sankey ?? { nodes: [], links: [], hasData: false }} onReady={setDownloadPlot} />
              </div>
            )}
          </main>
        </div>
      ) : null}

      {rootTab === "User Guide" ? (
        <section className="page-card content-card stack-lg">
          <Tabs tabs={[...GUIDE_TABS]} active={guideTab} onChange={(tab) => setGuideTab(tab as (typeof GUIDE_TABS)[number])} />
          {guideTab === "How to use the tool" ? (
            <div className="guide-layout">
              <iframe
                title="How to use the tool"
                src="https://www.youtube.com/embed/vBdH8lQQ4P0"
                allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
                allowFullScreen
              />
              <article className="markdown-card">
                <GuideMarkdown content={userGuide} />
              </article>
            </div>
          ) : (
            <div className="guide-layout">
              <iframe
                title="Intro to switch analysis"
                src="https://www.youtube.com/embed/eT5ddMXNMFs"
                allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
                allowFullScreen
              />
              <article className="markdown-card">
                <GuideMarkdown content={switchGuide} />
              </article>
            </div>
          )}
        </section>
      ) : null}

      {rootTab === "Report a bug" ? (
        <section className="page-card content-card stack-lg">
          <p>
            If you have found a bug either raise an{" "}
            <a href={env.NEXT_PUBLIC_GITHUB_URL} target="_blank" rel="noreferrer">
              issue on GitHub
            </a>{" "}
            or fill out the form below.
          </p>
          <iframe title="Report a bug" src={env.NEXT_PUBLIC_TYPEFORM_URL} className="bug-frame" />
        </section>
      ) : null}

      <footer className="page-card footer-card">
        <p>Hosted by Posit. Published and promoted by S Bate on behalf of ALDC all at Unit 2KLM, Beehive Mill, Jersey Street, Manchester, M4 6JG</p>
        <p>Disclaimer: this tool serves to help you interpret a switch analysis. The Sankey plot does not currently constitute a prediction. The responsibility to interpret the data aggregated here is your own. This is currently only valid in England.</p>
        <p>
          <a href={`${env.NEXT_PUBLIC_GITHUB_URL}/blob/main/LICENSE`} target="_blank" rel="noreferrer">
            Licenced under CC BY-NC-SA 4.0
          </a>
        </p>
      </footer>
    </div>
  );
}
