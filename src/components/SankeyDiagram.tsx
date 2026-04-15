"use client";

import { useMemo, useRef } from "react";
import { ResponsiveSankey } from "@nivo/sankey";
import type { SankeyModel } from "@/shared/types";

interface SankeyDiagramProps {
  data: SankeyModel;
  onReady?: (download: () => Promise<void>) => void;
}

async function exportSvg(container: HTMLDivElement) {
  const svg = container.querySelector("svg");
  if (!svg) throw new Error("No plot is available to download yet.");

  const blob = new Blob([new XMLSerializer().serializeToString(svg)], {
    type: "image/svg+xml;charset=utf-8",
  });
  const url = URL.createObjectURL(blob);
  const image = new Image();
  const canvas = document.createElement("canvas");
  canvas.width = 1200;
  canvas.height = 800;

  await new Promise<void>((resolve, reject) => {
    image.onload = () => {
      const context = canvas.getContext("2d");
      if (!context) {
        reject(new Error("Canvas rendering is not available."));
        return;
      }
      context.clearRect(0, 0, canvas.width, canvas.height);
      context.drawImage(image, 0, 0, canvas.width, canvas.height);
      resolve();
    };
    image.onerror = () => reject(new Error("Could not render the plot for download."));
    image.src = url;
  });

  URL.revokeObjectURL(url);
  const link = document.createElement("a");
  link.href = canvas.toDataURL("image/png");
  link.download = "switchanalysis_plot.png";
  link.click();
}

export function SankeyDiagram({ data, onReady }: SankeyDiagramProps) {
  const ref = useRef<HTMLDivElement>(null);
  const chartData = useMemo(
    () => ({
      nodes: data.nodes.map((node) => ({ ...node })),
      links: data.links.map((link) => ({ ...link })),
    }),
    [data],
  );

  useMemo(() => {
    if (!onReady) return;
    onReady(async () => {
      if (!ref.current) return;
      await exportSvg(ref.current);
    });
  }, [onReady]);

  if (!data.hasData) {
    return <p className="muted">Upload a file to see the Sankey diagram.</p>;
  }

  return (
    <div ref={ref} className="sankey-frame">
      <ResponsiveSankey
        data={chartData as never}
        align="justify"
        margin={{ top: 24, right: 220, bottom: 24, left: 220 }}
        colors={{ datum: "color" }}
        nodeOpacity={1}
        nodeThickness={18}
        nodeSpacing={18}
        linkOpacity={0.55}
        enableLinkGradient={false}
        sort="input"
        label="label"
        labelTextColor="#ffffff"
        linkTooltip={({ link }) => (
          <div className="tooltip-card">
            <strong>
              {String(link.source.id).replace(/^source:/, "")} → {String(link.target.id).replace(/^target:/, "")}
            </strong>
            <div>{Number(link.value).toLocaleString()}</div>
          </div>
        )}
        nodeTooltip={({ node }) => (
          <div className="tooltip-card">
            <strong>{String(node.label)}</strong>
          </div>
        )}
      />
    </div>
  );
}
