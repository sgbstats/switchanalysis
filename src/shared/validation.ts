import { z } from "zod";
import { DEFAULT_ASSUMPTIONS, PARTY_OPTIONS, WEIGHTED_PARTIES } from "@/shared/constants";

const parsedCellSchema = z.union([z.string(), z.number(), z.boolean(), z.null()]);

export const parsedWorkbookSchema = z.object({
  columns: z.array(z.string()),
  crosstabColumns: z.array(z.string()),
  targetColumns: z.array(z.string()),
  rows: z.array(z.record(z.string(), parsedCellSchema))
});

export const assumptionsSchema = z.object(
  Object.fromEntries(
    WEIGHTED_PARTIES.map((party) => [party, z.number().finite().nonnegative().optional()])
  ) as Record<(typeof WEIGHTED_PARTIES)[number], z.ZodOptional<z.ZodNumber>>
);

export const settingsSchema = z.object({
  removeUnknowns: z.boolean().default(true),
  sourceParties: z.array(z.enum(PARTY_OPTIONS)).default([...PARTY_OPTIONS]),
  crosstabFilter: z.string().nullable().default(null),
  weight: z.boolean().default(true),
  rawPercentages: z.boolean().default(true),
  expandColumns: z.boolean().default(false),
  showPercentageLabels: z.boolean().default(false),
  assumptions: assumptionsSchema.default(DEFAULT_ASSUMPTIONS)
});

export const analysisRequestSchema = z.object({
  workbook: parsedWorkbookSchema,
  settings: settingsSchema
});

export const envSchema = z.object({
  NEXT_PUBLIC_APP_NAME: z.string().default("Switch Analysis"),
  NEXT_PUBLIC_GITHUB_URL: z.string().url().default("https://github.com/sgbstats/switchanalysis"),
  NEXT_PUBLIC_TYPEFORM_URL: z.string().url().default("https://form.typeform.com/to/01KBJ8CNNC7J04THJAX5QH7592")
});
