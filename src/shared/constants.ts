export const PARTY_OPTIONS = [
  "Lib Dem",
  "Labour",
  "Conservative",
  "Green",
  "Reform",
  "Independent",
  "Unaligned and No Data",
  "Not Voting",
  "Not Lib Dem",
] as const;

export const WEIGHTED_PARTIES = [
  "Lib Dem",
  "Labour",
  "Conservative",
  "Green",
  "Reform",
  "Independent",
  "Unaligned and No Data",
] as const;

export const PARTY_ORDER = [
  "Lib Dem",
  "Labour",
  "Conservative",
  "Green",
  "Reform",
  "Independent",
  "Unaligned and No Data",
  "Unknown",
  "Not Voting",
  "Not Lib Dem",
] as const;

export const PARTY_COLORS: Record<string, string> = {
  "Lib Dem": "#ff6400",
  Labour: "#E4003B",
  Conservative: "#0087DC",
  Green: "#00a85a",
  Reform: "#00bed6",
  Independent: "#7c3aed",
  "Unaligned and No Data": "#888888",
  Unknown: "#888888",
  "Not Voting": "#444444",
  "Not Lib Dem": "#444444",
};

export const DEFAULT_ASSUMPTIONS = {
  "Lib Dem": 1,
  Labour: 1,
  Conservative: 1,
  Green: 1,
  Reform: 1,
  Independent: 1,
  "Unaligned and No Data": 1,
} as const;

export const ROOT_TABS = ["Home", "User Guide", "Report a bug"] as const;
export const HOME_TABS = ["Tabular", "Sankey Plot"] as const;
export const GUIDE_TABS = ["How to use the tool", "Intro to switch analysis"] as const;
