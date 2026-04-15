import { SwitchAnalysisApp } from "@/components/SwitchAnalysisApp";
import { loadGuideMarkdown } from "@/lib/content";

export default async function Page() {
  const [userGuide, switchGuide] = await Promise.all([
    loadGuideMarkdown("userguide.qmd"),
    loadGuideMarkdown("switch-guide.qmd"),
  ]);

  return <SwitchAnalysisApp userGuide={userGuide} switchGuide={switchGuide} />;
}
