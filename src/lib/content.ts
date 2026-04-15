import path from "node:path";
import { readFile } from "node:fs/promises";

function stripFrontMatter(content: string) {
  return content.replace(/^---[\s\S]*?---\s*/, "").replace(/^---\s*/, "").trim();
}

function normalizeImages(content: string) {
  return content
    .replace(/!\[(.*?)\]\(images\//g, "![$1](/images/")
    .replace(/\]\(images\//g, "](/images/")
    .replace(/!\[(.*?)\]\((.*?)\)\{[^}]+\}/g, "![$1]($2)");
}

export async function loadGuideMarkdown(relativePath: string) {
  const filePath = path.join(process.cwd(), "app", relativePath);
  const raw = await readFile(filePath, "utf8");
  return normalizeImages(stripFrontMatter(raw));
}
