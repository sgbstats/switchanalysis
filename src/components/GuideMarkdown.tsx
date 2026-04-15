"use client";

import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

interface GuideMarkdownProps {
  content: string;
}

export function GuideMarkdown({ content }: GuideMarkdownProps) {
  return <ReactMarkdown remarkPlugins={[remarkGfm]}>{content}</ReactMarkdown>;
}
