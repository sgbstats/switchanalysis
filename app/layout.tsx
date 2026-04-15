import type { Metadata } from "next";
import { env } from "@/lib/env";
import "../src/app/globals.css";

export const metadata: Metadata = {
  title: env.NEXT_PUBLIC_APP_NAME,
  description: "Browser-first TypeScript conversion of the Switch Analysis Shiny app.",
};

export default function RootLayout({ children }: Readonly<{ children: React.ReactNode }>) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
