import { NextResponse } from "next/server";
import { analyzeWorkbook } from "@/lib/analysis";
import { analysisRequestSchema } from "@/shared/validation";

export async function POST(request: Request) {
  const payload = await request.json();
  const parsed = analysisRequestSchema.safeParse(payload);

  if (!parsed.success) {
    return NextResponse.json({ ok: false, errors: parsed.error.flatten() }, { status: 400 });
  }

  return NextResponse.json({
    ok: true,
    result: analyzeWorkbook(parsed.data.workbook, parsed.data.settings),
  });
}
