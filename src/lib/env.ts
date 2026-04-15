import { envSchema } from "@/shared/validation";

const parsedEnv = envSchema.safeParse({
  NEXT_PUBLIC_APP_NAME: process.env.NEXT_PUBLIC_APP_NAME,
  NEXT_PUBLIC_GITHUB_URL: process.env.NEXT_PUBLIC_GITHUB_URL,
  NEXT_PUBLIC_TYPEFORM_URL: process.env.NEXT_PUBLIC_TYPEFORM_URL,
});

if (!parsedEnv.success) {
  throw new Error(`Invalid public environment configuration. Update .env.local from .env.example: ${parsedEnv.error.issues.map((issue) => `${issue.path.join(".")}: ${issue.message}`).join("; ")}`);
}

export const env = parsedEnv.data;
