import { envSchema } from "@/shared/validation";

export const env = envSchema.parse({
  NEXT_PUBLIC_APP_NAME: process.env.NEXT_PUBLIC_APP_NAME,
  NEXT_PUBLIC_GITHUB_URL: process.env.NEXT_PUBLIC_GITHUB_URL,
  NEXT_PUBLIC_TYPEFORM_URL: process.env.NEXT_PUBLIC_TYPEFORM_URL,
});
