# NSF Grant Disruption Tracker — Shiny App

Civic-data-design–styled dashboard reading from Neon Postgres.

## Run locally

```r
# from the project root
shiny::runApp("shiny/")
```

The app reads `NEON_DB_URL` from `.Renviron` at the project root. That file is
gitignored — see the project's top-level README for setup.

## Deploy to shinyapps.io

```r
# 1. one-time auth (token from https://www.shinyapps.io/admin/#/tokens)
rsconnect::setAccountInfo(name="<account>", token="<token>", secret="<secret>")

# 2. set the env var Shiny needs at runtime — this travels with the deploy
Sys.setenv(NEON_DB_URL = readRenviron(".Renviron")["NEON_DB_URL"])

# 3. deploy
rsconnect::deployApp(
  appDir       = "shiny/",
  appName      = "nsf-disruption",
  appTitle     = "NSF Grant Disruption Tracker",
  appFiles     = c("app.R"),  # don't push .Renviron or app_old.R
  forceUpdate  = TRUE,
  envVars      = "NEON_DB_URL"
)
```

`envVars = "NEON_DB_URL"` tells rsconnect to forward the value of that env var
to shinyapps.io as a server-side secret. It will not be visible in the deployed
app's code, only available to the running R process.

After the first deploy, you can also set the env var directly in the
shinyapps.io dashboard → Applications → nsf-disruption → Settings → Variables.

## Files

- `app.R` — the dashboard
- `app_old.R` — the previous DuckDB-based prototype, kept for reference
- `README.md` — this file

## Tabs

1. **Overview** — headline metrics + monthly obligations chart + action-type breakdown
2. **Trends** — year-over-year same-month chart with directorate filter and metric toggle
3. **Resilience** — open-opportunities-vs-dollars-retained scatter + currently-open opps table
4. **About** — methodology, data sources, stack notes

## Common issues

**"NEON_DB_URL not set."** — `.Renviron` isn't being found. Make sure it's at the
project root (one level up from `shiny/`) and has a line like
`NEON_DB_URL=postgresql://...`. The app tries the parent of cwd and one level up
from there.

**Slow first load** — Neon's free tier auto-pauses after ~5 minutes idle. The
first request after a pause has to wait for the database to spin back up
(usually 2–5 seconds). Subsequent queries are fast.

**Fonts look wrong** — `showtext` requires Google Fonts at runtime. On a system
without internet access, the app falls back to system serifs (still readable;
just not Playfair Display).
