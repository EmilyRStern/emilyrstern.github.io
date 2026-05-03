# =============================================================================
# app.R  —  NSF Grant Disruption Tracker
# EPPS 6354 Information Management · Spring 2026 · Emily Stern
#
# Reads from Neon Postgres (NEON_DB_URL) and renders a civic-data-design
# styled Shiny dashboard. Three tabs: Overview, Trends, Resilience.
#
# Run locally:    shiny::runApp("shiny/")
# Deploy:         rsconnect::deployApp("shiny/", appName = "nsf-disruption")
# =============================================================================

# ── Packages ─────────────────────────────────────────────────────────────────
required <- c("shiny", "bslib", "DBI", "RPostgres", "pool", "DT", "dplyr",
              "ggplot2", "scales", "showtext")
missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(shiny);   library(bslib);  library(DBI);     library(RPostgres)
  library(pool);    library(DT);     library(dplyr);   library(ggplot2)
  library(scales);  library(showtext)
})

# ── Fonts (civic-data-design) ────────────────────────────────────────────────
tryCatch({
  font_add_google("Playfair Display", "playfair")
  font_add_google("Source Serif 4",   "sourceserif")
  font_add_google("Source Sans 3",    "sourcesans")
  showtext_auto()
  showtext_opts(dpi = 100)
}, error = function(e) message("Google fonts unavailable; falling back to system serifs"))

# ── Civic palette ────────────────────────────────────────────────────────────
PAL <- list(
  bg_primary    = "#F5F0E8",
  bg_secondary  = "#EDE8DC",
  bg_surface    = "#FFFFFF",
  text_primary  = "#1C2B2B",
  text_body     = "#3A4A45",
  text_muted    = "#7A8C85",
  teal          = "#2D6A5F",
  teal_light    = "#6BAF9E",
  gold          = "#D4A843",
  gold_light    = "#E8C97A",
  border        = "#C8BFA8",
  border_strong = "#1C2B2B",
  seq           = c("#D9EDE8", "#8EC9BC", "#4A9E8E", "#2D6A5F", "#0F3830"),
  cat           = c("#2D6A5F", "#D4A843", "#6BAF9E", "#E8C97A", "#0F3830", "#C8BFA8",
                    "#A98D2F", "#4A7AA0", "#8C3A35", "#52606D", "#3D437B")
)

# Stable directorate → color mapping (used everywhere, so a directorate keeps
# the same color across charts).
DIR_COLORS <- c(
  "MPS"   = PAL$cat[1], "EDU"   = PAL$cat[2], "CISE"  = PAL$cat[3],
  "ENG"   = PAL$cat[4], "GEO"   = PAL$cat[5], "BIO"   = PAL$cat[6],
  "TIP"   = PAL$cat[7], "SBE"   = PAL$cat[8], "OIA"   = PAL$cat[9],
  "OD"    = PAL$cat[10], "OPP"  = PAL$cat[11], "OTHER" = "#C8BFA8"
)

# ── theme_civic ggplot helper ────────────────────────────────────────────────
theme_civic <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background  = element_rect(fill = PAL$bg_primary, color = NA),
      panel.background = element_rect(fill = PAL$bg_surface,  color = NA),
      panel.grid.major = element_line(color = PAL$border, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, color = PAL$border, linewidth = 0.5),
      plot.title       = element_text(family = "playfair", size = base_size * 1.4,
                                      color = PAL$text_primary, face = "bold",
                                      margin = margin(b = 6), hjust = 0),
      plot.subtitle    = element_text(family = "sourceserif", size = base_size * 0.95,
                                      color = PAL$text_body, margin = margin(b = 12), hjust = 0),
      plot.caption     = element_text(family = "sourcesans", size = base_size * 0.72,
                                      color = PAL$text_muted, hjust = 0,
                                      margin = margin(t = 8)),
      axis.title       = element_text(family = "sourcesans", size = base_size * 0.78,
                                      color = PAL$text_muted),
      axis.text        = element_text(family = "sourcesans", size = base_size * 0.75,
                                      color = PAL$text_body),
      axis.ticks       = element_line(color = PAL$border),
      legend.background = element_rect(fill = PAL$bg_primary, color = NA),
      legend.title      = element_text(family = "sourcesans", size = base_size * 0.78,
                                       color = PAL$text_muted),
      legend.text       = element_text(family = "sourcesans", size = base_size * 0.75,
                                       color = PAL$text_body),
      legend.key        = element_rect(fill = PAL$bg_primary, color = NA),
      strip.background  = element_rect(fill = PAL$bg_secondary, color = PAL$border),
      strip.text        = element_text(family = "sourcesans", size = base_size * 0.78,
                                       color = PAL$text_primary),
      plot.margin       = margin(16, 16, 16, 16)
    )
}

# ── DB pool (read NEON_DB_URL from .Renviron) ────────────────────────────────
parse_pg_url <- function(url) {
  m <- regmatches(url, regexec(
    "^postgres(?:ql)?://([^:]+):([^@]+)@([^:/]+)(?::(\\d+))?/([^?]+)(?:\\?(.*))?$", url))[[1]]
  list(user = m[2], password = m[3], host = m[4],
       port = if (nzchar(m[5])) as.integer(m[5]) else 5432L, dbname = m[6])
}
# Try project-local .Renviron (won't override an already-set env var)
proj_renv <- file.path(dirname(getwd()), ".Renviron")
if (file.exists(proj_renv)) readRenviron(proj_renv)
# also try one more level up (when running shiny/ as the wd)
proj_renv2 <- file.path(getwd(), "..", ".Renviron")
if (file.exists(proj_renv2)) readRenviron(proj_renv2)

NEON_URL <- Sys.getenv("NEON_DB_URL")
if (!nzchar(NEON_URL)) stop("NEON_DB_URL not set. Add it to .Renviron at the project root.")
P <- parse_pg_url(NEON_URL)

pool <- dbPool(
  drv     = Postgres(),
  host    = P$host,  port = P$port, dbname = P$dbname,
  user    = P$user,  password = P$password,
  sslmode = "require",
  minSize = 1, maxSize = 4, idleTimeout = 60000
)
onStop(function() poolClose(pool))

# ── bslib civic theme ────────────────────────────────────────────────────────
civic_theme <- bs_theme(
  version      = 5,
  bg           = PAL$bg_primary,
  fg           = PAL$text_primary,
  primary      = PAL$teal,
  secondary    = PAL$teal_light,
  warning      = PAL$gold,
  base_font    = font_google("Source Serif 4"),
  heading_font = font_google("Playfair Display"),
  font_scale   = 1.0,
  bootswatch   = NULL
) |>
  bs_add_rules(sprintf("
    .card                { border-radius: 0 !important; border-color: %s; background: %s; }
    .well                { background: %s; border-radius: 0; border-color: %s; }
    .sidebar             { background: %s !important; }
    .navbar              { border-bottom: 1px solid %s; }
    .nav-link.active     { color: %s !important; border-color: %s !important; }
    label                { font-family: 'Source Sans 3', sans-serif; font-size: 0.78rem;
                           text-transform: uppercase; letter-spacing: 0.08em; color: %s; }
    h1, h2, h3, .h1, .h2, .h3 {
      font-family: 'Playfair Display', Georgia, serif;
      color: %s; font-weight: 700;
    }
    .value-box           { border-radius: 0 !important; }
    .selectize-input,
    .form-control        { border-radius: 0 !important; border-color: %s; }
    .btn-primary         { background: %s; border-color: %s; border-radius: 0; }
    .nav-tabs .nav-link  { border-radius: 0; }
    body                 { background: %s; color: %s; }
    .dataTables_wrapper  { font-family: 'Source Sans 3', sans-serif; }
    table.dataTable thead th {
      font-family: 'Source Sans 3', sans-serif; font-size: 0.78rem;
      text-transform: uppercase; letter-spacing: 0.06em; color: %s !important;
      border-bottom: 2px solid %s !important;
    }
    .stat-callout {
      border-left: 3px solid %s; background: %s; padding: 18px 20px; margin-bottom: 0;
    }
    .stat-callout .label {
      font-family: 'Source Sans 3', sans-serif; font-size: 0.72rem;
      text-transform: uppercase; letter-spacing: 0.08em; color: %s;
    }
    .stat-callout .value {
      font-family: 'Playfair Display', Georgia, serif; font-size: 2.1rem;
      color: %s; line-height: 1.1; margin-top: 4px;
    }
    .stat-callout .sub {
      font-family: 'Source Sans 3', sans-serif; font-size: 0.78rem;
      color: %s; margin-top: 4px;
    }
  ",
  PAL$border, PAL$bg_primary,
  PAL$bg_secondary, PAL$border,
  PAL$bg_secondary, PAL$border,
  PAL$teal, PAL$teal,
  PAL$text_muted, PAL$text_primary,
  PAL$border, PAL$teal, PAL$teal,
  PAL$bg_primary, PAL$text_body,
  PAL$text_muted, PAL$border,
  PAL$teal, PAL$bg_secondary,
  PAL$text_muted, PAL$text_primary, PAL$text_body))

# ── helper: stat callout (used in headline row) ──────────────────────────────
stat_callout <- function(label, value, sub = NULL) {
  div(class = "stat-callout",
      div(class = "label", label),
      div(class = "value", value),
      if (!is.null(sub)) div(class = "sub", sub) else NULL)
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "NSF Grant Disruption Tracker",
  theme = civic_theme,
  bg    = PAL$bg_primary,
  fillable = FALSE,

  # ── Tab 1: Overview ────────────────────────────────────────────────────────
  nav_panel(
    "Overview",
    div(style = sprintf("padding: 24px 32px; max-width: 1280px; margin: 0 auto;
                         color: %s;", PAL$text_body),
      h2("How NSF funding has been disrupted",
         style = sprintf("font-family: 'Playfair Display'; color: %s;
                          margin-top: 0; margin-bottom: 4px;", PAL$text_primary)),
      p(style = sprintf("font-family: 'Source Serif 4'; color: %s; font-size: 1.05rem;
                         max-width: 780px; margin-bottom: 24px;", PAL$text_body),
        "Since the January 2025 administration change, NSF first-half-of-fiscal-year grant
        activity has declined in two phases: a moderate ~25% drop in FY25 H1 alongside the
        admin transition, then a deeper drop to roughly 30% of the FY24 baseline in FY26 H1.
        The chart below holds seasonality constant by stacking the same six-month window
        across three fiscal years — so the disruption can't be confused with NSF's normal
        August-spike, October-trough rhythm."),

      # Headline metrics
      uiOutput("stat_row"),

      br(),

      # Hero chart: FY24/FY25/FY26 first-half head-to-head
      card(
        card_header(span("Same six months, three fiscal years",
                         style = sprintf("font-family: 'Source Sans 3'; font-size: 0.78rem;
                                          text-transform: uppercase; letter-spacing: 0.08em;
                                          color: %s;", PAL$text_muted))),
        card_body(plotOutput("fy_comparison_chart", height = "440px"))
      ),

      br(),

      # Action-type breakdown
      card(
        card_header(span("New awards, continuations, and revisions over time",
                         style = sprintf("font-family: 'Source Sans 3'; font-size: 0.78rem;
                                          text-transform: uppercase; letter-spacing: 0.08em;
                                          color: %s;", PAL$text_muted))),
        card_body(plotOutput("action_type_chart", height = "360px"))
      )
    )
  ),

  # ── Tab 2: Trends ──────────────────────────────────────────────────────────
  nav_panel(
    "Trends",
    div(style = "padding: 24px 32px; max-width: 1280px; margin: 0 auto;",
      h2("Full timeline & year-over-year detail",
         style = sprintf("color: %s; margin-top: 0;", PAL$text_primary)),
      p(style = sprintf("color: %s; font-family: 'Source Serif 4'; max-width: 760px;",
                        PAL$text_body),
        "The full-timeline chart below preserves NSF's annual August obligation surge —
        every fiscal year ends with a budget-clearing crunch, so the August spike is
        normal and recurring. What's not normal is the magnitude of the FY26 trough.
        Use the year-over-year tool to slice by directorate or toggle between dollars
        and transaction counts."),
      br(),
      card(
        card_header(span("Full timeline: gross obligations by month, by directorate",
                         style = sprintf("font-family: 'Source Sans 3'; font-size: 0.78rem;
                                          text-transform: uppercase; letter-spacing: 0.08em;
                                          color: %s;", PAL$text_muted))),
        card_body(plotOutput("monthly_chart", height = "440px"))
      ),
      br(),
      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header(span("Filter", style = "color: #7A8C85;")),
          card_body(
            selectInput("yoy_directorate", "Directorate",
                        choices = c("All directorates" = "ALL"),
                        selected = "ALL"),
            radioButtons("yoy_metric", "Metric",
                         choices = c("Gross obligations ($)" = "gross",
                                     "Transactions (count)"  = "ntx"),
                         selected = "gross")
          )
        ),
        card(
          card_header(span("Year-over-year same calendar month",
                           style = sprintf("font-family: 'Source Sans 3';
                                            font-size: 0.78rem; text-transform: uppercase;
                                            letter-spacing: 0.08em; color: %s;", PAL$text_muted))),
          card_body(plotOutput("yoy_chart", height = "440px"))
        )
      )
    )
  ),

  # ── Tab 3: Resilience ──────────────────────────────────────────────────────
  nav_panel(
    "Resilience",
    div(style = "padding: 24px 32px; max-width: 1280px; margin: 0 auto;",
      h2("Which directorates kept their funding moving",
         style = sprintf("color: %s; margin-top: 0;", PAL$text_primary)),
      p(style = sprintf("color: %s; font-family: 'Source Serif 4'; max-width: 760px;",
                        PAL$text_body),
        "Each directorate's resilience score is the share of its FY25 first-half new-award
        dollars that reappeared in the equivalent FY26 window. Directorates with structural
        commitments (Polar Programs, STEM Education scholarships) held up best;
        discretionary new-mission funding (TIP, SBE) collapsed."),
      br(),
      card(
        card_header(span("Open opportunities vs. dollars retained",
                         style = sprintf("font-family: 'Source Sans 3'; font-size: 0.78rem;
                                          text-transform: uppercase; letter-spacing: 0.08em;
                                          color: %s;", PAL$text_muted))),
        card_body(plotOutput("resilience_scatter", height = "440px"))
      ),
      br(),
      card(
        card_header(span("Currently-open NSF opportunities",
                         style = sprintf("font-family: 'Source Sans 3'; font-size: 0.78rem;
                                          text-transform: uppercase; letter-spacing: 0.08em;
                                          color: %s;", PAL$text_muted))),
        card_body(DTOutput("open_opps_table"))
      )
    )
  ),

  # ── Tab 4: About ───────────────────────────────────────────────────────────
  nav_panel(
    "About",
    div(style = "padding: 32px; max-width: 760px; margin: 0 auto;
                 font-family: 'Source Serif 4'; line-height: 1.55;",
      h2("About this dashboard", style = sprintf("color: %s;", PAL$text_primary)),
      p("This tracker visualizes how NSF grant activity has shifted since the January
        2025 administration change, drawing on three federal sources joined into a single
        relational schema:"),
      tags$ul(
        tags$li(strong("USAspending.gov"), " — transaction-level federal assistance
                records (37,108 NSF awards · 57,290 transactions covering FY24–FY26)."),
        tags$li(strong("Grants.gov full extract"), " — 1,330 NSF opportunity records
                including currently-open solicitations."),
        tags$li(strong("NSF terminations dataset"), " — 1,996 terminated awards with
                deobligation amounts and reinstatement status.")
      ),
      h3("Methodology", style = sprintf("color: %s;", PAL$text_primary)),
      p(strong("Two-step decline since the 2025 administration change."),
        " Comparing the same six-month window (Oct–Mar) across three fiscal years
        controls for NSF's annual August-spike, October-trough cycle. FY24 H1 obligated
        $1,895M across 3,240 NEW awards. FY25 H1 — overlapping with the January 2025
        admin transition — dropped 26% to $1,408M and 2,149 NEW awards. FY26 H1 dropped
        another ~60% from there to $561M and just 590 NEW awards, a cumulative 70%
        decline from the FY24 baseline."),
      p(strong("Why same-month, same-FY-position comparisons matter."),
        " A naive month-over-month chart conflates the disruption with NSF's normal
        fiscal-year rhythm — every August is a budget-clearing surge, every October
        is a transition lull. The hero chart on the Overview tab holds that seasonality
        constant by stacking three Octobers, three Novembers, and so on, side by side.
        The October 2024 vs October 2025 transaction-count drop (409 → 2) is at
        equally mature reporting horizons, ruling out lag as an explanation."),
      p(strong("Outlay NULLs are reporting lag, not zeros."),
        " The 9% NULL-outlay rate among in-progress awards is treated as ",
        em("not yet reported"),
        " rather than $0 — a known quirk of Treasury's ~90-day cash-flow reporting
        cadence relative to the agency's faster obligation reporting."),
      h3("Stack", style = sprintf("color: %s;", PAL$text_primary)),
      p("Data ingestion in R, ETL through DuckDB, persistent storage on Neon Postgres
        17, dashboard rendered in Shiny with the ", em("civic-data-design"),
        " visual system (Playfair Display headlines, Source Serif body, warm linen
        background, teal/gold accents). Source code and ER diagram in the project repo."),
      p(style = sprintf("color: %s; font-size: 0.85rem;", PAL$text_muted),
        "EPPS 6354 Information Management · University of Texas at Dallas · Spring 2026 ·
        Emily Stern · Data current as of April 2026.")
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Data: cached pulls (poolWithTransaction not needed for read-only) ──────
  monthly_data <- reactive({
    dbGetQuery(pool, "
      SELECT month, directorate, n_transactions, net_obligation,
             gross_obligated, gross_deobligated
      FROM v_monthly_obligations
      WHERE month >= '2023-10-01'
      ORDER BY month, directorate")
  })

  resilience_data <- reactive({
    dbGetQuery(pool, "
      SELECT directorate,
             n_open_opportunities, open_advertised_funding,
             fy25_new_count, fy26_new_count, pct_count_retained,
             fy25_new_oblig, fy26_new_oblig, pct_dollar_retained
      FROM v_directorate_resilience
      WHERE directorate <> 'OD'  -- noisy with very small N")
  })

  termination_totals <- reactive({
    dbGetQuery(pool, "
      SELECT COUNT(*)                                                  AS n_terms,
             SUM(CASE WHEN reinstated THEN 1 ELSE 0 END)               AS n_reinst,
             ABS(COALESCE(SUM(post_termination_deobligation), 0))      AS deob
      FROM terminations")
  })

  fy_totals <- reactive({
    # Apples-to-apples: first 6 months of each FY (Oct–Mar)
    dbGetQuery(pool, "
      SELECT
        SUM(CASE WHEN action_date >= '2023-10-01' AND action_date < '2024-04-01'
                  AND action_type_description = 'NEW' THEN 1 ELSE 0 END) AS fy24_h1_new,
        SUM(CASE WHEN action_date >= '2024-10-01' AND action_date < '2025-04-01'
                  AND action_type_description = 'NEW' THEN 1 ELSE 0 END) AS fy25_h1_new,
        SUM(CASE WHEN action_date >= '2025-10-01' AND action_date < '2026-04-01'
                  AND action_type_description = 'NEW' THEN 1 ELSE 0 END) AS fy26_h1_new,
        SUM(CASE WHEN action_date >= '2023-10-01' AND action_date < '2024-04-01'
                  AND federal_action_obligation > 0 THEN federal_action_obligation END) AS fy24_h1_oblig,
        SUM(CASE WHEN action_date >= '2025-10-01' AND action_date < '2026-04-01'
                  AND federal_action_obligation > 0 THEN federal_action_obligation END) AS fy26_h1_oblig
      FROM transactions")
  })

  open_opp_data <- reactive({
    dbGetQuery(pool, "
      SELECT directorate,
             opportunity_number AS opp_num,
             title,
             estimated_total_funding AS funding_M,
             post_date, close_date
      FROM v_currently_open_opportunities
      ORDER BY estimated_total_funding DESC NULLS LAST")
  })

  # ── populate yoy directorate dropdown ──────────────────────────────────────
  observe({
    md <- monthly_data()
    dirs <- sort(unique(md$directorate))
    updateSelectInput(session, "yoy_directorate",
                      choices = c("All directorates" = "ALL", setNames(dirs, dirs)))
  }, priority = 100)

  # ── headline metrics row ───────────────────────────────────────────────────
  output$stat_row <- renderUI({
    tt <- termination_totals(); ft <- fy_totals()
    drop_pct <- 100 * (1 - ft$fy26_h1_new / ft$fy24_h1_new)
    obl_drop <- 100 * (1 - ft$fy26_h1_oblig / ft$fy24_h1_oblig)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      stat_callout(
        "Awards terminated",
        format(tt$n_terms, big.mark = ","),
        sprintf("%s reinstated since", format(tt$n_reinst, big.mark = ","))
      ),
      stat_callout(
        "Deobligated",
        scales::dollar(tt$deob, scale = 1e-6, suffix = "M"),
        "Post-termination, after reinstatements"
      ),
      stat_callout(
        "FY26 H1 new awards",
        format(ft$fy26_h1_new, big.mark = ","),
        sprintf("vs. %s in FY24 H1 baseline",
                format(ft$fy24_h1_new, big.mark = ","))
      ),
      stat_callout(
        "Drop vs FY24 baseline",
        sprintf("%.0f%%", drop_pct),
        sprintf("(%.0f%% drop in obligation $)", obl_drop)
      )
    )
  })

  # ── HERO chart: three FY first-halves, same six calendar months ────────────
  fy_comparison_data <- reactive({
    df <- dbGetQuery(pool, "SELECT fy, fy_month_ord, month_abbrev, n_tx,
                                   gross_obligated, n_new
                            FROM v_fy_h1_comparison
                            ORDER BY fy_month_ord, fy")
    df$month_label <- factor(df$month_abbrev,
                             levels = c("Oct","Nov","Dec","Jan","Feb","Mar"))
    df$gross_M     <- df$gross_obligated / 1e6
    df
  })

  output$fy_comparison_chart <- renderPlot({
    df <- fy_comparison_data()

    # Color palette: FY24 + FY25 = baseline (teals), FY26 = the disrupted year (gold)
    fy_colors <- c("FY24" = PAL$teal_light, "FY25" = PAL$teal, "FY26" = PAL$gold)

    ggplot(df, aes(x = month_label, y = gross_M, fill = fy)) +
      geom_col(position = position_dodge(width = 0.85), width = 0.78) +
      geom_text(aes(label = ifelse(is.na(gross_M), "0",
                                   sprintf("$%.0fM", gross_M))),
                position = position_dodge(width = 0.85),
                vjust = -0.4, size = 2.8, family = "sourcesans",
                color = PAL$text_body) +
      scale_fill_manual(values = fy_colors, name = "Fiscal year") +
      scale_y_continuous(labels = label_dollar(suffix = "M"),
                         name = "Gross obligations",
                         expand = expansion(mult = c(0, 0.12))) +
      scale_x_discrete(name = NULL) +
      labs(title    = "FY26 obligations are 30% of the FY24 baseline",
           subtitle = "Gross monthly obligations, first six months of three fiscal years (Oct–Mar)",
           caption  = "Source: USAspending.gov FY24–FY26 Assistance files (pulled Apr 2026).
FY25 H1 dropped 26% from FY24 baseline; FY26 H1 dropped another ~60% from there.") +
      theme_civic() +
      theme(legend.position = "top")
  }, res = 100)

  # ── Full-timeline monthly chart (moved to Trends tab) ──────────────────────
  output$monthly_chart <- renderPlot({
    md <- monthly_data()
    md$month <- as.Date(md$month)
    dir_order <- md |> group_by(directorate) |>
      summarise(t = sum(gross_obligated, na.rm = TRUE)) |>
      arrange(desc(t)) |> pull(directorate)
    md$directorate <- factor(md$directorate, levels = dir_order)

    ggplot(md, aes(x = month, y = gross_obligated/1e6, fill = directorate)) +
      geom_col(position = "stack", width = 28) +
      scale_fill_manual(values = DIR_COLORS, name = "Directorate") +
      scale_y_continuous(labels = label_dollar(suffix = "M"), name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b '%y", name = NULL) +
      geom_vline(xintercept = as.numeric(as.Date("2025-04-15")),
                 linetype = "dashed", color = PAL$gold, linewidth = 0.5) +
      annotate("text", x = as.Date("2025-04-15"),
               y = max(md$gross_obligated/1e6, na.rm = TRUE) * 0.95,
               label = "Termination wave\nApr–May 2025", hjust = -0.05, vjust = 1,
               size = 3.0, color = PAL$gold, fontface = "italic", family = "sourcesans") +
      geom_vline(xintercept = as.numeric(as.Date("2025-10-01")),
                 linetype = "dashed", color = PAL$teal, linewidth = 0.5) +
      annotate("text", x = as.Date("2025-10-01"),
               y = max(md$gross_obligated/1e6, na.rm = TRUE) * 0.95,
               label = "FY26 begins\nOct 1 2025", hjust = -0.05, vjust = 1,
               size = 3.0, color = PAL$teal, fontface = "italic", family = "sourcesans") +
      labs(title    = "Full-timeline view: the August spike is annual, the FY26 trough is not",
           subtitle = "Gross monthly obligations across all NSF directorates",
           caption  = "Source: USAspending.gov FY24–FY26 Assistance files (pulled Apr 2026).") +
      theme_civic()
  }, res = 100)

  # ── action-type chart ──────────────────────────────────────────────────────
  output$action_type_chart <- renderPlot({
    df <- dbGetQuery(pool, "
      SELECT date_trunc('month', action_date)::date AS month,
             COALESCE(action_type_description, 'OTHER') AS action_type,
             SUM(federal_action_obligation) AS obligation,
             COUNT(*) AS n_tx
      FROM transactions
      WHERE action_date >= '2023-10-01'
      GROUP BY 1, 2 ORDER BY 1, 2")
    df$month <- as.Date(df$month)
    df$action_type <- factor(df$action_type,
                             levels = c("NEW", "CONTINUATION", "REVISION", "OTHER"))

    action_colors <- c(NEW          = PAL$teal,
                       CONTINUATION = PAL$teal_light,
                       REVISION     = PAL$gold,
                       OTHER        = PAL$border)

    ggplot(df, aes(x = month, y = n_tx, fill = action_type)) +
      geom_col(position = "stack", width = 28) +
      scale_fill_manual(values = action_colors, name = "Action type") +
      scale_x_date(date_breaks = "3 months", date_labels = "%b '%y", name = NULL) +
      scale_y_continuous(name = "Transactions per month") +
      geom_vline(xintercept = as.numeric(as.Date("2025-10-01")),
                 linetype = "dashed", color = PAL$teal, linewidth = 0.4) +
      labs(title    = "NEW awards and CONTINUATIONS dropped first; REVISIONS continued",
           subtitle = "Monthly transaction count by USAspending action type",
           caption  = "REVISIONS are mostly deobligations (negative dollar entries).") +
      theme_civic()
  }, res = 100)

  # ── YoY chart ──────────────────────────────────────────────────────────────
  output$yoy_chart <- renderPlot({
    md <- monthly_data()
    if (input$yoy_directorate != "ALL") md <- md |> filter(directorate == input$yoy_directorate)

    md$month <- as.Date(md$month)
    md <- md |>
      group_by(month) |>
      summarise(gross = sum(gross_obligated, na.rm = TRUE),
                ntx   = sum(n_transactions, na.rm = TRUE), .groups = "drop") |>
      mutate(year     = lubridate::year(month),
             cal_mo   = lubridate::month(month, label = TRUE, abbr = TRUE),
             metric   = if (input$yoy_metric == "gross") gross else ntx)

    ggplot(md, aes(x = cal_mo, y = if (input$yoy_metric == "gross") metric/1e6 else metric,
                   fill = factor(year))) +
      geom_col(position = position_dodge(width = 0.85), width = 0.78) +
      scale_fill_manual(values = c("2023" = PAL$cat[6], "2024" = PAL$teal_light,
                                   "2025" = PAL$teal,    "2026" = PAL$gold),
                        name = "Calendar year") +
      scale_y_continuous(
        name = if (input$yoy_metric == "gross") "Gross obligations ($M)" else "Transactions",
        labels = if (input$yoy_metric == "gross") label_dollar(suffix = "M") else comma) +
      labs(x = NULL,
           title    = if (input$yoy_directorate == "ALL")
                       "Same calendar month, every year — apples to apples"
                       else paste(input$yoy_directorate, "— same calendar month YoY"),
           subtitle = "Removes fiscal-year seasonality so the FY26 collapse is unambiguous",
           caption  = "Source: USAspending.gov.  Compare Oct'24 to Oct'25 — both are >5 months past reporting deadline.") +
      theme_civic()
  }, res = 100)

  # ── resilience scatter ────────────────────────────────────────────────────
  output$resilience_scatter <- renderPlot({
    rd <- resilience_data()
    rd$open_M <- rd$open_advertised_funding / 1e6
    rd$pct_dollar_retained[is.na(rd$pct_dollar_retained)] <- 0

    ggplot(rd, aes(x = n_open_opportunities, y = pct_dollar_retained,
                   color = directorate, size = open_M)) +
      geom_hline(yintercept = 50, linetype = "dotted", color = PAL$border, linewidth = 0.5) +
      geom_hline(yintercept = 100, linetype = "dotted", color = PAL$border, linewidth = 0.5) +
      geom_point(alpha = 0.85) +
      ggrepel::geom_text_repel(aes(label = directorate), size = 3.4,
                               family = "sourcesans", color = PAL$text_primary,
                               box.padding = 0.4, point.padding = 0.3, show.legend = FALSE) +
      scale_color_manual(values = DIR_COLORS, guide = "none") +
      scale_size_continuous(range = c(3, 12), name = "Advertised\nfunding ($M)",
                            labels = label_dollar(suffix = "M")) +
      scale_y_continuous(name = "% of FY25 first-half new-award $ retained in FY26",
                         labels = label_percent(scale = 1)) +
      scale_x_continuous(name = "Currently-open opportunities (count)") +
      labs(title    = "Polar and STEM Education held up; TIP and SBE collapsed",
           subtitle = "Each dot is a directorate. Up = funding still flowing, right = many solicitations posted.",
           caption  = "Sources: USAspending.gov transactions + Grants.gov current opportunities.") +
      theme_civic()
  }, res = 100)

  # ── currently-open opportunities table ────────────────────────────────────
  output$open_opps_table <- renderDT({
    df <- open_opp_data()
    df$funding_M <- ifelse(is.na(df$funding_M), NA, round(df$funding_M / 1e6, 1))
    datatable(
      df,
      colnames  = c("Directorate", "Opp #", "Title", "Funding ($M)",
                    "Posted", "Closes"),
      rownames  = FALSE,
      filter    = "top",
      extensions = "Buttons",
      options   = list(pageLength = 12, scrollX = TRUE,
                       dom = "Bfrtip", buttons = c("csv"),
                       order = list(list(3, "desc"))),
      class     = "stripe hover compact"
    )
  })
}

shinyApp(ui = ui, server = server)
