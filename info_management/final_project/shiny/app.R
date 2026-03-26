# =============================================================================
# app.R  –  Federal Grant Volatility Tracker
# EPPS 6354 Information Management – Emily Stern – Spring 2026
#
# Shiny application that queries a local DuckDB database to visualize
# federal grant opportunity disappearances and cross-reference them against
# USASpending.gov award records.
#
# Run with: shiny::runApp("path/to/shiny/")
# =============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────
required_pkgs <- c("shiny", "bslib", "DBI", "duckdb", "DT",
                   "dplyr", "ggplot2", "plotly", "scales", "lubridate")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)

# ── Database path ─────────────────────────────────────────────────────────────
# Adjust this path if needed. Default assumes shiny/ and data/ are siblings.
# Resolve path to the DuckDB file.
# The app expects: shiny/app.R and data/grants_volatility.duckdb as siblings one level up.
# Override explicitly if needed:
# DB_PATH <- "C:/Users/Emily/Documents/final_project/data/grants_volatility.duckdb"
DB_PATH <- tryCatch(
  normalizePath(
    file.path(dirname(rstudioapi::getSourceEditorContext()$path),
              "..", "data", "grants_volatility.duckdb"),
    mustWork = FALSE),
  error = function(e) {
    # Fallback when launched via shiny::runApp("shiny/") from the project root
    normalizePath(file.path("data", "grants_volatility.duckdb"), mustWork = FALSE)
  }
)

# ── DB helper: open a read-only connection ────────────────────────────────────
get_con <- function() {
  dbConnect(duckdb(), dbdir = DB_PATH, read_only = TRUE)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Federal Grant Volatility Tracker",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#2E5FA3",
    base_font  = font_google("Source Sans Pro")
  ),

  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 280,
    title = "Filters",

    # Date range
    dateRangeInput(
      "date_range",
      "Removal Date Range",
      start = Sys.Date() - 90,
      end   = Sys.Date(),
      min   = "2025-01-01"
    ),

    hr(),

    # Agency filter (populated dynamically)
    selectizeInput(
      "agency_filter",
      "Agency",
      choices  = NULL,  # filled in server
      multiple = TRUE,
      options  = list(placeholder = "All agencies")
    ),

    # Category filter
    selectizeInput(
      "category_filter",
      "Funding Category",
      choices  = NULL,
      multiple = TRUE,
      options  = list(placeholder = "All categories")
    ),

    hr(),

    # Award status filter (for Missing Grants tab)
    checkboxGroupInput(
      "award_status",
      "Show opportunities:",
      choices  = c("Confirmed rescissions (no award)" = "no_award",
                   "Possibly awarded (CFDA match found)" = "has_award"),
      selected = "no_award"
    ),

    hr(),
    actionButton("refresh_btn", "Refresh Data",
                 icon = icon("rotate"), class = "btn-primary w-100"),
    br(), br(),
    tags$small(
      class = "text-muted",
      "Data: Grants.gov API + USASpending.gov API",
      br(),
      paste("Last updated:", format(Sys.Date(), "%B %d, %Y"))
    )
  ),

  # ── Main panel with tabs ───────────────────────────────────────────────────
  navset_card_underline(

    # ── Tab 1: Missing Grants (primary view) ─────────────────────────────────
    nav_panel(
      "Missing Grants",
      icon = icon("circle-xmark"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Removed Opportunities",
          value = textOutput("total_removed", inline = TRUE),
          showcase = icon("trash-can"),
          theme  = "danger"
        ),
        value_box(
          title = "Confirmed Rescissions",
          value = textOutput("confirmed_rescissions", inline = TRUE),
          showcase = icon("ban"),
          theme  = "warning"
        ),
        value_box(
          title = "Funding at Risk ($)",
          value = textOutput("funding_at_risk", inline = TRUE),
          showcase = icon("dollar-sign"),
          theme  = "primary"
        ),
        value_box(
          title = "Agencies Affected",
          value = textOutput("agencies_affected", inline = TRUE),
          showcase = icon("building-columns"),
          theme  = "secondary"
        )
      ),
      br(),
      card(
        card_header("Disappeared Grant Opportunities (no award record found)"),
        DTOutput("missing_grants_table")
      )
    ),

    # ── Tab 2: Change Timeline ─────────────────────────────────────────────
    nav_panel(
      "Change Timeline",
      icon = icon("chart-line"),
      card(
        card_header("Grant Listing Changes Per Snapshot"),
        plotlyOutput("timeline_plot", height = "400px")
      ),
      br(),
      card(
        card_header("Removals by Agency (Top 20)"),
        plotlyOutput("agency_bar_plot", height = "400px")
      )
    ),

    # ── Tab 3: Award Lookup ────────────────────────────────────────────────
    nav_panel(
      "Award Lookup",
      icon = icon("magnifying-glass-dollar"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Search by CFDA Number or Agency"),
          textInput("award_search_cfda", "CFDA Number", placeholder = "e.g. 93.224"),
          textInput("award_search_agency", "Awarding Agency", placeholder = "partial match"),
          actionButton("award_search_btn", "Search", class = "btn-primary"),
          br(), br(),
          DTOutput("award_results_table")
        ),
        card(
          card_header("Award Amount Distribution"),
          plotlyOutput("award_histogram", height = "350px")
        )
      )
    ),

    # ── Tab 4: Full Opportunity Browser ────────────────────────────────────
    nav_panel(
      "All Opportunities",
      icon = icon("table"),
      card(
        card_header("Browse All Opportunities"),
        DTOutput("all_opps_table")
      )
    ),

    # ── Tab 5: About / Methods ──────────────────────────────────────────────
    nav_panel(
      "About",
      icon = icon("circle-info"),
      card(
        card_body(
          h4("Federal Grant Volatility Tracker"),
          p("This application tracks changes to federal grant opportunity listings
            on Grants.gov over time and cross-references disappearances against
            award records from USASpending.gov."),
          h5("Key Finding"),
          p("Grants.gov retains formally closed and awarded opportunities in its
            public record. An opportunity that simply vanishes — with no closed
            status and no corresponding award in USASpending.gov — is classified
            as a ", strong("confirmed rescission"), ": evidence that the funding
            was administratively withdrawn rather than awarded or completed."),
          h5("Data Sources"),
          tags$ul(
            tags$li(strong("Grants.gov API"), " – Full opportunity listings
              (posted, forecasted, closed, archived). Snapshotted periodically;
              changes detected by diffing consecutive snapshots."),
            tags$li(strong("USASpending.gov API"), " – Federal grant award records
              including recipient, amount, and date. Linked to Grants.gov via
              CFDA number.")
          ),
          h5("Database"),
          p("All data is stored in a local DuckDB embedded analytical database
            (grants_volatility.duckdb). The Shiny app connects in read-only mode
            for all queries."),
          h5("Course"),
          p("EPPS 6354 Information Management – University of Texas at Dallas –
            Spring 2026 – Instructor: Karl Ho")
        )
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive: open DB connection for this session ─────────────────────────
  con <- get_con()
  onSessionEnded(function() {
    tryCatch(dbDisconnect(con, shutdown = FALSE), error = function(e) NULL)
  })

  # ── Populate filter dropdowns ──────────────────────────────────────────────
  observe({
    agencies <- dbGetQuery(con, "
      SELECT DISTINCT agency_name FROM opportunities
      WHERE agency_name IS NOT NULL
      ORDER BY agency_name
    ")$agency_name

    updateSelectizeInput(session, "agency_filter",
                         choices = agencies, server = TRUE)

    categories <- dbGetQuery(con, "
      SELECT category_code || ' – ' || category_name AS label,
             category_code AS value
      FROM categories ORDER BY category_name
    ")
    cat_choices <- setNames(categories$value, categories$label)
    updateSelectizeInput(session, "category_filter",
                         choices = cat_choices, server = TRUE)
  })

  # ── Reactive: refresh trigger ──────────────────────────────────────────────
  refresh_trigger <- reactiveVal(0)
  observeEvent(input$refresh_btn, {
    refresh_trigger(refresh_trigger() + 1)
  })

  # ── Reactive: missing grants data ─────────────────────────────────────────
  missing_data <- reactive({
    refresh_trigger()

    agency_clause    <- if (length(input$agency_filter) > 0)
      paste0("AND agency_name IN ('", paste(input$agency_filter, collapse = "','"), "')") else ""
    category_clause  <- if (length(input$category_filter) > 0)
      paste0("AND funding_activity_category IN ('",
             paste(input$category_filter, collapse = "','"), "')") else ""
    award_clause <- if (length(input$award_status) == 1) {
      if ("no_award" %in% input$award_status) "AND has_matching_award = FALSE"
      else "AND has_matching_award = TRUE"
    } else ""

    query <- sprintf("
      SELECT
        opportunity_id     AS 'Opp. ID',
        opportunity_number AS 'Opp. Number',
        title              AS 'Title',
        agency_name        AS 'Agency',
        funding_activity_category AS 'Category',
        CAST(removed_date AS VARCHAR)  AS 'Removed Date',
        PRINTF('$%%,.0f', COALESCE(estimated_total_funding, 0)) AS 'Est. Funding',
        PRINTF('$%%,.0f', COALESCE(award_ceiling, 0))           AS 'Award Ceiling',
        cfda_numbers       AS 'CFDA #(s)',
        CASE WHEN has_matching_award THEN 'Yes' ELSE 'NO ← RESCISSION' END AS 'Award Found?'
      FROM missing_grants
      WHERE CAST(removed_date AS DATE) BETWEEN DATE '%s' AND DATE '%s'
      %s %s %s
      ORDER BY removed_date DESC
    ", input$date_range[1], input$date_range[2],
       agency_clause, category_clause, award_clause)

    tryCatch(dbGetQuery(con, query),
             error = function(e) { message("missing_data error: ", e$message); data.frame() })
  })

  # ── Summary KPIs ──────────────────────────────────────────────────────────
  output$total_removed <- renderText({
    nrow(missing_data())
  })

  output$confirmed_rescissions <- renderText({
    sum(missing_data()$`Award Found?` == "NO ← RESCISSION", na.rm = TRUE)
  })

  output$funding_at_risk <- renderText({
    df <- missing_data()
    df <- df[df$`Award Found?` == "NO ← RESCISSION", ]
    # Re-query for numeric value
    n <- nrow(df)
    if (n == 0) return("$0")
    dollar(sum(as.numeric(gsub("[^0-9.]", "", df$`Est. Funding`)), na.rm = TRUE))
  })

  output$agencies_affected <- renderText({
    length(unique(missing_data()$Agency))
  })

  # ── Tab 1: Missing grants table ────────────────────────────────────────────
  output$missing_grants_table <- renderDT({
    df <- missing_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No records match current filters."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(
      df,
      rownames   = FALSE,
      selection  = "single",
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = c("csv", "excel"),
        pageLength = 25,
        scrollX    = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = c(5, 9)),
          # Highlight rescissions in red
          list(targets = 9,
               render = JS("function(data, type, row) {
                 if (type === 'display' && data.includes('RESCISSION')) {
                   return '<span style=\"color:#c0392b;font-weight:bold\">' + data + '</span>';
                 }
                 return data;
               }"))
        )
      )
    )
  })

  # ── Tab 2: Timeline plot ───────────────────────────────────────────────────
  output$timeline_plot <- renderPlotly({
    df <- tryCatch(
      dbGetQuery(con, "SELECT * FROM change_summary ORDER BY snap_date"),
      error = function(e) data.frame()
    )
    if (nrow(df) == 0) return(plotly_empty())

    p <- ggplot(df, aes(x = as.Date(snap_date), y = n, color = change_type,
                        group = change_type,
                        text = paste0(change_type, ": ", n, " on ", snap_date))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c(ADDED = "#27ae60", MODIFIED = "#f39c12",
                                    REMOVED = "#e74c3c")) +
      scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
      labs(x = NULL, y = "Count", color = "Change Type",
           title = "Grant Listing Changes Per Snapshot") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    ggplotly(p, tooltip = "text") |>
      layout(hovermode = "x unified")
  })

  output$agency_bar_plot <- renderPlotly({
    df <- tryCatch(
      dbGetQuery(con, "
        SELECT agency_name, COUNT(*) AS n
        FROM missing_grants
        WHERE has_matching_award = FALSE
        GROUP BY agency_name
        ORDER BY n DESC
        LIMIT 20
      "),
      error = function(e) data.frame()
    )
    if (nrow(df) == 0) return(plotly_empty())

    p <- ggplot(df, aes(x = reorder(agency_name, n), y = n,
                        text = paste0(agency_name, ": ", n, " rescissions"))) +
      geom_col(fill = "#2E5FA3", alpha = 0.85) +
      coord_flip() +
      labs(x = NULL, y = "Confirmed Rescissions", title = "Rescissions by Agency") +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = "text")
  })

  # ── Tab 3: Award lookup ────────────────────────────────────────────────────
  award_results <- eventReactive(input$award_search_btn, {
    cfda_clause   <- if (nzchar(trimws(input$award_search_cfda)))
      paste0("AND cfda_number ILIKE '%", trimws(input$award_search_cfda), "%'") else ""
    agency_clause <- if (nzchar(trimws(input$award_search_agency)))
      paste0("AND awarding_agency_name ILIKE '%", trimws(input$award_search_agency), "%'") else ""

    dbGetQuery(con, sprintf("
      SELECT
        cfda_number           AS 'CFDA',
        recipient_name        AS 'Recipient',
        recipient_state       AS 'State',
        awarding_agency_name  AS 'Awarding Agency',
        PRINTF('$%%,.0f', COALESCE(award_amount, 0)) AS 'Amount',
        CAST(award_date AS VARCHAR) AS 'Award Date',
        LEFT(description, 80) AS 'Description'
      FROM awards
      WHERE 1=1 %s %s
      ORDER BY award_amount DESC
      LIMIT 500
    ", cfda_clause, agency_clause))
  }, ignoreNULL = FALSE)

  output$award_results_table <- renderDT({
    df <- award_results()
    datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$award_histogram <- renderPlotly({
    df <- tryCatch(
      dbGetQuery(con, "SELECT award_amount FROM awards WHERE award_amount > 0 LIMIT 5000"),
      error = function(e) data.frame()
    )
    if (nrow(df) == 0) return(plotly_empty())

    p <- ggplot(df, aes(x = award_amount)) +
      geom_histogram(fill = "#2E5FA3", color = "white", bins = 40, alpha = 0.8) +
      scale_x_log10(labels = label_dollar(scale_cut = cut_short_scale())) +
      labs(x = "Award Amount (log scale)", y = "Count",
           title = "Distribution of Grant Award Amounts") +
      theme_minimal(base_size = 12)

    ggplotly(p)
  })

  # ── Tab 4: All opportunities table ────────────────────────────────────────
  output$all_opps_table <- renderDT({
    agency_clause   <- if (length(input$agency_filter) > 0)
      paste0("AND agency_name IN ('", paste(input$agency_filter, collapse = "','"), "')") else ""
    category_clause <- if (length(input$category_filter) > 0)
      paste0("AND funding_activity_category IN ('",
             paste(input$category_filter, collapse = "','"), "')") else ""

    df <- tryCatch(
      dbGetQuery(con, sprintf("
        SELECT
          opportunity_id     AS 'Opp. ID',
          opportunity_number AS 'Number',
          title              AS 'Title',
          agency_name        AS 'Agency',
          opportunity_status AS 'Status',
          CAST(post_date AS VARCHAR)  AS 'Posted',
          CAST(close_date AS VARCHAR) AS 'Closes',
          PRINTF('$%%,.0f', COALESCE(award_ceiling, 0)) AS 'Award Ceiling',
          cfda_numbers       AS 'CFDA',
          CASE WHEN is_active THEN 'Active' ELSE 'Removed' END AS 'DB Status'
        FROM opportunities
        WHERE 1=1 %s %s
        ORDER BY post_date DESC
        LIMIT 2000
      ", agency_clause, category_clause)),
      error = function(e) data.frame(Error = conditionMessage(e))
    )

    datatable(
      df,
      rownames   = FALSE,
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = c("csv"),
        pageLength = 20,
        scrollX    = TRUE
      )
    )
  })
}

# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
