# Federal Grant Disruption Database
## A Minimal Schema for Tracking Rescissions and Terminations

**EPPS 6354 — Information Management — Spring 2026**
**Emily Stern — Instructor: Karl Ho**

---

## Research question

> **How much federal grant funding has been disrupted since the 2025 administration change — either rescinded at the opportunity level (Grants.gov listing vanished) or terminated at the award level (per the five agency feeds) — and which agencies and recipients are most affected?**

This question drives every design choice in the schema below. A table earns a place if it is needed to answer some portion of this question; if it is not, it has been dropped or its content inlined.

---

## 1. How the question evolved from the original proposal

The original proposal was framed narrowly: track Grants.gov opportunity volatility (additions, modifications, removals) and detect "rescissions" — opportunities that vanished from Grants.gov without being awarded. The proposed schema had six tables: `agencies`, `opportunities`, `categories`, `opportunity_categories`, `snapshots`, `change_log`.

The data scope has since expanded to include the dependent variable from the supply side:

- **Five agency-level termination feeds** (NSF, NIH, EPA, CDC, SAMHSA) capturing awards that were terminated, frozen, suspended, or reinstated. The proposal could only infer disruption from absence; these feeds provide affirmative confirmation.
- **USA Spending bulk CSV exports** (FY2024–FY2026) providing the actual award records that opportunities resolve into.

The expanded research question now covers both ends of the disruption pipeline: opportunities that disappeared (rescissions, detected from Grants.gov change history) and awards that were stopped (terminations, detected from the five agency feeds). The schema has to support both.

---

## 2. What the proposal had that is no longer needed

| Proposal table | Status | Why |
|---|---|---|
| `agencies` | **Kept** | Cross-agency comparison is core to the research question |
| `opportunities` | **Kept** | Anchor of the rescission half of the question |
| `change_log` | **Kept** | The Type-2 SCD audit trail that detects rescissions |
| `snapshots` | **Inlined into change_log** | Snapshot metadata (record counts, ingestion notes) isn't queried in any disruption analysis. The snapshot date is the only thing needed and it lives directly on `change_log` rows |
| `categories` | **Dropped** | Funding-category aggregation is not part of the research question. The 25 Grants.gov category codes (HL, ED, ST, etc.) are a classification scheme, not a disruption signal |
| `opportunity_categories` | **Dropped** | Junction table that was only useful for category-based aggregation |

Two new tables were added because the data scope expanded:

| New table | Why |
|---|---|
| `recipients` | "Which recipients are most affected" requires recipient-level grouping. Self-FK on `recipient_parent_uei` lets us roll university-system campuses up to their parent organization |
| `awards` | The award-level entity that links opportunities to terminations. Already in the implementation as a flat dump; this version FKs it cleanly |
| `terminations` | The dependent variable. Unifies the five source-agency feeds into one shape |

---

## 3. The six-table schema

```
agencies ──┬──< opportunities ──< change_log
           │           │
           │           └──< awards ──< terminations
           │                ▲
           ├────────────────┤  (awarding + funding agency FKs)
           │                │
           └────────────────┴────< terminations.source_agency_id
                            ▲
                            │
                       recipients
```

| # | Table | Role | Why it earns its keep |
|---|---|---|---|
| 1 | `agencies` | Federal agencies with self-FK hierarchy | Canonical agency naming for "which agencies most affected" |
| 2 | `recipients` | Organizations receiving awards, self-FK on parent UEI | Recipient-level disruption analysis with parent rollup |
| 3 | `opportunities` | Grants.gov listings | The "what was promised" half of disruption |
| 4 | `awards` | USA Spending awards | The "what got funded" half; bridges opportunities to terminations |
| 5 | `terminations` | Unified across 5 source agencies | The dependent variable for award-level disruption |
| 6 | `change_log` | Type-2 SCD audit trail for opportunities | The detection mechanism for opportunity-level rescissions |

Plus three analytical views (`v_disruption_by_agency`, `v_disruption_by_recipient`, `v_confirmed_rescissions`) that are explicitly derived objects, not sources of truth.

---

## 4. Pragmatic compromises (acknowledged 1NF/3NF violations)

A 3NF-pure schema for this data lands at roughly thirty tables. For a 6-table version aimed at one specific research question, six pragmatic compromises were accepted:

| Compromise | Where | Why it's acceptable |
|---|---|---|
| Multiple CFDA per opportunity → primary CFDA only | `opportunities.cfda_number` | Multi-CFDA opportunities are a small minority. If the secondary CFDAs become important, an `opportunity_cfda` junction can be added in a single migration |
| `cfda_title` denormalized | `opportunities`, `awards` | Stored as a string rather than FK to a `cfda_programs` dimension. Same value can drift across rows; refresh job rewrites all rows from the canonical Assistance Listings file when it updates |
| State stored as inline VARCHAR | `recipients`, `awards` | A 50-row dimension table for states isn't worth its join cost at this schema scale |
| Small code/description pairs inlined | `agencies.agency_level`, `terminations.current_status`, `change_log.change_type` | 3–7 row vocabularies. CHECK constraints enforce the allowed values |
| Transaction-level modifications dropped | (no `award_transactions` table) | "Was this award terminated?" doesn't require the modification log. Award-level totals are sufficient |
| Concatenated event_history kept as text | `terminations.notes` | The chronological event log from the source CSVs (1NF violation) is preserved as free text rather than parsed into a separate events table. Searching the text is enough to answer the basic question; if event-level analysis becomes needed, parsing into a child table is mechanical |

Each compromise is explicitly documented and reversible. The schema is forward-compatible: adding back `cfda_programs`, `award_transactions`, or `termination_events` is a non-disruptive `CREATE TABLE` plus an `INSERT … SELECT` against existing rows.

---

## 5. Volatility tracking

The disruption signal lives in two append-only tables:

- **`change_log`** captures opportunity-level rescissions. Every detected `ADDED` / `REMOVED` / `MODIFIED` event is appended with a snapshot date; existing rows are never overwritten. This is a Type-2 slowly changing dimension. The live `opportunities` table holds the current state; `change_log` holds every prior state and every transition.

- **`terminations`** captures award-level terminations. Each row is the latest reconciled state of a (source_agency, grant_id) pair, including the latest_termination_date and latest_reinstatement_date and the boolean flags (has_been_terminated, has_been_reinstated, has_been_frozen) that summarize the event_history.

A confirmed rescission is the three-way intersection of:

1. A `change_log` row with `change_type = 'REMOVED'`,
2. No matching row in `awards` for the opportunity's CFDA number after its post date,
3. No matching row in `terminations` for the same CFDA with `has_been_terminated = TRUE`.

That definition is captured in the `v_confirmed_rescissions` view in `schema.sql` and exercised by query Q4 in `sample_queries.sql`.

---

## 6. Ingestion roadmap

1. **Reference layer**: load `agencies` (departments first, then sub-agencies and offices with `parent_agency_id` populated). One-time seed.
2. **Recipients**: deduplicate USA Spending CSV by `recipient_uei`. Parent UEI joined via second pass.
3. **Awards**: one row per `assistance_award_unique_key`. FKs to recipients, agencies (×2 roles), and opportunities (where the FAIN matches an opportunity number).
4. **Opportunities + change_log**: continue the existing daily Grants.gov pull; populate `change_log` via diff against the prior snapshot.
5. **Terminations**: load each of the five CSVs, projecting columns into the unified shape with `source_agency_id` populated. The free-text `event_history` is preserved in the `notes` column.

The existing R scripts cover phase 4 fully and phases 2–3 partially. Phase 1 (a small reference loader) and phase 5 (five thin projection scripts, one per agency CSV) are the new work.

---

## 7. Open trade-offs

**FAIN collisions across fiscal years.** The same `award_id_fain` can appear in two distinct `assistance_award_unique_key` rows when reused across fiscal years. The `terminations.award_id_fain` FK is unique-where-not-null, with explicit handling of the multi-match case in queries that join terminations back to awards.

**DuckDB vs PostgreSQL.** The DDL is standard SQL with DuckDB-compatible types. PostgreSQL migration substitutes `DOUBLE` → `DOUBLE PRECISION`, `TIMESTAMPTZ DEFAULT now()` → `TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP`, and surrogate-key auto-increment → `GENERATED BY DEFAULT AS IDENTITY`.

**Recipient deduplication.** UEI is the authoritative key, but the source data sometimes has the same organization under variant UEIs (legal-name changes, mergers). The schema uses `recipient_uei` as PK and trusts the source; if recipient-level disruption analysis surfaces obvious duplicates, a name-cleaning pass against the SAM.gov entity registry can be added without schema changes.

---

## 8. Companion files

- `schema.sql` — `CREATE TABLE` statements for all 6 tables and the 3 analytical views, DuckDB-compatible
- `sample_queries.sql` — six demonstration queries, each directly answering a sub-question of the research question
- `er_diagram.mmd` and `er_diagram.html` — entity-relationship diagram
- `slides.html` — in-class presentation, opens in any browser
