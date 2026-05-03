-- =============================================================================
-- sample_queries.sql
-- Six demonstration queries. Each one directly answers a sub-question of the
-- research question:
--
--   How much federal grant funding has been disrupted since the 2025
--   administration change — either rescinded or terminated — and which
--   agencies and recipients are most affected?
-- =============================================================================


-- -----------------------------------------------------------------------------
-- Q1. Total disruption — opportunities rescinded vs. awards terminated.
--     Establishes the headline numbers.
-- -----------------------------------------------------------------------------
SELECT
  COUNT(*) FILTER (WHERE change_type = 'REMOVED')                    AS rescinded_opportunities,
  (SELECT COUNT(*) FROM terminations WHERE has_been_terminated)       AS terminated_awards,
  (SELECT SUM(estimated_total_funding) FROM opportunities o
   JOIN change_log cl USING (opportunity_id) WHERE cl.change_type = 'REMOVED') AS rescinded_dollars,
  (SELECT SUM(award_value) FROM terminations WHERE has_been_terminated) AS terminated_dollars
FROM change_log;


-- -----------------------------------------------------------------------------
-- Q2. Disruption by agency — which agencies are most affected?
--     Uses the canonical agencies table with self-FK rollup so HHS sub-agencies
--     (NIH, CDC, SAMHSA) can be aggregated to HHS.
-- -----------------------------------------------------------------------------
SELECT
  COALESCE(parent.agency_name, child.agency_name) AS agency_or_parent,
  COUNT(DISTINCT cl.opportunity_id) FILTER (WHERE cl.change_type = 'REMOVED') AS rescinded,
  COUNT(DISTINCT tm.termination_id) FILTER (WHERE tm.has_been_terminated)     AS terminated,
  SUM(CASE WHEN cl.change_type = 'REMOVED' THEN o.estimated_total_funding END) AS rescinded_dollars,
  SUM(CASE WHEN tm.has_been_terminated     THEN tm.award_value             END) AS terminated_dollars
FROM agencies child
LEFT JOIN agencies parent ON parent.agency_id = child.parent_agency_id
LEFT JOIN opportunities o   ON o.agency_id        = child.agency_id
LEFT JOIN change_log    cl  ON cl.opportunity_id  = o.opportunity_id
LEFT JOIN terminations  tm  ON tm.source_agency_id = child.agency_id
GROUP BY agency_or_parent
ORDER BY (COALESCE(SUM(CASE WHEN cl.change_type='REMOVED' THEN o.estimated_total_funding END),0)
        + COALESCE(SUM(CASE WHEN tm.has_been_terminated THEN tm.award_value END),0)) DESC;


-- -----------------------------------------------------------------------------
-- Q3. Top 20 most-affected recipients (with parent UEI rollup).
--     Demonstrates the recipients self-join — terminated awards across all
--     campuses of a university system roll up to the parent organization.
-- -----------------------------------------------------------------------------
SELECT
  COALESCE(parent.recipient_name, child.recipient_name) AS recipient_or_parent,
  child.state_code,
  COUNT(DISTINCT a.award_id_fain)                       AS n_terminated_awards,
  SUM(tm.award_value)                                   AS at_risk_dollars,
  SUM(tm.post_termination_deobligation)                 AS deobligated_dollars
FROM terminations tm
JOIN awards     a       ON a.award_id_fain         = tm.award_id_fain
JOIN recipients child   ON child.recipient_uei     = a.recipient_uei
LEFT JOIN recipients parent ON parent.recipient_uei = child.recipient_parent_uei
WHERE tm.has_been_terminated = TRUE
GROUP BY recipient_or_parent, child.state_code
ORDER BY at_risk_dollars DESC NULLS LAST
LIMIT 20;


-- -----------------------------------------------------------------------------
-- Q4. Confirmed rescissions — opportunities that disappeared from Grants.gov
--     with no matching USA Spending award AND no source-agency termination
--     record either. Funding that simply vanished from the public record.
-- -----------------------------------------------------------------------------
SELECT
  o.opportunity_id,
  o.title,
  ag.agency_name,
  cl.snapshot_date AS removed_date,
  o.cfda_number,
  o.cfda_title,
  o.estimated_total_funding
FROM change_log cl
JOIN opportunities o ON o.opportunity_id = cl.opportunity_id
LEFT JOIN agencies ag ON ag.agency_id = o.agency_id
WHERE cl.change_type = 'REMOVED'
  AND NOT EXISTS (
    SELECT 1 FROM awards a
    WHERE a.cfda_number = o.cfda_number
      AND a.award_date >= o.post_date
  )
  AND NOT EXISTS (
    SELECT 1 FROM terminations tm
    WHERE tm.cfda_number = o.cfda_number
      AND tm.has_been_terminated = TRUE
  )
ORDER BY o.estimated_total_funding DESC NULLS LAST
LIMIT 50;


-- -----------------------------------------------------------------------------
-- Q5. Volatility timeline — month-by-month count of rescissions and
--     terminations on a shared time axis. The longitudinal view the proposal
--     motivated.
-- -----------------------------------------------------------------------------
WITH rescissions AS (
  SELECT DATE_TRUNC('month', cl.snapshot_date)::DATE AS month,
         'rescission'                                AS event_kind,
         COUNT(*)                                    AS n,
         SUM(o.estimated_total_funding)              AS dollars
  FROM change_log cl
  JOIN opportunities o ON o.opportunity_id = cl.opportunity_id
  WHERE cl.change_type = 'REMOVED'
  GROUP BY 1
),
terminations_monthly AS (
  SELECT DATE_TRUNC('month', latest_termination_date)::DATE AS month,
         'termination'                                       AS event_kind,
         COUNT(*)                                            AS n,
         SUM(award_value)                                    AS dollars
  FROM terminations
  WHERE has_been_terminated = TRUE
  GROUP BY 1
)
SELECT * FROM rescissions
UNION ALL
SELECT * FROM terminations_monthly
ORDER BY month, event_kind;


-- -----------------------------------------------------------------------------
-- Q6. Reinstatement rate by agency — how often does a termination get
--     reversed? Adds a partial counter-narrative to the headline disruption
--     numbers.
-- -----------------------------------------------------------------------------
SELECT
  ag.agency_name,
  COUNT(*)                                              AS n_terminations,
  COUNT(*) FILTER (WHERE has_been_reinstated)           AS n_reinstated,
  ROUND(100.0 * COUNT(*) FILTER (WHERE has_been_reinstated)
              / NULLIF(COUNT(*), 0), 1)                 AS pct_reinstated,
  AVG(latest_reinstatement_date - first_termination_date) AS avg_days_to_reinstate
FROM terminations tm
JOIN agencies ag ON ag.agency_id = tm.source_agency_id
WHERE tm.has_been_terminated = TRUE
GROUP BY ag.agency_name
ORDER BY pct_reinstated DESC;
