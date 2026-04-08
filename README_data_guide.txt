==============================================================================
DATA GUIDE & REPLICATION FILE MAP
Tariff Exemptions x Supplier Networks
==============================================================================
Last updated: 2026-03-18
Status: DRAFT — please review and correct any errors.

This document describes the data files in the Exemptions Project folder,
their provenance, structure, and intended role in the analysis pipeline.
Please confirm or correct each section.


==============================================================================
1. OVERVIEW OF DATA SOURCES
==============================================================================

The project uses four primary data sources:

  A) FactSet Supply Chain Data    — firm-supplier relationship ties (edges)
  B) ImportYeti / Yeti Records    — import shipping records (weighted edges)
  C) USTR Exemption Requests      — treatment variable (granted/denied)
  D) Compustat (via FactSet .dta) — firm-level financial controls

And two identifier systems:

  - GVKEY: Primary identifier. Links to Compustat and most financial databases.
           Used for main analyses.
  - ISIN:  Secondary identifier. Less comprehensive (suppliers often lack ISIN).
           Used for robustness checks.


==============================================================================
2. FOLDER-BY-FOLDER DATA DESCRIPTION
==============================================================================

------------------------------------------------------------------------------
2a. edge_nodes/Exemptions_SampleData.xlsx
    (also stored as data/factset.xlsx in the Dropbox repo)
------------------------------------------------------------------------------
SOURCE:   FactSet Supply Chain Relationships database
ROLE:     Edge Set #1 — TOP SUPPLIERS ONLY (unweighted, yearly)
ROWS:     ~74,793
YEARS:    2003–2023 (confirm range)
COLUMNS:
  - fyear             Year of the relationship
  - firm_gvkey        Buying firm GVKEY (numeric)
  - supplier_gvkey    Supplying firm GVKEY (numeric)
  - revenue_percent   Share of buyer revenue from this supplier (often missing)
  - firm_isin         Buyer ISIN code
  - firm_hq           Buyer HQ country (2-letter ISO: US, CN, GB, etc.)
  - supplier_isin     Supplier ISIN code
  - supplier_hq       Supplier HQ country (2-letter ISO)

NOTES:
  - Captures only major/disclosed supplier relationships.
  - Unweighted: a tie is present or absent, no volume measure.
  - CONFIRM: Supplier HQ country for firm nodes is pending from coauthor.
  - CONFIRM: Is the year range correct? Earliest observed year is 2011 in sample.

PIPELINE USE:
  - 01_load_data.R (Section A): Loaded as the bipartite edge list.
  - 02_build_metrics.R: Used to compute Jaccard similarity, churn, retention,
    add/drop rates, and pre-policy overlap network (W).

HYPOTHESES SERVED:
  - H1 (main): Do exemption grants stabilize supplier networks (lower churn)?
  - H2 (spillovers): Do peer firms' exemption outcomes affect a firm's churn?
  - H3 (geography): Do exemption outcomes shift China vs. non-China sourcing?


------------------------------------------------------------------------------
2b. FactSet/FactSet_19_23.dta
------------------------------------------------------------------------------
SOURCE:   FactSet merged with Compustat fundamentals
ROLE:     Firm-level financial controls + industry classification
ROWS:     ~38K (firm-supplier-year pairs, 2019–2023)
YEARS:    2019–2023
FORMAT:   Stata .dta
COLUMNS (29 total):
  Firm variables (prefix f):
  - fyear       Fiscal year (date format, extract year)
  - fgvkey      Firm GVKEY
  - ffic        Firm country of incorporation (ISO)
  - fsic        Firm SIC code (4-digit)
  - fsic_2      Firm SIC code (2-digit numeric)
  - fsic_2n     Firm SIC code (2-digit, labeled)
  - fat         Total assets
  - frevt       Total revenue
  - fcogs       Cost of goods sold
  - fib         Income before extraordinary items
  - fdltt       Long-term debt
  - fche        Cash and short-term investments
  - fsale       Net sales
  - finvt       Inventories
  - fxrd        R&D expenditure

  Supplier variables (prefix s):
  - sgvkey, sfic, ssic, sat, srevt, scogs, sib, sdltt, sche, ssale, sinvt, sxrd

  Match quality:
  - quantified  Whether the relationship is quantified (0/1)
  - Revenue     Reported revenue from the relationship

NOTES:
  - Coverage is 2019–2023 only. For panel years outside this range, firm
    financials will be forward/back-filled within firm or set to NA.
  - CONFIRM: Is additional Compustat data coming (pre-2019 or post-2023)?

PIPELINE USE:
  - 01_load_data.R (Section B): Extracts one row per firm-year for controls
    (log_assets, leverage, log_revenue, SIC industry).
  - 03_models.R: log_assets, leverage, log_degree used as regression controls.
  - 05_robustness.R: Entropy balancing on baseline covariates.

HYPOTHESES SERVED:
  - All models (controls for firm size, leverage, industry fixed effects).


------------------------------------------------------------------------------
2c. CSV Files/Tables/ — Exemption Summary Tables
------------------------------------------------------------------------------
SOURCE:   USTR Section 301 exclusion process (scraped from regulations.gov)
ROLE:     TREATMENT VARIABLE — exemption request outcomes
FILES:
  Requests1_Table.csv   ~10,813 rows   List 1 (docket USTR-2018-0025)
  Requests2_Table.csv    ~2,868 rows   List 2 (docket USTR-2018-0032)
  Requests3_Table.csv   ~30,282 rows   List 3 (docket USTR-2019-0005)
  Requests4a_Table.csv   ~8,779 rows   List 4a (docket USTR-2019-0017)

SCHEMA (Lists 1–2):
  - Doc Type             Always "REQUEST"
  - Request Doc-ID       Unique request identifier (e.g., USTR-2018-0025-0963)
  - Organization Name    Company that filed the request
  - 10-Digit HTS        HTS product code
  - Date Posted          Date posted (Excel serial number)
  - Response Closes      Response deadline
  - Reply Closes         Reply deadline
  - Stages               Decision outcome: Granted, Denied, etc.

SCHEMA (Lists 3–4a):
  - Request              Unique request ID
  - Org                  Organization name
  - Status               Granted / Denied
  - HTSUS                HTS product code
  - Product              Product description
  - Post Date            Date posted (text: DD-Mon-YY)
  - Close Date           Response deadline

NOTES:
  - Lists 1–2 use Excel serial date numbers; Lists 3–4a use text dates.
  - Status/Stages values need to be harmonized across lists.
  - The org-to-GVKEY match is the critical merge step. Organization names
    must be linked to FactSet GVKEYs via a crosswalk (fuzzy string matching
    or manual review). The pipeline creates a template crosswalk file at
    data/org_gvkey_crosswalk.csv.
  - CONFIRM: Are there additional decision statuses beyond Granted/Denied
    (e.g., Withdrawn, Pending)?
  - CONFIRM: Some firms filed across multiple lists. How should multi-list
    treatment be handled?

PIPELINE USE:
  - 01_load_data.R (Section D): All four tables are loaded and harmonized
    into a single requests table.
  - 01_load_data.R (Section E): Matched to FactSet GVKEYs via crosswalk.
  - 01_load_data.R (Section G): Aggregated to firm-year waiver variables
    (waiver_any, deny_any, grant_share, g_year for staggered adoption).

HYPOTHESES SERVED:
  - H1: Exemption grant/denial as the treatment in DiD designs.
  - H4 (dose–response): Proportion of requests granted (grant_share).
  - H5 (DDD): Interaction with political sensitivity (from comments data).


------------------------------------------------------------------------------
2d. CSV Files/FullRequests/ — Detailed Request Records
------------------------------------------------------------------------------
SOURCE:   Scraped from regulations.gov (full document metadata)
ROLE:     Comment/response data for separate analysis; rich text for NLP
FILES:
  Requests1_Fullgov.csv    ~22,654 rows   List 1 (regulations.gov metadata)
  Requests1a_Fullgov.csv    ~4,513 rows   List 1a supplement
  Requests2_Fullgov.csv     ~5,571 rows   List 2
  Requests3_Full.csv       ~30,282 rows   List 3 (custom-scraped format)
  Requests4a_Full.csv       ~8,779 rows   List 4a

SCHEMA (Lists 1–2, regulations.gov format, ~57 columns):
  Key fields: Document ID, Organization Name, Comment, Category,
  First Name, Last Name, City, State, Country, Posted Date,
  Attachment Files (URLs to PDFs)

SCHEMA (Lists 3–4a, custom format, ~38+ columns):
  Key fields: Request_ID, Org, HTSUS, Product_Name, Product_Desc,
  Product_Func, Relationship (Importer/Producer/Purchaser),
  Comp_US (US alternatives available?), Argue_US (argument text),
  Comp_Other (third-country alternatives?), Argue_Other,
  COO_CN (country of origin China?), CN25 (Made in China 2025 relevance),
  RespN1/Resp_Det1/Resp_Position1/Resp1/Reply1 (comment-response pairs),
  Attach1–Attach10 (attachment URLs)

NOTES:
  - The FullRequests contain the actual text of comments, responses, and
    arguments. This is the basis for the SEPARATE COMMENTS ANALYSIS.
  - Lists 1–2 use the regulations.gov bulk export format.
  - Lists 3–4a use a custom scraped format with structured fields for
    arguments about US alternatives, third-country alternatives, etc.
  - The Partial/ subdirectory contains intermediate scraping outputs.
  - The Links/ subdirectory contains URL mappings for document downloads.
  - CONFIRM: Will comments be used as a moderating variable (political
    sensitivity) or as a separate dependent variable, or both?

PIPELINE USE:
  - Not currently in the R pipeline.
  - Planned: Separate comments analysis (may require NLP/text processing).
  - Could extract comment counts and support/oppose ratios for the main
    models if desired.

HYPOTHESES SERVED:
  - H5 (comments): Separate analysis of comment dynamics.
  - H5 (DDD moderator): Political sensitivity via comment intensity.


------------------------------------------------------------------------------
2e. Yeti_Records/ — Import Shipping Data
------------------------------------------------------------------------------
SOURCE:   ImportYeti (import shipping/customs records)
ROLE:     Edge Set #2 — IMPORT VOLUMES (weighted, product-level)
FILES:    ~370 CSV files, one per US buyer company (~294 unique firms)
          Some firms have multiple files (e.g., "ABB 1.csv", "ABB 2.csv")
FORMAT:   CSV with ~58 columns per file

KEY COLUMNS:
  Shipment info:
  - Arrival Date           Shipment arrival (daily frequency)
  - House Bol Number       Bill of lading
  - Container ID/Size/Type Container details
  - Value in USD (CIF)     Shipment value
  - Quantity / Quantity Unit
  - Weight                 Shipment weight
  - TEU                    Twenty-foot equivalent units

  Buyer info:
  - Company Name           US importing company name
  - Company Address        US company address
  - Company Country        Always "United States"

  Supplier info:
  - Supplier Name          Foreign supplier name
  - Supplier Address       Supplier address
  - Supplier Country       Supplier country (e.g., "China", "Hong Kong S.A.R.")
  - Supplier Country Code  ISO code (CN, HK, DE, etc.)

  Product info:
  - HS Code                Harmonized System code (6-digit)
  - HS Code Description    Text description
  - HTS Code               HTS code (may be blank)
  - Product Description    Free-text product description

  Metadata:
  - Destination Port / Departure Port
  - Vessel Name / Voyage
  - Carrier SCAC Code
  - Company China Concentration  Share of imports from China

NOTES:
  - Daily granularity; will need to be aggregated to weekly/monthly/quarterly.
  - WEIGHTED edges: Value in USD, Quantity, and Weight provide edge weights.
  - Company names need cleaning — the same firm appears under different
    name variations across files and within files.
  - Non-US imports ONLY (by definition, these are US customs records of
    incoming shipments).
  - Must be matched to FactSet GVKEYs via a company-name crosswalk.
  - CONFIRM: What time period do the Yeti records cover?
  - CONFIRM: Some files appear to be multiple batches for the same firm
    (e.g., "Aceto 1.csv", "Aceto 2.csv", "Aceto 3.csv"). Are these
    different time periods or different product categories?

PIPELINE USE:
  - NOT YET IN PIPELINE. Needs a dedicated cleaning script.
  - Will become the second edge set (weighted) alongside FactSet (unweighted).
  - Potential for product-level regressions (FE by HS code) or
    product-weighted edge aggregation.

HYPOTHESES SERVED:
  - H1 (weighted): Does exemption status affect import volumes (not just ties)?
  - H3 (geography, weighted): Shifts in China sourcing by value/volume.
  - H6 (product-level): Product-level heterogeneity in exemption effects,
    with product fixed effects or product-specific regressions.
  - Robustness: Alternative edge definition (trade flows vs. disclosed ties).


------------------------------------------------------------------------------
2f. Excel Files/
------------------------------------------------------------------------------
SOURCE:   USTR exemption data (alternative format)
FILES:
  Requests_Table1.xlsx   (~3.1 MB)  List 1 exemption table
  Request_Table2.xlsx    (~145 KB)  List 2 exemption table

NOTES:
  - These appear to be Excel versions of the same data in CSV Files/Tables/.
  - Per Jared's note: can be ignored in favor of the CSV versions.

PIPELINE USE: Not used.


------------------------------------------------------------------------------
2g. PDFs/
------------------------------------------------------------------------------
SOURCE:   Downloaded from regulations.gov (exemption request attachments)
ROLE:     Raw source documents for the exemption requests
SUBDIRS:
  Req1_Attachments/     List 1 request attachments
  Req1a_Attachments/    List 1a request attachments
  Req2_Attachments/     List 2 request attachments
  Req3_Attachments1/    List 3 batch 1
  Req3_Attachments2/    List 3 batch 2
  Req3_Attachments3/    List 3 batch 3
  Req3_Links/           List 3 download links
  Req3_Links1/          List 3 additional links
  Req4a_Attachments/    List 4a attachments
  Req4a_Links/          List 4a download links

NOTES:
  - These are the original PDF filings submitted by companies.
  - File naming follows USTR docket IDs (e.g., USTR-2018-0025-10000).
  - failed_downloads.txt tracks PDFs that could not be retrieved.
  - Potentially useful for NLP/text analysis of request arguments.
  - CONFIRM: Are these needed for the main analysis or only for the
    comments analysis?

PIPELINE USE: Not currently used in R pipeline.


------------------------------------------------------------------------------
2h. Python/
------------------------------------------------------------------------------
SOURCE:   Coauthor's scraping/extraction code
ROLE:     Data collection pipeline (regulations.gov scraping)
STRUCTURE:
  Extractions Code/     Notebooks that scrape and parse request data
    - ExemptionTable_Extract.ipynb         Table extraction for Lists 1–2
    - Exemptions3Full_Extract.ipynb        Full request extraction, List 3
    - Exemptions3Full_Extract_Backwards.ipynb  List 3 (reverse order)
    - Exemptions4aFull_Extract.ipynb       Full request extraction, List 4a

  Links Code/           Notebooks that extract download URLs
    - Exemptions1Full_Links.ipynb          List 1 attachment URLs
    - Exemptions2Full_Links.ipynb          List 2 attachment URLs
    - Exemptions3Full_Links.ipynb          List 3 attachment URLs
    - Exemptions4aFull_Links.ipynb         List 4a attachment URLs

  Root level:
    - Exemptions1Full_Extract.ipynb        List 1 full extraction
    - Sample_Crawl (Brian).ipynb           Sample/prototype crawler

  old/                  Deprecated versions

NOTES:
  - These notebooks produced the CSV files in CSV Files/.
  - Need to be converted to clean, documented scripts for the replication file.
  - CONFIRM: Are there any manual steps in the extraction process that
    should be documented?

PIPELINE USE: Upstream data collection (not part of the R analysis pipeline).


------------------------------------------------------------------------------
2i. R/ — Analysis Pipeline (adapted from Dropbox scaffold)
------------------------------------------------------------------------------
ROLE:     Main analysis code
FILES:
  00_setup.R              Package loading, paths, helper functions
  01_load_data.R          Load FactSet + FactSet financials + exemption tables
  02_build_metrics.R      Jaccard, churn, retention, exposure variables
  03_models.R             TWFE, event-study (sunab), Callaway-Sant'Anna
  04_network_spillovers.R Exposure controls, community clustering, MRQAP
  05_robustness.R         Alt outcomes, alt treatments, balancing, placebo
  06_tables_figures.R     Summary stats, trend plots, network visualization

PIPELINE USE: This IS the analysis pipeline.


------------------------------------------------------------------------------
2j. data/ and output/
------------------------------------------------------------------------------
data/   — Working data directory. Place factset.xlsx here (from edge_nodes/
          or Dropbox). Intermediate .rds files are written here by the pipeline.
output/ — Generated tables (LaTeX, CSV) and figures (PNG). Currently empty.


==============================================================================
3. HOW DATA SOURCES MAP TO HYPOTHESES
==============================================================================

HYPOTHESIS H1 (Main Effect):
  "Firms granted tariff exemptions exhibit lower supplier network churn
  (higher Jaccard stability) relative to denied or non-requesting firms."
  DATA: FactSet edges (churn DV) + Exemption Tables (treatment)
  DESIGN: Staggered DiD (first grant year), Callaway-Sant'Anna

HYPOTHESIS H2 (Network Spillovers):
  "A firm's supplier churn is affected by the exemption outcomes of peer
  firms that share overlapping supplier networks."
  DATA: FactSet edges (pre-policy overlap W) + Exemption Tables
  DESIGN: Exposure mapping (W * treatment vector), community clustering

HYPOTHESIS H3 (Geographic Recomposition):
  "Denied firms shift sourcing away from China; granted firms maintain or
  increase China sourcing."
  DATA: FactSet edges (share_china DV) + Yeti Records (weighted volumes)
  DESIGN: DiD with share_china or share_usa as outcome

HYPOTHESIS H4 (Dose–Response):
  "The magnitude of network stabilization scales with the proportion of
  requests granted (grant_share) rather than being a binary effect."
  DATA: Exemption Tables (grant_share = n_grant / n_decided)
  DESIGN: Continuous treatment intensity FE model

HYPOTHESIS H5 (Political Sensitivity / Comments):
  "Politically sensitive requests (high comment counts, contested support/
  oppose ratios) are treated differently, and this moderates the network
  effect."
  DATA: FullRequests (comment text, response counts) + Exemption Tables
  DESIGN: DDD (treatment × post × sensitivity) — may be separate analysis

HYPOTHESIS H6 (Product-Level Heterogeneity):
  "Exemption effects vary by product type (HTS code), with products
  lacking US/third-country alternatives showing larger effects."
  DATA: Yeti Records (product-level trade flows) + Exemption Tables (HTS)
  DESIGN: Product FE or product-specific regressions; weighted edges


==============================================================================
4. TWO EDGE SETS (NETWORK CONSTRUCTION)
==============================================================================

The project constructs firm-supplier networks from TWO sources:

  EDGE SET 1: FactSet (top disclosed suppliers)
    - Source:    edge_nodes/Exemptions_SampleData.xlsx
    - Coverage:  Major supplier relationships reported in SEC filings
    - Weighting: UNWEIGHTED (tie present/absent)
    - Frequency: YEARLY
    - ID:        GVKEY (both firm and supplier)
    - Scope:     Global (not limited to imports)
    - Use:       MAIN ANALYSIS

  EDGE SET 2: Yeti Records (import shipments)
    - Source:    Yeti_Records/*.csv (~370 files, ~294 firms)
    - Coverage:  All US customs import records for sampled firms
    - Weighting: WEIGHTED (by USD value, quantity, or weight)
    - Frequency: DAILY (to be aggregated to weekly/monthly/quarterly)
    - ID:        Company name (needs matching to GVKEY)
    - Scope:     Non-US imports into the US only
    - Use:       ROBUSTNESS / WEIGHTED ANALYSIS / PRODUCT-LEVEL

  Combined: The two edge sets will be merged into a single network.
  The weighted edges from Yeti can support product-level regressions
  or product fixed effects.

  CLEANING NEEDED FOR YETI:
    - Firm name harmonization (same firm appears under different names)
    - Match firm names to GVKEYs
    - Aggregate daily records to appropriate time frequency
    - Handle multiple files per firm (concatenate)


==============================================================================
5. IDENTIFIER STRATEGY
==============================================================================

  PRIMARY:   GVKEY (numeric, connects to Compustat, FactSet, most databases)
  SECONDARY: ISIN (alphanumeric, less common in the field, not all suppliers
             have ISIN; used for robustness only)

  Matching challenges:
    - Exemption data uses COMPANY NAMES (no GVKEY or ISIN)
    - Yeti data uses COMPANY NAMES (no GVKEY or ISIN)
    - A name-to-GVKEY crosswalk is required for both
    - The pipeline generates a template at data/org_gvkey_crosswalk.csv

  Pending data:
    - Company HQ for FactSet firms (coming from coauthor)
    - Additional GVKEY coverage


==============================================================================
6. ITEMS REQUIRING CONFIRMATION
==============================================================================

Please review and confirm or correct the following:

  [ ] Year range for FactSet edge data (README says 2003–2023, but earliest
      observed year in sample is 2011)
  [ ] Time coverage of Yeti Records (what years/months?)
  [ ] Multiple Yeti files per firm — different time windows or product batches?
  [ ] Status values in exemption data beyond Granted/Denied (Withdrawn? Pending?)
  [ ] Multi-list treatment handling (firm files across Lists 1, 2, 3, 4a)
  [ ] Role of comments: moderator in main models, separate analysis, or both?
  [ ] Are PDFs needed for analysis or only archival?
  [ ] Is pre-2019 Compustat data coming for financial controls?
  [ ] Confirm company HQ data timeline from coauthor
  [ ] Any manual steps in the Python extraction that need documentation?


==============================================================================
7. REPLICATION FILE TODO
==============================================================================

  [ ] Convert Python scraping notebooks to documented scripts
  [ ] Build Yeti Records cleaning script (name harmonization + GVKEY match)
  [ ] Complete org_gvkey_crosswalk.csv (exemption names → GVKEYs)
  [ ] Build Yeti-to-GVKEY crosswalk
  [ ] Decide on Yeti aggregation frequency (weekly/monthly/quarterly)
  [ ] Add Compustat controls for pre-2019 years if available
  [ ] Comments/text analysis pipeline (separate from main R pipeline)
  [ ] ISIN robustness analysis
