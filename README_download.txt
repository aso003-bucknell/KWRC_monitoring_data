================================================================================
KEYSTONE WATER RESOURCES CENTER (KWRC)
Water Temperature Monitoring Data
================================================================================

Generated from the KWRC Water Temperature Explorer.
This download contains a subset of data you selected (site(s), date range, and
QC options as chosen in the app).

--------------------------------------------------------------------------------
FILES IN THIS DOWNLOAD
--------------------------------------------------------------------------------
  temperature_data.csv   Daily temperature records for the sites/dates you chose.
  site_locations.csv     Location and type for each monitoring site.
  README.txt             This file.

--------------------------------------------------------------------------------
HOW THE DATA WERE COLLECTED
--------------------------------------------------------------------------------
Water temperature is recorded by Onset HOBO data loggers deployed at each site.
Loggers record at sub-daily intervals (5-30 min) and are downloaded in
the field periodically by KWRC staff and trained volunteers.

Raw sub-daily logger readings are processed into the DAILY summaries provided
here: for each site and date we report the daily MEAN and daily MAXIMUM water
temperature in degrees Celsius (C).

Data span:
  - Surface stream sites: 1999-2025
  - Spring sites (AXS, BES, BIS, BLS, COS, LIS, WAS, WIS): 2018-2020

Some sites and years are compiled from more than one source record; the
"mean_source" and "max_source" columns identify which underlying record each
daily value came from.

--------------------------------------------------------------------------------
QUALITY CONTROL (QC)
--------------------------------------------------------------------------------
Each daily value carries automated QC flags. A flag of TRUE means the value met
a condition that warrants review; it does NOT automatically mean the value is
wrong. If you downloaded with the QC filter ON, flagged rows have already been
removed. If OFF, all rows are included and you can filter on the flag columns
yourself.

  qc_mean_hard_range      Daily mean fell outside the physically plausible range.
  qc_max_hard_range       Daily max fell outside the physically plausible range.
  qc_mean_review_high     Daily mean unusually high; flagged for review.
  qc_max_review_high      Daily max unusually high; flagged for review.
  qc_max_below_mean       Daily max was below the daily mean (data integrity).
  qc_exact_zero           Value was exactly 0 (possible sensor error).
  qc_source_disagreement  Multiple source records disagreed for this day.
  qc_any_flag             TRUE if ANY flag above is TRUE (quick screen).

--------------------------------------------------------------------------------
COLUMN DEFINITIONS (temperature_data.csv)
--------------------------------------------------------------------------------
  site_id                 Site abbreviation (join to site_locations.csv).
  date                    Calendar date (YYYY-MM-DD).
  year, month             Year and month, for convenience.
  daily_mean_c            Daily mean water temperature, C (QC-adjusted).
  daily_max_c             Daily maximum water temperature, C (QC-adjusted).
  daily_mean_raw_c        Daily mean before QC adjustment, C.
  daily_max_raw_c         Daily maximum before QC adjustment, C.
  mean_source             Source record for the daily mean.
  max_source              Source record for the daily maximum.
  n_candidate_sources     Number of source records available for this day.
  mean_source_range_c     Spread across sources for the mean, C.
  max_source_range_c      Spread across sources for the maximum, C.
  qc_*                    QC flags (see QUALITY CONTROL above).

COLUMN DEFINITIONS (site_locations.csv)
  site_id                 Site abbreviation.
  (plus site name, latitude, longitude, site type, and watershed as available)

--------------------------------------------------------------------------------
NEED THE RAW (SUB-DAILY) LOGGER DATA?
--------------------------------------------------------------------------------
This download provides DAILY summaries. Raw hourly logger files are available
on request, but require staff time to compile and deliver, so please allow
additional time.

  Contact:  Elyse Johnson
            elyse@keystonewaterresources.org
            Keystone Water Resources Center

--------------------------------------------------------------------------------
HOW TO CITE
--------------------------------------------------------------------------------
Keystone Water Resources Center (KWRC). Water Temperature Monitoring Data.
Retrieved [DOWNLOAD DATE] from the KWRC Water Temperature Explorer.

All KWRC monitoring data are publicly available.

================================================================================
