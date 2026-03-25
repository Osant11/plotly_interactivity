library(dplyr)
library(tidyr)
library(purrr)

set.seed(42)

# ── Study setup ––––––––––––––––––––––––––––––––

STUDYID <- "STUDY001"
n_subj  <- 20

treatments <- c("Placebo", "Drug A 10mg", "Drug A 20mg")

visits <- tibble(
  VISIT       = c("SCREENING", "BASELINE", "WEEK 13", "WEEK 26"),
  VISITCD     = c("SCRN",      "BL",       "WK13",    "WK26"),
  VISITNUM    = c(1, 2, 3, 4),
  PLANNED_DAY = c(-14, 0, 91, 182)
)

REFDATE <- as.Date("2023-01-15")

# ── Subject-level table ––––––––––––––––––––––––––––

subjects <- tibble(
  USUBJID = sprintf("%s-%03d", STUDYID, seq_len(n_subj)),
  SUBJID = seq_len(n_subj),
  TRTA    = rep(treatments, length.out = n_subj),
  hr_bl   = rnorm(n_subj, mean = 72, sd = 10),
  bmi_bl  = rnorm(n_subj, mean = 27, sd =  4)
)

# ── Helper: simulate one AVAL given param, visit, treatment —————––

sim_aval <- function(bl_val, param, visit_cd, trta) {
  trend <- if (param == "HR") {
    switch(trta,
      "Placebo"     = c(SCRN = 0, BL = 0, WK13 = rnorm(1, -1,  3), WK26 = rnorm(1,  -1,  4)),
      "Drug A 10mg" = c(SCRN = 0, BL = 0, WK13 = rnorm(1, -5,  3), WK26 = rnorm(1,  -7,  4)),
      "Drug A 20mg" = c(SCRN = 0, BL = 0, WK13 = rnorm(1, -9,  3), WK26 = rnorm(1, -12,  4))
    )
  } else {  # BMI
    switch(trta,
      "Placebo"     = c(SCRN = 0, BL = 0, WK13 = rnorm(1,  0.1, 0.3), WK26 = rnorm(1,  0.2, 0.4)),
      "Drug A 10mg" = c(SCRN = 0, BL = 0, WK13 = rnorm(1, -0.3, 0.3), WK26 = rnorm(1, -0.6, 0.4)),
      "Drug A 20mg" = c(SCRN = 0, BL = 0, WK13 = rnorm(1, -0.6, 0.3), WK26 = rnorm(1, -1.1, 0.4))
    )
  }

  noise     <- rnorm(1, 0, if (param == "HR") 1.5 else 0.2)
  aval      <- round(bl_val + trend[[visit_cd]] + noise, 1)
  floor_val <- if (param == "HR") 40 else 15
  max(aval, floor_val)
}

# ── Build records –––––––––––––––––––––––––––––––

vs <- subjects |>
  cross_join(visits) |>
  # ~18% dropout for post-baseline visits only (Screening & Baseline always present)
  mutate(drop = VISITCD %in% c("WK13", "WK26") & runif(n()) < 0.18) |>
  filter(!drop) |>
  select(-drop) |>
  # date jitter
  mutate(
    jitter = if_else(VISITCD == "SCRN",
                     sample(-2:0, n(), replace = TRUE),
                     sample(-3:5, n(), replace = TRUE)),
    VSDY  = PLANNED_DAY + jitter,
    ADY   = VSDY,
    VSDTC = format(REFDATE + VSDY, "%Y-%m-%d")
  ) |>
  # expand to two parameters
  pivot_longer(
    cols      = c(hr_bl, bmi_bl),
    names_to  = "param_key",
    values_to = "bl_val"
  ) |>
  mutate(
    VSTESTCD = if_else(param_key == "hr_bl", "HR",          "BMI"),
    VSTEST   = if_else(param_key == "hr_bl", "HEART RATE",  "BODY MASS INDEX"),
    VSSTRESU = if_else(param_key == "hr_bl", "beats/min",   "kg/m2"),
    VSCAT    = "VITAL SIGNS"
  ) |>
  # simulate AVAL
  mutate(
    AVAL = pmap_dbl(list(bl_val, VSTESTCD, VISITCD, TRTA), sim_aval)
  ) |>
  # baseline flag
  mutate(VSBLFL = if_else(VISITCD == "BL", "Y", NA_character_)) |>
  # CHG / PCHG (post-baseline only, relative to true BL)
  mutate(
    CHG  = if_else(VISITCD %in% c("SCRN", "BL"), NA_real_, round(AVAL - bl_val, 1)),
    PCHG = if_else(VISITCD %in% c("SCRN", "BL"), NA_real_, round(100 * (AVAL - bl_val) / bl_val, 1)),
    TYPET = "MSD" 
  ) |>
  # SDTM result columns
  mutate(
    VSORRES  = as.character(AVAL),
    VSORRESU = VSSTRESU,
    VSSTRESC = as.character(AVAL),
    VSSTRESN = AVAL,
    STUDYID  = STUDYID,
    DOMAIN   = "VS"
  ) |>
  arrange(USUBJID, VSTESTCD, VISITNUM) |>
  mutate(VSSEQ = row_number()) |>
  select(
    STUDYID, DOMAIN, USUBJID, SUBJID, TRTA, VSSEQ,
    VSTESTCD, VSTEST, VSCAT,
    VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU,
    VSBLFL,
    VISITNUM, VISIT, VSDTC, VSDY, ADY,
    AVAL, CHG, PCHG, TYPET
  )

# ── Quick check —————————————————————

message(sprintf("VS dataset: %d records | %d subjects | %d treatments",
                nrow(vs), n_distinct(vs$USUBJID), n_distinct(vs$TRTA)))

print(vs |> count(TRTA, VISIT) |> tidyr::pivot_wider(names_from = VISIT, values_from = n))

saveRDS( vs, "data/vs.rds" ) 
