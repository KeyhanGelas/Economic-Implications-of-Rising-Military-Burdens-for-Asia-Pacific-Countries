# =============================================================================
# ESCAP - PIPELINE v6 | ACADEMIC BLACK & WHITE VERSION
# Identical graphs to v6, re-styled for publication / journal output:
#   • Full greyscale palette (no colour ink)
#   • Line types & fill patterns replace colour coding
#   • Clean serif-adjacent minimal theme (no grid noise)
#   • 300 dpi PNG + optional PDF output
# =============================================================================

pkgs <- c("tidyverse","lmtest","sandwich","zoo","plm",
          "officer","ggplot2","scales","readxl","lubridate")
new_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

OUTPUT_DIR <- "C:/Users/Keyha/OneDrive/Desktop/Stage/UN_policy_papers/Output 1/Output"
RAW_DIR    <- "C:/Users/Keyha/OneDrive/Desktop/Stage/UN_policy_papers/Output 1/Raw"
OUT_DOCX   <- file.path(OUTPUT_DIR, "escap_unified_v6_bw.docx")
TEMP_DIR   <- file.path(OUTPUT_DIR, "temp_v6_bw")
dir.create(TEMP_DIR, showWarnings = FALSE)

# =============================================================================
# MODEL PIPELINE  (identical to v6)
# =============================================================================
MODEL <- list(
  dep_var   = "gdp_growth",
  dep_label = "GDP growth",
  milex_var = "milex_lag1",
  controls  = c("trade_gdp","inflation","fdi_inflows","invest","pop_gr",
                "conflict","log_gdppc_lag","rents_adj"),
  ctrl_base = c("trade_gdp","inflation","fdi_inflows","invest","pop_gr",
                "log_gdppc_lag","rents_adj")
)

# =============================================================================
# FIGURE PARAMETERS  (identical to v6)
# =============================================================================
F1_SHOW_RED_SHADING  <- FALSE
F1_SHOW_EVENT_LINES  <- FALSE
F2_REARMAMENT_LABEL  <- TRUE
F3_BREAKS   <- c(1991, 2014)
F3_REF_YRS  <- c(1998, 2008)
F3_REF_LBLS <- c("Asian financial\ncrisis", "Global financial\ncrisis")
AT_N_TOP    <- 5
YEAR_EXCL   <- 2020
RENT_CUTOFF <- 25

FOREST_MILEX_LABEL <- "Military burden, 1-year lag (% of GDP)"

LABEL_F5 <- c(
  "β(milex|non-top10)"   = "Effect for\nnon-exporters",
  "β(milex:top10_exp)"   = "Does being an exporter make\nspending more effective?",
  "β(milex|top10 total)" = "Total milex effect\nfor top exporters"
)
F5_TITLE    <- "Does Being an Arms Exporter Change the Effect of Military Spending?"
F5_SUBTITLE <- "Top exporters = ESCAP top 10% by arms exports (% of GDP)\nClassification based, per year, on 3-year trailing average TIV to smooth delivery spikes"
F5_CAPTION  <- "Total effect = β1(milex) + β3(milex×exporter) | SE via delta method: √(Var₁ + Var₃ + 2·Cov₁₃)"

LABEL_F7 <- c(
  "β(milex|peaceful)"       = "Spending effect\nin peaceful years",
  "β(milex:conflict)"       = "Does conflict make\nspending more effective?",
  "β(milex|conflict total)" = "Total milex effect\nduring conflict"
)
F7_TITLE    <- "Does Active Conflict Amplify the Economic Cost of Military Spending?"
F7_SUBTITLE <- "Conflict = > 1,000 battle-related deaths per year (UCDP)"
F7_CAPTION  <- "Total effect = β1(milex) + β3(milex×conflict) | SE via delta method: √(Var₁ + Var₃ + 2·Cov₁₃)"

# =============================================================================
# ACADEMIC B&W DESIGN SYSTEM
# =============================================================================

# ── Greyscale palette ─────────────────────────────────────────────────────────
BW <- c(
  black   = "#000000",
  dk_grey = "#333333",
  md_grey = "#666666",
  lt_grey = "#999999",
  vlt_gry = "#CCCCCC",
  white   = "#FFFFFF"
)

# ── Line types used in place of colour ───────────────────────────────────────
LT_SOLID  <- "solid"
LT_DASH   <- "dashed"
LT_DOT    <- "dotted"
LT_DASHDOT<- "dotdash"

# ── Colour shortcuts for the two "groups" across all scatter/forest plots ─────
COL_GRP0  <- BW["black"]           # group 0 / non-exporter / peaceful
COL_GRP1  <- BW["md_grey"]         # group 1 / exporter / conflict
COL_GROSS <- BW["black"]
COL_NET   <- BW["md_grey"]
COL_EXP   <- BW["black"]
COL_IMP   <- BW["md_grey"]

# ── Academic theme ────────────────────────────────────────────────────────────
THEME_BW <- function(bs = 11) {
  theme_classic(base_size = bs, base_family = "sans") +
    theme(
      # Titles
      plot.title       = element_text(size = rel(1.05), face = "bold",
                                      color = BW["black"], margin = margin(b = 6)),
      plot.subtitle    = element_text(size = rel(0.82), color = BW["dk_grey"],
                                      margin = margin(b = 8)),
      plot.caption     = element_text(size = rel(0.68), color = BW["md_grey"],
                                      hjust = 0, margin = margin(t = 6)),
      # Axes
      axis.title       = element_text(size = rel(0.88), color = BW["dk_grey"]),
      axis.text        = element_text(size = rel(0.82), color = BW["dk_grey"]),
      axis.line        = element_line(color = BW["dk_grey"], linewidth = 0.4),
      axis.ticks       = element_line(color = BW["dk_grey"], linewidth = 0.35),
      # Panels / grid: only light horizontal guides
      panel.grid.major.y = element_line(color = BW["vlt_gry"], linewidth = 0.3,
                                        linetype = "dotted"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.background    = element_rect(fill = "white", color = NA),
      # Facet strips
      strip.text         = element_text(face = "bold", color = BW["black"],
                                        size = rel(0.88)),
      strip.background   = element_rect(fill = BW["vlt_gry"], color = NA),
      # Legend
      legend.position    = "top",
      legend.title       = element_blank(),
      legend.text        = element_text(size = rel(0.85), color = BW["dk_grey"]),
      legend.key         = element_rect(fill = "white", color = NA),
      legend.background  = element_blank(),
      plot.margin        = margin(12, 16, 10, 16)
    )
}

sig_stars <- function(p) case_when(
  is.na(p)  ~ "-",
  p < 0.01  ~ "***",
  p < 0.05  ~ "**",
  p < 0.10  ~ "*",
  TRUE      ~ "ns"
)

# ── Figure registry ───────────────────────────────────────────────────────────
FIGS  <- list()
fig_n <- 0L
save_fig <- function(p, tag, w = 22, h = 12) {
  fig_n <<- fig_n + 1L
  path <- file.path(TEMP_DIR, sprintf("%02d_%s.png", fig_n,
                                      substr(gsub("[^a-zA-Z0-9]", "_", tag), 1, 40)))
  ggsave(path, p, width = w, height = h, units = "cm", dpi = 300, bg = "white")
  FIGS[[length(FIGS) + 1]] <<- list(path = path, w = w, h = h, tag = tag)
  invisible(path)
}

# =============================================================================
# HARMONIZATION & COUNTRY LISTS  (unchanged)
# =============================================================================
CANONICAL <- c(
  "Korea, Rep."="Korea, South","Lao PDR"="Laos","Brunei Darussalam"="Brunei",
  "Timor-Leste"="Timor Leste","Iran, Islamic Rep."="Iran",
  "Russian Federation"="Russia","Korea, Dem. People's Rep."="Korea, North",
  "Turkiye"="Turkiye","Turkey"="Turkiye","Kyrgyzstan"="Kyrgyz Republic"
)
harmonize <- function(x) {
  x <- str_trim(x); idx <- match(x, names(CANONICAL))
  ifelse(!is.na(idx), CANONICAL[idx], x)
}

ESCAP_ALL <- c(
  "China","Japan","Korea, South","Korea, North","Mongolia",
  "Afghanistan","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Sri Lanka",
  "Brunei","Cambodia","Indonesia","Laos","Malaysia","Myanmar","Philippines",
  "Singapore","Thailand","Timor Leste","Viet Nam",
  "Kazakhstan","Kyrgyz Republic","Tajikistan","Turkmenistan","Uzbekistan",
  "Australia","Fiji","Kiribati","Marshall Islands","Micronesia","Nauru",
  "New Zealand","Palau","Papua New Guinea","Samoa","Solomon Islands",
  "Tonga","Tuvalu","Vanuatu",
  "Armenia","Azerbaijan","Georgia",
  "Iran","Turkiye","Russia"
)
ESCAP_CLEAN <- ESCAP_ALL
EXCLUDE_4   <- c("France","Netherlands","United Kingdom","United States")

SUBREGION_MAP <- tribble(
  ~country, ~subregion,
  "China","East Asia","Japan","East Asia","Korea, North","East Asia",
  "Korea, South","East Asia","Mongolia","East Asia",
  "Bangladesh","South Asia","Bhutan","South Asia","India","South Asia",
  "Maldives","South Asia","Nepal","South Asia","Pakistan","South Asia","Sri Lanka","South Asia",
  "Brunei","South-East Asia","Cambodia","South-East Asia","Indonesia","South-East Asia",
  "Laos","South-East Asia","Malaysia","South-East Asia","Philippines","South-East Asia",
  "Singapore","South-East Asia","Thailand","South-East Asia",
  "Timor Leste","South-East Asia","Viet Nam","South-East Asia",
  "Kazakhstan","Central Asia","Kyrgyz Republic","Central Asia",
  "Tajikistan","Central Asia","Turkmenistan","Central Asia","Uzbekistan","Central Asia",
  "Australia","Pacific","Fiji","Pacific","New Zealand","Pacific",
  "Papua New Guinea","Pacific","Samoa","Pacific","Solomon Islands","Pacific",
  "Tonga","Pacific","Kiribati","Pacific","Marshall Islands","Pacific",
  "Micronesia","Pacific","Nauru","Pacific","Palau","Pacific",
  "Tuvalu","Pacific","Vanuatu","Pacific",
  "Armenia","South Caucasus","Azerbaijan","South Caucasus","Georgia","South Caucasus",
  "Iran","Other ESCAP","Turkiye","Other ESCAP","Russia","Other ESCAP"
)

mk_region <- function(v) v[!v %in% ESCAP_ALL]
COUNTRIES_MENA <- mk_region(c("Algeria","Bahrain","Djibouti","Egypt, Arab Rep.","Egypt",
                              "Iraq","Israel","Jordan","Kuwait","Lebanon","Libya","Malta","Morocco","Oman","Qatar",
                              "Saudi Arabia","Syrian Arab Republic","Syria","Tunisia","United Arab Emirates",
                              "Yemen, Rep.","Yemen","West Bank and Gaza"))
COUNTRIES_SSA <- mk_region(c("Angola","Benin","Botswana","Burkina Faso","Burundi",
                             "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros","Congo, Dem. Rep.",
                             "Congo, Rep.","Cote d'Ivoire","Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
                             "Gabon","Gambia, The","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
                             "Madagascar","Malawi","Mali","Mauritania","Mauritius","Mozambique","Namibia","Niger",
                             "Nigeria","Rwanda","Sao Tome and Principe","Senegal","Sierra Leone","Somalia",
                             "South Africa","South Sudan","Sudan","Tanzania","Togo","Uganda","Zambia","Zimbabwe"))
COUNTRIES_LAC <- mk_region(c("Argentina","Belize","Bolivia","Brazil","Chile","Colombia",
                             "Costa Rica","Cuba","Dominican Republic","Ecuador","El Salvador","Guatemala","Guyana",
                             "Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay","Peru",
                             "Trinidad and Tobago","Uruguay","Venezuela, RB","Venezuela"))
COUNTRIES_EUR <- mk_region(c("Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina",
                             "Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland",
                             "Germany","Greece","Hungary","Iceland","Ireland","Italy","Kosovo","Latvia",
                             "Lithuania","Luxembourg","Moldova","Montenegro","Netherlands","North Macedonia",
                             "Norway","Poland","Portugal","Romania","Serbia","Slovak Republic","Slovenia",
                             "Spain","Sweden","Switzerland","Ukraine","United Kingdom"))
COUNTRIES_NAM <- c("United States","Canada")

EVENTS_DF <- tibble(
  year  = c(1991, 2001, 2014, 2022),
  label = c("USSR collapse", "9/11", "Crimea invasion", "Ukraine invasion")
)

# =============================================================================
# DATA LOADING
# =============================================================================
raw <- readRDS(file.path(OUTPUT_DIR, "wb_panel.rds")) %>%
  mutate(country = harmonize(country))

gpr_path      <- file.path(RAW_DIR, "data_gpr_export.xlsx")
gpr_available <- file.exists(gpr_path)
if (gpr_available) {
  cols_g   <- names(read_excel(gpr_path, n_max = 0))
  ctypes_g <- ifelse(cols_g == "month", "date", "numeric")
  gpr_raw  <- read_excel(gpr_path, col_types = ctypes_g)
  GPR_ISO_MAP <- c(
    CHN="China",IDN="Indonesia",IND="India",JPN="Japan",
    KOR="Korea, South",MYS="Malaysia",PHL="Philippines",
    THA="Thailand",TUR="Turkiye",VNM="Viet Nam",AUS="Australia"
  )
  escap_iso_clean <- names(GPR_ISO_MAP)
  gprhc_cols      <- paste0("GPRHC_", escap_iso_clean)
  gdp_weights <- raw %>%
    filter(country %in% GPR_ISO_MAP, !is.na(gdp_const), gdp_const > 0) %>%
    select(country, year, gdp_const)
  gpr_annual <- gpr_raw %>%
    mutate(year = year(month)) %>%
    group_by(year) %>%
    summarise(across(all_of(gprhc_cols), mean, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(all_of(gprhc_cols), names_to = "iso_col", values_to = "gpr_val") %>%
    mutate(country = GPR_ISO_MAP[sub("GPRHC_", "", iso_col)]) %>%
    left_join(gdp_weights, by = c("country", "year")) %>%
    group_by(year) %>%
    summarise(GPRHC_ESCAP = weighted.mean(gpr_val, gdp_const, na.rm = TRUE),
              .groups = "drop") %>%
    filter(year >= 1980, year <= 2024)
}

rents_2021 <- raw %>%
  filter(year == 2021, !is.na(rents_total_pct_gdp)) %>%
  select(country, rents_2021 = rents_total_pct_gdp)

# =============================================================================
# PANEL CONSTRUCTION
# =============================================================================
panel <- raw %>%
  filter(country %in% ESCAP_CLEAN, year >= 1993, year <= 2024, year != YEAR_EXCL) %>%
  left_join(rents_2021, by = "country") %>%
  left_join(SUBREGION_MAP, by = "country") %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    invest         = savings_gdp_pct,
    pop_gr         = pop_growth,
    conflict       = as.integer(!is.na(battle_deaths) & battle_deaths > 1000),
    milex          = milex_gdp_pct,
    milex_lag1     = dplyr::lag(milex_gdp_pct, 1),
    rents_adj      = case_when(
      !is.na(rents_total_pct_gdp)      ~ rents_total_pct_gdp,
      year > 2021 & !is.na(rents_2021) ~ rents_2021,
      TRUE                             ~ NA_real_),
    rents_ok       = is.na(rents_adj) | rents_adj <= 0,
    arms_tiv_gdp   = if_else(!is.na(gdp_const) & gdp_const > 0 & !is.na(arms_exports_tiv),
                             arms_exports_tiv / gdp_const * 1e6, NA_real_),
    roll3_exp      = rollapply(replace_na(arms_tiv_gdp, 0), 3, mean,
                               fill = NA, align = "right", na.rm = TRUE),
    gdp_growth_lag   = dplyr::lag(gdp_growth, 1),
    log_gdppc_lag    = log(pmax(dplyr::lag(gdp_per_cap, 1), 1, na.rm = TRUE)),
    delta_log_milex  = log(pmax(milex_gdp_pct, 0.001)) -
      log(pmax(dplyr::lag(milex_gdp_pct, 1), 0.001))
  ) %>%
  mutate(milex_x_conflict = milex_gdp_pct * conflict) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    pct_exp   = percent_rank(replace_na(roll3_exp, 0)),
    top10_exp = as.integer(pct_exp >= 0.90 & replace_na(roll3_exp, 0) > 0)
  ) %>%
  ungroup() %>%
  mutate(
    y         = .data[[MODEL$dep_var]],
    milex_use = .data[[MODEL$milex_var]],
    subregion = factor(subregion, levels = c("East Asia","Central Asia","Other ESCAP",
                                             "Pacific","South Asia","South-East Asia",
                                             "South Caucasus"))
  )

df_reg <- panel %>%
  filter(!is.na(rents_adj), !is.na(y),
         !is.na(milex_use), is.finite(milex_use),
         !is.na(trade_gdp), !is.na(inflation),
         !is.na(fdi_inflows), !is.na(invest),
         !is.na(pop_gr), !is.na(conflict))

cat(sprintf("Regression sample: %d obs | %d countries\n",
            nrow(df_reg), n_distinct(df_reg$country)))

# =============================================================================
# TWFE ENGINE  (unchanged logic)
# =============================================================================
run_twfe <- function(df, fml_str, section, key_vars) {
  if (n_distinct(df$country) < 3 || nrow(df) < 30) {
    message("[SKIP] ", section); return(NULL)
  }
  pdata <- pdata.frame(df %>% arrange(country, year), index = c("country","year"))
  m <- tryCatch(
    plm(as.formula(fml_str), data = pdata, model = "within", effect = "twoways"),
    error = function(e) { message("[PLM] ", e$message); NULL })
  if (is.null(m)) return(NULL)
  ctm <- tryCatch(
    as.matrix(coeftest(m, vcov = vcovHC(m, method = "arellano",
                                        type = "HC1", cluster = "group"))),
    error = function(e) { message("[SE] ", e$message); NULL })
  if (is.null(ctm)) return(NULL)
  r2w <- round(summary(m)$r.squared[[1]], 3)
  pc  <- grep("Pr", colnames(ctm), ignore.case = TRUE)[1]
  map_dfr(names(key_vars), function(nm) {
    varname <- key_vars[[nm]]; rn <- rownames(ctm); hit <- rn[rn == varname]
    if (length(hit) == 0 && grepl(":", varname, fixed = TRUE)) {
      parts <- strsplit(varname, ":", fixed = TRUE)[[1]]
      hit   <- rn[rn == paste(rev(parts), collapse = ":")]
    }
    if (length(hit) == 0) hit <- rn[grepl(varname, rn, fixed = TRUE)][1]
    if (length(hit) == 0 || is.na(hit))
      return(tibble(section=section, coef_name=nm, beta=NA_real_, se=NA_real_,
                    pval=NA_real_, sig="-", n=nrow(df),
                    n_ctry=n_distinct(df$country), r2_within=r2w))
    tibble(section=section, coef_name=nm,
           beta=round(as.numeric(ctm[hit,"Estimate"]),   4),
           se  =round(as.numeric(ctm[hit,"Std. Error"]), 4),
           pval=round(as.numeric(ctm[hit, pc]),          4),
           sig =sig_stars(round(as.numeric(ctm[hit, pc]), 4)),
           n=nrow(df), n_ctry=n_distinct(df$country), r2_within=r2w)
  })
}

partial_resid_fwl <- function(df, ctrl_vars = MODEL$ctrl_base) {
  needed <- c("country","year","y","milex_use", ctrl_vars)
  df <- df %>% select(all_of(needed)) %>%
    filter(if_all(everything(), ~!is.na(.)))
  if (nrow(df) < 50 || n_distinct(df$country) < 3) return(NULL)
  rhs <- paste("factor(country) + factor(year) +", paste(ctrl_vars, collapse=" + "))
  m_y <- lm(as.formula(paste("y ~",         rhs)), data = df)
  m_x <- lm(as.formula(paste("milex_use ~", rhs)), data = df)
  tibble(y_resid = resid(m_y), x_resid = resid(m_x),
         country = df$country, year = df$year)
}

lincom_se <- function(df, fml_str, var1, var2) {
  pdata <- pdata.frame(df %>% arrange(country, year), index = c("country","year"))
  m  <- plm(as.formula(fml_str), data = pdata, model = "within", effect = "twoways")
  vc <- vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
  rn <- rownames(vc)
  i1 <- which(rn == var1);  if (length(i1)==0) i1 <- which(grepl(var1, rn, fixed=TRUE))[1]
  i2 <- which(rn == var2)
  if (length(i2)==0) {
    parts <- strsplit(var2,":",fixed=TRUE)[[1]]
    i2 <- which(rn == paste(rev(parts),collapse=":"))[1]
  }
  if (is.na(i1) || is.na(i2)) return(NA_real_)
  sqrt(vc[i1,i1] + vc[i2,i2] + 2*vc[i1,i2])
}

# =============================================================================
# RUN MODELS
# =============================================================================
ctrl_str     <- paste(MODEL$controls, collapse = " + ")
ctrl_no_conf <- paste(setdiff(MODEL$controls, "conflict"), collapse = " + ")
res_list     <- list()

res_list[["M0"]] <- run_twfe(
  df_reg, paste("y ~ milex_use +", ctrl_str),
  "M0 - Baseline", c("β(milex)" = "milex_use"))

r1 <- run_twfe(
  df_reg, paste("y ~ milex_use * top10_exp +", ctrl_str),
  "M1 - Arms exporters",
  c("β(milex|non-top10)" = "milex_use", "β(top10_exp)" = "top10_exp",
    "β(milex:top10_exp)" = "milex_use:top10_exp"))
if (!is.null(r1)) {
  b1 <- r1$beta[r1$coef_name=="β(milex|non-top10)"][1]
  b3 <- r1$beta[r1$coef_name=="β(milex:top10_exp)"][1]
  fml1 <- paste("y ~ milex_use * top10_exp +", ctrl_str)
  se_sum1   <- lincom_se(df_reg, fml1, "milex_use", "milex_use:top10_exp")
  b_sum1    <- round(b1 + b3, 4)
  pval_sum1 <- 2 * pt(abs(b_sum1 / se_sum1),
                      df = r1$n[1] - r1$n_ctry[1] - 1, lower.tail = FALSE)
  r1 <- bind_rows(r1, tibble(
    section = "M1 - Arms exporters", coef_name = "β(milex|top10 total)",
    beta = b_sum1, se = round(se_sum1,4), pval = round(pval_sum1,4),
    sig = sig_stars(pval_sum1), n = r1$n[1], n_ctry = r1$n_ctry[1],
    r2_within = r1$r2_within[1]))
}
res_list[["M1"]] <- r1

r3 <- run_twfe(
  df_reg, paste("y ~ milex_use * conflict +", ctrl_no_conf),
  "M3 - Conflict",
  c("β(milex|peaceful)" = "milex_use", "β(conflict)" = "conflict",
    "β(milex:conflict)" = "milex_use:conflict"))
if (!is.null(r3)) {
  b1 <- r3$beta[r3$coef_name=="β(milex|peaceful)"][1]
  b3 <- r3$beta[r3$coef_name=="β(milex:conflict)"][1]
  fml3 <- paste("y ~ milex_use * conflict +", ctrl_no_conf)
  se_sum3   <- lincom_se(df_reg, fml3, "milex_use", "milex_use:conflict")
  b_sum3    <- round(b1 + b3, 4)
  pval_sum3 <- 2 * pt(abs(b_sum3 / se_sum3),
                      df = r3$n[1] - r3$n_ctry[1] - 1, lower.tail = FALSE)
  r3 <- bind_rows(r3, tibble(
    section = "M3 - Conflict", coef_name = "β(milex|conflict total)",
    beta = b_sum3, se = round(se_sum3,4), pval = round(pval_sum3,4),
    sig = sig_stars(pval_sum3), n = r3$n[1], n_ctry = r3$n_ctry[1],
    r2_within = r3$r2_within[1]))
}
res_list[["M3"]] <- r3

all_res <- bind_rows(res_list) %>%
  filter(!is.na(beta)) %>%
  mutate(sig_flag = !is.na(pval) & pval < 0.10,
         ci_lo = beta - 1.96*se, ci_hi = beta + 1.96*se)

pull1 <- function(sect, cname, col)
  all_res %>% filter(section==sect, coef_name==cname) %>%
  pull({{col}}) %>% `[`(1)

# =============================================================================
# FOREST & SCATTER HELPERS  (B&W re-skin)
# =============================================================================

# ── Forest plot ───────────────────────────────────────────────────────────────
make_forest <- function(df, title, subtitle = "", caption = "",
                        milex_label = FOREST_MILEX_LABEL) {
  r2v <- df %>% filter(!is.na(r2_within)) %>% pull(r2_within) %>% `[`(1)
  df  <- df %>% filter(!is.na(beta)) %>%
    mutate(
      # significant = solid black; ns = grey
      pt_col  = if_else(!is.na(pval) & pval < 0.10, BW["black"], BW["md_grey"]),
      # sum row gets open circle
      pt_shape = if_else(is.na(pval), 1L, 16L),
      x_lbl   = factor(coef_name, levels = unique(coef_name)),
      t_top   = case_when(
        is.na(pval) ~ sprintf("(sum)\nβ=%.3f", beta),
        TRUE        ~ sprintf("%s\nβ=%.3f", sig, beta)),
      t_bot   = if_else(is.na(pval), "", sprintf("p=%.3f", pval))
    )
  p <- ggplot(df, aes(x = x_lbl, y = beta)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = BW["md_grey"],
               linewidth = 0.6) +
    geom_errorbar(data = ~filter(., !is.na(ci_lo)),
                  aes(ymin = ci_lo, ymax = ci_hi),
                  width = 0.12, linewidth = 0.8, color = BW["dk_grey"]) +
    geom_point(aes(shape = I(pt_shape), color = I(pt_col)),
               size = 4.0, fill = "white", stroke = 1.2) +
    geom_text(aes(label = t_top, y = ci_hi), vjust = -0.4, hjust = 0.5,
              size = 2.7, fontface = "bold", color = BW["black"],
              lineheight = 0.85, na.rm = TRUE) +
    geom_text(data = ~filter(., t_bot != ""),
              aes(label = t_bot, y = ci_lo), vjust = 1.6, hjust = 0.5,
              size = 2.3, color = BW["md_grey"], na.rm = TRUE) +
    scale_y_continuous(labels = label_number(suffix = " pp", accuracy = 0.001),
                       expand = expansion(mult = c(0.22, 0.38))) +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    labs(title = title,
         subtitle = paste0(subtitle, "\n", milex_label,
                           " | TWFE country + year FE | Arellano SE"),
         caption = caption, x = NULL,
         y = paste0("Effect on ", MODEL$dep_label, " (percentage points)")) +
    THEME_BW(bs = 11) +
    theme(axis.text.x = element_text(size = 9, face = "bold", color = BW["black"],
                                     lineheight = 0.85, margin = margin(t = 4)),
          panel.grid.major.y = element_line(color = BW["vlt_gry"], linewidth = 0.3,
                                            linetype = "dotted"),
          panel.grid.major.x = element_blank())
  if (!is.na(r2v))
    p <- p + annotate("text", x = Inf, y = Inf,
                      label = sprintf("Within R² = %.3f", r2v),
                      hjust = 1.05, vjust = -0.5, size = 2.8,
                      fontface = "italic", color = BW["md_grey"])
  p
}

# ── FWL scatter ───────────────────────────────────────────────────────────────
make_scatter <- function(df_base, split_var, group_labels, group_linetypes,
                         ctrl_vars, beta_grp0, pval_grp0, beta_grp1, pval_grp1,
                         n_grp0, n_grp1, title, subtitle, caption) {
  
  res <- partial_resid_fwl(df_base, ctrl_vars = ctrl_vars)
  if (is.null(res) || nrow(res) < 10)
    return(ggplot() + labs(title = title) + THEME_BW())
  
  sv_df <- df_base %>% select(country, year, all_of(split_var)) %>%
    distinct(country, year, .keep_all = TRUE)
  
  plot_data <- res %>%
    left_join(sv_df, by = c("country","year")) %>%
    filter(!is.na(.data[[split_var]])) %>%
    mutate(sv_char = as.character(.data[[split_var]]),
           grp_lbl = { idx <- match(sv_char, names(group_labels))
           ifelse(!is.na(idx), group_labels[idx], sv_char) },
           grp = factor(grp_lbl, levels = names(group_linetypes)))
  
  if (nrow(plot_data) == 0)
    return(ggplot() + labs(title = title) + THEME_BW())
  
  centroids <- plot_data %>% group_by(grp) %>%
    summarise(mx = mean(x_resid, na.rm = TRUE),
              my = mean(y_resid, na.rm = TRUE), .groups = "drop")
  x_range <- plot_data %>% group_by(grp) %>%
    summarise(x_lo = quantile(x_resid, 0.02, na.rm = TRUE),
              x_hi = quantile(x_resid, 0.98, na.rm = TRUE), .groups = "drop")
  
  grp_names <- names(group_linetypes)
  
  lines_df <- tibble(
    grp      = factor(grp_names, levels = grp_names),
    slope    = c(beta_grp0, beta_grp1),
    se_slope = c(
      summary(lm(y_resid ~ x_resid,
                 data = plot_data %>% filter(grp == grp_names[1])))$coefficients["x_resid","Std. Error"],
      summary(lm(y_resid ~ x_resid,
                 data = plot_data %>% filter(grp == grp_names[2])))$coefficients["x_resid","Std. Error"]
    ),
    pval_v   = c(pval_grp0, pval_grp1),
    sig_v    = c(sig_stars(pval_grp0), sig_stars(pval_grp1)),
    n_v      = c(n_grp0, n_grp1),
    sig_flag = c(!is.na(pval_grp0) && pval_grp0 < 0.10,
                 !is.na(pval_grp1) && pval_grp1 < 0.10),
    ltype    = unname(group_linetypes)
  ) %>%
    left_join(centroids, by = "grp") %>%
    left_join(x_range,   by = "grp") %>%
    mutate(
      intercept = my - slope * mx,
      lbl = sprintf("%s  β=%.3f\np=%.3f  n=%d",
                    sig_v, slope, replace_na(pval_v, 1), n_v)
    )
  
  # point shapes: group 0 = open circle (1), group 1 = solid triangle (17)
  shape_map <- setNames(c(1L, 17L), grp_names)
  
  ggplot(plot_data, aes(x = x_resid, y = y_resid)) +
    geom_point(aes(shape = grp), alpha = 0.10, size = 1.1,
               color = BW["dk_grey"]) +
    # regression lines – distinguish by linetype, not colour
    geom_abline(data = lines_df,
                aes(slope = slope, intercept = intercept, linetype = grp),
                color = BW["black"], linewidth = 1.4, inherit.aes = FALSE) +
    geom_hline(yintercept = 0, color = BW["lt_grey"], linewidth = 0.35) +
    geom_text(data = lines_df[1,],
              aes(x = -Inf, y = Inf, label = lbl),
              hjust = -0.05, vjust = 1.5, size = 3.0,
              fontface = "bold", color = BW["black"], inherit.aes = FALSE) +
    geom_text(data = lines_df[2,],
              aes(x = Inf, y = -Inf, label = lbl),
              hjust = 1.05, vjust = -0.5, size = 3.0,
              fontface = "bold", color = BW["dk_grey"], inherit.aes = FALSE) +
    scale_shape_manual(values = shape_map) +
    scale_linetype_manual(values = unname(group_linetypes)) +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = "Military burden (lagged, % of GDP) — FWL residual",
         y = "GDP growth (%) — FWL residual") +
    THEME_BW(bs = 11) +
    theme(legend.position = "top", plot.margin = margin(14, 22, 12, 16))
}

# =============================================================================
# FIGURE 1 - Battle Deaths
# =============================================================================
assign_region <- function(df) {
  df %>% mutate(wr = case_when(
    country %in% ESCAP_ALL      ~ "ESCAP (all members)",
    country %in% COUNTRIES_MENA ~ "Middle East & N. Africa",
    country %in% COUNTRIES_SSA  ~ "Sub-Saharan Africa",
    country %in% COUNTRIES_LAC  ~ "Lat. Am. & Caribbean",
    country %in% COUNTRIES_EUR  ~ "Europe & C. Asia",
    !country %in% c(ESCAP_ALL,COUNTRIES_MENA,COUNTRIES_SSA,
                    COUNTRIES_LAC,COUNTRIES_EUR,COUNTRIES_NAM) ~ "Rest of World",
    TRUE ~ NA_character_)) %>%
    filter(!is.na(wr))
}

raw_wr     <- assign_region(raw)
pop_agg    <- raw_wr %>% filter(!is.na(population), population > 0) %>%
  group_by(wr, year) %>%
  summarise(total_pop = sum(population, na.rm = TRUE), .groups = "drop")
deaths_agg <- raw_wr %>% filter(!is.na(battle_deaths), battle_deaths >= 0) %>%
  group_by(wr, year) %>%
  summarise(total_deaths = sum(battle_deaths, na.rm = TRUE), .groups = "drop")
fig1_data  <- pop_agg %>%
  left_join(deaths_agg, by = c("wr","year")) %>%
  replace_na(list(total_deaths = 0)) %>%
  mutate(dpm = total_deaths / total_pop * 1e6) %>%
  filter(year >= 1980, year <= 2024) %>%
  group_by(wr) %>%
  mutate(dpm_roll = rollapply(dpm, 3, mean, fill = NA, align = "center", na.rm = TRUE)) %>%
  ungroup()

fig1_regions <- fig1_data %>% filter(wr != "ESCAP (all members)")
fig1_escap   <- fig1_data %>% filter(wr == "ESCAP (all members)")
last_escap_y <- fig1_escap %>% filter(!is.na(dpm_roll)) %>%
  filter(year == max(year)) %>% pull(dpm_roll)
last_escap_x <- fig1_escap %>% filter(!is.na(dpm_roll)) %>%
  filter(year == max(year)) %>% pull(year)

# B&W: distinguish regions by line type + grey shade
REGION_LT <- c(
  "Europe & C. Asia"       = "solid",
  "Middle East & N. Africa"= "dashed",
  "Rest of World"          = "dotted",
  "Sub-Saharan Africa"     = "dotdash",
  "Lat. Am. & Caribbean"   = "longdash"
)
REGION_GR <- c(
  "Europe & C. Asia"       = "#000000",
  "Middle East & N. Africa"= "#333333",
  "Rest of World"          = "#888888",
  "Sub-Saharan Africa"     = "#555555",
  "Lat. Am. & Caribbean"   = "#AAAAAA"
)

fig1 <- ggplot() +
  geom_line(data = fig1_regions,
            aes(x = year, y = dpm_roll, linetype = wr, color = wr),
            linewidth = 0.85, na.rm = TRUE) +
  geom_line(data = fig1_escap, aes(x = year, y = dpm_roll),
            color = "black", linetype = "solid", linewidth = 1.4, na.rm = TRUE) +
  annotate("text", x = last_escap_x + 0.5, y = last_escap_y,
           label = "ESCAP\n(all members)", hjust = 0, vjust = 0.5,
           size = 2.6, fontface = "bold", color = "black") +
  scale_linetype_manual(values = REGION_LT, name = NULL) +
  scale_color_manual(values = REGION_GR, name = NULL) +
  scale_x_continuous(breaks = seq(1990, 2024, 5),
                     expand = expansion(mult = c(0.01, 0.14))) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = " /M"),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  labs(title = "Figure 1. Battle Deaths Per Million by World Region, 1990–2024",
       subtitle = "3-year centred rolling average | UCDP/World Bank\nESCAP members = thick solid black | Other regions exclude ESCAP countries",
       x = NULL, y = "Battle deaths per million inhabitants",
       caption = "Source: UCDP/PRIO Battle-Related Deaths Dataset; World Bank WDI. Population denominator = all countries per region.") +
  THEME_BW(bs = 11) +
  theme(legend.position = "right", plot.margin = margin(12, 60, 10, 16))
save_fig(fig1, "F1_battle_deaths", w = 30, h = 14)

# ── Stacked battle deaths by conflict ─────────────────────────────────────────
bd_raw_path <- file.path(RAW_DIR, "BD_war_pivot.xlsx")
if (file.exists(bd_raw_path)) {
  bd_raw <- read_excel(bd_raw_path, sheet = "bd", col_names = TRUE)
  
  CONFLICT_GREY <- c(
    "Afghanistan Civil War"        = "#000000",
    "Azerbaijan - Karabakh War"    = "#111111",
    "India Insurgences"            = "#222222",
    "India-Pakistan War"           = "#333333",
    "Iran-Israel War"              = "#444444",
    "Myanmar Civil War"            = "#555555",
    "Nepal"                        = "#666666",
    "Pakistan Insurgences"         = "#777777",
    "Philippines Insurgences"      = "#888888",
    "Russia - Post USSR conflicts" = "#999999",
    "Russia - Ukraine War"         = "#AAAAAA",
    "Sri Lanka Civil War"          = "#BBBBBB",
    "Turkiye Insurgences"          = "#CCCCCC"
  )
  
  bd_long <- bd_raw %>%
    rename(year = Year) %>%
    pivot_longer(-year, names_to = "conflict", values_to = "bd") %>%
    filter(!is.na(bd), bd > 0) %>%
    mutate(conflict = factor(conflict, levels = names(CONFLICT_GREY)))
  
  fig_bd <- ggplot(bd_long, aes(x = year, y = bd, fill = conflict)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.15) +
    scale_fill_manual(values = CONFLICT_GREY, name = NULL, na.translate = FALSE) +
    scale_x_continuous(breaks = seq(1989, 2024, by = 3)) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ","),
                       expand = expansion(mult = c(0, 0.04))) +
    labs(title    = "Battle Deaths in ESCAP Member States by Conflict, 1989–2024",
         subtitle = "Stacked annual battle-related deaths — UCDP best estimate",
         x = NULL, y = "Battle deaths",
         caption  = "Source: UCDP Battle-Related Deaths Dataset v25.1.") +
    THEME_BW() +
    theme(legend.position = "bottom", legend.key.size = unit(0.40, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.80)))
  save_fig(fig_bd, "BD_stacked_conflicts", w = 26, h = 14)
  
  # ── Per-capita version ─────────────────────────────────────────────────────
  CONFLICT_COUNTRY <- tribble(
    ~conflict,                       ~country,
    "Afghanistan Civil War",         "Afghanistan",
    "Azerbaijan - Karabakh War",     "Azerbaijan",
    "India Insurgences",             "India",
    "India-Pakistan War",            "India",
    "Iran-Israel War",               "Iran",
    "Myanmar Civil War",             "Myanmar",
    "Nepal",                         "Nepal",
    "Pakistan Insurgences",          "Pakistan",
    "Philippines Insurgences",       "Philippines",
    "Russia - Post USSR conflicts",  "Russia",
    "Russia - Ukraine War",          "Russia",
    "Sri Lanka Civil War",           "Sri Lanka",
    "Turkiye Insurgences",           "Turkiye"
  )
  country_pop <- raw %>% filter(!is.na(population), population > 0) %>%
    select(country, year, population)
  bd_long_pc <- bd_raw %>%
    rename(year = Year) %>%
    pivot_longer(-year, names_to = "conflict", values_to = "bd") %>%
    filter(!is.na(bd), bd > 0) %>%
    left_join(CONFLICT_COUNTRY, by = "conflict") %>%
    left_join(country_pop, by = c("country","year")) %>%
    mutate(bd_pm  = bd / population * 1e6,
           conflict = factor(conflict, levels = names(CONFLICT_GREY)))
  fig_bd_pc <- ggplot(bd_long_pc, aes(x = year, y = bd_pm, fill = conflict)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.15) +
    scale_fill_manual(values = CONFLICT_GREY, name = NULL, na.translate = FALSE) +
    scale_x_continuous(breaks = seq(1989, 2024, by = 3)) +
    scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = " /M"),
                       expand = expansion(mult = c(0, 0.04))) +
    labs(title    = "Battle Deaths per Million Inhabitants by Conflict, ESCAP Member States, 1989–2024",
         subtitle = "Deaths normalised by population of the primary country affected — UCDP best estimate",
         x = NULL, y = "Battle deaths per million inhabitants",
         caption  = paste0(
           "Source: UCDP Battle-Related Deaths Dataset v25.1; World Bank WDI (population).\n",
           "Denominator = population of primary ESCAP country involved in each conflict.")) +
    THEME_BW() +
    theme(legend.position = "bottom", legend.key.size = unit(0.40, "cm"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.80)))
  save_fig(fig_bd_pc, "BD_stacked_conflicts_percapita", w = 26, h = 14)
}

# =============================================================================
# FIGURE 2 - Military Burden
# =============================================================================
fig2_data <- raw %>%
  filter(!country %in% EXCLUDE_4, !is.na(milex_gdp_pct),
         !is.na(gdp_const), gdp_const > 0) %>%
  group_by(year) %>%
  summarise(burden_wtd = weighted.mean(milex_gdp_pct, gdp_const, na.rm = TRUE),
            .groups = "drop")

# Shade the rearmament period with a light grey rectangle
fig2 <- ggplot(fig2_data, aes(x = year, y = burden_wtd)) +
  annotate("rect", xmin = 2014, xmax = max(fig2_data$year),
           ymin = -Inf, ymax = Inf, alpha = 0.10, fill = BW["md_grey"]) +
  {if (F2_REARMAMENT_LABEL)
    annotate("text", x = 2019.5, y = max(fig2_data$burden_wtd, na.rm=TRUE) * 0.50,
             label = "Rearmament\nwave", color = BW["dk_grey"], size = 3.0,
             fontface = "bold", lineheight = 0.85, hjust = 0.5)
    else geom_blank()} +
  geom_line(linewidth = 0.9, color = BW["black"]) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE,
              linetype = "dotted", color = BW["dk_grey"], linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = "%"),
                     limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(1960, 2024, 10)) +
  labs(title    = "Figure 2. Military Burden in Asia and the Pacific, 1980–2024",
       subtitle = "GDP-weighted average military expenditure as % of GDP\nESCAP member states (excl. France, Netherlands, UK, USA)",
       x = NULL, y = "Military burden (% of GDP)",
       caption  = "Source: SIPRI Military Expenditure Database.") +
  THEME_BW(bs = 11)
save_fig(fig2, "F2_military_burden", w = 22, h = 12)

# =============================================================================
# FIGURE 3 - Three-Period OLS
# =============================================================================
fig3_base <- raw %>%
  filter(!country %in% EXCLUDE_4, year >= 1980, year != YEAR_EXCL,
         !is.na(milex_gdp_pct), !is.na(gdp_const), gdp_const > 0,
         !is.na(gdp_growth)) %>%
  mutate(period = case_when(
    year <= F3_BREAKS[1] ~ paste0("P1: \u2264", F3_BREAKS[1]),
    year <= F3_BREAKS[2] ~ paste0("P2: ", F3_BREAKS[1]+1, "\u2013", F3_BREAKS[2]),
    TRUE                 ~ paste0("P3: ", F3_BREAKS[2]+1, "\u20132024")
  ))

fig3_wt <- fig3_base %>%
  group_by(year) %>%
  summarise(burden = weighted.mean(milex_gdp_pct, gdp_const, na.rm = TRUE),
            gdpgr  = weighted.mean(gdp_growth,    gdp_const, na.rm = TRUE),
            .groups = "drop")

ols_stats <- fig3_base %>% group_by(period) %>%
  group_modify(~{
    yr_seq  <- seq(min(.x$year), max(.x$year))
    mid_yr  <- median(.x$year)
    mb  <- lm(milex_gdp_pct~year, data=.x, weights=gdp_const); smb <- summary(mb)
    mg  <- lm(gdp_growth~year,    data=.x, weights=gdp_const); smg <- summary(mg)
    bind_rows(
      tibble(year=yr_seq, fit=predict(mb,tibble(year=yr_seq)), series="burden",
             coef=round(coef(mb)["year"],4),
             pval=smb$coefficients["year","Pr(>|t|)"],
             r2=round(smb$r.squared,3),
             sig=sig_stars(smb$coefficients["year","Pr(>|t|)"]),mid_yr=mid_yr),
      tibble(year=yr_seq, fit=predict(mg,tibble(year=yr_seq)), series="gdpgr",
             coef=round(coef(mg)["year"],4),
             pval=smg$coefficients["year","Pr(>|t|)"],
             r2=round(smg$r.squared,3),
             sig=sig_stars(smg$coefficients["year","Pr(>|t|)"]),mid_yr=mid_yr)
    )
  }) %>% ungroup()

sf3        <- max(fig3_wt$burden, na.rm=TRUE) / max(abs(fig3_wt$gdpgr), na.rm=TRUE) * 0.70
max_burden <- max(fig3_wt$burden, na.rm=TRUE)

# B&W: periods coded by dash pattern, not colour
PERIOD_LT <- setNames(
  c("solid", "dashed", "dotdash"),
  unique(fig3_base$period)[order(unique(fig3_base$period))]
)
PERIOD_COLS_BW <- setNames(rep(BW["black"], 3),
                           names(PERIOD_LT))

annot_bottom <- ols_stats %>%
  distinct(period, series, coef, pval, r2, sig, mid_yr) %>%
  arrange(period, desc(series)) %>%
  group_by(period) %>% mutate(row_p = row_number()) %>% ungroup() %>%
  mutate(
    lbl   = case_when(
      series == "burden" ~
        sprintf("Military burden: %+.4f pp/yr  R\u00B2=%.3f  %s", coef, r2, sig),
      TRUE ~
        sprintf("GDP growth:      %+.4f pp/yr  R\u00B2=%.3f  %s", coef, r2, sig)),
    y_pos = -max_burden * (0.08 + (row_p-1)*0.07)
  )

fig3 <- ggplot() +
  annotate("rect", xmin=F3_BREAKS[2], xmax=max(fig3_wt$year, na.rm=TRUE),
           ymin=-Inf, ymax=Inf, alpha=0.07, fill=BW["md_grey"]) +
  geom_col(data=fig3_wt, aes(x=year, y=gdpgr*sf3),
           fill=BW["lt_grey"], alpha=0.65, width=0.85) +
  geom_line(data=fig3_wt, aes(x=year, y=burden),
            color=BW["lt_grey"], linewidth=0.5, alpha=0.6) +
  geom_line(data=ols_stats %>% filter(series=="burden"),
            aes(x=year, y=fit, linetype=period),
            color=BW["black"], linewidth=1.5) +
  geom_line(data=ols_stats %>% filter(series=="gdpgr"),
            aes(x=year, y=fit*sf3, linetype=period),
            color=BW["dk_grey"], linewidth=1.0) +
  geom_vline(xintercept=F3_BREAKS, linetype="dotted",
             color=BW["dk_grey"], linewidth=0.8) +
  geom_text(data=tibble(x=F3_BREAKS, lbl=c("USSR\ncollapse","Rearmament\nonset")),
            aes(x=x+0.4, y=max_burden*1.01, label=lbl),
            hjust=0, vjust=1, size=2.3, color=BW["dk_grey"],
            lineheight=0.85, inherit.aes=FALSE) +
  geom_vline(data=tibble(year=F3_REF_YRS),
             aes(xintercept=year), linetype="dashed",
             color=BW["lt_grey"], linewidth=0.55, inherit.aes=FALSE) +
  geom_text(data=tibble(year=F3_REF_YRS, lbl=F3_REF_LBLS),
            aes(x=year+0.4, y=max_burden*0.95, label=lbl),
            hjust=0, vjust=1, size=2.2, color=BW["lt_grey"],
            lineheight=0.80, inherit.aes=FALSE) +
  geom_text(data=annot_bottom,
            aes(x=mid_yr, y=y_pos, label=lbl,
                linetype=period),
            hjust=0.5, vjust=1, size=2.4, fontface="bold",
            color=BW["dk_grey"], inherit.aes=FALSE, show.legend=FALSE) +
  scale_linetype_manual(values=PERIOD_LT, name="Period") +
  scale_x_continuous(breaks=seq(1980,2024,5)) +
  scale_y_continuous(
    name="Military burden (% of GDP)",
    labels=function(x) paste0(x, "%"),
    sec.axis=sec_axis(~./sf3,
                      name="GDP growth, GDP-weighted avg. (%)",
                      labels=function(x) paste0(x,"%"))) +
  coord_cartesian(clip="off") +
  labs(title    = "Figure 3. Military Burden and GDP Growth — Three-Period OLS Trends",
       subtitle = paste0(
         "GDP constant USD-weighted ESCAP average | ",
         "Thick line = Military burden OLS | Thin line = GDP growth OLS | ",
         "Grey bars = GDP growth\nLine types distinguish periods"),
       x = NULL,
       caption = "Sources: SIPRI Military Expenditure Database; World Bank WDI. GDP-weighted OLS by period.") +
  THEME_BW(bs=11) +
  theme(axis.title.y.left  = element_text(color=BW["black"], face="bold"),
        axis.text.y.left   = element_text(color=BW["black"]),
        axis.title.y.right = element_text(color=BW["dk_grey"]),
        legend.position    = "top",
        plot.margin        = margin(12, 16, 100, 16))
save_fig(fig3, "F3_three_period_OLS", w=30, h=17)

# =============================================================================
# FIGURE 4 - GPR
# =============================================================================
if (gpr_available) {
  fig4_data <- raw %>%
    filter(country %in% GPR_ISO_MAP, year>=1980, year<=2024,
           !is.na(gdp_const), gdp_const>0) %>%
    group_by(year) %>%
    summarise(gdp_growth_w = weighted.mean(gdp_growth, gdp_const, na.rm=TRUE),
              .groups = "drop") %>%
    left_join(gpr_annual, by="year") %>%
    arrange(year) %>%
    mutate(
      gdp_growth_w = rollapply(gdp_growth_w, 3, mean, fill=NA, align="center", na.rm=TRUE),
      GPRHC_ESCAP  = rollapply(GPRHC_ESCAP,  3, mean, fill=NA, align="center", na.rm=TRUE)
    )
  
  sf4 <- max(abs(fig4_data$gdp_growth_w), na.rm=TRUE) /
    max(fig4_data$GPRHC_ESCAP, na.rm=TRUE) * 0.80
  
  fig4 <- ggplot(fig4_data, aes(x=year)) +
    annotate("rect", xmin=2015, xmax=2024,
             ymin=-Inf, ymax=Inf, alpha=0.07, fill=BW["md_grey"]) +
    geom_area(aes(y=GPRHC_ESCAP*sf4), alpha=0.12,
              fill=BW["md_grey"], color=NA) +
    geom_line(aes(y=GPRHC_ESCAP*sf4), color=BW["md_grey"],
              linetype="dashed", linewidth=0.9) +
    geom_col(aes(y=gdp_growth_w), alpha=0.25,
             fill=BW["dk_grey"], width=0.75) +
    geom_line(aes(y=gdp_growth_w), color=BW["black"], linewidth=1.15) +
    geom_hline(yintercept=0, color=BW["dk_grey"], linewidth=0.4) +
    scale_x_continuous(breaks=seq(1990,2024,5)) +
    scale_y_continuous(
      name="GDP growth, GDP-weighted avg. (%)",
      labels=label_percent(scale=1, suffix="%"),
      sec.axis=sec_axis(~./sf4, name="GPRHC index, ESCAP avg.",
                        labels=label_number(accuracy=1))) +
    THEME_BW(bs=11) +
    labs(title    = "Figure E2. ESCAP Economic Growth and Geopolitical Risk Index, 1980–2024",
         subtitle = paste0(
           "Solid black = GDP growth, GDP-weighted avg. (left axis)\n",
           "Dashed grey = GPRHC index, GDP-weighted avg. — 11 ESCAP economies (right axis)\n",
           "3-year centred rolling average"),
         x=NULL,
         caption = "Sources: World Bank WDI; Caldara & Iacoviello (2022), American Economic Review.")
  save_fig(fig4, "F4_growth_GPR", w=26, h=13)
}

# =============================================================================
# FIGURES 5–8 - Forest + Scatter
# =============================================================================

# ── Figure 5 (forest: exporters) ─────────────────────────────────────────────
fig5 <- all_res %>%
  filter(section=="M1 - Arms exporters", coef_name %in% names(LABEL_F5)) %>%
  mutate(coef_name = { idx <- match(as.character(coef_name), names(LABEL_F5))
  factor(ifelse(!is.na(idx), LABEL_F5[idx], as.character(coef_name)),
         levels=unname(LABEL_F5)) }) %>%
  make_forest(F5_TITLE, F5_SUBTITLE, F5_CAPTION)
save_fig(fig5, "F5_exporter_forest", w=22, h=12)

# ── Figure 6 (scatter: exporters vs non-exporters) ────────────────────────────
b0_f6 <- pull1("M1 - Arms exporters","β(milex|non-top10)", beta)
bi_f6 <- pull1("M1 - Arms exporters","β(milex:top10_exp)", beta)
p0_f6 <- pull1("M1 - Arms exporters","β(milex|non-top10)", pval)
pi_f6 <- pull1("M1 - Arms exporters","β(milex:top10_exp)", pval)

fig6 <- make_scatter(
  df_reg, "top10_exp",
  c("0"="Non-exporter year", "1"="Top-10% exporter year"),
  c("Non-exporter year"="solid", "Top-10% exporter year"="dashed"),
  MODEL$ctrl_base,
  b0_f6, p0_f6, b0_f6+bi_f6, pi_f6,
  sum(df_reg$top10_exp==0, na.rm=TRUE),
  sum(df_reg$top10_exp==1, na.rm=TRUE),
  "Military Spending vs. Growth: Arms Exporters vs. Non-Exporters",
  "Country + year FE + controls partialled out (FWL theorem) | Exact TWFE slopes",
  F5_CAPTION)
save_fig(fig6, "F6_exporter_scatter", w=22, h=14)

# ── Figure 7 (forest: conflict) ───────────────────────────────────────────────
fig7 <- all_res %>%
  filter(section=="M3 - Conflict", coef_name %in% names(LABEL_F7)) %>%
  mutate(coef_name = { idx <- match(as.character(coef_name), names(LABEL_F7))
  factor(ifelse(!is.na(idx), LABEL_F7[idx], as.character(coef_name)),
         levels=unname(LABEL_F7)) }) %>%
  make_forest(F7_TITLE, F7_SUBTITLE, F7_CAPTION)
save_fig(fig7, "F7_conflict_forest", w=22, h=12)

# ── Figure 8 (scatter: conflict vs peaceful) ──────────────────────────────────
b0_f8 <- pull1("M3 - Conflict","β(milex|peaceful)", beta)
bi_f8 <- pull1("M3 - Conflict","β(milex:conflict)", beta)
p0_f8 <- pull1("M3 - Conflict","β(milex|peaceful)", pval)
pi_f8 <- pull1("M3 - Conflict","β(milex:conflict)", pval)

fig8 <- make_scatter(
  df_reg, "conflict",
  c("0"="Peaceful year", "1"="Conflict year"),
  c("Peaceful year"="solid", "Conflict year"="dashed"),
  MODEL$ctrl_base,
  b0_f8, p0_f8, b0_f8+bi_f8, pi_f8,
  sum(df_reg$conflict==0, na.rm=TRUE),
  sum(df_reg$conflict==1, na.rm=TRUE),
  "Military Spending vs. Growth: Conflict vs. Peaceful Years",
  "Country + year FE + controls partialled out (FWL theorem) | Exact TWFE slopes",
  F7_CAPTION)
save_fig(fig8, "F8_conflict_scatter", w=22, h=14)

# =============================================================================
# FIGURE 9a - GDP Growth: Exporters vs Importers
# =============================================================================
raw9 <- raw %>% filter(!country %in% c("France","Netherlands","United Kingdom","United States"))

global_pct <- raw9 %>%
  filter(year>=1980, year!=YEAR_EXCL,
         !is.na(arms_exports_tiv), !is.na(arms_imports_tiv),
         !is.na(gdp_const), gdp_const>0) %>%
  mutate(arms_tiv_gdp = arms_exports_tiv/gdp_const*1e6,
         arms_imp_gdp = arms_imports_tiv/gdp_const*1e6) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    roll3_exp = rollapply(replace_na(arms_tiv_gdp,0), 2, mean,
                          fill=NA, align="right", na.rm=TRUE),
    roll3_imp = rollapply(replace_na(arms_imp_gdp,0), 2, mean,
                          fill=NA, align="right", na.rm=TRUE)
  ) %>%
  ungroup() %>%
  filter(country %in% ESCAP_ALL) %>%
  group_by(year) %>%
  mutate(
    exp_thr    = quantile(replace_na(roll3_exp,0), 0.90, na.rm=TRUE),
    imp_thr    = quantile(replace_na(roll3_imp,0), 0.90, na.rm=TRUE),
    is_top_exp = roll3_exp>=exp_thr & replace_na(roll3_exp,0)>0,
    is_top_imp = roll3_imp>=imp_thr & replace_na(roll3_imp,0)>0 & !is_top_exp
  ) %>% ungroup()

df_escap9 <- global_pct %>%
  filter(!is.na(gdp_growth),
         is.na(rents_total_pct_gdp) | rents_total_pct_gdp<=25,
         year!=YEAR_EXCL)

df_exp_ann <- df_escap9 %>% filter(is_top_exp) %>%
  group_by(year) %>%
  summarise(mean_growth=mean(gdp_growth,na.rm=TRUE),.groups="drop") %>%
  mutate(group="Top 10% exporter")
df_imp_ann <- df_escap9 %>% filter(is_top_imp) %>%
  group_by(year) %>%
  summarise(mean_growth=mean(gdp_growth,na.rm=TRUE),.groups="drop") %>%
  mutate(group="Top 10% importer")
df_ann9    <- bind_rows(df_exp_ann, df_imp_ann)

df_roll9   <- df_ann9 %>% group_by(group) %>% arrange(year) %>%
  mutate(roll_growth=rollapply(mean_growth,3,mean,fill=NA,align="center",na.rm=TRUE)) %>%
  ungroup()
last_pts9  <- df_roll9 %>% filter(!is.na(roll_growth)) %>%
  group_by(group) %>% slice_max(year,n=1) %>% ungroup()

fig9a <- ggplot() +
  # faint bars for raw annual data (both groups in grey)
  geom_col(data=df_ann9 %>% filter(group=="Top 10% exporter"),
           aes(x=year, y=mean_growth), fill=BW["dk_grey"], alpha=0.12, width=0.7) +
  geom_col(data=df_ann9 %>% filter(group=="Top 10% importer"),
           aes(x=year, y=mean_growth), fill=BW["md_grey"], alpha=0.12, width=0.7) +
  geom_hline(yintercept=0, color=BW["dk_grey"], linewidth=0.4) +
  geom_vline(xintercept=1991, linetype="dashed", color=BW["md_grey"], linewidth=0.6) +
  annotate("text", x=1991.3, y=Inf, label="End of Cold War",
           hjust=0, vjust=1.5, size=2.8, color=BW["md_grey"]) +
  # rolling average lines — differ by line type
  geom_line(data=df_roll9 %>% filter(group=="Top 10% exporter", !is.na(roll_growth)),
            aes(x=year, y=roll_growth), color=BW["black"],
            linetype="solid", linewidth=1.3) +
  geom_line(data=df_roll9 %>% filter(group=="Top 10% importer", !is.na(roll_growth)),
            aes(x=year, y=roll_growth), color=BW["dk_grey"],
            linetype="dashed", linewidth=1.3) +
  geom_text(data=last_pts9,
            aes(x=year+0.4,
                y=roll_growth + if_else(group=="Top 10% importer", 0.8, 0),
                label=group,
                fontface=if_else(group=="Top 10% exporter","bold","plain")),
            hjust=0, size=3.0, color=BW["black"]) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  scale_x_continuous(breaks=seq(1980,2024,5),
                     expand=expansion(mult=c(0.01,0.18))) +
  coord_cartesian(clip="off") +
  labs(title="Year-by-Year GDP Growth — Top 10% Exporters vs. Importers, 1980–2024",
       subtitle=paste0(
         "Solid line = Top 10% arms exporters by TIV/GDP | Dashed line = Top 10% arms importers (ESCAP, annual)\n",
         "Excludes resource-rent economies (>",RENT_CUTOFF,"% GDP) and year 2020 | Lines = 3-year rolling average"),
       x="Year", y="GDP growth (%)",
       caption=paste0(
         "Sources: World Bank WDI; SIPRI Arms Transfers Database.\n",
         "Classification on 3-year trailing average TIV/GDP among ESCAP member states. ",
         "France, Netherlands, UK, USA excluded.")) +
  THEME_BW(bs=11) +
  theme(panel.grid.minor=element_blank(), plot.margin=margin(10,80,10,10))
save_fig(fig9a, "F9a_exporter_vs_importer_growth", w=28, h=13)

# =============================================================================
# FIGURES AT1 + AT2 - Arms Transfer Charts
# =============================================================================
PERIOD_LABELS_AT <- c("2012\u201314", "2022\u201324")
arms_raw_at <- raw %>%
  filter(country %in% ESCAP_ALL) %>%
  mutate(arms_exp = replace_na(arms_exports_tiv, 0),
         arms_imp = replace_na(arms_imports_tiv, 0),
         net_arms = arms_exp - arms_imp,
         gross_gdp = if_else(gdp_const>0, arms_exp/gdp_const*100, NA_real_),
         net_gdp   = if_else(gdp_const>0, net_arms/gdp_const*100, NA_real_),
         period = case_when(year %in% 2012:2014 ~ PERIOD_LABELS_AT[1],
                            year %in% 2022:2024 ~ PERIOD_LABELS_AT[2],
                            TRUE ~ NA_character_)) %>%
  filter(!is.na(period), year!=YEAR_EXCL) %>%
  group_by(country, period) %>%
  summarise(gross=mean(gross_gdp,na.rm=TRUE),
            net  =mean(net_gdp,  na.rm=TRUE), .groups="drop") %>%
  mutate(period=factor(period, levels=PERIOD_LABELS_AT))

rank_ref_at      <- arms_raw_at %>% filter(period==PERIOD_LABELS_AT[2]) %>%
  select(country, gross_2224=gross, net_2224=net)
top_exporters_at <- rank_ref_at %>% arrange(desc(gross_2224)) %>%
  slice_head(n=AT_N_TOP) %>% pull(country)
top_importers_at <- rank_ref_at %>% arrange(net_2224) %>%
  slice_head(n=AT_N_TOP) %>% pull(country)

build_at_data <- function(country_list, order_var, ascending=TRUE) {
  df  <- arms_raw_at %>% filter(country %in% country_list)
  ref <- df %>% filter(period==PERIOD_LABELS_AT[2]) %>%
    select(country, val=!!sym(order_var))
  lvls <- if(ascending) ref%>%arrange(val)%>%pull(country) else
    ref%>%arrange(desc(val))%>%pull(country)
  df %>%
    mutate(country=factor(country, levels=c(setdiff(unique(country),lvls),lvls))) %>%
    mutate(y_key=interaction(country,period,sep=" | ",lex.order=FALSE)) %>%
    mutate(y_key=factor(y_key, levels=unique(arrange(.,country,period)$y_key%>%as.character())))
}

make_at_chart <- function(df_in, title, subtitle) {
  long_df <- df_in %>%
    pivot_longer(c(gross,net), names_to="metric", values_to="value") %>%
    mutate(metric=factor(metric, levels=c("gross","net"),
                         labels=c("Gross arms exports","Net arms exports")))
  y_labs  <- setNames(gsub("^.* \\| ","",levels(long_df$y_key)), levels(long_df$y_key))
  ctry_lb <- long_df %>% distinct(country,y_key) %>%
    mutate(y_num=as.numeric(y_key)) %>% group_by(country) %>%
    summarise(y_mid=mean(y_num), .groups="drop")
  seps <- long_df %>% distinct(country,y_key) %>%
    mutate(y_num=as.numeric(y_key)) %>% group_by(country) %>%
    summarise(y_bottom=min(y_num), .groups="drop") %>%
    filter(y_bottom>1) %>% mutate(sep=y_bottom-0.5)
  lbl_base <- long_df %>%
    pivot_wider(id_cols=c(country,period,y_key), names_from=metric, values_from=value,
                names_repair="minimal") %>%
    rename(gross_val=`Gross arms exports`, net_val=`Net arms exports`) %>%
    filter(!is.na(gross_val)) %>%
    mutate(x_anchor=gross_val, hjust_lbl=if_else(gross_val>=0,-0.10,1.10))
  
  # B&W: gross=dark fill, net=light grey; periods by fill alpha (dark=recent)
  ggplot(long_df, aes(x=value, y=y_key, fill=interaction(metric,period))) +
    geom_hline(data=seps, aes(yintercept=sep),
               color=BW["lt_grey"], linewidth=0.4, inherit.aes=FALSE) +
    geom_vline(xintercept=0, color=BW["dk_grey"], linewidth=0.5) +
    geom_col(data=long_df%>%filter(metric=="Gross arms exports"),
             aes(fill=period), width=0.80, alpha=0.85,
             color="white", linewidth=0.1, show.legend=TRUE) +
    geom_col(data=long_df%>%filter(metric=="Net arms exports"),
             aes(fill=period), width=0.40, alpha=0.70,
             color="white", linewidth=0.1, show.legend=FALSE) +
    geom_text(data=lbl_base%>%filter(abs(gross_val)>0.0001),
              aes(x=x_anchor, y=y_key, label=sprintf("%.2f%%",gross_val),
                  hjust=hjust_lbl), nudge_y=0.26, size=2.1,
              color=BW["black"], fontface="bold", inherit.aes=FALSE) +
    geom_text(data=lbl_base%>%filter(!is.na(net_val),abs(net_val)>0.0001),
              aes(x=x_anchor, y=y_key, label=sprintf("%.2f%%",net_val),
                  hjust=hjust_lbl), nudge_y=-0.26, size=2.1,
              color=BW["dk_grey"], fontface="bold", inherit.aes=FALSE) +
    annotate("text", x=-Inf, y=ctry_lb$y_mid,
             label=as.character(ctry_lb$country),
             hjust=1.08, size=2.55, color=BW["black"], fontface="bold") +
    scale_fill_manual(
      values=c(
        "2022\u201324"=BW["black"],
        "2012\u201314"=BW["md_grey"]
      ), name="Period") +
    scale_y_discrete(labels=y_labs) +
    scale_x_continuous(labels=label_number(accuracy=0.01,suffix="%"),
                       expand=expansion(mult=c(0.02,0.22))) +
    coord_cartesian(clip="off") +
    labs(title=title, subtitle=subtitle,
         x="Arms transfers as % of GDP (constant 2015 USD)",
         caption="Sources: SIPRI Arms Transfers Database; World Bank WDI.\nDark fill = 2022–24; grey fill = 2012–14. Wide bars = gross exports; narrow bars = net exports.") +
    THEME_BW(bs=9) +
    theme(axis.title.x=element_text(size=rel(0.80)), axis.title.y=element_blank(),
          axis.text.y=element_text(size=rel(0.74)),
          legend.position="top",
          plot.margin=margin(12,16,10,110))
}

h_at     <- AT_N_TOP*2*0.5+4
df_exp_at <- build_at_data(top_exporters_at,"gross",ascending=TRUE)
fig_at1  <- make_at_chart(df_exp_at,
                          title=paste0("Figure AT1. Top ",AT_N_TOP," Arms Exporters — ESCAP (% of GDP)"),
                          subtitle=paste0("Ranked by gross arms exports 2022–24 | ESCAP members only\n",
                                          "Dark fill = 2022–24 | Grey fill = 2012–14 | Wide bars = gross | Narrow bars = net"))
save_fig(fig_at1,"F_AT1_exporters",w=22,h=h_at)

df_imp_at <- build_at_data(top_importers_at,"net",ascending=FALSE)
fig_at2  <- make_at_chart(df_imp_at,
                          title=paste0("Figure AT2. Top ",AT_N_TOP," Arms Importers — ESCAP (% of GDP)"),
                          subtitle=paste0("Ranked by most negative net exports 2022–24 | ESCAP members only\n",
                                          "Dark fill = 2022–24 | Grey fill = 2012–14 | Negative net = net importer"))
save_fig(fig_at2,"F_AT2_importers",w=22,h=h_at)

# =============================================================================
# PEACE DIVIDEND - Viet Nam & Cambodia
# =============================================================================
fill_series <- function(x) {
  x <- zoo::na.locf(x, na.rm=FALSE)
  x <- zoo::na.locf(x, fromLast=TRUE, na.rm=FALSE)
  x
}

prep_country <- function(country_name, year_min=1980, year_max=2024) {
  raw %>%
    filter(country==country_name, year>=year_min, year<=year_max) %>%
    arrange(year) %>%
    mutate(
      milex_gdp_locf   = fill_series(milex_gdp_pct),
      milex_abs_locf   = fill_series(milex_const_usd),
      gdppc_locf       = fill_series(gdp_per_cap),
      milex_gdp_filled = rollapply(milex_gdp_locf,3,mean,fill=NA,align="center",na.rm=TRUE),
      gdp_roll3        = rollapply(gdp_growth,    3,mean,fill=NA,align="center",na.rm=TRUE)
    )
}

make_pd_growth_burden <- function(country_name) {
  df    <- prep_country(country_name)
  b_max <- max(df$milex_gdp_filled, na.rm=TRUE)
  b_min <- min(df$milex_gdp_filled, na.rm=TRUE)
  g_max <- max(abs(df$gdp_roll3),   na.rm=TRUE)
  sf    <- (b_max - b_min) / (g_max*2) * 0.75
  offs  <- (b_max + b_min) / 2
  
  ggplot(df, aes(x=year)) +
    geom_col(aes(y=gdp_roll3*sf + offs),
             fill=BW["lt_grey"], alpha=0.60, width=0.8, na.rm=TRUE) +
    geom_line(aes(y=gdp_roll3*sf + offs),
              color=BW["dk_grey"], linetype="dashed", linewidth=0.9, na.rm=TRUE) +
    geom_line(aes(y=milex_gdp_filled),
              color=BW["black"], linewidth=1.2, na.rm=TRUE) +
    geom_point(aes(y=milex_gdp_filled),
               color=BW["black"], size=1.5, alpha=0.55, na.rm=TRUE) +
    scale_x_continuous(breaks=seq(1980,2024,5)) +
    scale_y_continuous(
      name="Military burden (% of GDP, 3-yr rolling avg.)",
      labels=function(x) paste0(x,"%"),
      sec.axis=sec_axis(trans=~(.-offs)/sf,
                        name="GDP growth, 3-year rolling avg. (%)",
                        labels=function(x) paste0(x,"%"))) +
    labs(title    = paste0("Peace Dividend — ", country_name,
                           ": Military Burden & GDP Growth, 1980–2024"),
         subtitle = paste0(
           "Solid black = Military burden, % of GDP (left axis)\n",
           "Grey bars + dashed = GDP growth (right axis) | ",
           "Both: 3-year centred rolling average | Gaps filled by LOCF"),
         x=NULL,
         caption="Sources: SIPRI Military Expenditure Database; World Bank WDI.") +
    THEME_BW(bs=11) +
    theme(axis.title.y.left  = element_text(color=BW["black"], face="bold"),
          axis.text.y.left   = element_text(color=BW["black"]),
          axis.title.y.right = element_text(color=BW["dk_grey"]),
          axis.text.y.right  = element_text(color=BW["dk_grey"]),
          plot.margin        = margin(12,16,28,16))
}

for (ctry in c("Viet Nam","Cambodia")) {
  tag <- gsub(" ","_",ctry)
  save_fig(make_pd_growth_burden(ctry), paste0("PD2_",tag,"_growth_burden"), w=26, h=13)
}

# =============================================================================
# WORD DOCUMENT
# =============================================================================
doc <- read_docx() %>%
  body_add_par("ESCAP — Unified Visualization Pipeline v6 (Academic B&W)", style="heading 1") %>%
  body_add_par(paste0("Model: ", MODEL$dep_var, " | ", MODEL$milex_var,
                      " | controls: ", paste(MODEL$controls, collapse=", ")),
               style="Normal") %>%
  body_add_par(paste("Generated:", format(Sys.Date(),"%d %B %Y")), style="Normal") %>%
  body_add_par("", style="Normal")

for (fig_info in FIGS) {
  doc <- doc %>%
    body_add_par(fig_info$tag, style="heading 2") %>%
    body_add_img(src=fig_info$path,
                 width=min(fig_info$w,16.5)/2.54,
                 height=fig_info$h/2.54) %>%
    body_add_par("", style="Normal")
}
print(doc, target=OUT_DOCX)
unlink(TEMP_DIR, recursive=TRUE)
cat("\n\u2713 Saved:", OUT_DOCX, "\n")

# =============================================================================
# CONSOLE REGRESSION SUMMARY
# =============================================================================
run_twfe_verbose <- function(df, fml_str, label) {
  cat("\n", strrep("=", 70), "\n")
  cat(" ", label, "\n Formula:", fml_str, "\n")
  cat(" N obs:", nrow(df), "| N countries:", n_distinct(df$country), "\n")
  cat(strrep("=", 70), "\n")
  pdata <- pdata.frame(df %>% arrange(country,year), index=c("country","year"))
  m     <- plm(as.formula(fml_str), data=pdata, model="within", effect="twoways")
  ctm   <- coeftest(m, vcov=vcovHC(m, method="arellano", type="HC1", cluster="group"))
  cat("\nWithin R²:", round(summary(m)$r.squared[[1]], 4), "\n\n")
  print(ctm)
  invisible(list(model=m, coeftest=ctm))
}

run_twfe_verbose(df_reg, paste("y ~ milex_use +", ctrl_str),       "M0 - Baseline")
run_twfe_verbose(df_reg, paste("y ~ milex_use * top10_exp +", ctrl_str), "M1 - Arms exporters")
run_twfe_verbose(df_reg, paste("y ~ milex_use * conflict +", ctrl_no_conf), "M3 - Conflict")

cat("\n", strrep("=",70), "\n  COMPACT RESULTS TABLE\n", strrep("=",70), "\n\n")
print(all_res %>%
        select(section, coef_name, beta, se, pval, sig, n, n_ctry, r2_within) %>%
        mutate(across(c(beta,se,pval), ~round(.,4))), n=50)