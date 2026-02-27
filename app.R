# app.R
# Shiny app for the revised methodological quality scale (RMQS) — MVP
# Option B: Templates + user-uploaded configuration overrides (per session)
# Improvements included:
# - App skeleton (tabs + templates + About/Cite) available from launch
# - Robust initial state (no blank status before clicking "run")
# - loaded() is reactiveVal() + observeEvent() (cleaner than eventReactive for UI)
# - cfg_downloads_ui implemented (was referenced but missing)
# - Manual validity used to enable/disable manual UI
# - Small landing panel shown when no data are loaded

library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(munsell)
library(ggplot2)

# ---------------------------
# Small utilities
# ---------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.atomic(x) && length(x) == 1 && is.na(x))) y else x

# ---------------------------
# Base scale contract (still fixed here; option C would remove this constraint)
# ---------------------------
ITEMS <- paste0("item", 1:17)
ITEM_LEVELS <- ITEMS
ALLOWED_4 <- c("YES", "NO", "NR", "NA")

DEFAULT_ITEMS_DF <- data.frame(
  item = ITEMS,
  short_label = paste("Short label for", ITEMS),
  domain = rep("Domain 1", length(ITEMS)),
  stringsAsFactors = FALSE
)


THEMES <- list(
  "Cochrane" = list(
    values = c(
      "YES" = "#2ca02c",
      "NO"  = "#d62728",
      "NR"  = "#7f7f7f",
      "NA"  = "#d9d9d9"
    ),
    na_fill = "#ffffff"
  ),
  "Grayscale" = list(
    values = c(
      "YES" = "#4d4d4d",
      "NO"  = "#000000",
      "NR"  = "#9e9e9e",
      "NA"  = "#e0e0e0"
    ),
    na_fill = "#ffffff"
  )
)

# ---------------------------
# UI text dictionary (bilingual)
# ---------------------------
TXT <- list(
  es = list(
    title = "Revised Methodological Quality Scale (RMQS) v2.0 — Shiny app",
    upload = "Sube un fichero (.xlsx o .csv)",
    sheet = "Hoja de Excel",
    view_mode = "Vista",
    by_items = "Por ítems",
    by_domains = "Por dominios",
    language = "Idioma",
    theme = "Tema",
    apa_labels = "Usar etiquetas APA (citation) en figuras",
    run = "Cargar y generar salidas",
    notes = "Notas / validación",
    loaded_ok = "✅ Cargado correctamente.",
    issues = "❌ Se han detectado problemas (ver detalles).",
    show_details = "Ver detalles",
    not_loaded_yet = "Aún no se ha cargado ningún fichero.",
    ready_title = "Listo cuando quieras",
    ready_steps = c(
      "1) Sube un .xlsx o .csv con columnas: study_id, citation, item1..item17.",
      "2) Selecciona la hoja (si es .xlsx).",
      "3) Pulsa “Cargar y generar salidas”."
    ),
    ready_hint = "Las salidas aparecerán cuando el fichero pase la validación.",
    tab_cfg = "Configuración / Plantillas",
    tab_preview = "Vista previa",
    tab_freq = "Frecuencias",
    tab_summary = "Figura resumen",
    tab_table = "Tabla",
    tab_key = "Clave de ítems",
    tab_manual = "Manual de la escala",
    tab_export = "Exportar",
    tab_about = "Acerca de / Citar",
    showing_items = "Mostrando: frecuencias por ítem (tabla wide: n y %).",
    showing_domains = "Mostrando: resumen por dominio (media de % por ítem, normalizado dentro del dominio).",
    download_items_csv = "Descargar frecuencias por ítem (CSV)",
    download_domains_csv = "Descargar tabla por dominios (CSV)",
    download_items_paper = "Descargar frecuencias por ítem (paper-ready CSV)",
    download_domains_paper = "Descargar tabla por dominios (paper-ready CSV)",
    download_summary_png = "Descargar resumen (PNG, 600 dpi)",
    download_summary_pdf = "Descargar resumen (PDF)",
    download_table_png = "Descargar tabla (PNG, 600 dpi)",
    download_table_pdf = "Descargar tabla (PDF)",
    download_clean = "Descargar datos normalizados (CSV)",
    download_audit = "Descargar audit log (TXT)",
    item_help = "Ayuda del ítem",
    manual_filter = "Filtrar por dominio",
    manual_select = "Selecciona ítem",
    decision_rules = "Reglas de decisión",
    using = "Mostrando:",
    cfg_intro = "Puedes subir ficheros de configuración para personalizar la escala (solo para esta sesión).",
    cfg_items = "Subir items.csv (opcional)",
    cfg_manual = "Subir items_extended_bilingual.xlsx/.csv (opcional)",
    cfg_status = "Estado de la configuración",
    cfg_download_template = "Descargar plantilla de codificación (CSV)",
    cfg_download_items_template = "Descargar plantilla items.csv (CSV)",
    cfg_download_manual_template = "Descargar plantilla manual bilingüe (CSV)",
    cfg_effective_items = "Descargar items.csv efectivo (CSV)",
    cfg_effective_manual = "Descargar manual efectivo (CSV)",
    manual_disabled = "Manual deshabilitado: falta o es inválido. Sube un manual válido en Configuración / Plantillas."
  ),
  en = list(
    title = "Revised Methodological Quality Scale (RMQS) v2.0 — Shiny app",
    upload = "Upload file (.xlsx or .csv)",
    sheet = "Excel sheet",
    view_mode = "View mode",
    by_items = "By items",
    by_domains = "By domains",
    language = "Language",
    theme = "Theme",
    apa_labels = "Use APA labels (citation) on plots",
    run = "Load & generate outputs",
    notes = "Notes / validation",
    loaded_ok = "✅ Loaded OK.",
    issues = "❌ Issues detected (see details).",
    show_details = "Show details",
    not_loaded_yet = "No file loaded yet.",
    ready_title = "Ready when you are",
    ready_steps = c(
      "1) Upload a .xlsx or .csv with columns: study_id, citation, item1..item17.",
      "2) Select sheet (if .xlsx).",
      "3) Click “Load & generate outputs”."
    ),
    ready_hint = "Outputs will appear once the file is validated and loaded.",
    tab_cfg = "Configuration / Templates",
    tab_preview = "Preview",
    tab_freq = "Frequencies",
    tab_summary = "Summary figure",
    tab_table = "Table",
    tab_key = "Item key",
    tab_manual = "Scale manual",
    tab_export = "Export",
    tab_about = "About / Cite",
    showing_items = "Showing: item frequencies (wide table: n and %).",
    showing_domains = "Showing: domain summary (mean item-level %, normalized within domain).",
    download_items_csv = "Download item frequencies (CSV)",
    download_domains_csv = "Download domain table (CSV)",
    download_items_paper = "Download item frequencies (paper-ready CSV)",
    download_domains_paper = "Download domain table (paper-ready CSV)",
    download_summary_png = "Download summary (PNG, 600 dpi)",
    download_summary_pdf = "Download summary (PDF)",
    download_table_png = "Download table (PNG, 600 dpi)",
    download_table_pdf = "Download table (PDF)",
    download_clean = "Download normalized data (CSV)",
    download_audit = "Download audit log (TXT)",
    item_help = "Item help",
    manual_filter = "Filter by domain",
    manual_select = "Select item",
    decision_rules = "Decision rules",
    using = "Showing:",
    cfg_intro = "You can upload configuration files to customize the scale (session-only).",
    cfg_items = "Upload items.csv (optional)",
    cfg_manual = "Upload items_extended_bilingual.xlsx/.csv (optional)",
    cfg_status = "Configuration status",
    cfg_download_template = "Download coding template (CSV)",
    cfg_download_items_template = "Download items.csv template (CSV)",
    cfg_download_manual_template = "Download bilingual manual template (CSV)",
    cfg_effective_items = "Download effective items.csv (CSV)",
    cfg_effective_manual = "Download effective manual (CSV)",
    manual_disabled = "Manual disabled: missing or invalid configuration. Upload a valid manual file in Configuration / Templates."
  )
)

t_txt <- function(lang, key) {
  lang <- if (identical(lang, "en")) "en" else "es"
  TXT[[lang]][[key]]
}

# ---------------------------
# Helpers (data)
# ---------------------------
normalize_vals <- function(x) {
  x <- str_trim(toupper(as.character(x)))
  x <- str_replace_all(x, c("SÍ" = "YES", "SI" = "YES"))
  x <- na_if(x, "")
  x
}

read_any <- function(path, sheet = NULL) {
  ext <- tools::file_ext(path)
  if (ext == "xlsx") {
    if (is.null(sheet)) sheet <- readxl::excel_sheets(path)[1]
    readxl::read_xlsx(path, sheet = sheet)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Unsupported file type. Use .xlsx or .csv")
  }
}

validate_structure <- function(df, items) {
  errs <- c()
  if (!"study_id" %in% names(df)) errs <- c(errs, "Missing required column: study_id")
  if (!"citation" %in% names(df)) errs <- c(errs, "Missing required column: citation (APA label)")
  missing_items <- setdiff(items, names(df))
  if (length(missing_items) > 0) errs <- c(errs, paste0("Missing item columns: ", paste(missing_items, collapse = ", ")))
  list(ok = length(errs) == 0, errors = errs)
}

validate_values <- function(df, items) {
  long <- df %>%
    select(all_of(items)) %>%
    pivot_longer(everything(), names_to = "item", values_to = "val") %>%
    mutate(val = normalize_vals(val))
  
  bad <- long %>%
    filter(!is.na(val) & !val %in% ALLOWED_4) %>%
    distinct(item, val)
  
  list(ok = nrow(bad) == 0, bad = bad)
}

apply_hardened_rules <- function(df, items) {
  warnings <- c()
  df <- df %>% mutate(across(all_of(items), normalize_vals))
  
  # Example hardened rule (your current one)
  if ("item13" %in% items && any(df$item13 == "NR", na.rm = TRUE)) {
    n_fix <- sum(df$item13 == "NR", na.rm = TRUE)
    df <- df %>% mutate(item13 = ifelse(item13 == "NR", "NO", item13))
    warnings <- c(warnings, paste0("HARDENED RULE applied: item13 NR->NO (", n_fix, " cell(s))."))
  }
  
  if ("item12" %in% items && any(df$item12 == "NA", na.rm = TRUE)) warnings <- c(warnings, "Warning: item12 contains NA (check if intended).")
  if ("item13" %in% items && any(df$item13 == "NA", na.rm = TRUE)) warnings <- c(warnings, "Warning: item13 contains NA (check if intended).")
  
  list(df = df, warnings = warnings)
}

freq_table <- function(df, items) {
  df %>%
    pivot_longer(all_of(items), names_to = "item", values_to = "value") %>%
    mutate(
      item  = factor(item, levels = items),
      value = factor(value, levels = c("YES", "NO", "NR", "NA"))
    ) %>%
    count(item, value, name = "n") %>%
    group_by(item) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    arrange(item, value)
}

freq_table_wide <- function(freq_long, item_labels) {
  out <- freq_long %>%
    mutate(item_label = item_labels[as.character(item)]) %>%
    mutate(cell = sprintf("%d (%.1f%%)", n, pct)) %>%
    select(item_label, value, cell) %>%
    tidyr::pivot_wider(
      names_from  = value,
      values_from = cell,
      values_fill = "0 (0.0%)"
    )
  
  for (v in c("YES", "NO", "NR", "NA")) {
    if (!v %in% names(out)) out[[v]] <- "0 (0.0%)"
  }
  
  out %>% select(Item = item_label, YES, NO, NR, `NA`)
}

domain_table <- function(freq_long, item_domains, domain_levels) {
  df_dom <- freq_long %>%
    mutate(
      item_chr = as.character(item),
      domain   = item_domains[item_chr],
      value    = as.character(value)
    ) %>%
    tidyr::complete(domain, item_chr, value = c("YES", "NO", "NR", "NA"), fill = list(pct = 0)) %>%
    group_by(domain, value) %>%
    summarise(pct = mean(pct), .groups = "drop") %>%
    group_by(domain) %>%
    mutate(pct = 100 * pct / sum(pct)) %>%
    ungroup()
  
  df_dom$domain <- factor(df_dom$domain, levels = domain_levels)
  df_dom$value  <- factor(df_dom$value, levels = c("YES", "NO", "NR", "NA"))
  
  out <- df_dom %>%
    mutate(cell = sprintf("%.1f%%", pct)) %>%
    select(domain, value, cell) %>%
    tidyr::pivot_wider(names_from = value, values_from = cell)
  
  for (v in c("YES", "NO", "NR", "NA")) {
    if (!v %in% names(out)) out[[v]] <- "0.0%"
  }
  
  out %>%
    mutate(
      YES = ifelse(is.na(YES), "0.0%", YES),
      NO  = ifelse(is.na(NO),  "0.0%", NO),
      NR  = ifelse(is.na(NR),  "0.0%", NR),
      `NA`= ifelse(is.na(`NA`),"0.0%", `NA`)
    ) %>%
    select(Domain = domain, YES, NO, NR, `NA`)
}

# Paper-ready
freq_table_wide_paper <- function(freq_long, item_labels, item_domains, domain_levels = NULL, items) {
  df <- freq_long %>%
    mutate(
      item_chr   = as.character(item),
      domain     = unname(item_domains[item_chr]),
      item_code  = item_chr,
      item_label = unname(item_labels[item_chr]),
      cell       = sprintf("%d (%.1f%%)", n, pct)
    ) %>%
    select(domain, item_code, item_label, value, cell) %>%
    tidyr::pivot_wider(
      names_from  = value,
      values_from = cell,
      values_fill = "0 (0.0%)"
    )
  
  for (v in c("YES", "NO", "NR", "NA")) if (!v %in% names(df)) df[[v]] <- "0 (0.0%)"
  if (!is.null(domain_levels)) df$domain <- factor(df$domain, levels = domain_levels)
  df$item_code <- factor(df$item_code, levels = items)
  
  df %>%
    arrange(domain, item_code) %>%
    mutate(item_code = as.character(item_code)) %>%
    select(Domain = domain, Item = item_code, Label = item_label, YES, NO, NR, `NA`)
}

domain_table_paper <- function(freq_long, item_domains, domain_levels = NULL) {
  df_dom_long <- freq_long %>%
    mutate(
      item_chr = as.character(item),
      domain   = unname(item_domains[item_chr]),
      value    = as.character(value)
    ) %>%
    tidyr::complete(domain, item_chr, value = c("YES", "NO", "NR", "NA"), fill = list(pct = 0)) %>%
    group_by(domain, item_chr, value) %>%
    summarise(pct = mean(pct), .groups = "drop") %>%
    group_by(domain, value) %>%
    summarise(pct = mean(pct), n_items = n_distinct(item_chr), .groups = "drop") %>%
    group_by(domain) %>%
    mutate(pct = 100 * pct / sum(pct)) %>%
    ungroup()
  
  if (!is.null(domain_levels)) df_dom_long$domain <- factor(df_dom_long$domain, levels = domain_levels)
  
  out <- df_dom_long %>%
    mutate(cell = sprintf("%.1f%%", pct)) %>%
    select(domain, n_items, value, cell) %>%
    tidyr::pivot_wider(names_from = value, values_from = cell)
  
  for (v in c("YES", "NO", "NR", "NA")) if (!v %in% names(out)) out[[v]] <- "0.0%"
  
  out %>%
    mutate(
      YES = ifelse(is.na(YES), "0.0%", YES),
      NO  = ifelse(is.na(NO),  "0.0%", NO),
      NR  = ifelse(is.na(NR),  "0.0%", NR),
      `NA`= ifelse(is.na(`NA`),"0.0%", `NA`)
    ) %>%
    arrange(domain) %>%
    select(Domain = domain, n_items, YES, NO, NR, `NA`)
}

# ---------------------------
# Configuration readers + validators (Option B)
# ---------------------------
read_items_csv <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  names(df) <- tolower(trimws(names(df)))
  # allow legacy column names
  if ("short" %in% names(df) && !"short_label" %in% names(df)) df <- dplyr::rename(df, short_label = short)
  if ("label" %in% names(df) && !"short_label" %in% names(df)) df <- dplyr::rename(df, short_label = label)
  if ("dom"   %in% names(df) && !"domain" %in% names(df)) df <- dplyr::rename(df, domain = dom)
  
  req_cols <- c("item", "short_label", "domain")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) stop("items.csv missing columns: ", paste(miss, collapse = ", "))
  df
}

validate_items_csv_contract <- function(df) {
  msgs <- c()
  if (anyDuplicated(df$item)) msgs <- c(msgs, paste0("items.csv: duplicated item(s): ", paste(unique(df$item[duplicated(df$item)]), collapse = ", ")))
  missing_items <- setdiff(ITEMS, df$item)
  extra_items   <- setdiff(unique(df$item), ITEMS)
  if (length(missing_items) > 0) msgs <- c(msgs, paste0("items.csv: missing items: ", paste(missing_items, collapse = ", ")))
  if (length(extra_items) > 0)   msgs <- c(msgs, paste0("items.csv: extra/unknown items: ", paste(extra_items, collapse = ", ")))
  if (any(is.na(df$short_label) | trimws(df$short_label) == "")) msgs <- c(msgs, "items.csv: some short_label values are empty.")
  if (any(is.na(df$domain) | trimws(df$domain) == "")) msgs <- c(msgs, "items.csv: some domain values are empty.")
  list(ok = length(msgs) == 0, messages = msgs)
}

read_manual_bilingual <- function(path) {
  ext <- tools::file_ext(path)
  dfm <- if (ext == "xlsx") readxl::read_xlsx(path) else readr::read_csv(path, show_col_types = FALSE)
  names(dfm) <- tolower(trimws(names(dfm)))
  req_cols <- c(
    "item",
    "domain_en","domain_es",
    "short_label_es","short_label_en",
    "long_label_es","long_label_en",
    "definition_brief_es","definition_brief_en",
    "criteria_yes_es","criteria_yes_en",
    "criteria_no_es","criteria_no_en",
    "criteria_nr_es","criteria_nr_en",
    "criteria_na_es","criteria_na_en",
    "decision_tree_es","decision_tree_en"
  )
  miss <- setdiff(req_cols, names(dfm))
  if (length(miss) > 0) stop("Manual file missing columns: ", paste(miss, collapse = ", "))
  dfm
}

validate_manual_contract <- function(dfm) {
  msgs <- c()
  if (anyDuplicated(dfm$item)) msgs <- c(msgs, paste0("Manual: duplicated item(s): ", paste(unique(dfm$item[duplicated(dfm$item)]), collapse = ", ")))
  missing_items <- setdiff(ITEMS, dfm$item)
  extra_items   <- setdiff(unique(dfm$item), ITEMS)
  if (length(missing_items) > 0) msgs <- c(msgs, paste0("Manual: missing items: ", paste(missing_items, collapse = ", ")))
  if (length(extra_items) > 0)   msgs <- c(msgs, paste0("Manual: extra/unknown items: ", paste(extra_items, collapse = ", ")))
  list(ok = length(msgs) == 0, messages = msgs)
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  uiOutput("title_ui"),
  sidebarLayout(
    sidebarPanel(
      tags$label(uiOutput("file_label_ui")),
      fileInput("file", label = NULL, accept = c(".xlsx", ".csv")),
      uiOutput("sheet_ui"),
      
      uiOutput("view_mode_ui"),
      uiOutput("lang_ui"),
      uiOutput("theme_ui"),
      uiOutput("apa_ui"),
      uiOutput("run_ui"),
      hr(),
      strong(textOutput("notes_label")),
      textOutput("config_short"),
      textOutput("status_short"),
      uiOutput("results_ready_hint"),
      uiOutput("status_details")
    ),
    mainPanel(
      uiOutput("main_ui")
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # ---- Templates directory (for local Shiny; not required for shinylive export) ----
  # ---- Templates directory (safe: works in local Shiny; won't break shinylive) ----
  observeEvent(TRUE, {
    tryCatch({
      dir.create("www/templates", recursive = TRUE, showWarnings = FALSE)
      
      tpl <- data.frame(study_id = "study_001", citation = "Author (Year)", stringsAsFactors = FALSE)
      for (it in ITEMS) tpl[[it]] <- ""
      readr::write_csv(tpl, "www/templates/quality_coding_template.csv")
      
      tpl_items <- data.frame(
        item = ITEMS,
        short_label = paste("Short label for", ITEMS),
        domain = rep("Domain 1", length(ITEMS)),
        stringsAsFactors = FALSE
      )
      readr::write_csv(tpl_items, "www/templates/items_template.csv")
      
      tpl_manual <- data.frame(
        item = ITEMS,
        domain_en = "Domain",
        domain_es = "Dominio",
        short_label_es = "Etiqueta corta",
        short_label_en = "Short label",
        long_label_es  = "Etiqueta larga",
        long_label_en  = "Long label",
        definition_brief_es = "Definición breve",
        definition_brief_en = "Brief definition",
        criteria_yes_es = "Criterios YES",
        criteria_yes_en = "YES criteria",
        criteria_no_es  = "Criterios NO",
        criteria_no_en  = "NO criteria",
        criteria_nr_es  = "Criterios NR",
        criteria_nr_en  = "NR criteria",
        criteria_na_es  = "Criterios NA",
        criteria_na_en  = "NA criteria",
        decision_tree_es = "Árbol de decisión",
        decision_tree_en = "Decision tree",
        stringsAsFactors = FALSE
      )
      readr::write_csv(tpl_manual, "www/templates/items_extended_bilingual_template.csv")
    }, error = function(e) {
      message("Skipping template file writes (likely shinylive/read-only FS): ", conditionMessage(e))
    })
  }, once = TRUE)
  
  
  
  # ---- Dynamic UI labels ----
  output$title_ui <- renderUI({ tags$div(tags$h2(t_txt(input$lang %||% "es", "title"))) })
  output$file_label_ui <- renderUI({ t_txt(input$lang %||% "es", "upload") })
  
  output$view_mode_ui <- renderUI({
    radioButtons(
      inputId = "view_mode",
      label   = t_txt(input$lang %||% "es", "view_mode"),
      choices = setNames(
        c("items", "domains"),
        c(t_txt(input$lang %||% "es", "by_items"), t_txt(input$lang %||% "es", "by_domains"))
      ),
      selected = "items",
      inline = TRUE
    )
  })
  
  output$lang_ui <- renderUI({
    selectInput(
      inputId = "lang",
      label   = t_txt(input$lang %||% "es", "language"),
      choices = c("Español" = "es", "English" = "en"),
      selected = isolate(input$lang %||% "es")
    )
  })
  
  output$theme_ui <- renderUI({
    selectInput("theme", t_txt(input$lang %||% "es", "theme"), choices = names(THEMES), selected = "Cochrane")
  })
  
  output$apa_ui <- renderUI({
    checkboxInput("use_apa_labels", t_txt(input$lang %||% "es", "apa_labels"), TRUE)
  })
  
  output$run_ui <- renderUI({
    actionButton("run", t_txt(input$lang %||% "es", "run"), class = "btn-primary")
  })
  
  output$notes_label <- renderText({ t_txt(input$lang %||% "es", "notes") })
  
  # ---- sheet selector ----
  output$sheet_ui <- renderUI({
    req(input$file)
    if (tools::file_ext(input$file$name) == "xlsx") {
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput("sheet", t_txt(input$lang %||% "es", "sheet"), choices = sheets, selected = sheets[1])
    } else NULL
  })
  
  # ---------------------------
  # Configuration reactives (default files + user overrides)
  # ---------------------------
  default_items_path  <- "items.csv"
  default_manual_xlsx <- "items_extended_bilingual.xlsx"
  default_manual_csv  <- "items_extended_bilingual.csv"
  
  cfg_items_upload <- reactive({
    if (is.null(input$cfg_items_csv)) return(NULL)
    input$cfg_items_csv$datapath
  })
  
  cfg_manual_upload <- reactive({
    if (is.null(input$cfg_manual_file)) return(NULL)
    input$cfg_manual_file$datapath
  })
  
  items_dict_raw <- reactive({
    path <- cfg_items_upload()
    if (!is.null(path)) {
      df <- read_items_csv(path)
      attr(df, "source") <- "uploaded"
      attr(df, "source_name") <- input$cfg_items_csv$name
      return(df)
    }
    
    if (file.exists(default_items_path)) {
      df <- read_items_csv(default_items_path)
      attr(df, "source") <- "default"
      attr(df, "source_name") <- default_items_path
      return(df)
    }
    
    df <- DEFAULT_ITEMS_DF
    attr(df, "source") <- "built-in"
    attr(df, "source_name") <- "DEFAULT_ITEMS_DF"
    df
  })
  
  
  items_dict_validation <- reactive({
    df <- items_dict_raw()
    validate_items_csv_contract(df)
  })
  
  # Effective item dictionary:
  # - If a valid bilingual manual is available, derive labels/domains from it (single source of truth)
  # - Otherwise fall back to items.csv (or built-in defaults)
  effective_items_df <- reactive({
    mdf <- manual_raw()
    mv  <- manual_validation()
    if (!is.null(mdf) && isTRUE(mv$ok)) {
      # Prefer EN for the internal dictionary (stable across UI language),
      # but fall back to ES when EN fields are missing.
      df <- mdf %>%
        dplyr::filter(item %in% ITEMS) %>%
        dplyr::mutate(
          short_label = dplyr::if_else(
            !is.na(short_label_en) & trimws(as.character(short_label_en)) != "",
            as.character(short_label_en),
            as.character(short_label_es)
          ),
          domain = dplyr::if_else(
            !is.na(domain_en) & trimws(as.character(domain_en)) != "",
            as.character(domain_en),
            as.character(domain_es)
          )
        ) %>%
        dplyr::select(item, short_label, domain) %>%
        dplyr::mutate(item = factor(item, levels = ITEMS)) %>%
        dplyr::arrange(item)
      df$item <- as.character(df$item)
      attr(df, "source") <- "derived_from_manual"
      return(df)
    }
    df <- items_dict_raw()
    df <- df %>%
      dplyr::mutate(item = factor(item, levels = ITEMS)) %>%
      dplyr::arrange(item)
    df$item <- as.character(df$item)
    df
  })

  item_labels <- reactive({
    df <- effective_items_df()
    setNames(df$short_label, df$item)
  })

  item_domains <- reactive({
    df <- effective_items_df()
    setNames(df$domain, df$item)
  })

  domain_levels <- reactive({
    unique(effective_items_df()$domain)
  })

  manual_raw <- reactive({
    path <- cfg_manual_upload()
    if (!is.null(path)) {
      dfm <- read_manual_bilingual(path)
      attr(dfm, "source") <- "uploaded"
      attr(dfm, "source_name") <- input$cfg_manual_file$name
      return(dfm)
    }
    if (file.exists(default_manual_xlsx)) {
      dfm <- read_manual_bilingual(default_manual_xlsx)
      attr(dfm, "source") <- "default"
      attr(dfm, "source_name") <- default_manual_xlsx
      return(dfm)
    }
    if (file.exists(default_manual_csv)) {
      dfm <- read_manual_bilingual(default_manual_csv)
      attr(dfm, "source") <- "default"
      attr(dfm, "source_name") <- default_manual_csv
      return(dfm)
    }
    NULL
  })
  
  manual_validation <- reactive({
    dfm <- manual_raw()
    if (is.null(dfm)) return(list(ok = TRUE, messages = c("Manual: not provided.")))
    validate_manual_contract(dfm)
  })
  
  manual_ok <- reactive({
    mv <- manual_validation()
    (!is.null(manual_raw())) && isTRUE(mv$ok)
  })
  
  output$config_short <- renderText({
    idf <- items_dict_raw()
    items_src <- paste0(attr(idf, "source"), " (", attr(idf, "source_name"), ")")
    
    mdf <- manual_raw()
    manual_src <- if (is.null(mdf)) "(none)" else paste0(attr(mdf, "source"), " (", attr(mdf, "source_name"), ")")
    
    paste0("Config: items.csv ", items_src, " | manual ", manual_src)
  })
  
  manual_dict <- reactive({
    if (!manual_ok()) return(NULL)
    dfm0 <- manual_raw()
    if (is.null(dfm0)) return(NULL)
    
    dfm <- dfm0
    lang <- input$lang %||% "es"
    
    pick <- function(base) {
      es <- paste0(base, "_es")
      en <- paste0(base, "_en")
      if (identical(lang, "en")) {
        out <- dfm[[en]]
        ifelse(is.na(out) | trimws(as.character(out)) == "", dfm[[es]], out)
      } else {
        dfm[[es]]
      }
    }
    pick_domain <- function() {
      if (identical(lang, "en")) dfm[["domain_en"]] else dfm[["domain_es"]]
    }
    
    dfm$domain_active           <- pick_domain()
    dfm$short_label_active      <- pick("short_label")
    dfm$long_label_active       <- pick("long_label")
    dfm$definition_brief_active <- pick("definition_brief")
    dfm$criteria_yes_active     <- pick("criteria_yes")
    dfm$criteria_no_active      <- pick("criteria_no")
    dfm$criteria_nr_active      <- pick("criteria_nr")
    dfm$criteria_na_active      <- pick("criteria_na")
    dfm$decision_tree_active    <- pick("decision_tree")
    
    dfm <- dfm %>%
      dplyr::filter(item %in% ITEMS) %>%
      dplyr::mutate(item = factor(item, levels = ITEMS)) %>%
      dplyr::arrange(item)
    
    dfm$item <- as.character(dfm$item)
    dfm
  })
  
  # ---------------------------
  # Load data (coding file) — robust initial state
  # ---------------------------
  loaded <- reactiveVal(list(ok = FALSE, errors = NULL, warnings = NULL, df = NULL))
  
  observeEvent(input$run, {
    req(input$file)
    
    iv <- items_dict_validation()
    if (!iv$ok) {
      loaded(list(ok = FALSE, errors = c("Configuration error in items.csv:", iv$messages), warnings = NULL, df = NULL))
      return()
    }
    
    mv <- manual_validation()
    manual_warn <- if (!mv$ok) c("Manual configuration issues:", mv$messages) else character(0)
    
    path  <- input$file$datapath
    sheet <- if (tools::file_ext(input$file$name) == "xlsx") input$sheet else NULL
    
    df <- read_any(path, sheet)
    
    # Backward compatible mapping: first two columns + next 17
    if (ncol(df) >= (2 + length(ITEMS))) {
      names(df)[1] <- "study_id"
      names(df)[2] <- "citation"
      names(df)[3:(2 + length(ITEMS))] <- ITEMS
    } else {
      loaded(list(ok = FALSE, errors = c(
        paste0("File has ", ncol(df), " columns. Expected at least ", 2 + length(ITEMS),
               " (study_id, citation, and ", length(ITEMS), " items).")
      ), warnings = NULL, df = NULL))
      return()
    }
    
    vs <- validate_structure(df, ITEMS)
    if (!vs$ok) {
      loaded(list(ok = FALSE, errors = vs$errors, warnings = NULL, df = NULL))
      return()
    }
    
    res <- apply_hardened_rules(df, ITEMS)
    df2 <- res$df
    
    vv <- validate_values(df2, ITEMS)
    if (!vv$ok) {
      errs <- paste0("Invalid values detected: ",
                     paste(paste0(vv$bad$item, "=", vv$bad$val), collapse = "; "),
                     ". Allowed: YES/NO/NR/NA (and SI/SÍ will be mapped to YES).")
      loaded(list(ok = FALSE, errors = c(res$warnings, manual_warn, errs), warnings = NULL, df = NULL))
      return()
    }
    
    loaded(list(ok = TRUE, df = df2, warnings = c(res$warnings, manual_warn), errors = NULL))
  }, ignoreInit = TRUE)
  
  data_ok <- reactive({ isTRUE(loaded()$ok) })
  
  observeEvent(loaded(), {
    L <- loaded()
    if (isTRUE(L$ok)) {
      updateTabsetPanel(
        session,
        inputId = "main_tabs",
        selected = t_txt(input$lang %||% "es", "tab_preview")
      )
    }
  }, ignoreInit = TRUE)
  
  
  # ---- Status ----
  output$status_short <- renderText({
    L <- loaded()
    if (isFALSE(L$ok) && is.null(L$errors) && is.null(L$warnings) && is.null(L$df)) {
      return(t_txt(input$lang %||% "es", "not_loaded_yet"))
    }
    if (!isTRUE(L$ok)) t_txt(input$lang %||% "es", "issues") else t_txt(input$lang %||% "es", "loaded_ok")
  })
  
  output$results_ready_hint <- renderUI({
    L <- loaded()
    if (!isTRUE(L$ok)) return(NULL)
    
    lang <- input$lang %||% "es"
    msg <- if (identical(lang, "en")) {
      "Results are ready. Open: Preview / Frequencies / Summary figure / Table."
    } else {
      "Resultados listos. Abre: Vista previa / Frecuencias / Figura resumen / Tabla."
    }
    
    tags$div(
      style = "margin-top:6px; padding:8px; border-left:4px solid #2c7be5; background:#f3f8ff;",
      tags$strong(msg)
    )
  })
  
  
  output$status_details <- renderUI({
    L <- loaded()
    if (isFALSE(L$ok) && is.null(L$errors) && is.null(L$warnings)) return(NULL)
    
    summ <- t_txt(input$lang %||% "es", "show_details")
    
    if (!isTRUE(L$ok)) {
      return(
        tags$details(
          open = TRUE,
          tags$summary(summ),
          tags$pre(style = "white-space:pre-wrap; margin-top:8px;",
                   paste("- ", paste(L$errors, collapse = "\n- ")))
        )
      )
    }
    
    warns <- L$warnings
    if (is.null(warns) || length(warns) == 0) return(NULL)
    
    tags$details(
      open = FALSE,
      tags$summary(summ),
      tags$pre(style = "white-space:pre-wrap; margin-top:8px;",
               paste("- ", paste(warns, collapse = "\n- ")))
    )
  })
  
  # ---------------------------
  # Derived tables
  # ---------------------------
  freq <- reactive({
    req(data_ok())
    freq_table(loaded()$df, ITEMS)
  })
  
  domain_df <- reactive({
    req(data_ok(), freq())
    domain_table(freq(), item_domains(), domain_levels())
  })
  
  freq_wide_paper_df <- reactive({
    req(data_ok(), freq())
    freq_table_wide_paper(
      freq_long     = freq(),
      item_labels   = item_labels(),
      item_domains  = item_domains(),
      domain_levels = domain_levels(),
      items         = ITEMS
    )
  })
  
  domain_paper_df <- reactive({
    req(data_ok(), freq())
    domain_table_paper(
      freq_long     = freq(),
      item_domains  = item_domains(),
      domain_levels = domain_levels()
    )
  })
  
  # ---------------------------
  # Plots
  # ---------------------------
  summary_plot_items <- reactive({
    req(data_ok(), freq())
    pal <- THEMES[[input$theme %||% "Cochrane"]]$values
    labs_map <- item_labels()
    freq() %>%
      mutate(
        item = factor(item, levels = ITEM_LEVELS),
        item_lab = factor(labs_map[as.character(item)], levels = labs_map[ITEM_LEVELS])
      ) %>%
      ggplot(aes(x = item_lab, y = pct, fill = value)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = pal, drop = FALSE) +
      labs(x = NULL, y = "Percent of studies", fill = "Rating") +
      theme_minimal()
  })
  
  summary_plot_domains <- reactive({
    req(data_ok(), freq())
    pal <- THEMES[[input$theme %||% "Cochrane"]]$values
    
    df_dom <- freq() %>%
      mutate(
        item_chr = as.character(item),
        domain = item_domains()[item_chr],
        value = as.character(value)
      ) %>%
      tidyr::complete(domain, item_chr, value = c("YES", "NO", "NR", "NA"), fill = list(pct = 0)) %>%
      group_by(domain, value) %>%
      summarise(pct = mean(pct), .groups = "drop") %>%
      group_by(domain) %>%
      mutate(pct = 100 * pct / sum(pct)) %>%
      ungroup()
    
    df_dom$domain <- factor(df_dom$domain, levels = domain_levels())
    df_dom$value  <- factor(df_dom$value, levels = c("YES", "NO", "NR", "NA"))
    
    ggplot(df_dom, aes(x = domain, y = pct, fill = value)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = pal, drop = FALSE) +
      labs(x = NULL, y = "Mean % of items within domain (normalized)", fill = "Rating") +
      theme_minimal()
  })
  
  summary_any_plot <- reactive({
    req(data_ok())
    if ((input$view_mode %||% "items") == "domains") summary_plot_domains() else summary_plot_items()
  })
  
  table_plot <- reactive({
    req(data_ok())
    df <- loaded()$df %>% arrange(citation)
    
    labs_map <- item_labels()
    pal <- THEMES[[input$theme %||% "Cochrane"]]$values
    
    long <- df %>%
      select(study_id, citation, all_of(ITEMS)) %>%
      pivot_longer(all_of(ITEMS), names_to = "item", values_to = "value") %>%
      mutate(
        item = factor(item, levels = ITEM_LEVELS),
        item_lab = factor(labs_map[as.character(item)], levels = labs_map[ITEM_LEVELS]),
        value = factor(value, levels = c("YES", "NO", "NR", "NA")),
        study_label = if (isTRUE(input$use_apa_labels)) citation else study_id
      )
    
    ggplot(long, aes(x = item_lab, y = factor(study_label, levels = rev(unique(study_label))), fill = value)) +
      geom_tile(color = "white", linewidth = 0.2) +
      scale_fill_manual(values = pal, drop = FALSE, na.value = THEMES[[input$theme %||% "Cochrane"]]$na_fill) +
      labs(x = NULL, y = NULL, fill = "Rating") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  })
  
  # ---------------------------
  # Outputs (DT/plots)
  # ---------------------------
  output$preview <- renderDT({
    req(data_ok())
    datatable(loaded()$df, options = list(scrollX = TRUE, pageLength = 15))
  })
  
  output$freq_dt <- renderDT({
    req(data_ok(), freq())
    if ((input$view_mode %||% "items") == "domains") {
      datatable(domain_df(), options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(freq_table_wide(freq(), item_labels()), options = list(pageLength = 25, scrollX = TRUE))
    }
  })
  
  output$stacked_plot <- renderPlot({ req(data_ok()); summary_any_plot() })
  output$g1_plot      <- renderPlot({ req(data_ok()); table_plot() })
  
  output$item_key_dt <- renderDT({
    labs_map <- item_labels()
    key_df <- data.frame(
      Item  = paste0("Item ", 1:length(ITEMS)),
      Code  = ITEMS,
      Label = unname(labs_map[ITEMS]),
      stringsAsFactors = FALSE
    )
    datatable(key_df, options = list(pageLength = length(ITEMS), dom = "t"))
  })
  
  # ---------------------------
  # Manual help blocks
  # ---------------------------
  block_manual <- function(title, text) {
    if (is.null(text) || is.na(text) || trimws(as.character(text)) == "") return(NULL)
    tags$div(
      tags$h5(title),
      tags$pre(style = "white-space:pre-wrap; background:#f7f7f7; padding:10px; border-radius:6px;", text)
    )
  }
  
  observeEvent(manual_dict(), {
    dfm <- manual_dict()
    req(dfm)
    
    doms <- unique(dfm$domain_active)
    doms <- doms[!is.na(doms) & trimws(as.character(doms)) != ""]
    doms <- sort(doms)
    
    updateSelectInput(session, "manual_domain", choices = c("All", doms), selected = "All")
    
    ch <- setNames(dfm$item, dfm$short_label_active)
    updateSelectInput(session, "freq_item_help",  choices = ch, selected = dfm$item[1])
    updateSelectInput(session, "table_item_help", choices = ch, selected = dfm$item[1])
  }, ignoreInit = TRUE)
  
  output$manual_item_ui <- renderUI({
    dfm <- manual_dict()
    if (is.null(dfm)) return(tags$em(t_txt(input$lang %||% "es", "manual_disabled")))
    dom <- input$manual_domain %||% "All"
    choices <- if (dom == "All") dfm$item else dfm$item[dfm$domain_active == dom]
    labels <- dfm$short_label_active[match(choices, dfm$item)]
    names(choices) <- labels
    selectInput("manual_item", t_txt(input$lang %||% "es", "manual_select"), choices = choices, selected = choices[1])
  })
  
  manual_filtered <- reactive({
    dfm <- manual_dict()
    req(dfm)
    dom <- input$manual_domain %||% "All"
    if (dom == "All") dfm else dfm[dfm$domain_active == dom, ]
  })
  
  output$manual_dt <- renderDT({
    dfm <- manual_filtered()
    req(dfm)
    show <- dfm %>%
      select(
        item,
        domain = domain_active,
        short_label = short_label_active,
        long_label = long_label_active,
        definition_brief = definition_brief_active
      ) %>%
      arrange(domain, item)
    datatable(show, selection = "single", options = list(pageLength = length(ITEMS), scrollX = TRUE))
  })
  
  observeEvent(input$manual_dt_rows_selected, {
    dfm <- manual_filtered()
    req(dfm)
    idx <- input$manual_dt_rows_selected
    if (length(idx) == 1) updateSelectInput(session, "manual_item", selected = dfm$item[idx])
  }, ignoreInit = TRUE)
  
  output$manual_rules_ui <- renderUI({
    dfm <- manual_dict()
    req(dfm, input$manual_item)
    row <- dfm[dfm$item == input$manual_item, ]
    req(nrow(row) == 1)
    tags$div(
      tags$h4(paste0(row$item, " — ", row$long_label_active)),
      tags$p(tags$b("Domain: "), row$domain_active, " | ", tags$b("Short label: "), row$short_label_active),
      tags$hr(),
      block_manual("Definition (brief)", row$definition_brief_active),
      block_manual("YES", row$criteria_yes_active),
      block_manual("NO",  row$criteria_no_active),
      block_manual("NR",  row$criteria_nr_active),
      block_manual("NA",  row$criteria_na_active),
      block_manual("Decision tree", row$decision_tree_active)
    )
  })
  
  output$freq_item_rules <- renderUI({
    dfm <- manual_dict()
    req(dfm, input$freq_item_help)
    row <- dfm[dfm$item == input$freq_item_help, ]
    req(nrow(row) == 1)
    tags$div(
      tags$h5(paste0(row$item, " — ", row$long_label_active)),
      tags$p(tags$b("Domain: "), row$domain_active, " | ", tags$b("Short label: "), row$short_label_active),
      tags$hr(),
      block_manual("Definition (brief)", row$definition_brief_active),
      block_manual("YES", row$criteria_yes_active),
      block_manual("NO",  row$criteria_no_active),
      block_manual("NR",  row$criteria_nr_active),
      block_manual("NA",  row$criteria_na_active),
      block_manual("Decision tree", row$decision_tree_active)
    )
  })
  
  output$table_item_rules <- renderUI({
    dfm <- manual_dict()
    req(dfm, input$table_item_help)
    row <- dfm[dfm$item == input$table_item_help, ]
    req(nrow(row) == 1)
    tags$div(
      tags$h5(paste0(row$item, " — ", row$long_label_active)),
      tags$p(tags$b("Domain: "), row$domain_active, " | ", tags$b("Short label: "), row$short_label_active),
      tags$hr(),
      block_manual("Definition (brief)", row$definition_brief_active),
      block_manual("YES", row$criteria_yes_active),
      block_manual("NO",  row$criteria_no_active),
      block_manual("NR",  row$criteria_nr_active),
      block_manual("NA",  row$criteria_na_active),
      block_manual("Decision tree", row$decision_tree_active)
    )
  })
  
  # ---------------------------
  # Frequencies: note + download UI
  # ---------------------------
  output$freq_mode_note <- renderUI({
    mode <- input$view_mode %||% "items"
    msg <- if (mode == "domains") t_txt(input$lang %||% "es", "showing_domains") else t_txt(input$lang %||% "es", "showing_items")
    tags$div(tags$strong(paste0(t_txt(input$lang %||% "es", "using"), " ")), msg)
  })
  
  output$down_freq_any_ui <- renderUI({
    mode <- input$view_mode %||% "items"
    lab <- if (mode == "domains") t_txt(input$lang %||% "es", "download_domains_csv") else t_txt(input$lang %||% "es", "download_items_csv")
    downloadButton("down_freq_any", lab)
  })
  
  # ---------------------------
  # Downloads: frequencies/data/figures/tables
  # ---------------------------
  output$down_freq_any <- downloadHandler(
    filename = function() {
      if ((input$view_mode %||% "items") == "domains") "quality_domain_table.csv"
      else "quality_item_frequencies_wide.csv"
    },
    content = function(file) {
      req(data_ok())
      if ((input$view_mode %||% "items") == "domains") {
        readr::write_csv(domain_df(), file)
      } else {
        readr::write_csv(freq_table_wide(freq(), item_labels()), file)
      }
    }
  )
  
  output$down_freq_wide_paper <- downloadHandler(
    filename = function() { "quality_item_frequencies_paper_ready.csv" },
    content  = function(file) { req(data_ok()); readr::write_csv(freq_wide_paper_df(), file) }
  )
  
  output$down_domain_table_paper <- downloadHandler(
    filename = function() { "quality_domain_table_paper_ready.csv" },
    content  = function(file) { req(data_ok()); readr::write_csv(domain_paper_df(), file) }
  )
  
  output$down_clean <- downloadHandler(
    filename = function() { "quality_coding_normalized.csv" },
    content  = function(file) { req(data_ok()); readr::write_csv(loaded()$df, file) }
  )
  
  output$down_summary_png <- downloadHandler(
    filename = function() {
      if ((input$view_mode %||% "items") == "domains") "quality_summary_by_domain_600dpi.png"
      else "quality_summary_by_item_600dpi.png"
    },
    content = function(file) {
      req(data_ok())
      ggsave(file, plot = summary_any_plot(), width = 11, height = 6, dpi = 600)
    }
  )
  
  output$down_summary_pdf <- downloadHandler(
    filename = function() {
      if ((input$view_mode %||% "items") == "domains") "quality_summary_by_domain.pdf"
      else "quality_summary_by_item.pdf"
    },
    content = function(file) {
      req(data_ok())
      ggsave(file, plot = summary_any_plot(), width = 11, height = 6, device = "pdf")
    }
  )
  
  output$down_table_png <- downloadHandler(
    filename = function() { "quality_table_600dpi.png" },
    content  = function(file) { req(data_ok()); ggsave(file, plot = table_plot(), width = 12, height = 8, dpi = 600) }
  )
  
  output$down_table_pdf <- downloadHandler(
    filename = function() { "quality_table.pdf" },
    content  = function(file) { req(data_ok()); ggsave(file, plot = table_plot(), width = 12, height = 8, device = "pdf") }
  )
  
  # ---------------------------
  # Templates + configuration downloads (Option B)
  # ---------------------------
  output$down_coding_template <- downloadHandler(
    filename = function() { "quality_coding_template.csv" },
    content  = function(file) {
      row1 <- data.frame(study_id = "study_001", citation = "Author (Year)", stringsAsFactors = FALSE)
      for (it in ITEMS) row1[[it]] <- "YES"
      
      row2 <- data.frame(study_id = "NOTE", citation = "Allowed values: YES / NO / NR / NA", stringsAsFactors = FALSE)
      for (it in ITEMS) row2[[it]] <- "YES|NO|NR|NA"
      
      row3 <- data.frame(study_id = "", citation = "", stringsAsFactors = FALSE)
      for (it in ITEMS) row3[[it]] <- ""
      
      tpl <- dplyr::bind_rows(row1, row2, row3)
      readr::write_csv(tpl, file)
    }
  )
  
  output$down_items_template <- downloadHandler(
    filename = function() { "items_template.csv" },
    content  = function(file) {
      # Provide a meaningful editable template: current effective items dictionary.
      tpl <- effective_items_df() %>% dplyr::select(item, short_label, domain)
      readr::write_csv(tpl, file)
    }
  )
  
  output$down_manual_template <- downloadHandler(
    filename = function() { "items_extended_bilingual_template.csv" },
    content  = function(file) {
      # Provide the current default/effective manual as an editable template when available.
      dfm <- manual_raw()
      if (is.null(dfm)) {
        tpl <- data.frame(
          item = ITEMS,
          domain_en = "Domain",
          domain_es = "Dominio",
          short_label_es = "Etiqueta corta",
          short_label_en = "Short label",
          long_label_es  = "Etiqueta larga",
          long_label_en  = "Long label",
          definition_brief_es = "Definición breve",
          definition_brief_en = "Brief definition",
          criteria_yes_es = "Criterios YES",
          criteria_yes_en = "YES criteria",
          criteria_no_es  = "Criterios NO",
          criteria_no_en  = "NO criteria",
          criteria_nr_es  = "Criterios NR",
          criteria_nr_en  = "NR criteria",
          criteria_na_es  = "Criterios NA",
          criteria_na_en  = "NA criteria",
          decision_tree_es = "Árbol de decisión",
          decision_tree_en = "Decision tree",
          stringsAsFactors = FALSE
        )
        readr::write_csv(tpl, file)
      } else {
        readr::write_csv(dfm, file)
      }
    }
  )
  
  output$down_effective_items <- downloadHandler(
    filename = function() { "items_effective.csv" },
    content  = function(file) {
      df <- effective_items_df() %>% dplyr::select(item, short_label, domain)
      readr::write_csv(df, file)
    }
  )
  
  output$down_effective_manual <- downloadHandler(
    filename = function() { "items_extended_bilingual_effective.csv" },
    content  = function(file) {
      dfm <- manual_raw()
      if (is.null(dfm)) {
        readr::write_csv(data.frame(message = "Manual not provided."), file)
      } else {
        readr::write_csv(dfm, file)
      }
    }
  )
  
  output$cfg_downloads_ui <- renderUI({
    tagList(
      downloadButton("down_coding_template", t_txt(input$lang %||% "es", "cfg_download_template")),
      tags$br(), tags$br(),
      downloadButton("down_items_template", t_txt(input$lang %||% "es", "cfg_download_items_template")),
      tags$br(), tags$br(),
      downloadButton("down_manual_template", t_txt(input$lang %||% "es", "cfg_download_manual_template")),
      tags$hr(),
      downloadButton("down_effective_items", t_txt(input$lang %||% "es", "cfg_effective_items")),
      tags$br(), tags$br(),
      downloadButton("down_effective_manual", t_txt(input$lang %||% "es", "cfg_effective_manual"))
    )
  })
  
  # ---------------------------
  # Audit log (expanded with config source)
  # ---------------------------
  output$down_audit <- downloadHandler(
    filename = function() {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("quality_audit_log_", ts, ".txt")
    },
    content = function(file) {
      lines <- c()
      lines <- c(lines, "=== Audit log ===")
      lines <- c(lines, paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
      lines <- c(lines, paste0("Language: ", input$lang %||% ""))
      lines <- c(lines, paste0("Theme: ", input$theme %||% ""))
      lines <- c(lines, paste0("View mode: ", input$view_mode %||% ""))
      
      if (!is.null(input$file)) lines <- c(lines, paste0("Uploaded data file: ", input$file$name))
      else lines <- c(lines, "Uploaded data file: (none)")
      
      idf <- items_dict_raw()
      lines <- c(lines, paste0("items.csv source: ", attr(idf, "source"), " (", attr(idf, "source_name"), ")"))
      
      mdf <- manual_raw()
      if (is.null(mdf)) {
        lines <- c(lines, "manual source: (none)")
      } else {
        lines <- c(lines, paste0("manual source: ", attr(mdf, "source"), " (", attr(mdf, "source_name"), ")"))
      }
      
      iv <- items_dict_validation()
      lines <- c(lines, paste0("items.csv valid: ", iv$ok))
      if (!iv$ok) lines <- c(lines, paste0("- ", iv$messages))
      
      mv <- manual_validation()
      lines <- c(lines, paste0("manual valid: ", mv$ok))
      if (!mv$ok) lines <- c(lines, paste0("- ", mv$messages))
      
      lines <- c(lines, "")
      
      if (!data_ok()) {
        lines <- c(lines, "STATUS: NOT LOADED / INVALID DATA")
        L <- loaded()
        if (!isTRUE(L$ok) && !is.null(L$errors)) {
          lines <- c(lines, "Data errors:")
          lines <- c(lines, paste0("- ", L$errors))
        }
        writeLines(lines, file)
        return(NULL)
      }
      
      df <- loaded()$df
      lines <- c(lines, "STATUS: LOADED OK")
      lines <- c(lines, paste0("n_studies: ", nrow(df)))
      lines <- c(lines, paste0("n_items: ", length(ITEMS)))
      lines <- c(lines, "")
      
      warns <- loaded()$warnings
      if (is.null(warns) || length(warns) == 0) lines <- c(lines, "Warnings: (none)")
      else {
        lines <- c(lines, "Warnings:")
        lines <- c(lines, paste0("- ", warns))
      }
      lines <- c(lines, "")
      
      long <- df %>%
        select(all_of(ITEMS)) %>%
        pivot_longer(everything(), names_to = "item", values_to = "value") %>%
        mutate(value = normalize_vals(value))
      
      dist <- long %>%
        mutate(value = ifelse(is.na(value), "(NA_missing)", value)) %>%
        count(value, name = "n") %>%
        mutate(pct = 100 * n / sum(n)) %>%
        arrange(desc(n))
      
      lines <- c(lines, "Overall value distribution (including missing):")
      lines <- c(lines, paste0(sprintf("%-12s", dist$value), "  ", sprintf("%6d", dist$n), "  ", sprintf("%6.2f%%", dist$pct)))
      
      writeLines(lines, file)
    }
  )
  
  # ---------------------------
  # Config status UI
  # ---------------------------
  output$cfg_status_ui <- renderUI({
    iv <- items_dict_validation()
    mv <- manual_validation()
    
    items_src <- paste0(attr(items_dict_raw(), "source"), " (", attr(items_dict_raw(), "source_name"), ")")
    manual_src <- if (is.null(manual_raw())) "(none)" else paste0(attr(manual_raw(), "source"), " (", attr(manual_raw(), "source_name"), ")")
    
    tags$div(
      tags$p(tags$b("items.csv: "), items_src),
      if (iv$ok) tags$p("✅ items.csv OK") else tags$p(tags$b("❌ items.csv issues:"), tags$br(), paste(iv$messages, collapse = "\n")),
      tags$hr(),
      tags$p(tags$b("manual: "), manual_src),
      if (mv$ok) tags$p("✅ manual OK (or not provided)") else tags$p(tags$b("❌ manual issues:"), tags$br(), paste(mv$messages, collapse = "\n"))
    )
  })
  
  # ---------------------------
  # About/Cite (static placeholders you can edit later)
  # ---------------------------
  output$about_ui <- renderUI({
    lang <- input$lang %||% "es"
    if (identical(lang, "en")) {
      tagList(
        tags$h3("About / Cite"),
        tags$p("RMQS Shiny App (demo). This app supports coding and summarizing a revised methodological quality scale."),
        tags$ul(
          tags$li(tags$b("Name:"), " Revised Methodological Quality Scale (RMQS) Shiny App"),
          tags$li(tags$b("Preprint:"), " (to be added: EdArXiv link)"),
          tags$li(tags$b("OSF project:"), " (to be added: OSF link)"),
          tags$li(tags$b("Codebook:"), " available via the repository / configuration files")
        ),
        tags$hr(),
        tags$h4("Suggested citation (placeholder)"),
        tags$pre("León, S.P. (2026). RMQS Shiny App (Version 0.1). [Software]. Preprint/OSF link.")
      )
    } else {
      tagList(
        tags$h3("Acerca de / Citar"),
        tags$p("RMQS Shiny App (demo). Esta app permite codificar y resumir una escala revisada de calidad metodológica."),
        tags$ul(
          tags$li(tags$b("Nombre:"), " Revised Methodological Quality Scale (RMQS) Shiny App"),
          tags$li(tags$b("Preprint:"), " (pendiente: enlace EdArXiv)"),
          tags$li(tags$b("Proyecto OSF:"), " (pendiente: enlace OSF)"),
          tags$li(tags$b("Codebook:"), " disponible en el repositorio / ficheros de configuración")
        ),
        tags$hr(),
        tags$h4("Cita sugerida (placeholder)"),
        tags$pre("León, S.P. (2026). RMQS Shiny App (Version 0.1). [Software]. Enlace preprint/OSF.")
      )
    }
  })
  
  # ---------------------------
  # MAIN UI (always available skeleton)
  # ---------------------------
  output$main_ui <- renderUI({
    
    needs_data_box <- function() {
      steps <- t_txt(input$lang %||% "es", "ready_steps")
      tagList(
        tags$div(
          tags$h3(t_txt(input$lang %||% "es", "ready_title")),
          tags$p(steps[1]),
          tags$p(steps[2]),
          tags$p(steps[3]),
          tags$hr(),
          tags$p(tags$em(t_txt(input$lang %||% "es", "ready_hint"))),
          tags$br(),
          tags$p(tags$em(
            if (identical(input$lang %||% "es", "en"))
              "See the “About / Cite” tab for citation details."
            else
              "Consulta la pestaña “Acerca de / Citar” para la cita y detalles."
          ))
        )
      )
    }
    
    
    tabsetPanel(
      id = "main_tabs",
      tabPanel(t_txt(input$lang %||% "es", "tab_cfg"),
               tags$p(t_txt(input$lang %||% "es", "cfg_intro")),
               fileInput("cfg_items_csv", t_txt(input$lang %||% "es", "cfg_items"), accept = c(".csv")),
               fileInput("cfg_manual_file", t_txt(input$lang %||% "es", "cfg_manual"), accept = c(".xlsx", ".csv")),
               hr(),
               strong(t_txt(input$lang %||% "es", "cfg_status")),
               uiOutput("cfg_status_ui"),
               hr(),
               uiOutput("cfg_downloads_ui")
      ),
      
      tabPanel(t_txt(input$lang %||% "es", "tab_preview"),
               if (data_ok()) DTOutput("preview") else needs_data_box()
      ),
      
      tabPanel(t_txt(input$lang %||% "es", "tab_freq"),
               if (!data_ok()) needs_data_box() else tagList(
                 uiOutput("freq_mode_note"),
                 DTOutput("freq_dt"),
                 hr(),
                 uiOutput("down_freq_any_ui"),
                 hr(),
                 conditionalPanel(
                   condition = "input.view_mode == 'items'",
                   downloadButton("down_freq_wide_paper", t_txt(input$lang %||% "es", "download_items_paper"))
                 ),
                 conditionalPanel(
                   condition = "input.view_mode == 'domains'",
                   downloadButton("down_domain_table_paper", t_txt(input$lang %||% "es", "download_domains_paper"))
                 ),
                 conditionalPanel(
                   condition = "input.view_mode == 'items'",
                   hr(),
                   selectInput("freq_item_help", t_txt(input$lang %||% "es", "item_help"), choices = ITEMS, selected = ITEMS[1]),
                   uiOutput("freq_item_rules")
                 )
               )
      ),
      
      tabPanel(t_txt(input$lang %||% "es", "tab_summary"),
               if (!data_ok()) needs_data_box() else tagList(
                 plotOutput("stacked_plot", height = 520),
                 downloadButton("down_summary_png", t_txt(input$lang %||% "es", "download_summary_png")),
                 downloadButton("down_summary_pdf", t_txt(input$lang %||% "es", "download_summary_pdf"))
               )
      ),
      
      tabPanel(t_txt(input$lang %||% "es", "tab_table"),
               if (!data_ok()) needs_data_box() else tagList(
                 plotOutput("g1_plot", height = 760),
                 hr(),
                 selectInput("table_item_help", t_txt(input$lang %||% "es", "item_help"), choices = ITEMS, selected = ITEMS[1]),
                 uiOutput("table_item_rules"),
                 hr(),
                 downloadButton("down_table_png", t_txt(input$lang %||% "es", "download_table_png")),
                 downloadButton("down_table_pdf", t_txt(input$lang %||% "es", "download_table_pdf"))
               )
      ),
      
      tabPanel(t_txt(input$lang %||% "es", "tab_key"),
               DTOutput("item_key_dt")
      ),
      
      tabPanel(t_txt(input$lang %||% "es", "tab_manual"),
               if (!data_ok()) needs_data_box() else tagList(
                 if (is.null(manual_dict())) tags$div(tags$em(t_txt(input$lang %||% "es", "manual_disabled"))) else NULL,
                 fluidRow(
                   column(
                     4,
                     selectInput("manual_domain", t_txt(input$lang %||% "es", "manual_filter"), choices = c("All"), selected = "All"),
                     uiOutput("manual_item_ui")
                   ),
                   column(8, DTOutput("manual_dt"))
                 ),
                 hr(),
                 h4(t_txt(input$lang %||% "es", "decision_rules")),
                 uiOutput("manual_rules_ui")
               )
      ),
      
      tabPanel(t_txt(input$lang %||% "es", "tab_export"),
               if (!data_ok()) needs_data_box() else tagList(
                 downloadButton("down_clean", t_txt(input$lang %||% "es", "download_clean")),
                 tags$div(style = "height:10px;"),
                 downloadButton("down_audit", t_txt(input$lang %||% "es", "download_audit"))
               )
      ),
      tabPanel(t_txt(input$lang %||% "es", "tab_about"),
               uiOutput("about_ui")
      )
    )
  })
  
}

shinyApp(ui, server)
