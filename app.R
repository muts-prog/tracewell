# Tracewell live demo — exported to static files with Shinylive.
# Preview locally: shiny::runApp("demo-app")

library(shiny)
library(bslib)

# ---- Synthetic programme dataset (fictional NGO) -------------------------
make_demo_data <- function(seed = 42) {
  set.seed(seed)
  n <- 240
  df <- data.frame(
    record_id   = sprintf("HH-%04d", 1:n),
    project     = sample(c("Maji Safi WASH", "Shamba Bora Agri", "Afya Mama Health"), n, TRUE),
    district    = sample(c("Kitui", "Makueni", "Machakos", "Kajiado"), n, TRUE),
    visit_date  = as.Date("2026-07-01") + sample(0:80, n, TRUE),
    respondent_age = round(rnorm(n, 38, 11)),
    household_size = rpois(n, 5) + 1,
    water_access   = sample(c("Yes", "No"), n, TRUE, prob = c(.62, .38)),
    income_kes     = round(rlnorm(n, 9.3, .5), -1),
    stringsAsFactors = FALSE
  )
  # Inject realistic problems
  df$record_id[c(15, 88, 131)]    <- df$record_id[c(14, 87, 130)]        # duplicate IDs
  df$respondent_age[c(7, 52, 199)] <- c(4, 132, -1)                      # out of range
  df$household_size[sample(n, 9)]  <- NA                                  # missing
  df$water_access[sample(n, 12)]   <- NA
  df$district[c(20, 61, 143, 210)] <- c("kitui ", "MAKUENI", "Machakos.", "Kajado")
  df$visit_date[c(33, 177)]        <- as.Date(c("2027-02-11", "2026-12-30"))  # future
  df$income_kes[c(90, 160)]        <- c(9999999, 0)
  df
}

canonical_districts <- c("Kitui", "Makueni", "Machakos", "Kajiado")
fix_district <- function(x) {
  y <- tools::toTitleCase(tolower(trimws(gsub("[.]", "", x))))
  y[y == "Kajado"] <- "Kajiado"
  y
}

run_checks <- function(df, as_of = as.Date("2026-10-09")) {
  checks <- list(
    list("Duplicate record IDs", "Same household ID entered twice",
         which(duplicated(df$record_id) | duplicated(df$record_id, fromLast = TRUE))),
    list("Age out of range", "Respondent age outside 15–100",
         which(!is.na(df$respondent_age) & (df$respondent_age < 15 | df$respondent_age > 100))),
    list("Missing household size", "Required field left blank",
         which(is.na(df$household_size))),
    list("Missing water access", "Key indicator left blank",
         which(is.na(df$water_access))),
    list("Non-standard district", "Spelling or case differs from list",
         which(!df$district %in% canonical_districts)),
    list("Visit date in the future", paste("Date after", format(as_of, "%d %b %Y")),
         which(df$visit_date > as_of)),
    list("Implausible income", "Zero or above KES 1,000,000",
         which(df$income_kes <= 0 | df$income_kes > 1e6))
  )
  data.frame(
    Check = sapply(checks, `[[`, 1),
    Rule = sapply(checks, `[[`, 2),
    Records = sapply(checks, function(x) length(x[[3]])),
    `Example rows` = sapply(checks, function(x)
      if (length(x[[3]])) paste(head(x[[3]], 4), collapse = ", ") else "—"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

clean_data <- function(df, as_of = as.Date("2026-10-09")) {
  log <- c()
  n0 <- nrow(df)
  df$district <- fix_district(df$district)
  log <- c(log, "Standardised district names to the programme list.")
  df <- df[!duplicated(df$record_id), ]
  log <- c(log, sprintf("Removed %d duplicate record(s), keeping the first entry.", n0 - nrow(df)))
  bad_age <- df$respondent_age < 15 | df$respondent_age > 100
  df$respondent_age[bad_age] <- NA
  log <- c(log, sprintf("Set %d out-of-range age(s) to missing and flagged for field follow-up.", sum(bad_age, na.rm = TRUE)))
  fut <- df$visit_date > as_of
  df <- df[!fut, ]
  log <- c(log, sprintf("Excluded %d record(s) with future visit dates pending verification.", sum(fut)))
  bad_inc <- df$income_kes <= 0 | df$income_kes > 1e6
  df$income_kes[bad_inc] <- NA
  log <- c(log, sprintf("Set %d implausible income value(s) to missing.", sum(bad_inc, na.rm = TRUE)))
  list(data = df, log = log)
}

profile_upload <- function(df) {
  data.frame(
    Column = names(df),
    Type = sapply(df, function(x) class(x)[1]),
    `Missing (%)` = round(100 * sapply(df, function(x) mean(is.na(x) | (is.character(x) & trimws(x) == ""))), 1),
    `Distinct values` = sapply(df, function(x) length(unique(x))),
    check.names = FALSE, row.names = NULL
  )
}


# ---- Theme & styles ------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#FBFAF7", fg = "#1E2A2F",
  primary = "#0F6E56", secondary = "#5F5E5A",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Fraunces", local = FALSE),
  "border-radius" = "10px"
)

css <- "
.tw-nav { border-bottom:1px solid #E6E3DA; background:#FBFAF7; }
.tw-nav .inner { display:flex; align-items:center; justify-content:space-between; padding:14px 0; }
.tw-brand { font-family:Fraunces,serif; font-size:1.3rem; font-weight:600; color:#1E2A2F; text-decoration:none; display:flex; align-items:center; gap:10px; }
.eyebrow { text-transform:uppercase; letter-spacing:.12em; font-size:.76rem; color:#0F6E56; font-weight:600; margin:36px 0 10px; }
.box { background:#fff; border:1px solid #E6E3DA; border-radius:14px; padding:24px; margin-bottom:40px; }
.kpi { background:#F1EFE8; border-radius:10px; padding:14px 16px; }
.kpi .v { font-family:Fraunces,serif; font-size:1.7rem; }
.kpi .l { font-size:.82rem; color:#5d686c; }
.tile { background:#FBFAF7; border:1px solid #E6E3DA; border-radius:12px; padding:20px; }
.muted { color:#5d686c; }
table.table { font-size:.88rem; }
.nav-pills .nav-link { margin-right:6px; }
.table-wrap { overflow-x:auto; }
"

logo <- HTML('<svg width="26" height="26" viewBox="0 0 32 32" aria-hidden="true"><rect width="32" height="32" rx="7" fill="#0F6E56"/><path d="M8 21l5-6 4 3 7-9" stroke="#fff" stroke-width="2.6" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg>')

ui <- fluidPage(
  theme = theme,
  title = "Tracewell — live data-quality demo",
  tags$head(tags$style(HTML(css))),
  div(class = "tw-nav", div(class = "container inner",
    a(class = "tw-brand", href = "../", logo, "Tracewell"),
    div(a(href = "../", class = "btn btn-outline-secondary btn-sm me-2", "Back to site"),
        a(href = "../#contact", class = "btn btn-primary btn-sm", "Talk to us")))),
  div(class = "container",
    div(class = "eyebrow", "Live demo · runs entirely in your browser"),
    h2("See what happens after collection."),
    p(class = "muted mb-4", style = "max-width:66ch",
      "This is a fictional NGO dataset with three projects and the kinds of problems we see every reporting cycle. Run the checks, clean it, and see the traceable summary. You can also profile your own CSV, which stays on your computer."),
    div(class = "box",
      tabsetPanel(type = "pills",
        tabPanel("1 · Raw data",
          div(class = "row g-3 my-3",
            div(class = "col-6 col-md-3", div(class = "kpi", div(class = "v", textOutput("k_rows", inline = TRUE)), div(class = "l", "Records"))),
            div(class = "col-6 col-md-3", div(class = "kpi", div(class = "v", "3"), div(class = "l", "Projects"))),
            div(class = "col-6 col-md-3", div(class = "kpi", div(class = "v", "4"), div(class = "l", "Districts"))),
            div(class = "col-6 col-md-3", div(class = "kpi", div(class = "v", textOutput("k_issues", inline = TRUE)), div(class = "l", "Issues found")))),
          div(class = "table-wrap", tableOutput("raw_preview"))),
        tabPanel("2 · Quality checks",
          div(class = "my-3 table-wrap", tableOutput("checks"))),
        tabPanel("3 · Clean & summarise",
          div(class = "my-3",
            actionButton("clean", "Clean the dataset", class = "btn-primary"),
            uiOutput("clean_out"))),
        tabPanel("Profile your own CSV",
          div(class = "my-3",
            p(class = "muted small", "Your file is read inside your browser and never uploaded to any server. Please still use de-identified data."),
            fileInput("upload", NULL, accept = ".csv", buttonLabel = "Choose CSV", placeholder = "No file selected"),
            uiOutput("upload_out")))
      )),
    div(class = "box", style = "text-align:center",
      h4("Want this running on your programme data?"),
      p(class = "muted", "Start with a reporting audit: a review of your data sources and a plan for what to automate."),
      a(href = "../#contact", class = "btn btn-primary", "Book a reporting audit"))
  )
)

server <- function(input, output, session) {
  raw <- make_demo_data()
  checks <- run_checks(raw)

  output$k_rows <- renderText(nrow(raw))
  output$k_issues <- renderText(sum(checks$Records))
  output$raw_preview <- renderTable({
    r <- raw[c(14, 15, 7, 20, 33, 61, 90, 1:3), ]
    r$visit_date <- format(r$visit_date)
    r
  }, striped = TRUE, hover = TRUE, na = "— missing —")

  output$checks <- renderTable(checks, striped = TRUE, hover = TRUE, digits = 0)

  cleaned <- reactive({ req(input$clean); clean_data(raw) })

  output$clean_out <- renderUI({
    res <- cleaned()
    tagList(
      h4(class = "mt-4", "What changed"),
      tags$ol(class = "muted", lapply(res$log, tags$li)),
      div(class = "row g-4 mt-1",
        div(class = "col-lg-6", h5("Households with water access, by project"), plotOutput("plot_access", height = "260px")),
        div(class = "col-lg-6", h5("Draft summary (for human review)"), div(class = "tile", uiOutput("draft")))))
  })

  output$plot_access <- renderPlot({
    d <- cleaned()$data
    d <- d[!is.na(d$water_access), ]
    pct <- sort(tapply(d$water_access == "Yes", d$project, mean) * 100)
    par(mar = c(4, 11, 1, 3), bg = "#FFFFFF")
    b <- barplot(pct, horiz = TRUE, las = 1, col = "#1D9E75", border = NA, xlim = c(0, 100),
                 xlab = "% of households", cex.names = .9)
    text(pct + 1, b, sprintf("%.0f%%", pct), adj = 0, cex = .9)
  })

  output$draft <- renderUI({
    d <- cleaned()$data
    w <- d[!is.na(d$water_access), ]
    by_p <- tapply(w$water_access == "Yes", w$project, mean) * 100
    tagList(
      p(sprintf("Across %d verified household records (%d analysed for water access), %.0f%% of households reported access to an improved water source.",
                nrow(d), nrow(w), mean(w$water_access == "Yes") * 100)),
      p(sprintf("Access was highest in %s (%.0f%%). Median household income was KES %s.",
                names(which.max(by_p)), max(by_p), format(median(d$income_kes, na.rm = TRUE), big.mark = ","))),
      p(class = "small muted mb-0", "Traceability: figures are computed from the cleaned dataset after the rules listed on the left. Excluded and corrected rows are kept in an audit log."))
  })

  output$upload_out <- renderUI({
    req(input$upload)
    df <- tryCatch(read.csv(input$upload$datapath, stringsAsFactors = FALSE, check.names = FALSE),
                   error = function(e) NULL)
    if (is.null(df)) return(div(class = "text-danger", "Couldn't read that file. Please upload a comma-separated CSV."))
    tagList(
      p(sprintf("%s rows · %s columns · %s fully duplicated rows",
                format(nrow(df), big.mark = ","), ncol(df), sum(duplicated(df)))),
      div(class = "table-wrap", renderTable(profile_upload(df), striped = TRUE, hover = TRUE)))
  })
}

shinyApp(ui, server)
