## rasblocking explorer -- a browser front end to the diagnostics, feasibility
## checks, cost model and verification gates.
##
## Launch with rasblocking::run_app(), or open this file in RStudio and Run App.

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(rasblocking)

options(shiny.maxRequestSize = 512 * 1024^2)   #link.csv is large

NAVY <- "#17153B"; SAGE <- "#6A9C89"; LAV <- "#C8ACD6"; PINK <- "#D96098"

ras_theme <- theme(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  plot.title = element_text(hjust = 0.5, size = 15),
  axis.text = element_text(size = 11), text = element_text(size = 13)
)

ui <- page_sidebar(
  title = "rasblocking explorer",
  theme = bs_theme(version = 5, primary = NAVY),

  sidebar = sidebar(
    width = 340,
    helpText("Point at a directory holding node.csv, link.csv and demand.csv",
             "in GMNS format. Any network in that format works, not only the",
             "RAS 2026 instances."),
    textInput("dir", "Instance directory", value = ""),
    textInput("od", "OD distance matrix (.csv)", value = ""),
    selectInput("mult", "Demand multiplier",
                choices = c("0.5" = 0.5, "1.0" = 1, "2.0" = 2), selected = 1),
    hr(),
    fileInput("sol", "Solution JSON (optional)", accept = ".json"),
    helpText("With a solution loaded you also get feasibility, cost and the",
             "two verification gates."),
    hr(),
    actionButton("go", "Analyse", class = "btn-primary w-100")
  ),

  navset_card_tab(
    nav_panel("Instance",
      layout_columns(
        value_box("Demands",   textOutput("n_dem"),   theme = "primary"),
        value_box("Yards",     textOutput("n_yard"),  theme = "secondary"),
        value_box("Links",     textOutput("n_link"),  theme = "secondary")
      ),
      card(card_header("Structural diagnosis"), verbatimTextOutput("diag")),
      card(card_header("Sub-tier share across multipliers"),
           plotOutput("subtier", height = "330px"))
    ),

    nav_panel("Feasibility",
      card(card_header("Official checks"), DTOutput("checks"))),

    nav_panel("Cost and score",
      layout_columns(
        value_box("Operating cost", textOutput("cost"),   theme = "primary"),
        value_box("Stress score",   textOutput("stress"), theme = "primary"),
        value_box("Served",         textOutput("served"), theme = "secondary")
      ),
      card(card_header("Cost decomposition"), plotOutput("costplot", height = "330px"))),

    nav_panel("Verification",
      card(card_header("Structural gate"),  verbatimTextOutput("gate1")),
      card(card_header("Arithmetic gate"),  verbatimTextOutput("gate2")),
      card(card_header("Why two gates"),
           p("Neither gate suffices alone. The arithmetic gate accepts a cost",
             "absent from both the payload and the recomputation, because both",
             "read the same missing record and agree. The structural gate",
             "accepts a design that is well formed but mispriced. Agreement",
             "between two implementations is evidence of consistency, not of",
             "correctness, when both draw on the same source."))),

    nav_panel("Opportunity",
      card(card_header("Donor-funded destination-pooling candidates"),
           p("Minimum volume is evaluated on aggregate block volume, so a",
             "stranded demand can reach a destination by riding a block that",
             "another demand's volume already justifies. These are screening",
             "estimates: they rank candidates, they do not certify them."),
           DTOutput("pool")))
  )
)

server <- function(input, output, session) {

  state <- eventReactive(input$go, {
    validate(need(nzchar(input$dir), "Give an instance directory."),
             need(dir.exists(input$dir), "That directory does not exist."),
             need(nzchar(input$od), "Give the OD distance matrix path."),
             need(file.exists(input$od), "That OD matrix file does not exist."))
    withProgress(message = "Reading instance", value = 0.2, {
      inst <- ras_read_instance(input$dir)
      incProgress(0.4, message = "Reading OD matrix")
      od <- ras_read_od(input$od)
      incProgress(0.3, message = "Diagnosing")
      list(inst = inst, od = od, mult = as.numeric(input$mult))
    })
  })

  solution <- reactive({
    req(input$sol)
    ras_read_solution(input$sol$datapath)
  })

  output$n_dem  <- renderText(format(nrow(state()$inst$demands), big.mark = ","))
  output$n_yard <- renderText(format(sum(state()$inst$nodes$node_type == "yard",
                                         na.rm = TRUE), big.mark = ","))
  output$n_link <- renderText(format(nrow(state()$inst$links), big.mark = ","))

  output$diag <- renderPrint({
    print(ras_diagnose(state()$inst, state()$od, state()$mult))
  })

  output$subtier <- renderPlot({
    s <- state()
    d <- do.call(rbind, lapply(c(0.5, 1, 2), function(m) {
      g <- ras_diagnose(s$inst, s$od, m)
      data.frame(mult = sprintf("%.1fx", m), pct = g$sub_tier_pct,
                 locked = g$locked_n)
    }))
    ggplot(d, aes(x = mult, y = pct)) +
      geom_col(fill = NAVY, width = 0.6) +
      geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4.5) +
      scale_y_continuous(limits = c(0, 100),
                         expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Demand multiplier", y = "Sub-tier demands (%)",
           title = "Demands that cannot open a block alone") +
      ras_theme
  })

  output$checks <- renderDT({
    s <- state()
    v <- ras_validate(solution(), s$inst, s$od, s$mult)
    df <- do.call(rbind, lapply(names(v$checks), function(n) {
      z <- v$checks[[n]]
      data.frame(check = n,
                 result = ifelse(z$pass, "pass", "FAIL"),
                 violations = z$n_violations)
    }))
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("result", color = styleEqual(c("pass", "FAIL"),
                                               c(SAGE, "#B8001F")),
                  fontWeight = "bold")
  })

  costs <- reactive({
    s <- state()
    ras_stress(solution(), s$inst, s$od, s$mult)
  })

  output$cost   <- renderText(sprintf("$%s", format(round(costs()$cost$total),
                                                    big.mark = ",")))
  output$stress <- renderText(sprintf("$%s", format(round(costs()$stress),
                                                    big.mark = ",")))
  output$served <- renderText(sprintf("%.2f%%", 100 * costs()$served_ratio))

  output$costplot <- renderPlot({
    c <- costs()$cost
    d <- data.frame(
      component = factor(c("Transport", "Handling", "Interchange", "Block fixed"),
                         levels = c("Transport", "Interchange", "Handling",
                                    "Block fixed")),
      usd = c(c$transport, c$handling, c$interchange, c$fixed))
    ggplot(d, aes(x = component, y = usd / 1e6, fill = component)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = c("Transport" = NAVY, "Interchange" = PINK,
                                   "Handling" = SAGE, "Block fixed" = LAV)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL, y = "Cost ($M)", title = "Operating cost by component") +
      ras_theme
  })

  output$gate1 <- renderPrint(print(ras_gate_structure(solution(), state()$inst)))
  output$gate2 <- renderPrint(print(ras_gate_arithmetic(solution(), state()$inst)))

  output$pool <- renderDT({
    s <- state()
    cand <- ras_pool_candidates(s$inst, solution(), s$od, s$mult,
                                max_candidates = 100L)
    validate(need(nrow(cand) > 0, "No pooling candidates found."))
    cand$net_gain <- round(cand$net_gain)
    cand$detour_miles <- round(cand$detour_miles, 2)
    datatable(cand[, c("donor_block", "X", "Y", "D", "donor_volume",
                       "pooled_cars", "detour_miles", "net_gain")],
              rownames = FALSE,
              options = list(pageLength = 15, order = list(list(7, "desc"))))
  })
}

shinyApp(ui, server)
