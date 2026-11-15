# Uncomment the lines below if running locally for the first time:
required_pkgs <- c("shiny", "ggplot2")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(shiny)
library(ggplot2)

# ==============================================================================
# USER INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  # Custom WCAG-compliant CSS for clean typography and high contrast
  tags$head(
    tags$style(HTML("
      body { color: #111111; background-color: #FFFFFF; font-family: system-ui, -apple-system, sans-serif; }
      .well { background-color: #F4F6F8; border: 1px solid #CCCCCC; }
      .stat-box { background-color: #F8F9FA; border-left: 5px solid #005A9C; padding: 12px 15px; margin-bottom: 15px; border-radius: 4px; }
      .stat-box-warn { border-left-color: #9E2A6B; }
      h2 { font-weight: 700; color: #111111; }
      h4 { font-weight: 600; color: #111111; margin-top: 0; }
    "))
  ),
  
  titlePanel("Mann-Whitney U Explorer: Distribution Shapes, Spread & Medians"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Quick Teaching Presets
      h4("1. Quick Teaching Scenarios"),
      selectInput(
        "preset", "Load a Preset Condition:",
        choices = c(
          "Scenario 1: Equal Variances & Pure Shift" = "eq_shift",
          "Scenario 2: Unequal Variances & Same Median" = "uneq_same",
          "Scenario 3: Unequal Variances & Shifted Median" = "uneq_shift",
          "Null Case: Identical Distributions" = "null"
        ),
        selected = "eq_shift"
      ),
      
      hr(),
      
      # Sample Size & Seed Controls
      h4("2. Sample Settings"),
      sliderInput("n", "Sample Size per Group (n):", min = 20, max = 500, value = 100, step = 10),
      actionButton("resim", "Draw New Random Sample", class = "btn-primary", style = "background-color: #005A9C; border-color: #004070; width: 100%; margin-bottom: 15px;"),
      
      hr(),
      
      # Group A Controls
      h4("3. Group A (Deep Blue / Solid)"),
      selectInput("dist_a", "Distribution Shape:", choices = c("Normal (Symmetric)" = "norm", "Log-Normal (Right-Skewed)" = "lnorm"), selected = "norm"),
      sliderInput("med_a", "True Population Median:", min = 5, max = 20, value = 10, step = 0.5),
      sliderInput("spread_a", "Spread (SD for Normal / sdlog for Log-Normal):", min = 0.2, max = 4.0, value = 2.0, step = 0.1),
      
      hr(),
      
      # Group B Controls
      h4("4. Group B (Deep Berry / Dashed)"),
      selectInput("dist_b", "Distribution Shape:", choices = c("Normal (Symmetric)" = "norm", "Log-Normal (Right-Skewed)" = "lnorm"), selected = "norm"),
      sliderInput("med_b", "True Population Median:", min = 5, max = 20, value = 11.5, step = 0.5),
      sliderInput("spread_b", "Spread (SD for Normal / sdlog for Log-Normal):", min = 0.2, max = 4.0, value = 2.0, step = 0.1)
    ),
    
    mainPanel(
      width = 8,
      
      # Density Plot Output
      plotOutput("densityPlot", height = "430px"),
      
      br(),
      
      # Live Diagnostic & Hypothesis Test Results
      fluidRow(
        column(
          width = 6,
          div(
            class = "stat-box",
            h4("Step 1: Equal Variance Check"),
            p(strong("Fligner-Killeen Test"), "(Tests if spreads are equal):"),
            verbatimTextOutput("flignerOut"),
            uiOutput("varInterpret")
          )
        ),
        column(
          width = 6,
          div(
            class = "stat-box stat-box-warn",
            h4("Step 2: Mann-Whitney U Test"),
            p(strong("Wilcoxon Rank-Sum Test"), "(Tests stochastic dominance):"),
            verbatimTextOutput("mwOut"),
            uiOutput("mwInterpret")
          )
        )
      ),
      
      # Sample Summary Table
      div(
        class = "stat-box",
        style = "border-left-color: #222222;",
        h4("Sample Summary Statistics & Probability of Superiority"),
        tableOutput("summaryTable")
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # Update sliders when a student picks a preset scenario
  observeEvent(input$preset, {
    if (input$preset == "eq_shift") {
      updateSelectInput(session, "dist_a", selected = "norm")
      updateSliderInput(session, "med_a", value = 10)
      updateSliderInput(session, "spread_a", value = 2.0)
      
      updateSelectInput(session, "dist_b", selected = "norm")
      updateSliderInput(session, "med_b", value = 11.5)
      updateSliderInput(session, "spread_b", value = 2.0)
      
    } else if (input$preset == "uneq_same") {
      updateSelectInput(session, "dist_a", selected = "norm")
      updateSliderInput(session, "med_a", value = 10)
      updateSliderInput(session, "spread_a", value = 1.0)
      
      updateSelectInput(session, "dist_b", selected = "lnorm")
      updateSliderInput(session, "med_b", value = 10)
      updateSliderInput(session, "spread_b", value = 0.8)
      
    } else if (input$preset == "uneq_shift") {
      updateSelectInput(session, "dist_a", selected = "norm")
      updateSliderInput(session, "med_a", value = 10)
      updateSliderInput(session, "spread_a", value = 1.0)
      
      updateSelectInput(session, "dist_b", selected = "lnorm")
      updateSliderInput(session, "med_b", value = 13)
      updateSliderInput(session, "spread_b", value = 0.8)
      
    } else if (input$preset == "null") {
      updateSelectInput(session, "dist_a", selected = "norm")
      updateSliderInput(session, "med_a", value = 10)
      updateSliderInput(session, "spread_a", value = 2.0)
      
      updateSelectInput(session, "dist_b", selected = "norm")
      updateSliderInput(session, "med_b", value = 10)
      updateSliderInput(session, "spread_b", value = 2.0)
    }
  })
  
  # Reactive data generator
  sim_data <- reactive({
    input$resim
    n <- input$n
    
    # Generate Group A (for log-normal, meanlog = log(median))
    vals_a <- if (input$dist_a == "norm") {
      rnorm(n, mean = input$med_a, sd = input$spread_a)
    } else {
      rlnorm(n, meanlog = log(input$med_a), sdlog = input$spread_a)
    }
    
    # Generate Group B
    vals_b <- if (input$dist_b == "norm") {
      rnorm(n, mean = input$med_b, sd = input$spread_b)
    } else {
      rlnorm(n, meanlog = log(input$med_b), sdlog = input$spread_b)
    }
    
    data.frame(
      value = c(vals_a, vals_b),
      group = factor(rep(c("Group A", "Group B"), each = n))
    )
  })
  
  # Render WCAG-Accessible Density Plot
  output$densityPlot <- renderPlot({
    df <- sim_data()
    medians_df <- aggregate(value ~ group, data = df, FUN = median)
    
    ggplot(df, aes(x = value, fill = group, color = group, linetype = group)) +
      geom_density(alpha = 0.45, linewidth = 1.2) +
      geom_vline(
        data = medians_df,
        aes(xintercept = value, color = group, linetype = group),
        linewidth = 1.2,
        show.legend = FALSE
      ) +
      scale_fill_manual(values = c("Group A" = "#005A9C", "Group B" = "#9E2A6B")) +
      scale_color_manual(values = c("Group A" = "#004070", "Group B" = "#6E1B49")) +
      scale_linetype_manual(values = c("Group A" = "solid", "Group B" = "dashed")) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.width = unit(1.8, "cm"),
        text = element_text(color = "#111111"),
        axis.text = element_text(color = "#222222"),
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      ) +
      labs(
        title = "Simulated Group Distributions & Sample Medians",
        subtitle = "Vertical lines mark sample medians (Solid = Group A, Dashed = Group B)",
        x = "Observed Value",
        y = "Density"
      )
  })
  
  # Fligner-Killeen Test Output
  output$flignerOut <- renderText({
    df <- sim_data()
    ft <- fligner.test(value ~ group, data = df)
    sprintf("Chi-squared = %.2f, p-value = %s", ft$statistic, format.pval(ft$p.value, digits = 3, eps = 0.001))
  })
  
  output$varInterpret <- renderUI({
    df <- sim_data()
    ft <- fligner.test(value ~ group, data = df)
    if (ft$p.value < 0.05) {
      tags$p(style = "color: #6E1B49; font-weight: bold;",
             "Result: Spreads/shapes differ significantly (p < 0.05). The equal-variance condition is violated.")
    } else {
      tags$p(style = "color: #004070; font-weight: bold;",
             "Result: No significant difference in spread (p >= 0.05). Equal-variance assumption holds.")
    }
  })
  
  # Mann-Whitney U Test Output
  output$mwOut <- renderText({
    df <- sim_data()
    wt <- wilcox.test(value ~ group, data = df, conf.int = TRUE)
    sprintf("W = %.0f, p-value = %s\nHodges-Lehmann Est. = %.2f", wt$statistic, format.pval(wt$p.value, digits = 3, eps = 0.001), wt$estimate)
  })
  
  output$mwInterpret <- renderUI({
    df <- sim_data()
    ft <- fligner.test(value ~ group, data = df)
    wt <- wilcox.test(value ~ group, data = df)
    
    if (wt$p.value >= 0.05) {
      tags$p(style = "color: #222222; font-weight: bold;",
             "Result: Not statistically significant (p >= 0.05). No evidence of a shift or stochastic dominance.")
    } else if (ft$p.value < 0.05) {
      tags$p(style = "color: #6E1B49; font-weight: bold;",
             "Caution (p < 0.05): Significant due to stochastic dominance/shape differences—do NOT interpret strictly as a pure median test!")
    } else {
      tags$p(style = "color: #004070; font-weight: bold;",
             "Valid Median Shift (p < 0.05): Equal spread holds, so you can validly conclude the population medians differ.")
    }
  })
  
  # Summary Table Output
  output$summaryTable <- renderTable({
    df <- sim_data()
    vals_a <- df$value[df$group == "Group A"]
    vals_b <- df$value[df$group == "Group B"]
    
    # Calculate empirical P(Group B > Group A)
    wt <- wilcox.test(vals_b, vals_a)
    prob_sup <- as.numeric(wt$statistic) / (length(vals_a) * length(vals_b))
    
    data.frame(
      Group = c("Group A (Deep Blue)", "Group B (Deep Berry)"),
      Sample_Median = c(median(vals_a), median(vals_b)),
      IQR = c(IQR(vals_a), IQR(vals_b)),
      Sample_Mean = c(mean(vals_a), mean(vals_b)),
      Sample_SD = c(sd(vals_a), sd(vals_b)),
      P_GroupB_Exceeds_A = c("-", sprintf("%.1f%%", prob_sup * 100)),
      check.names = FALSE
    )
  }, digits = 2)
}

# Run the application
shinyApp(ui = ui, server = server)