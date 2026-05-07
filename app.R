library(shiny)
library(shinydashboard)
library(tidyquant)
library(tidyverse)
library(plotly)
library(scales)

# 1. Configuration & Data Setup
# Symbols: 10Y-2Y Spread, 10Y-3M Spread, 10Y Breakeven Inflation
symbols <- c("T10Y2Y", "T10Y3M", "T10YIE")

ui <- dashboardPage(
    dashboardHeader(title = "Bond_Rotation Tool"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            hr(),
            numericInput("portfolio_val", "Total Portfolio Value ($)", 100000, min = 0),
            sliderInput("sat_pct", "Tactical Satellite %", min = 0, max = 100, value = 40),
            sliderInput("target_yield_change", "Projected Rate Change (%)", -2, 2, 0, step = 0.1),
            helpText("Negative % = Falling Rates (Bullish for AGG)")
        )
    ),
    
    dashboardBody(
        fluidRow(
            valueBoxOutput("signalBox", width = 4),
            valueBoxOutput("coreBox", width = 4),
            valueBoxOutput("satBox", width = 4)
        ),
        
        fluidRow(
            box(title = "Yield Curve & Inflation Trends", width = 8, status = "primary",
                plotlyOutput("trendPlot")),
            box(title = "Rotation Logic & Reasoning", width = 4, status = "warning",
                uiOutput("logicText"))
        ),
        
        fluidRow(
            box(title = "Proposed Allocation Table", width = 12,
                tableOutput("allocationTable"))
        )
    )
)

server <- function(input, output, session) {
    
    # 2. Reactive Data Retrieval
    market_data <- reactive({
        # Pulling last 2 years for context
        tq_get(symbols, get = "economic.data", from = Sys.Date() - 730) %>%
            pivot_wider(names_from = symbol, values_from = price) %>%
            drop_na()
    })
    
    # 3. Rotation Logic Engine
    analysis <- reactive({
        df <- market_data()
        latest <- tail(df, 1)
        
        # Core Constants (As of May 2026)
        agg_dur <- 5.8
        agg_yield <- 0.044
        
        # Logic Checks
        is_inverted <- latest$T10Y2Y < 0 | latest$T10Y3M < 0
        inflation_spike <- latest$T10YIE > 2.5
        
        if (is_inverted) {
            rec <- "SPAXX / SGOV"
            reason <- "Yield curve inversion detected. Cash yields outperform duration with zero price risk."
            color <- "red"
        } else if (inflation_spike) {
            rec <- "FIPDX"
            reason <- "Breakeven inflation exceeds 2.5%. Shifting to TIPS to protect purchasing power."
            color <- "yellow"
        } else {
            rec <- "AGG"
            reason <- "Normal curve environment. Capturing yield and potential gains from rate stability."
            color <- "green"
        }
        
        # Total Return Estimate
        rate_move <- input$target_yield_change / 100
        price_impact <- (agg_dur * rate_move) * -1
        est_return <- agg_yield + price_impact
        
        list(rec = rec, reason = reason, color = color, latest = latest, est_ret = est_return)
    })
    
    # 4. Outputs
    output$signalBox <- renderValueBox({
        valueBox("Signal", analysis()$rec, icon = icon("signal"), color = analysis()$color)
    })
    
    output$coreBox <- renderValueBox({
        val <- input$portfolio_val * (1 - (input$sat_pct / 100))
        valueBox("Core (Static)", dollar(val), icon = icon("shield"), color = "blue")
    })
    
    output$satBox <- renderValueBox({
        val <- input$portfolio_val * (input$sat_pct / 100)
        valueBox("Satellite (Active)", dollar(val), icon = icon("rocket"), color = "purple")
    })
    
    output$trendPlot <- renderPlotly({
        df_long <- market_data() %>% 
            pivot_longer(cols = -date, names_to = "Indicator", values_to = "Value")
        
        p <- ggplot(df_long, aes(x = date, y = Value, color = Indicator)) +
            geom_line(size = 0.8) +
            geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
            theme_minimal() +
            labs(x = "", y = "Yield / Spread (%)") +
            scale_color_manual(values = c("T10Y2Y" = "#2c3e50", "T10Y3M" = "#e74c3c", "T10YIE" = "#27ae60"))
        
        ggplotly(p)
    })
    
    output$logicText <- renderUI({
        tagList(
            h4("Current Market Signal:"),
            p(analysis()$reason),
            hr(),
            h4("Projected Total Return (AGG):"),
            h3(percent(analysis()$est_ret, accuracy = 0.01)),
            p("Includes 4.4% yield + simulated price movement.")
        )
    })
    
    output$allocationTable <- renderTable({
        sat_p <- input$sat_pct / 100
        core_p <- 1 - sat_p
        total <- input$portfolio_val
        
        tibble(
            Segment = c("Core (Broad Market)", "Core (Short-Term)", "Tactical Satellite"),
            Asset = c("AGG", "SGOV", analysis()$rec),
            Allocation_Pct = c(percent(core_p/2), percent(core_p/2), percent(sat_p)),
            Estimated_Value = c(dollar(total * core_p/2), dollar(total * core_p/2), dollar(total * sat_p))
        )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui, server)