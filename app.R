library(shiny)
library(shinydashboard)
# library(tidyquant) # REMOVED: No longer scraping the web
library(fredr)       # ADDED: Official API wrapper
library(tidyverse)
library(plotly)
library(scales)

# Authenticate with the Fed (Replace with your actual 32-character key)
# For shinyapps.io deployment, it's safer to use Sys.getenv("FRED_API_KEY") 
# but hardcoding it here is fine for testing.
fredr_set_key("API-Key_Removed-For-Privacy")

# 1. Configuration & Data Setup
symbols <- c("T10Y2Y", "T10Y3M", "T10YIE", "CPIAUCSL")

ui <- dashboardPage(
    dashboardHeader(title = "Bond_Rotation Tool"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            hr(),
            numericInput("portfolio_val", "Total Portfolio Value ($)", 100000, min = 0),
            sliderInput("sat_pct", "Tactical Satellite %", min = 0, max = 100, value = 40),
            
            hr(),
            h5("Scenario Modeling", style="padding-left:15px; font-weight:bold; color:#b8c7ce;"),
            sliderInput("target_yield_change", "Projected Rate Change (%)", -2, 2, 0, step = 0.1),
            uiOutput("rate_hint") 
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
    
    # 2. Reactive Data Retrieval (Official API Method with Error Handling)
    market_data <- reactive({
        
        # Map through the symbols SAFELY
        raw_df <- map_dfr(symbols, function(sym) {
            tryCatch({
                fredr(
                    series_id = sym,
                    observation_start = Sys.Date() - 1095 # 3 year lookback
                ) %>%
                    select(date, price = value, symbol = series_id) 
                
            }, error = function(e) {
                # If FRED throws a 500 error, log it and return an empty table
                # so the loop doesn't crash the whole app.
                message(paste("⚠️ FRED API failed to pull", sym, "-", e$message))
                return(tibble(date = as.Date(character()), price = numeric(), symbol = character()))
            })
        })
        
        # Safety: Ensure we downloaded at least SOME data
        req(nrow(raw_df) > 0) 
        
        # Safely extract CPI Date
        cpi_raw <- raw_df %>% filter(symbol == "CPIAUCSL") %>% drop_na(price)
        latest_cpi_date <- if(nrow(cpi_raw) > 0) max(cpi_raw$date) else Sys.Date()
        
        df <- raw_df %>%
            pivot_wider(names_from = symbol, values_from = price) %>%
            arrange(date) %>%
            fill(CPIAUCSL, .direction = "downup")
        
        # Safety: Ensure ALL required columns successfully downloaded before cleaning
        req("T10Y2Y" %in% names(df), "T10Y3M" %in% names(df), "T10YIE" %in% names(df))
        
        df <- df %>% drop_na(T10Y2Y, T10Y3M, T10YIE)
        
        req(nrow(df) > 0) 
        
        attr(df, "cpi_date") <- latest_cpi_date
        return(df)
    })
    
    # 3. Rotation Logic Engine
    analysis <- reactive({
        df <- market_data()
        latest <- tail(df, 1)
        cpi_report_date <- attr(df, "cpi_date")
        
        # SAFE CPI YoY Calculation
        cpi_current <- latest$CPIAUCSL
        cpi_historical_df <- df %>% filter(date <= (max(date) - 365))
        
        if (nrow(cpi_historical_df) > 0) {
            cpi_1yr_ago <- cpi_historical_df %>% tail(1) %>% pull(CPIAUCSL)
            cpi_yoy <- (cpi_current / cpi_1yr_ago) - 1
        } else {
            cpi_yoy <- 0 # Fallback if historical data is missing
        }
        
        # Double guard against NA
        if (is.na(cpi_yoy)) cpi_yoy <- 0
        
        # Core Constants
        agg_dur <- 5.8
        agg_yield <- 0.044
        cash_yield <- 0.040 
        
        # Total Return Estimate
        rate_move <- input$target_yield_change / 100
        price_impact <- (agg_dur * rate_move) * -1
        est_return <- agg_yield + price_impact
        
        # Logic Checks wrapped in isTRUE() to prevent NA crashes
        is_inverted <- isTRUE(latest$T10Y2Y < 0) | isTRUE(latest$T10Y3M < 0)
        inflation_spike <- isTRUE(latest$T10YIE > 2.5) | isTRUE(cpi_yoy >= 0.035)
        
        # STRICT HIERARCHY
        if (inflation_spike) {
            rec <- "FIPDX"
            reason <- "Inflation shock detected. Shifting to TIPS to protect purchasing power against energy/macro spikes."
            color <- "yellow"
        } else if (is_inverted) {
            rec <- "SPAXX / SGOV"
            reason <- "Yield curve inversion detected. Cash yields outperform duration with zero price risk."
            color <- "red"
        } else if (isTRUE(est_return < cash_yield)) {
            rec <- "SPAXX / SGOV"
            reason <- paste0("Projected rate hikes reduce AGG total return to ", percent(est_return, 0.01), ". Rotating to cash to preserve capital.")
            color <- "red"
        } else {
            rec <- "AGG"
            reason <- "Normal curve environment. Capturing yield and potential gains from rate stability."
            color <- "green"
        }
        
        list(rec = rec, reason = reason, color = color, latest = latest, 
             est_ret = est_return, cpi_yoy = cpi_yoy, cpi_date = cpi_report_date)
    })
    
    # 4. Outputs
    output$rate_hint <- renderUI({
        cpi <- analysis()$cpi_yoy
        
        if (cpi >= 0.035) {
            msg <- paste0("CPI Shock (", percent(cpi, 0.1), " YoY): Fed cuts unlikely. Recommend setting projection to +0.4% to +0.8%.")
            txt_color <- "#e74c3c" 
        } else if (cpi >= 0.028) {
            msg <- paste0("CPI Elevated (", percent(cpi, 0.1), " YoY): Recommend setting projection to +0.1% to +0.3%.")
            txt_color <- "#f39c12" 
        } else if (cpi <= 0.02) {
            msg <- paste0("CPI Cooling (", percent(cpi, 0.1), " YoY): Recommend setting projection to -0.25% to -0.75%.")
            txt_color <- "#27ae60" 
        } else {
            msg <- paste0("CPI Near Target (", percent(cpi, 0.1), " YoY): Recommend leaving projection near 0.0%.")
            txt_color <- "#a0a0a0" 
        }
        
        helpText(msg, style = paste0("padding: 0px 15px; font-size: 0.9em; font-weight: bold; color: ", txt_color, ";"))
    })
    
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
            select(-CPIAUCSL) %>% 
            pivot_longer(cols = -date, names_to = "Indicator", values_to = "Value")
        
        # Fixed 'size' deprecation warning by changing to 'linewidth'
        p <- ggplot(df_long, aes(x = date, y = Value, color = Indicator)) +
            geom_line(linewidth = 0.8) +
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
            h4("Macro Data:"),
            h5(paste0("Latest CPI YoY: ", percent(analysis()$cpi_yoy, 0.01))),
            p(paste0("(Report Sourced: ", format(analysis()$cpi_date, "%B %d, %Y"), ")"), 
              style = "font-size: 0.85em; color: #777; margin-top: -8px; font-style: italic;"),
            hr(),
            h4("Projected Total Return (AGG):"),
            h3(percent(analysis()$est_ret, accuracy = 0.01)),
            p("Includes 4.4% base yield + simulated price impact from duration.")
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