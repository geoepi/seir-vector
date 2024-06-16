library(shiny)
library(deSolve)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(shinyjs)

seir_vect_f <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Force of infection 
    lambda_h = upsilon_h * beta_h * Iv / N_v
    lambda_v = upsilon_v * beta_v * Ih / N_h
    
    # Differential equations
    dSh = -lambda_h * Sh
    dEh = lambda_h * Sh - sigma_h * Eh
    dIh = sigma_h * Eh*rho_h - gamma_h * Ih
    dRh = gamma_h * Ih
    dSv = mu_v * N_v - lambda_v * Sv - mu_v * Sv
    dEv = lambda_v * Sv - (sigma_v + mu_v) * Ev
    dIv = sigma_v * Ev*rho_v - mu_v * Iv
    
    return(list(c(dSh, dEh, dIh, dRh, dSv, dEv, dIv)))
  })
}

ui <- fluidPage(
  useShinyjs(), 
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    
    tags$style(HTML("
      .title-container {
        display: flex;
        align-items: center;
        justify-content: space-between;
        width: 100%;
      }
      .logo-container {
        height: 50px;
        width: auto;
      }
      .main-title {
        font-weight: bold; 
      }
      .bottom-left {
        position: fixed;
        bottom: 0;
        left: 0;
        padding: 10px;
      }
      .bottom-left table {
        margin-left:auto;
        margin-right:auto;
      }
      .bottom-left p {
        text-align: center;
      }
      .bottom-left .fa-lg {
        font-size: 3.5em; /* Adjust this value as needed */
      }
    "))
  ),
  
  # Title panel with text links
  titlePanel(
    div(
      class = "title-container",
      span("VSV SEIR-SEI Dynamics", class = "main-title"),
      div(class = "link-container",
          a(href = "https://geoepi.github.io/seir-vector/overview", target = "_blank", "Model Description")
      )
    ),
    windowTitle = "SEIR-SEI Dynamics"
  ),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Host Population", 
                 sliderInput("N_h",
                             "Host Population (thousands):",
                             min = 1, max = 10, step = 0.5, value = 14),
                 sliderInput("upsilon_h",
                             "Contact Rate:",
                             min = 0.05, max = 1, step = 0.05, value = 0.6),
                 sliderInput("beta_h",
                             "Vector to Host VSV Transmission Probabilty:",
                             min = 0.05, max = 1, step = 0.05, value = 0.95),
                 sliderInput("sigma_h",
                             "Intrinsic Incubation Period:",
                             min = 0.05, max = 1, step = 0.05, value = 0.25),
                 sliderInput("rho_h",
                             "Host Competency:",
                             min = 0.05, max = 1, step = 0.05, value = 0.30),
                 sliderInput("gamma_h",
                             "Removal/Recovery Rate:",
                             min = 0.05, max = 1, step = 0.05, value = 0.10)
        ), 
        tabPanel("Vector Population",
                 sliderInput("N_v",
                             "Vector Population (hundreds, per host):",
                             min = 1, max = 20, step = 0.5, value = 3),
                 sliderInput("upsilon_v",
                             "Bite Rate:",
                             min = 0.05, max = 1, step = 0.05, value = 0.35),
                 sliderInput("beta_v",
                             "Host to Vector VSV Transmission Probability:",
                             min = 0.05, max = 1, step = 0.05, value = 0.85),
                 sliderInput("sigma_v",
                             "Extrinsic Incubation Period:",
                             min = 0.05, max = 1, step = 0.05, value = 0.4),
                 sliderInput("rho_v",
                             "Vector Competency:",
                             min = 0.05, max = 1, step = 0.05, value = 0.85),
                 sliderInput("mu_v",
                             "Background Mortality Rate:",
                             min = 0.05, max = 1, step = 0.05, value = 0.1)
        ),
        tabPanel("Initial Conditions",
                 sliderInput("e_0",
                             "Initial Hosts Exposed:",
                             min = 0, max = 5, step = 1, value = 1),
                 sliderInput("i_0",
                             "Initial Hosts Infections:",
                             min = 0, max = 5, step = 1, value = 0),
                 sliderInput("ev_0",
                             "Initial Vectors Exposed:",
                             min = 0, max = 5, step = 1, value = 0),
                 sliderInput("iv_0",
                             "Initial Vectors Infected:",
                             min = 0, max = 5, step = 1, value = 1)
        )
      ),
      actionButton("reset", "Reset to Defaults") 
    ),
    
    mainPanel(
      div(id = "results-section", style = "display:none;",
          tabsetPanel(
            tabPanel("Host Population Dynamics", plotlyOutput("dynPlot"), plotlyOutput("incidencePlot")),
            tabPanel("Vector Population Dynamics", plotlyOutput("dynPlot2"))
          )
      )
    )
  ),
  tags$div(
    class = "bottom-left",
    HTML(paste0(
      "<table>",
      "<tr>",
      "<td style='padding: 10px;'><a href='https://github.com/geoepi/seir-vector' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
      "<td colspan='2' style='text-align: center;'><a href='https://github.com/geoepi' target='_blank'><img src='logo.png' alt='GeoEpi' class='logo-container'></a></td>",
      "</tr>",
      "</table>",
      "<br>"
    )),
    HTML(paste0(
      "<script>",
      "var today = new Date();",
      "var yyyy = today.getFullYear();",
      "</script>",
      "<p><small>&copy; - <a href='https://geoepi.github.io/' target='_blank'>John Humphreys</a> - <script>document.write(yyyy);</script></small></p>"
    ))
  )
)

server <- function(input, output, session) {
  observe({
    shinyjs::show("results-section") # Show results section when sliders change
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "N_h", value = 14)
    updateSliderInput(session, "upsilon_h", value = 0.6)
    updateSliderInput(session, "beta_h", value = 0.95)
    updateSliderInput(session, "sigma_h", value = 0.25)
    updateSliderInput(session, "rho_h", value = 0.30)
    updateSliderInput(session, "gamma_h", value = 0.10)
    updateSliderInput(session, "N_v", value = 3)
    updateSliderInput(session, "upsilon_v", value = 0.35)
    updateSliderInput(session, "beta_v", value = 0.85)
    updateSliderInput(session, "sigma_v", value = 0.4)
    updateSliderInput(session, "rho_v", value = 0.85)
    updateSliderInput(session, "mu_v", value = 0.1)
    updateSliderInput(session, "e_0", value = 1)
    updateSliderInput(session, "i_0", value = 0)
    updateSliderInput(session, "ev_0", value = 0)
    updateSliderInput(session, "iv_0", value = 1)
  })
  
  dataInput <- reactive({
    init <- c(
      Sh = input$N_h * 1000 - (input$e_0 + input$i_0),
      Eh = input$e_0,
      Ih = input$i_0,
      Rh = 0,
      Sv = input$N_v*100 * input$N_h * 1000 - (input$iv_0 + input$ev_0), 
      Ev = input$ev_0,
      Iv = input$iv_0
    )
    
    parameters <- c(
      beta_h = input$beta_h,   
      gamma_h = input$gamma_h,   
      sigma_h = input$sigma_h,  
      sigma_v = input$sigma_v, 
      upsilon_h = input$upsilon_h,
      upsilon_v = input$upsilon_v,
      rho_h = input$rho_h,
      rho_v = input$rho_v,     
      beta_v = input$beta_v,   
      mu_v = input$mu_v,    
      N_h = input$N_h * 1000,     
      N_v = input$N_v*100 * input$N_h*1000,      
      i_0 = input$i_0,        
      e_0 = input$e_0,        
      ev_0 = input$ev_0,       
      iv_0 = input$iv_0   
    )
    
    times <- seq(0, 365, by = 1)
    
    out <- ode(
      y = init,
      times = times,
      func = seir_vect_f,
      parms = parameters
    )   
    as.data.frame(out)
  })
  
  output$dynPlot <- renderPlotly({
    out <- dataInput() %>%
      pivot_longer(cols = -time, names_to = "key", values_to = "value") %>%
      mutate(key = recode(key,
                          Sh = "Susceptible (S)",
                          Eh = "Exposed (E)",
                          Ih = "Infected (I)",
                          Rh = "Removed (R)"
      ))
    
    hout <- out %>% 
      filter(key %in% c("Susceptible (S)", "Exposed (E)", "Infected (I)", "Removed (R)")) %>%
      mutate(key = factor(key, levels = c("Susceptible (S)", "Exposed (E)", "Infected (I)", "Removed (R)")))
    
    p <- ggplot(hout, aes(x = time, y = value, color = key)) +
      geom_line(linewidth = 1.25) +
      labs(title = "Host Population Dynamics",  color = "Compartment", 
           x = "Outbreak Day", y = "Population") +
      theme_minimal()
    
    p <- p + theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  output$incidencePlot <- renderPlotly({
    out <- dataInput()
    out$incidence <- c(0, diff(out$Sh))
    
    inc_data <- data.frame(time = out$time[-1], 
                           total_incidence = -1*out$incidence[-1],
                           prop_incidence = -1*out$incidence[-1] * input$rho_h)
    
    inc_data$remaining_incidence <- inc_data$total_incidence - inc_data$prop_incidence
    
    p_inc <- ggplot(inc_data, aes(x = time)) +
      geom_bar(aes(y = remaining_incidence, fill = "Total Incidence"), stat="identity") +
      geom_bar(aes(y = prop_incidence, fill = "Clinical Symptoms"), stat="identity") +
      scale_fill_manual(name = "Incidence Type",
                        values = c("Total Incidence" = "blue", "Clinical Symptoms" = "red")) +
      labs(title = "Incident Infections", x = "Outbreak Day", y = "Incidence") +
      theme_minimal()
    
    p_inc <- p_inc + theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
    
    ggplotly(p_inc, tooltip = c("x", "y", "fill"))
  })
  
  output$dynPlot2 <- renderPlotly({
    out <- dataInput() %>%
      pivot_longer(cols = -time, names_to = "key", values_to = "value") %>%
      mutate(key = recode(key,
                          Sh = "Susceptible (S)",
                          Eh = "Exposed (E)",
                          Ih = "Infected (I)",
                          Rh = "Removed (R)"
      ))
    
    vout <- out %>% filter(key %in% c("Exposed (E)", "Infected (I)"))
    
    p <- ggplot(vout, aes(x = time, y = value, color = key)) +
      geom_line(linewidth = 1.25) +
      labs(title = "Vector Population Dynamics",  color = "Compartment", 
           x = "Outbreak Day", y = "Population") +
      theme_minimal()
    
    p <- p + theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size =       14),
      axis.title.y = element_text(size = 14),
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

                                    
