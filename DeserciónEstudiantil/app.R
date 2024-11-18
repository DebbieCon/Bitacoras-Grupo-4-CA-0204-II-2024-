library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(janitor)
library(tidyverse)


data <- read_csv2("universidades_europeas.csv")
datos <- data %>% 
  clean_names() %>%
  mutate(
    across(
      c(curricular_units_1st_sem_grade, curricular_units_2nd_sem_grade),
      ~round(as.numeric(gsub("\\s+", "", .)), 2)
    ),
    age_at_enrollment = as.numeric(age_at_enrollment)
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ifelse(is.infinite(.) | is.nan(.), NA, .)
    )
  )

ui <- fluidPage(
  titlePanel("Análisis de Estudiantes"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("edad_rango",
                  "Rango de Edad:",
                  min = min(datos$age_at_enrollment, na.rm = TRUE),
                  max = max(datos$age_at_enrollment, na.rm = TRUE),
                  value = c(min(datos$age_at_enrollment, na.rm = TRUE), 
                            max(datos$age_at_enrollment, na.rm = TRUE))),
      
      selectInput("tipo_estudiante",
                  "Tipo de Estudiante:",
                  choices = unique(datos$target))
    ),
    
    mainPanel(

      verbatimTextOutput("resumen_estadistico"),
      

      h4("Tabla de Estudiantes"),
      DTOutput("tabla_estudiantes"),
      

      h4("Distribución de Estudiantes por Edad"),
      plotOutput("grafico_densidad"),
      
      h4("Notas Primer Semestre vs Edad"),
      plotOutput("grafico_notas_1"),
      
      h4("Notas Segundo Semestre vs Edad"),
      plotOutput("grafico_notas_2")
    )
  )
)

server <- function(input, output) {
  
  
  datos_filtrados <- reactive({
    datos %>%
      filter(
        age_at_enrollment >= input$edad_rango[1],
        age_at_enrollment <= input$edad_rango[2],
        target == input$tipo_estudiante
      )
  })
  

  output$resumen_estadistico <- renderPrint({
    df <- datos_filtrados()
    cat("Resumen de los datos filtrados:\n")
    cat("Número de estudiantes:", nrow(df), "\n")
    cat("Promedio de edad:", round(mean(df$age_at_enrollment, na.rm = TRUE), 2), "\n")
    cat("Promedio notas 1er semestre:", round(mean(df$curricular_units_1st_sem_grade, na.rm = TRUE), 2), "\n")
    cat("Promedio notas 2do semestre:", round(mean(df$curricular_units_2nd_sem_grade, na.rm = TRUE), 2), "\n")
  })
  

  output$tabla_estudiantes <- renderDT({
    datos_filtrados() %>%
      select(age_at_enrollment, 
             target, 
             curricular_units_1st_sem_grade, 
             curricular_units_2nd_sem_grade) %>%
      rename(
        "Edad" = age_at_enrollment,
        "Tipo de Estudiante" = target,
        "Nota 1er Semestre" = curricular_units_1st_sem_grade,
        "Nota 2do Semestre" = curricular_units_2nd_sem_grade
      ) %>%
      mutate(
        across(
          c("Nota 1er Semestre", "Nota 2do Semestre"),
          ~round(., 2)
        )
      )
  })
  

  output$grafico_densidad <- renderPlot({
    ggplot(datos_filtrados(), aes(x = age_at_enrollment)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(x = "Edad", y = "Densidad") +
      theme_minimal() +
      ggtitle(paste("Distribución de edad para estudiantes:", input$tipo_estudiante))
  })
  

  output$grafico_notas_1 <- renderPlot({
    datos_filtrados() %>% filter(curricular_units_1st_sem_grade >10, curricular_units_1st_sem_grade<20) %>% 
    ggplot( 
           aes(x = age_at_enrollment, 
               y = curricular_units_1st_sem_grade)) +
      geom_point(alpha = 0.5, color = "blue") +
      geom_smooth(method = "lm", color = "red") +
      labs(x = "Edad", 
           y = "Nota Primer Semestre") +
      theme_minimal() +
      ggtitle(paste("Notas Primer Semestre vs Edad -", input$tipo_estudiante))
  })
  

  output$grafico_notas_2 <- renderPlot({
    datos_filtrados() %>% filter(curricular_units_2nd_sem_grade >10, curricular_units_2nd_sem_grade<20) %>% 
      ggplot(
           aes(x = age_at_enrollment, 
               y = curricular_units_2nd_sem_grade)) +
      geom_point(alpha = 0.5, color = "blue") +
      geom_smooth(method = "lm", color = "red") +
      labs(x = "Edad", 
           y = "Nota Segundo Semestre") +
      theme_minimal() +
      ggtitle(paste("Notas Segundo Semestre vs Edad -", input$tipo_estudiante))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)