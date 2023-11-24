
library(readr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(factoextra)
library(shiny)
library(shinyjs)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # ------------------------ DATASET ----
    df <- read.csv("www/credit_data.csv")
    selected_columns <- df %>%
      select("Age", "Occupation", "Income", "Credit.Score") 
  # ------------ INFORMACION GENERAL-----
    
    output$credit_dt <- renderDataTable(
      {head(selected_columns,n = 10)},
      options = list(aLengthMenu = c(5,25,50),
                     iDisplayLength = 5))
    
    output$IG <- renderUI({
      if (input$IGoptions == "Registros y Nulos") {
        tagList(
          strong("Cantidad de registros:"),
          textOutput("IG.NoReg"),
          strong("Cantidad de nulos:"),
          textOutput("IG.NoNull"),
          strong("Porcentaje de nulos:"),
          textOutput("IG.NullPer"),
          br(),
          strong("Todos los nulos se encuentran en la columna 'Occupation'"))
      }else{
        tagList(
          strong("Columnas"),
          verbatimTextOutput("IG.Col")
        )
      }
    })
    
    n.rows <- nrow(df)
    n.nulls <- sum(is.na(df))
    p.nulls <- (n.nulls / n.rows) * 100
    output$IG.NoReg <- renderText(n.rows)
    output$IG.NoNull <- renderText(n.nulls)
    output$IG.NullPer <- renderText({
      paste0(round(p.nulls, 2), "%")
    })
    output$IG.Col <- renderPrint(
      {str(df)}
    )
    shinyjs::onclick("logo",  updateTabsetPanel(session, inputId="navbar", selected="main"))
    
    
  # ---- ANALISIS UNIVARIADO --------
    output$AU <- renderUI({
      switch(
        input$AUoptions,
        "Distribución de Género" = {
          plotOutput("GDistGen", height = "600px")
        },
        "Distribución de clientes existentes" = {
          plotOutput("GDclient", height = "600px")
        },
        "Distribución de perfil laboral" = {
          plotOutput("GDperfil", height = "600px")
        },
        "Distribución de ingresos" = {
          plotOutput("GDingresos", height = "600px")
        },
        "Distribución de puntuación crediticia" = {
          plotOutput("GDpuntuacion", height = "600px")
        },
        "Distribuciíon del monto de préstamo" = {
          plotOutput("GDmontoPrestamo", height = "600px")
        }
      )
    })
    
    output$AU.Desc <- renderUI({
      switch(
        input$AUoptions,
        "Distribución de Género" = {
          p("Virtualmente la misma cantidad de mujeres (47.57%) y hombres (47.43%) con un pequeño
            porcentaje de género no especificado (4.98%)")
        },
        "Distribución de clientes existentes" = {
          p("La mayoría de los clientes que pidieron un préstamo fueron nuevos 
            clientes",strong("(62.15% vs 37.84%)"))
        },
        "Distribución de perfil laboral" = {
          tagList(
            p(strong("Freelancer:")," 8.08%"),
            p(strong("Salaried:")," 48.61%"),
            p(strong("Self-Employed:")," 30.14%"),
            p(strong("Student:")," 6.61%"),
            p(strong("Unemployed:")," 6.52%")
          )
        },
        "Distribución de ingresos" = {
          tagList(
            strong("Promedio:"),
            textOutput("Ingresos.Mean"),
            strong("Desviación estándar"),
            textOutput("Ingresos.SD"),
            strong("Min:"),
            textOutput("Ingresos.Min"),
            strong("Max:"),
            textOutput("Ingresos.Max")
          )
        },
        "Distribución de puntuación crediticia" = {
          p("Varía de ", strong("300 a 850,"),"lo que es típico para las puntuaciones de crédito. 
            La mediana es de", strong("584,"),"indicando que la mitad de las puntuaciones está 
            por debajo de 584 y la otra mitad por encima.")
        },
        "Distribuciíon del monto de préstamo" = {
          p("Varía entre ", strong("$5,294 y $150,000,"), "con una mediana de ", strong("$111,263,"), "lo que 
            sugiere que la mayoría de los préstamos son de cantidades significativas.")
        }
      )
    })
    
    output$Ingresos.Mean <- renderText(paste0("$ ",round(mean(df$Income),3)))
    output$Ingresos.SD <- renderText(paste0("$ ", round(sd(df$Income),3)))
    output$Ingresos.Min <- renderText(paste0("$ ", min(df$Income)))
    output$Ingresos.Max <- renderText(paste0("$ ", max(df$Income)))
    
  # -----------------------Plots-------------------
    output$GDistGen <- renderPlot({
      ggplot(df, aes(x = Gender)) + 
        geom_bar(fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Género", x = "Género", y = "Frecuencia") +
        theme(axis.text.x = element_text(size = 12),  
              axis.text.y = element_text(size = 12))
    })
    
    output$GDclient <- renderPlot({
      ggplot(df, aes(x = Existing.Customer)) + 
        geom_bar(fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Clientes Existentes", x = "Cliente Existente", y = "Frecuencia")
    })
    output$GDperfil <- renderPlot({
      ggplot(df, aes(x = Employment.Profile)) + 
        geom_bar(fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Perfil Laboral", x = "Perfil Laboral", y = "Frecuencia")
    })
    output$GDingresos <- renderPlot({
      ggplot(df, aes(x = Income)) + 
        geom_histogram(bins = 30, fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de Ingresos", x = "Ingresos", y = "Frecuencia")
    })
    output$GDpuntuacion <- renderPlot({
      ggplot(df, aes(x = Credit.Score)) + 
        geom_histogram(bins = 30, fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución de la Puntuación de Crédito", x = "Puntuación de Crédito", y = "Frecuencia")
    })
    output$GDmontoPrestamo <- renderPlot({
      ggplot(df, aes(x = Loan.Amount)) + 
        geom_histogram(bins = 30, fill = "#FF6A39", color = "black") +
        theme_minimal() +
        labs(title = "Distribución del Monto del Préstamo", x = "Monto del Préstamo", y = "Frecuencia")
    })
  # --------- RELACION ENTRE C ------
    numeric.df <- select(df, Age, Income, 
                           Credit.Score,Credit.History.Length, 
                           Number.of.Existing.Loans, Loan.Amount, 
                           Loan.Tenure, LTV.Ratio, Profile.Score)  
    cor_matrix <- cor(numeric.df, use = "complete.obs")
    cor_matrix_melted <- melt(cor_matrix)
    output$AB <- renderUI({
      switch(
        input$ABoptions,
        "Matriz de correlación" = {
          plotOutput("GMatriz", height = "600px")
        },
        "Ingresos vs Puntuación de Crédito" = {
          plotOutput("GRIngresos", height = "600px")
        },
        "Puntuación de Perfil y de Crédito" = {
          plotOutput("GRPerfil", height = "600px")
        },
        "Préstamo-valor vs puntuación de perfil" = {
          plotOutput("GRPrestamo", height = "600px")
        }
      )
    })
    
    output$AB.Desc <- renderUI({
      switch(
        input$ABoptions,
        "Matriz de correlación" = {
          p("Visualización de la relación lineal de nuestras variables")
        },
        "Ingresos vs Puntuación de Crédito" = {
          tagList(
            p("Una relación positiva podría indicar que individuos con mayores 
            ingresos tienden a tener mejores puntuaciones de crédito. Esto podría 
            deberse a una mayor capacidad para manejar deudas y pagos a tiempo."),
            p(strong("Implicaciones:")," Esta relación puede ser útil para las instituciones 
              financieras para evaluar la solvencia de los solicitantes de crédito.")
          )
        },
        "Puntuación de Perfil y de Crédito" = {
          tagList(
            p("Aquí hay una fuerte relación entre estas dos variables, lo cual indica 
            que la puntuación de perfil, que podría incluir factores como estabilidad 
            laboral o historial financiero, es un buen indicador de la puntuación de crédito."),
            p(strong("Implicaciones:"), " Esto puede ser valioso para desarrollar modelos predictivos 
          de riesgo crediticio.")
          )
        },
        "Préstamo-valor vs puntuación de perfil" = {
          tagList(
            p("Existe una relación significativa, aunque es negativa, podría sugerir 
            que las personas con perfiles más fuertes tienden a tener ratios de 
            préstamo-valor más bajos."),
            p(strong("Implicaciones:"), "  Esto podría informar 
            las decisiones sobre la concesión de préstamos, especialmente en 
            préstamos garantizados como hipotecas.")
          )
        }
      )
    })
    
    output$GMatriz <- renderPlot({
      ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
        theme_minimal() +
        coord_fixed() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        labs(x = "", y = "", title = "Matriz de Correlación")
    })
    output$GRIngresos <- renderPlot({
      ggplot(df, aes(x = Income, y = Credit.Score)) + 
        geom_point(alpha = 0.5, color = "#ffae80") +
        theme_minimal() +
        geom_smooth(method = "lm", color = "#000000") +
        labs(title = "Relación entre Ingresos y Puntuación de Crédito", x = "Ingresos", y = "Puntuación de Crédito")
    })
    
    output$GRPerfil <- renderPlot({
      ggplot(df, aes(x = Profile.Score, y = Credit.Score)) + 
        geom_point(alpha = 0.5, color = "#ffae80") +
        theme_minimal() +
        geom_smooth(method = "lm", color = "#000000") +
        labs(title = "Relación entre Puntuación de Perfil y Puntuación de Crédito", x = "Puntuación de Perfil", y = "Puntuación de Crédito")
    })
    
    output$GRPrestamo <- renderPlot({
      ggplot(df, aes(x = LTV.Ratio, y = Profile.Score)) + 
        geom_point(alpha = 0.5, color = "#ffae80") +
        theme_minimal() + 
        geom_smooth(method = "lm", color = "#000000") +
        labs(title = "Relación entre el préstamo-valor y la puntuación de perfil", x = "Préstamo valor", y = "Puntuación de perfil")
    })

  # --------- SOLUCIONES---------
    
}
