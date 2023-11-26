
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
    shinyjs::onclick("masBtn",  updateTabsetPanel(session, inputId="navbar", selected="nuestro"))
    
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
          #plotOutput("GMatriz", height = "600px")
          #imageOutput("GMatriz", height = "600px")
          img(src = "plots/GMatriz.png", height = "80%", width = "80%", align = "left")
          
        },
        "Ingresos vs Puntuación de Crédito" = {
          #plotOutput("GRIngresos", height = "600px")
          img(src = "plots/GRIngresos.png", height = "600px", width = "auto", align = "left")
        },
        "Puntuación de Perfil y de Crédito" = {
          #plotOutput("GRPerfil", height = "600px")
          img(src = "plots/perfilgrafica.png", height = "600px", width = "auto", align = "left")
        },
        "Préstamo-valor vs puntuación de perfil" = {
          #plotOutput("GRPrestamo", height = "600px")
          img(src = "plots/GRPrestamo.png", height = "500px", width = "auto", align = "left")
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
    # ------- DEPRECATED PLOTS -----------------------------------------
    # output$GMatriz <- renderPlot({
    #   ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
    #     geom_tile() +
    #     scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    #     theme_minimal() +
    #     coord_fixed() +
    #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    #     labs(x = "", y = "", title = "Matriz de Correlación")
    # })
    # output$GMatriz <- renderImage({
    #     filename <- "plots/GMatriz.png"
    #         list(src = filename, contentType = "image/png")
    #       }, deleteFile = FALSE)
    #   })
    # output$GRIngresos <- renderPlot({
    #   ggplot(df, aes(x = Income, y = Credit.Score)) + 
    #     geom_point(alpha = 0.5, color = "#ffae80") +
    #     theme_minimal() +
    #     geom_smooth(method = "lm", color = "#000000") +
    #     labs(title = "Relación entre Ingresos y Puntuación de Crédito", x = "Ingresos", y = "Puntuación de Crédito")
    # })
    # 
    # output$GRPerfil <- renderPlot({
    #   ggplot(df, aes(x = Profile.Score, y = Credit.Score)) + 
    #     geom_point(alpha = 0.5, color = "#ffae80") +
    #     theme_minimal() +
    #     geom_smooth(method = "lm", color = "#000000") +
    #     labs(title = "Relación entre Puntuación de Perfil y Puntuación de Crédito", x = "Puntuación de Perfil", y = "Puntuación de Crédito")
    # })
    # 
    # output$GRPrestamo <- renderPlot({
    #   ggplot(df, aes(x = LTV.Ratio, y = Profile.Score)) + 
    #     geom_point(alpha = 0.5, color = "#ffae80") +
    #     theme_minimal() + 
    #     geom_smooth(method = "lm", color = "#000000") +
    #     labs(title = "Relación entre el préstamo-valor y la puntuación de perfil", x = "Préstamo valor", y = "Puntuación de perfil")
    # })

  # --------- SOLUCIONES---------
    
    kmeans_result <- readRDS("www/models/kmeans.rds")
    df$Cluster <- kmeans_result$cluster
    
    cluster_summary <- df %>%
      group_by(Cluster) %>%
      summarise(
        Income_Mean = mean(Income, na.rm = TRUE),
        Age_Mean = mean(Age, na.rm = TRUE),
        Credit_Score_Mean = mean(Credit.Score, na.rm = TRUE),
        Existing_Loans_Count = mean(Number.of.Existing.Loans, na.rm = TRUE),
        Loan_Amount_Mean = mean(Loan.Amount, na.rm = TRUE),
        Loan_Tenure_Mean = mean(Loan.Tenure, na.rm = TRUE),
        LTV_Ratio_Mean = mean(LTV.Ratio, na.rm = TRUE),
        Count = n()
      )
    code <- "    df_filtered <- data.frame(df)
    df_filtered <- df_filtered %>%
      mutate(Profile.Score = Profile.Score / 100)
    df_filtered <- df_filtered %>%
      mutate(LTV.Ratio = LTV.Ratio / 100)
    df_filtered <- na.omit(df_filtered)
    
    library(randomForest)
    
    numericas <- c('Income', 'Credit.Score', 'Credit.History.Length', 'Loan.Amount',
                   'Loan.Tenure', 'LTV.Ratio', 'Age', 'Number.of.Existing.Loans')
    categoricas <- c('Employment.Profile', 'Occupation', 'Gender', 
                     'Existing.Customer')
    
    df_filtered[categoricas] <- lapply(df_filtered[categoricas], function(x) as.factor(x))
    df_filtered[numericas] <- scale(df_filtered[numericas])
    
    predictors <- c(numericas, categoricas)
    target <- 'Profile.Score'
    set.seed(123)
    
    train_indices <- createDataPartition(df_filtered[[target]], p = 0.65, list = FALSE)
    train_data <- df_filtered[train_indices, ]
    test_data <- df_filtered[-train_indices, ]
    
    modelRF <- randomForest(train_data[, predictors], train_data[[target]], 
                            ntree = 15, seed = 100)"
    
    code.k <- "data_cluster <- select(df, Income, Age, Credit.Score, Number.of.Existing.Loans,
                       Loan.Amount)
    data_cluster <- na.omit(data_cluster)
    data_cluster_scaled <- scale(data_cluster)
    
    set.seed(123) 
    kmeans_result <- kmeans(data_cluster_scaled, centers = 4, nstart = 25)
    
    cluster_summary <- df %>%
      group_by(Cluster) %>%
      summarise(
        Income_Mean = mean(Income, na.rm = TRUE),
        Age_Mean = mean(Age, na.rm = TRUE),
        Credit_Score_Mean = mean(Credit.Score, na.rm = TRUE),
        Existing_Loans_Count = mean(Number.of.Existing.Loans, na.rm = TRUE),
        Loan_Amount_Mean = mean(Loan.Amount, na.rm = TRUE),
        Loan_Tenure_Mean = mean(Loan.Tenure, na.rm = TRUE),
        LTV_Ratio_Mean = mean(LTV.Ratio, na.rm = TRUE),
        Count = n()
      )"
    
    output$SOL <- renderUI({
      switch (input$SOLoptions,
        "K-Means" = {
          tabsetPanel(
            tabPanel(
              "Resumen",
              tagList(
                div(class = "desc",
                  strong("Descripción estadística de cada cluster")
                ) ,
                div(class = "desc",
                  verbatimTextOutput("summary"),
                  tags$pre(code.k)
                )
              )
            ),
            tabPanel(
              "Gráfica",
              img(src = "plots/Kmeans.png", height = "600px", width = "1000px", align = "left")
            )
          )
          
        },
        "Random Forest" = {
          tabsetPanel(
            tabPanel(
              "Código",
              tagList(
                div(class = "desc",
                  tags$pre(code)
                )
              )
            ),
            tabPanel(
              "Gráfica",
              img(src = "plots/random2.png", height = "600px", width = "1000px", align = "left")
            )
          )
        }
      )
    })
    
    output$SOL.Desc <- renderUI({
      switch (input$SOLoptions,
              "K-Means" = {
                tagList(
                  strong("Ingresos Altos, Puntuación de Crédito Baja:"),
                  p(""),
                  p("Este grupo podría representar a individuos que han tenido 
                    una vida laboral extensa y exitosa, reflejada en sus ingresos
                    altos y edad mayor, pero que tal vez han tenido dificultades 
                    financieras recientes o decisiones de préstamos no óptimas que 
                    afectaron su puntuación de crédito."),
                  strong("Jóvenes con Menor Capacidad Crediticia:"),
                  p(""),
                  p("Este clúster podría consistir en jóvenes profesionales en 
                    las etapas iniciales de su carrera con ingresos relativamente 
                    bajos y puntuaciones de crédito en desarrollo. "),
                  strong("Jóvenes con Buena Salud Crediticia:"),
                  p(""),
                  p("Estos individuos podrían ser más jóvenes, posiblemente en 
                    la mitad de su carrera, con ingresos moderados y excelentes 
                    puntuaciones de crédito, lo que indica un manejo financiero 
                    prudente. "),
                  strong("Acomodados y Financieramente Seguros:"),
                  p(""),
                  p("Este grupo parece tener individuos de edad avanzada con 
                    ingresos muy altos y puntuaciones de crédito superiores, 
                    lo que sugiere una estabilidad financiera considerable y un 
                    buen manejo de deudas y créditos. ")
                )
              },
              "Random Forest" = {
                tagList(
                  strong("MSE:"),
                  p("0.002515296"),
                  strong("R2:"),
                  p("0.9560031"),
                  p("Podemos notar una precision excepcionalmente buena, pues se
                    adapta el 96% de nuestros datos, este modelo puede ser usado para 
                    la creación de una aplicación al público. En este momento el 
                    poder computacional necesario para generar el modelo es nuestro
                    principal impedimento.")
                  
                )
              }
      )
    })
    
    output$summary <- renderPrint({
      capture.output(print(cluster_summary))
    })
}
