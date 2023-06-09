
library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(plotrix)

dataset_mouse <- read.csv("Data_Cortex_Nuclear.csv")

numeric_attributes_vec <-
  c(
    "DYRK1A_N",
    "ITSN1_N",
    "BDNF_N",
    "NR1_N",
    "NR2A_N",
    "pAKT_N",
    "pBRAF_N",
    "pCAMKII_N",
    "pCREB_N",
    "pELK_N",
    "pERK_N",
    "pJNK_N",
    "PKCA_N",
    "pMEK_N",
    "pNR1_N",
    "pNR2A_N",
    "pNR2B_N",
    "pPKCAB_N",
    "pRSK_N",
    "AKT_N",
    "BRAF_N",
    "CAMKII_N",
    "CREB_N",
    "ELK_N",
    "ERK_N",
    "GSK3B_N",
    "JNK_N",
    "MEK_N",
    "TRKA_N",
    "RSK_N",
    "APP_N",
    "Bcatenin_N",
    "SOD1_N",
    "MTOR_N",
    "P38_N",
    "pMTOR_N",
    "DSCR1_N",
    "AMPKA_N",
    "NR2B_N",
    "pNUMB_N",
    "RAPTOR_N",
    "TIAM1_N",
    "pP70S6_N",
    "NUMB_N",
    "P70S6_N",
    "pGSK3B_N",
    "pPKCG_N",
    "CDK5_N",
    "S6_N",
    "ADARB1_N",
    "AcetylH3K9_N",
    "RRP1_N",
    "BAX_N",
    "ARC_N",
    "ERBB4_N",
    "nNOS_N",
    "Tau_N",
    "GFAP_N",
    "GluR3_N",
    "GluR4_N",
    "IL1B_N",
    "P3525_N",
    "pCASP9_N",
    "PSD95_N",
    "SNCA_N",
    "Ubiquitin_N",
    "pGSK3B_Tyr216_N",
    "SHH_N",
    "BAD_N",
    "BCL2_N",
    "pS6_N",
    "pCFOS_N",
    "SYP_N",
    "H3AcK18_N",
    "EGR1_N",
    "H3MeK4_N",
    "CaNA_N"
  )

categorical_attributes_vec <-
  c("Genotype", "Treatment", "Behavior", "class")



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(
    h1(
      "Mouse Protein DataSet Visualization by Bimal Parajuli (20BDS0405) ",
      align = "center"
    )
  ),
  
  br(),
  br(),
  
  titlePanel(h2("Histograms Visualization ",
                align = "center")),
  h3("Select the Attributes and parameters to view histograms:"),
  
  # For Histograms.
  sidebarLayout(
    sidebarPanel(
      h3("For First histogram, "),
      selectInput("Attribute1", "Select Attribute1:", choices = numeric_attributes_vec, "DYRK1A_N"),
      sliderInput(
        "bins1",
        "Select Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      h3("For Second histogram, "),
      selectInput("Attribute2", "Select Attribute2:", choices = numeric_attributes_vec, "ITSN1_N"),
      sliderInput(
        "bins2",
        "Select Number of bins:",
        min = 1,
        max = 50,
        value = 20
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      h3("For Third histogram, "),
      selectInput("Attribute3", "Select Attribute3:", choices = numeric_attributes_vec, "BDNF_N"),
      sliderInput(
        "bins3",
        "Select Number of bins:",
        min = 1,
        max = 50,
        value = 40
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      h3("For Fourth histogram, "),
      selectInput("Attribute4", "Select Attribute4:", choices = numeric_attributes_vec, "pBRAF_N"),
      sliderInput(
        "bins4",
        "Select Number of bins:",
        min = 1,
        max = 50,
        value = 25
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot1"),
      plotOutput("distPlot2"),
      plotOutput("distPlot3"),
      plotOutput("distPlot4")
      
    )
  ),
  
  
  
  
  # For Scatter Plots.
  
  
  titlePanel(h2("Scatter Plots Visualization:", align = "center")),
  h3("Select the attributes for Scatter plot: "),
  sidebarLayout(
    sidebarPanel(
      h4("For the first scatter plot: "),
      
      selectInput("AttributeX1", "Select X attribute:", choices = numeric_attributes_vec, "ITSN1_N"),
      selectInput("AttributeY1", "Select Y attribute:", choices = numeric_attributes_vec, "BDNF_N"),
      selectInput("AttributeColor1", "Color by: ", choices = categorical_attributes_vec, "class"),
      
      br(),
      br(),
      br(),
      br(),
      
      h4("For the Second scatter plot: "),
      
      selectInput("AttributeX2", "Select X attribute:", choices = numeric_attributes_vec, "pCASP9_N"),
      selectInput("AttributeY2", "Select Y attribute:", choices = numeric_attributes_vec, "Ubiquitin_N"),
      selectInput("AttributeColor2", "Color by: ", choices = categorical_attributes_vec, "Behavior")
      
      
      
      
    ),
    mainPanel(plotOutput("scatterPlot1"),
              plotOutput("scatterPlot2"))
  ),
  
  
  # For pie charts
  titlePanel(h2("Pie chart Visualization:", align = "center")),
  h3("Select the attributes for Pie chart: "),
  sidebarLayout(
    sidebarPanel(
      selectInput("AttributePie1", "Select an Attribute: ", choices = categorical_attributes_vec, "class"),
      sliderInput(
        "explode",
        "Pls. Select Explode Size",
        min = 0,
        max = 1,
        value = 0.25,
        step = 0.01
      ),
      sliderInput(
        "angle",
        "Pls. Select viewing angle: x for (pi/x)",
        min = 1,
        max = 10,
        value = 4,
        step = 1
      ),
      
      
      
      
    ),
    mainPanel(plotOutput("PieChart1"))
  ),
  
  
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot1 <- renderPlot({
    ############################################################
    # First Histogram Plot:
    hist_data_1    <-
      dataset_mouse %>% select(input$Attribute1) %>% na.omit()
    
    hist_plot_1 <- ggplot(hist_data_1,
                          aes(x = .data[[input$Attribute1]])) +
      geom_histogram(
        bins = input$bins1,
        color = "black",
        fill = "lightblue"
      ) +
      ggtitle(paste(
        "Histogram for ",
        input$Attribute1,
        " by Bimal parajuli (20BDS0405)"
      ))
    
    
    hist_plot_1
    
  })
  
  output$distPlot2 <- renderPlot({
    ############################################################
    # Second Histogram Plot:
    
    hist_data_2    <-
      dataset_mouse %>% select(input$Attribute2) %>% na.omit()
    
    hist_plot_2 <- ggplot(hist_data_2,
                          aes(x = .data[[input$Attribute2]])) +
      geom_histogram(
        bins = input$bins2,
        color = "black",
        fill = "yellow"
      ) +
      ggtitle(paste(
        "Histogram for ",
        input$Attribute2,
        " by Bimal parajuli (20BDS0405)"
      ))
    
    hist_plot_2
    
  })
  
  
  output$distPlot3 <- renderPlot({
    ############################################################
    # Third Histogram Plot:
    
    hist_data_3   <-  dataset_mouse %>%
      select(input$Attribute3) %>%
      na.omit()
    
    hist_plot_3 <- ggplot(hist_data_3,
                          aes(x = .data[[input$Attribute3]])) +
      geom_histogram(
        bins = input$bins3,
        color = "black",
        fill = "pink"
      ) +
      ggtitle(paste(
        "Histogram for ",
        input$Attribute3,
        " by Bimal parajuli (20BDS0405)"
      ))
    
    hist_plot_3
    
  })
  
  
  
  
  
  output$distPlot4 <- renderPlot({
    ############################################################
    # Fourth Histogram Plot:
    
    hist_data_4    <- dataset_mouse %>%
      select(input$Attribute4) %>%
      na.omit()
    
    hist_plot_4 <- ggplot(hist_data_4,
                          aes(x = .data[[input$Attribute4]])) +
      geom_histogram(
        bins = input$bins4,
        color = "black",
        fill = "darkred"
      ) +
      ggtitle(paste(
        "Histogram for ",
        input$Attribute4,
        " by Bimal parajuli (20BDS0405)"
      ))
    
    hist_plot_4
    
  })
  
  
  output$scatterPlot1 <- renderPlot({
    scatterData <- dataset_mouse %>%
      select(input$AttributeX1,
             input$AttributeY1,
             input$AttributeColor1) %>%
      na.omit()
    
    scatter_plot1 <- ggplot(scatterData,
                            aes(
                              x = .data[[input$AttributeX1]],
                              y = .data[[input$AttributeY1]],
                              color = .data[[input$AttributeColor1]]
                            )) +
      geom_point() +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle(
        paste(
          "Scatter plot of ",
          input$AttributeX1,
          " vs ",
          input$AttributeY1,
          " by Bimal Parajuli"
        )
      )
    
    scatter_plot1
    
  })
  
  output$scatterPlot2 <- renderPlot({
    scatterData <- dataset_mouse %>%
      select(input$AttributeX2,
             input$AttributeY2,
             input$AttributeColor2) %>%
      na.omit()
    
    scatter_plot2 <- ggplot(scatterData,
                            aes(
                              x = .data[[input$AttributeX2]],
                              y = .data[[input$AttributeY2]],
                              color = .data[[input$AttributeColor2]]
                            )) +
      geom_point() +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle(
        paste(
          "Scatter plot of ",
          input$AttributeX2,
          " vs ",
          input$AttributeY2,
          " by Bimal Parajuli"
        )
      )
    
    scatter_plot2
    
  })
  
  output$PieChart1 <- renderPlot({
    pie_data <- select(dataset_mouse, input$AttributePie1)
    pie_data <- na.omit(pie_data)
    aaaaa <-
      pie_data %>% group_by(.data[[input$AttributePie1]]) %>% count()
    x_123 <- aaaaa$n
    y_123 <- aaaaa$.data[[input$AttributePie1]]
    pie3D(
      x = x_123,
      labels = y_123,
      shade = 0.5,
      border = "white",
      theta = pi / input$angle,
      main = paste(
        "Pie Chart of ",
        input$AttributePie1,
        " atribute by Bimal Parajuli (20BDS0405)"
      ),
      explode = input$explode
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
