library(shiny)
library(shinyjs)
library(shinydashboard)

#####################################
# ANALYZE PAGE FOR RUNNING ProST #
#####################################
analysis_tab <- tabItem(
  shinyjs::useShinyjs(),
  tabName = "analyze",
  fluidRow(
    tags$div(
      class = "col-lg-4",
      tags$div(
        class = "box", style = "height:700px",
        # tags$div(
        #  class = "box-header",
        #  tags$h3(class = "box-title", "Dimensional Reduction Method Parameters")
        # ),
        tags$div(
          class = "box-body",
          fileInput(
            inputId = "file1",
            label = "Upload a file for dimensional reduction (*.csv).",
          ),
          selectInput("reductionMethod",
            label = "Select dimensional reduction method:",
            c(
              "tSNE",
              "PCA",
              "UMAP",
              "LDA"
            ),
            selected = "PCA"
          ),
          disabled(selectInput("classCol",
            "Enter the name of the column containing the sample class:",
            choices = list()
          )),
          disabled(checkboxInput("noLabels",
            label = "Don't use labels for Dimensional Reduction",
            value = FALSE
          )),br(),
          # Run Button
        actionButton("runPROST",
            label = "Run ProST",
            icon = icon("play"),
            class = "btn-warning",
            style = "color: #fff;"
          ),br(),
          # Button
          disabled(downloadButton("downloadPlot", "Download Plot")),br(),br(),
          disabled(downloadButton("downloadTable", "Download Current Table"))
        )
      )
    ),
    tags$div(
      class = "col-lg-4",
      tags$div(
        class = "box", style = "height:700px",
        # tags$div(
        #  class = "box-header",
        #  tags$h3(class = "box-title", "Dimensional Reduction Method Parameters")
        # ),
        tags$div(
          class = "box-body",
          selectInput("statisticalTest",
            label = "Select dimensional reduction component statistical test:",
            list(
              "Mann-Whitney U test" = "mannu",
              "Two (independent) Sample t-Test" = "ttest"
            )
          ),
          selectInput("imputation_method",
            label = "Select Imputation method:",
            selected = "knn",
            list(
              "Set missing values to 1/5 of lowest feature value" = "min1.5",
              "Set missing values to feature median" = "median",
              "K-Nearest Neighbours (k=2)" = "knn",
              "Multivariate Imputation by Chained Equations (MICE)" = "mice",
              "None" = "none"
            ),
            width = "100%"
          ),
          selectInput("normalization_method",
            label = "Select Normalization method:",
            selected = "zscore",
            list(
              "Min-Max" = "minmax",
              "Z-Score" = "zscore",
              "Pareto" = "pareto",
              "None" = "none"
            ),
            width = "100%"
          ),
          checkboxInput("logTransform",
            label = "log2 Transform Data prior to analysis"
          ) # ,
          # numericInput("random_state",
          #  label = "Random Seed",
          #  value = NULL
          # )
        )
      )
    ),
    tags$div(
      class = "col-lg-4",
      tags$div(
        class = "box", style = "height:700px",
        tags$div(
          class = "box-header",
          tags$h3(class = "box-title", "Dimensional Reduction Method Parameters")
        ),
        tags$div(
          class = "box-body",
          conditionalPanel(
            condition = "input.reductionMethod == 'tSNE'",
            tags$div(
              class = "param_flex",
              tags$div(style = "width:75%", selectInput("affinityMetric",
                label = "Select Affinity metric:",
                selected = "euclidean",
                list(
                  "Euclidean Distance" = "euclidean",
                  "Cosine Distance" = "cosine"
                ),
                width = "100%"
              )),
              br(),
              tags$div(style = "width:25%;padding-top:15px", checkboxInput("use_multiscale", "Multiscale perplexity", value = FALSE))
            ),
            tags$div(
              class = "param_flex",
              tags$div(style = "width:50%", sliderInput("perplexity", "Perplexity", min = 1, max = 1000, value = 30, step = 1)),
              tags$div(style = "width:50%", sliderInput("early_exaggeration_coefficient", "E.E. Coefficient", min = 4, max = 12, value = 12, step = 8)),
            ),
            tags$div(
              class = "param_flex",
              tags$div(style = "width:50%", sliderInput("n_iter_early", "E.E. Iterations", min = 100, max = 1000, value = 250, step = 50)),
              tags$div(style = "width:50%", sliderInput("n_iter", "Iterations", min = 250, max = 1000, value = 500, step = 50)),
            )
          ),
          conditionalPanel(
            condition = "input.reductionMethod == 'UMAP'",
            sliderInput("n_neighbors", "# of Neighbours", min = 2, max = 200, value = 15, step = 1),
            sliderInput("min_dist", "Minimum Distance", min = 0.0, max = 0.99, value = 0.1, step = 0.01),
          ),
          conditionalPanel(
            condition = "input.reductionMethod == 'LDA'",
            p("No settings"),
            p("Warning: Number of components must be <= min(# of classes - 1, # of feature). At least 3 classes and 2 features are required for LDA dimensionality reduction to work.", style = "color:red")
          ),
          conditionalPanel(
            condition = "input.reductionMethod == 'PCA'",
            p("No settings")
          )
        )
      ),
    ),
    width = 12
  ),
  fluidRow(
    tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      tabPanel("Plots", column(
        8,
        offset = 2,
        withSpinner(plotOutput(outputId = "distPlot", height = "auto"))
      ), ),
      tabPanel("InputTable", column(
        12,
        DTOutput("inputTable")
      )),
      tabPanel("TransformedTable", column(
        12,
        DTOutput("transformTable")
      )),
      tabPanel("ReductionTable", column(
        12,
        DTOutput("redxnTable")
      )),
      tabPanel("StatisticsTable", column(
        12,
        DTOutput("heatTable")
      )),
      width = 12
    ),
    hidden(div(downloadButton("download1", "Download as CSV")))
  )
)
