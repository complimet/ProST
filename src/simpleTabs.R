library(shinydashboard)

########################
# GETTING STARTED PAGE #
########################
home_tab <- tabItem(
  tabName = "start",
  box(
    column(
      12,
      h1("Overview of ProST"),
      p(
        "ProST - Projection STatistics - is a software solution for visualizing and analyzing
            high dimensional data projected into two dimensions."
      ),
      br(),
      p("Projection of data for visualization of main trends is one of the first steps in data mining.
            A number of different methods have been developed for this task. However, with only a visual
            interpretation of the projection the result interpretation lacks statistical rigour.
            ProST provides a user friendly application for several dimensionality reduction methods:"),
      tags$ul(
        tags$li("Principal Component Analysis (PCA) [1]"),
        tags$li("t-distributed Stochastic Neighbor Embedding (t-SNE) [2]"),
        tags$li("Uniform Manifold Approximation and Projection for Dimension Reduction (UMAP) [3]"),
        tags$li("Linear discriminant analysis (LDA) [4]")
      ),
      p("We provide statistical analysis on the level of separation of sample groups for each method. Users can select the preferred projection method and then choose between Mann-Whitney and t-test for the calculation of p-value for pairwise group separation. Projection as well as significance of the sample separation in first two dimensions are provided."),
      p("Additionally, prior to analysis user can choose between several different normalization and imputation methods or provide previously normalized data with no missing values. "),
      p("Details of sample format are provided with an example input file in the Download sample data tab. Sample label can be provided in any column with user providing column name in the analysis."),
      br(),
      h3("References:"),
      p("[1]", a("https://scikit-learn.org/stable/modules/generated/sklearn.decomposition.PCA.html", href = "https://scikit-learn.org/stable/modules/generated/sklearn.decomposition.PCA.html")),
      p("[2]", a("https://github.com/pavlin-policar/openTSNE", href = "https://github.com/pavlin-policar/openTSNE")),
      p("[3]", a("https://github.com/lmcinnes/umap", href = "https://github.com/lmcinnes/umap")),
      p("[4]", a("https://scikit-learn.org/stable/modules/generated/sklearn.discriminant_analysis.LinearDiscriminantAnalysis.html", href = "https://scikit-learn.org/stable/modules/generated/sklearn.discriminant_analysis.LinearDiscriminantAnalysis.html")),
      br(),
    ),
    width = 12
  )
)

###########################
# DOWNLOAD TEMPALTES PAGE #
###########################
samples_tab <- tabItem(
  tabName = "samples",
  box(
    column(
      8,
      h3("Preparing your data for ProST"),
      br(),
      br(),
      p(
        "ProST input must be a single .CSV file with features in columns and samples in rows."
      ),
      br(),
      p(
        "Any column can contain the sample class label. The user needs to select the name of this column from the dropdown list or enter the name of this column when
            submitting the file for analysis. Input set can have any number of labels and data can be imputed and normalized prior to analysis by selecting one of the provided methods. LDA requires that the number of sample groups (e.g. classes) is greater then 2."
      ),
      br(),
      h3("Sample Data"),
      br(),
      p("Provided example dataset has sample class label under column: \'Class\'."),
      br(),
      downloadButton("sampleDataDownload", "Sample Dataset")
    ),
    width = 12
  )
)
#############################
# PAGE FOR TROUBLESHOOTING #
#############################
troubleshoot_tab <- tabItem(
  tabName = "troubleshoot",
  box(
    column(
      12,
      h1("Troubleshooting ProST"),
      p(
        "When troubleshooting, please review this list of common reasons for ProST failing to run. If you are still
            experiencing difficulties running our tool, please contact ", a("ldomic@uottawa.ca", href = "mailto: ldomic@uottawa.ca"),
        " for further assistance. Please include your input dataset and a description of the problem that you experienced.
            We will reproduce the problem and provide you with a solution."
      ),
      br(),
      h4("1. My file does not load or does not produce any results"),
      p(
        "ProST only accepts comma-delimited (.csv) files as input. Tab-delimited or excel files will be read but will not produce any
            results. Please convert your input data into .csv format before running ProST. Data must start from row 2 with the name of
            the Sample Class column provided by user. Sample information can be listed in any column, with all numeric value columns in
            the input file used in the analysis."
      ),
      br(),
      h4("2. ProST can impute missing data, but my file with missing data produces an error."),
      p(
        "ProST recognizes empty strings as missing values. \"NA\"  values, null values, and whitespaces (e.g. single space) will produce
            an error when attempting to impute data. Please convert your missing value indicators to empty strings and/or strip your
            input file of whitespaces."
      ),
      br(),
      h4("3. ProST runs successfully, however the violin plot is difficult to read"),
      p(
        "Although ProST will accept any number of classes  and will provide results, the visualization of the violin plot 
            becomes difficult with more then 10 sample groups, and may not display in a way that is easy to read. Try running ProST with a subset of your sample groups."
      ),
      br(),
      h4("4. At least 2 samples per class are required for statistical analysis"),
      p(
        "Sample groups that have only 1 member will show in the projection plot but will not be used in statistical analysis of separation."
      ),
      br(),
      h4("5. LDA is not running but all other methods are"),
      p(
        "Algorithmically LDA can only be performed when number of sample groups (e.g. classes) is greater then 2 as the number of projections in LDA will be equal to one minus the number of sample classes."
      ),
      br()
    ),
    width = 12
  )
)

################################
# PAGE FOR AUTHORS AND CITING  #
################################
cite_tab <- tabItem(
  tabName = "cite",
  box(
    column(
      12,
      h3("Contact us"),
      p(a("ldomic@uottawa.ca", href = "mailto: ldomic@uottawa.ca")),
      br(),
      h3("Cite the use of ProST in a publication"),
      p(
        "Danny Salem, Anuradha Surendra, Graeme SV McDowell1, Miroslava Čuperlović-Culf (2024) Projection Statistics – ProST – an online implementation of data dimensionality reduction methods with statistical assessment of group separation.",
        a("{Link to article}", href = "#", target = "_blank")
      ),
      br(),
      h3("Public Server"),
      p("ProST: ", a("https://complimet.ca/shiny/dev_site/ProST/", href = "https://complimet.ca/shiny/dev_site/ProST/")),
      br(),
      h3("Software License"),
      p(
        "ProST is free software. You can redistribute it and/or modify it under the terms of the ",
        a("GNU General Public License", href = "https://www.gnu.org/licenses/", target = "_blank"),
        " v3 (or later versions) as published by the Free Software Foundation. As per the GNU General Public License, ProST is distributed as a bioinformatic
            lipidomic tool to assist users WITHOUT ANY WARRANTY and without any implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. All
            limitations of warranty are indicated in the GNU General Public License."
      ),
      br()
    ),
    width = 12
  )
)

########################
# Parameter Help PAGE #
########################
parameter_tab <- tabItem(
  tabName = "param",
  box(
    column(
      12,
      h2("t-SNE"),
      h3("Early Exaggeration Coefficient"),
      p("tSNE optimization generally occurs in two steps, starting with an early exaggeration phase where points are attracted much more strongly. The strength of this attraction exaggeration is controlled by the EE coefficient. The most commonly used value is 12 but tSNE may yield better dimensional reduction on smaller datasets with a value of 4, as prescribed in the original t-SNE paper. (https://github.com/pavlin-policar/openTSNE)"),
      h3("Early Exaggeration Iterations"),
      p("tSNE optimization occurs in two steps for a pre-determined number of iterations. The early exaggeration and 'normal', with the main difference between these two steps are the EE coefficient (1 for normal, >1 for EE) and the number of iterations."),
      p("Allowing early exaggeration to run long enough is vital for preserving global structure. It may be necessary to increase the EE iterations to improve the global structure when analyzing very large datasets. Increasing the number of normal iterations may improve local structure preservation as well."),
      h3("Affinity Measure"),
      p("The affinity between points (which the optimization in tSNE is trying to preserve with its loss function) is determined using a distance measure; ProST provides the option to use Euclidean or cosine distance which may be more appropriate in very high dimensional datasets."),
      h3("Perplexity"),
      p("Perplexity is related to the variance of the Gaussian distribution used to estimate the distance between points. Increasing the perplexity will cause the affinity calculation step to consider wider-ranging attractive forces. This can improve the global structure reconstruction at the cost of losing some local structure detail."),
      p("Changing the perplexity for small datasets can lead to qualitatively different dimensional reduction plots but when using the optimal learning rate (as used in ProSt), there does not appear to be an improvement."),
      h3("Multiscale Perplexity"),
      p("For very large datasets (where n >> 30000), it has been shown that global structure can be better preserved using multiscale perplexity. Checking this box will automatically select a second perplexity value (n/100), and calculate affinity using two gaussian distributions, each with different variance (30 and n/100)."),
      p("This setting is not recommended for small datasets with fewer than 30000 datapoints."),
      br(),
      h2("UMAP"),
      h3("Nearest Neighbours"),
      p("This parameter represents the balance between global and local structure preservation. You may increase the nearest neighbours to get better global structure preservation, at the risk of reduced local structure quality."),
      h3("Minimum Distance"),
      p("This is the minimum distance between points allowed by UMAP, the smaller this parameter is, the closer UMAP will pack together similar points."),
    ),
    width = 12
  )
)
