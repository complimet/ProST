# ProST:
# Shiny app
# Template By Emily Hashimoto-Roth

library(shiny)
library(shinyjs)
library(shinydashboard)
library(reticulate)
library(ggplot2)
library(bslib)
library(DT)
library(gridExtra)
library(shinycssloaders)
library(svglite)
library(cowplot)
library(reshape)
#use_python("/usr/bin/python3.10")
use_condaenv("/home/condaenv/ProST/")
source("src/simpleTabs.R")
source("src/analysisTab.R")
source("src/sidebar.R")
source("src/utils.R")

sidebar <- dashboardSidebar(
  sidebar_menu
)

body <- dashboardBody(
  tabItems(
    home_tab,
    samples_tab,
    analysis_tab,
    parameter_tab,
    troubleshoot_tab,
    cite_tab
  ),

  ################
  # HEADER TITLE #
  ################
  tags$head(tags$script(HTML(
    '$(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">Projection Statistics</span>\');
      })'
  )))
)


ui <- dashboardPage(
  dashboardHeader(title = tags$a(id="logomainlink",href='https://complimet.ca/',"CompLiMet")),
  sidebar,
  body,
  skin = "black",
  tags$head(tags$title("ProST"),
  includeHTML("./googleanalytics.html"),
    includeCSS("/var/www/compLiMet/public_html/shiny/ProST/www/complimetGUI.css")
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  })

  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    # Load dataset and stop if r is unable to load it for any reason.
    table <- tryCatch(
      {
        read.csv(file$datapath)
      },
      error = function(e) {
        stop(paste("There was an error importing the provided csv. Import failed with: ", e))
      }
    )
    x <- colnames(table)
    updateSelectInput(session, "classCol",
      choices = x,
      selected = head(x, 1)
    )
    shinyjs::enable(id = "classCol")
    shinyjs::enable(id = "noLabels")
  })
  observeEvent(input$noLabels, {
    if (input$noLabels) {
      updateSelectInput(session, "reductionMethod",
        choices = c(
          "tSNE",
          "PCA",
          "UMAP"
        ),
        selected = if (input$reductionMethod == "LDA") "PCA" else input$reductionMethod
      )
    } else {
      updateSelectInput(session, "reductionMethod",
        choices = c(
          "tSNE",
          "PCA",
          "UMAP",
          "LDA"
        ),
        selected = input$reductionMethod
      )
    }
  })
  # Disable Learning Rate Slider if user asks for recommended rate
  ##### Example: sliderInput(inputId = "learningRateSlider", "learningRate", "optSNE Learning Rate", min = 1, max = 1000, value = 10, step = 1)
  # observeEvent(input$useRecRate, {
  #  if (input$useRecRate == TRUE) {
  #    shinyjs::disable(id = "learningRateSlider")
  #  } else {
  #    shinyjs::enable(id = "learningRateSlider")
  #  }
  # })

  plot_data <- eventReactive(input$runPROST, {
    # create a dataset
    if (is.null(input$file1)) {
      stop("No File Selected. Please Upload a File")
    }
    table <- read.csv(input$file1$datapath)
    # Check if provided class column is present in columns of provided dataset
    if (!(input$classCol %in% colnames(table))) {
      stop(paste("There is no column named ", input$classCol, " in the uploaded dataset.", sep = ""))
    }

    source_python("src/dimension-library/bridge_lib.py")
    # learning_rate <- (if (input$useRecRate) NULL else input$learningRateSlider)

    # Disable download buttons, until plots and tables are ready.
    shinyjs::disable(id = "downloadTable")
    shinyjs::disable(id = "downloadPlot")
    out <- tryCatch(
      {
        getDimensionalReduction(
          table,
          class_column = input$classCol,
          method = input$reductionMethod,
          noLabels = input$noLabels,
          statistic_type = input$statisticalTest,
          normalization_method = input$normalization_method,
          imputation_method = input$imputation_method,
          logTransform = input$logTransform,
          tsne_perplexity = input$perplexity,
          tsne_early_exaggeration_coefficient = input$early_exaggeration_coefficient,
          tsne_affinity_metric = input$affinityMetric,
          tsne_use_multiscale = input$use_multiscale,
          tsne_n_iter_early = input$n_iter_early,
          tsne_n_iter = input$n_iter,
          umap_min_dist = input$min_dist,
          umap_n_neighbors = input$n_neighbors,
          random_state = NULL # input$random_state
        )
      },
      warning = function(w) {
        2
      },
      error = function(e) {
        stop(paste("There was an error:", py_last_error()$type, py_last_error()$value))
        e
      }
    )
    dim_prefix <- switch(isolate(input$reductionMethod),
      "tSNE" = "T",
      "optSNE" = "T",
      "PCA" = "PC",
      "UMAP" = "UMAP",
      "LDA" = "LD"
    )
    dim1_label <- paste(dim_prefix, "1", sep = "")
    dim2_label <- paste(dim_prefix, "2", sep = "")

    # Extract Data from Python Output
    Y <- out[[1]]
    pc1_table <- out[[2]]
    pc2_table <- out[[3]]
    sample_classes <- out[[4]]
    unique_classes <- out[[5]]
    transformed_data <- out[[6]]
    pc1_df <- out[[7]]
    pc2_df <- out[[8]]
    # Create Data Frames for Plots
    # heat_data <- data.frame(
    #  Z1 = as.vector(pc1_table),
    #  Z2 = as.vector(pc2_table),
    #  Y = rep(unique_classes, , , length(unique_classes)),
    #  X = rep(unique_classes, length(unique_classes)),
    #  stringsAsFactors = FALSE
    # )

    # meltnew <- reshape::melt.matrix
    # body(meltnew)[8][[1]] <- dn[char] <- lapply(dn[char], type.convert, as.is = TRUE)


    meltnew <- function(data, varnames = names(dimnames(data)), ...) {
      values <- as.vector(data)
      dn <- dimnames(data)
      if (is.null(dn)) {
        dn <- vector("list", length(dim(data)))
      }
      dn_missing <- sapply(dn, is.null)
      dn[dn_missing] <- lapply(dim(data), function(x) 1:x)[dn_missing]
      char <- sapply(dn, is.character)
      dn[char] <- lapply(dn[char], type.convert, as.is = TRUE)
      indices <- do.call(expand.grid, dn)
      names(indices) <- varnames
      data.frame(indices, value = values)
    }

    melted_pc1_table <- meltnew(as.matrix(get_upper_tri(pc1_df)))
    melted_pc2_table <- meltnew(as.matrix(get_upper_tri(pc2_df)))


    heat_data <- data.frame(
      Z1 = melted_pc1_table$value,
      Z2 = melted_pc2_table$value,
      Y = melted_pc1_table$X1,
      X = melted_pc1_table$X2,
      stringsAsFactors = TRUE
    )

    heat_data$Z1bin <- cut(heat_data$Z1, breaks = c(0, 0.01, 0.05, 1), label = c("p<=0.01", "0.01<p<=0.05", "p>0.05"))
    heat_data$Z2bin <- cut(heat_data$Z2, breaks = c(0, 0.01, 0.05, 1), label = c("p<=0.01", "0.01<p<=0.05", "p>0.05"))
    heat_data$Z3bin <- rep_len(c("p<=0.01", "0.01<p<=0.05", "p>0.05"), nrow(heat_data))
    reduction_data <- data.frame(
      Class = sample_classes,
      PC1 = Y[, 1],
      PC2 = Y[, 2],
      stringsAsFactors = FALSE
    )
    heat_data <- heat_data[complete.cases(heat_data), ]
    plot_data <- list(heat = heat_data, reduction = reduction_data, method = input$reductionMethod, input = table, transformed = transformed_data)

    heat_table_data <- heat_data[c("Z1", "Z2", "Y", "X")]
    output$inputTable <- renderDT({
      DT::datatable(table, options = list(scrollX = TRUE))
    }
    %>% formatSignif(columns = colnames(table[, sapply(table, is.numeric)]), digits = 4))
    output$transformTable <- renderDT({
      DT::datatable(transformed_data, options = list(scrollX = TRUE))
    } %>% formatSignif(columns = colnames(transformed_data[, sapply(transformed_data, is.numeric)]), digits = 4))
    output$redxnTable <- renderDT({
      DT::datatable(reduction_data, colnames = c("Class", dim1_label, dim2_label), options = list(scrollX = TRUE))
    } %>% formatSignif(columns = colnames(reduction_data[, sapply(reduction_data, is.numeric)]), digits = 4))
    output$heatTable <- renderDT({
      DT::datatable(heat_table_data, colnames = c(paste(dim1_label, " p-value"), paste(dim2_label, " p-value"), "Y Class", "X Class"), options = list(scrollX = TRUE))
    } %>% formatSignif(columns = colnames(heat_table_data[, sapply(heat_table_data, is.numeric)]), digits = 4))

    return(plot_data)
  })

  observeEvent(input$tabset1, {
    if (input$tabset1 == "Plots") {
      shinyjs::disable(id = "downloadTable")
    } else {
      req(plot_data())
      shinyjs::enable(id = "downloadTable")
    }
  })


  table_for_download <- eventReactive(input$tabset1, {
    dim_prefix <- switch(isolate(input$reductionMethod),
      "tSNE" = "T",
      "optSNE" = "T",
      "PCA" = "PC",
      "UMAP" = "UMAP",
      "LDA" = "LD"
    )
    dim1_label <- paste(dim_prefix, "1", sep = "")
    dim2_label <- paste(dim_prefix, "2", sep = "")
    if (input$tabset1 == "StatisticsTable") {
      table_data <- plot_data()$heat[c("Z1", "Z2", "Y", "X")]
      table_data <- setNames(table_data, c(paste(dim1_label, "p-value"), paste(dim2_label, "p-value"), "Y Class", "X Class"))
    } else if (input$tabset1 == "InputTable") {
      table_data <- plot_data()$input
    } else if (input$tabset1 == "TransformedTable") {
      table_data <- plot_data()$transformed
    } else if (input$tabset1 == "ReductionTable") {
      table_data <- plot_data()$reduction
      table_data <- setNames(table_data, c("Class", dim1_label, dim2_label))
    } else {
      table_data <- plot_data()$input
    }
    return(list(data = table_data, name = input$tabset1))
  })

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(table_for_download()$name, "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(table_for_download()$data, file)
    }
  )

  plot_input <- eventReactive(plot_data(), {
    dim_prefix <- switch(isolate(input$reductionMethod),
      "tSNE" = "T",
      "optSNE" = "T",
      "PCA" = "PC",
      "UMAP" = "UMAP",
      "LDA" = "LD"
    )
    dim1_label <- paste(dim_prefix, "1", sep = "")
    dim2_label <- paste(dim_prefix, "2", sep = "")
    labels <- list(dim1 = dim1_label, dim2 = dim2_label, prefix = dim_prefix)
    # Scatter Plot
    p1 <- ggplot(plot_data()$reduction, aes(x = PC1, y = PC2, color = Class)) +
      geom_point() +
      xlab(labels$dim1) +
      ylab(labels$dim2) +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16, face = "bold"), legend.title = element_text(size = 14), legend.text = element_text(size = 12))
    # Violin Charts
    p2 <- ggplot(plot_data()$reduction, aes(x = Class, y = PC1, fill = Class)) +
      geom_violin() +
      geom_point(position = position_jitter(seed = 1, width = 0.2, height = 0)) +
      ylab(paste(labels$dim1, "Score")) +
      xlab("Class") +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16, face = "bold"))
    p3 <- ggplot(plot_data()$reduction, aes(x = Class, y = PC2, fill = Class)) +
      geom_violin() +
      geom_point(position = position_jitter(seed = 1, width = 0.2, height = 0)) +
      # theme(axis.title.y = element_blank()) +
      ylab(paste(labels$dim2, "Score")) +
      xlab("Class") +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16, face = "bold"), legend.title = element_text(size = 14), legend.text = element_text(size = 12))

    # Heatmaps
    p4 <- ggplot(plot_data()$heat, aes(X, Y, fill = Z1bin)) +
      geom_tile(color = "grey92", lwd = 1.5, linetype = 1) +
      scale_fill_manual(values = c("p<=0.01" = "brown2", "0.01<p<=0.05" = "orange", "p>0.05" = "white"), drop = FALSE, na.value = "transparent") + # , breaks=c("[0,0.01]", "(0.01,0.05]", "(0.05,1]")
      # geom_label(aes(label = signif(Z1, 3)), fill = "white", size = 4.5) +
      # ggtitle(paste(labels$dim1_label, "Stats")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 14)) +
      labs(fill = "p-value")
    p5 <- ggplot(plot_data()$heat, aes(X, Y, fill = Z2bin)) +
      geom_tile(color = "grey92", lwd = 1.5, linetype = 1) +
      scale_fill_manual(values = c("p<=0.01" = "brown2", "0.01<p<=0.05" = "orange", "p>0.05" = "white"), drop = FALSE, na.value = "transparent") + # , breaks=c("(0,0.01]", "(0.01,0.05]", "(0.05,1]")
      # geom_label(aes(label = signif(Z2, 3)), fill = "white", size = 4.5) +
      # ggtitle(paste(labels$dim2_label, "Stats")) +
      # theme(axis.text.y = element_blank()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 14)) +
      labs(fill = "p-value")

    p6 <- ggplot(plot_data()$heat, aes(X, Y, fill = Z3bin)) +
      geom_tile() +
      scale_fill_manual(values = c("p<=0.01" = "brown2", "0.01<p<=0.05" = "orange", "p>0.05" = "white"), breaks = c("p<=0.01", "0.01<p<=0.05", "p>0.05"), drop = FALSE) + # , breaks=c("(0,0.01]", "(0.01,0.05]", "(0.05,1]")
      # geom_label(aes(label = signif(Z2, 3)), fill = "white", size = 4.5) +
      # ggtitle(paste(labels$dim2_label, "Stats")) +
      # theme(axis.text.y = element_blank()) +
      theme(legend.key = element_rect(color = "grey92"), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + # nolint: object_usage_linter.
      labs(fill = "p-value")


    # first align the top-row plot (p3) with the left-most plot of the bottom row (p1)
    plots <- align_plots(p1 + theme(legend.position = "none"), p2 + theme(legend.position = "none"), p4 + theme(legend.position = "none"), align = "v", axis = "l")
    # middle_row <- plot_grid(plots[[2]], p3 + theme(legend.position = "none"))
    # bottom_row <- plot_grid(plots[[3]], p5 + theme(legend.position = "none"))

    l1 <- ggdraw() + draw_label(label = paste(labels$dim1, "Statistics", sep = " "), size = 16, hjust = 0.4)
    l2 <- ggdraw() + draw_label(label = paste(labels$dim2, "Statistics", sep = " "), size = 16, hjust = 0.4)
    # label_row <- plot_grid(l1, l2)

    # now add the title
    title <- ggdraw() +
      draw_label(
        paste("Statistical analysis of", input$reductionMethod, "components"),
        fontface = "bold",
        size = 18,
        hjust = 0.5
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7),
      )
    p1_legend <- cowplot::get_plot_component(p1, "guide-box", return_all = TRUE)[[1]]
    p3_legend <- cowplot::get_plot_component(p3, "guide-box", return_all = TRUE)[[1]]
    p6_legend <- cowplot::get_plot_component(p6, "guide-box", return_all = TRUE)[[1]]

    shinyjs::enable(id = "downloadPlot")

    if (input$noLabels) {
      left_col <- plot_grid(l1, plots[[2]], ncol = 1, rel_heights = c(2, 10))
      right_col <- plot_grid(l2, p3 + theme(legend.position = "none"), ncol = 1, rel_heights = c(2, 10))

      plot_grid(
        title,
        plot_grid(
          plot_grid(plots[[1]], plot_grid(
            left_col, right_col,
            ncol = 2
          ), ncol = 1, rel_heights = c(5, 7)),
          plot_grid(p1_legend, NULL, p3_legend, rel_heights = c(7, 1, 5), align = "v", ncol = 1),
          rel_widths = c(9, 1)
        ),
        ncol = 1,
        rel_heights = c(1, 11)
      )
    } else {
      left_col <- plot_grid(l1, plots[[2]], NULL, plots[[3]], ncol = 1, rel_heights = c(1, 5, 1, 5))
      right_col <- plot_grid(l2, p3 + theme(legend.position = "none"), NULL, p5 + theme(legend.position = "none"), ncol = 1, rel_heights = c(1, 5, 1, 5))

      plot_grid(
        title,
        plot_grid(
          plot_grid(plots[[1]], plot_grid(
            left_col, right_col,
            ncol = 2
          ), ncol = 1, rel_heights = c(5, 7)),
          plot_grid(p1_legend, NULL, p3_legend, p6_legend, rel_heights = c(7, 1, 5, 5), align = "v", ncol = 1),
          rel_widths = c(9, 1)
        ),
        ncol = 1,
        rel_heights = c(1, 11)
      )
    }
  })

  plot_height <- eventReactive(input$runPROST, {
    if (input$noLabels) {
      return(1000)
    } else {
      return(1600)
    }
  })
  output$distPlot <- renderPlot(
    {
      plot_input()
    },
    height = function() {
      return(plot_height())
    }
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot", ".svg", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_input(), device = "svg", dpi = 72, height = 1200, units = "px", width = 1000)
    }
  )
  output$sampleDataDownload <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("sample_data", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      data1 <- read.csv("data/winedata.csv")
      write.csv(data1, file, row.names = FALSE)
    }
  )
}

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Run
options(reticulate.pandas_use_nullable_dtypes = TRUE)
options(shiny.autoreload = TRUE)
shinyApp(ui, server)
