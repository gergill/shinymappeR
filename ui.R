library(shiny)
library(mappeR)
library(RColorBrewer)
source("dataset_generation.R")
source("lens_functions.R")

app_css <- "
body, html {
  height: 100%;
  margin: 0;
  font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  background-color: #f1f3f5;
}

/* Main Layout ----------------------------------------------*/
#main-container {
  display: flex;
  height: 100vh;
  overflow: hidden;
}

/* Left Panel: Parameter Controls --------------------------------*/
#left-panel {
  flex: 0 0 22%;
  background-color: #f8f9fa;
  border-right: 2px solid #d0d0d0;
  padding: 20px 24px;
  overflow-y: auto;
  font-size: 16px;
  line-height: 1.5;
}

/* Right Panel ----------------------------------------------*/
#right-panel {
  flex: 1;
  display: flex;
  flex-direction: column;
  background-color: #ffffff;
  padding: 10px 20px;
  overflow-y: auto;
  font-size: 16px;
  line-height: 1.5;
}

/* Visualization Cards on Right Panel -----------------------*/
.viz-card {
  background-color: #ffffff;
  border: 1px solid #ccc;
  border-radius: 8px;
  padding: 18px;
  margin-bottom: 18px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08);
}
.viz-card h4 {
  font-size: 19px;
  margin-top: 0;
  color: #222;
}
.viz-card p {
  margin-top: 0;
  font-size: 15px;
  color: #555;
}

/* Plot container accessibility -----------------------------*/
.plot-container {
  border: 1px solid #ddd;
  border-radius: 6px;
  background-color: #fafafa;
  padding: 10px;
  transition: box-shadow 0.2s ease;
}
.plot-container:hover, .plot-container:focus-within {
  box-shadow: 0 0 0 3px #007bff55;
}

/* Tabs inside right panel ----------------------------------*/
.nav-tabs {
  background-color: #f7f9fb;
  border-bottom: 2px solid #ccc;
}
.nav-tabs > li {
  margin-bottom: -1px;
}
.nav-tabs > li > a {
  font-size: 17px;
  padding: 10px 22px;
  color: #222;
  background-color: #eaeef2;
  border: 1px solid #ccc;
  border-bottom: none;
  border-radius: 6px 6px 0 0;
  transition: background-color 0.2s ease, color 0.2s ease;
}
.nav-tabs > li > a:hover {
  background-color: #d9e4f5;
  color: #000;
  border-color: #007bff;
}
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  background-color: #007bff;
  color: #ffffff !important;
  border-color: #007bff #007bff #ffffff;
  border-bottom: 1px solid #ffffff;
  box-shadow: 0 -1px 2px rgba(0,0,0,0.15);
}

/* Tab content panel itself */
.tab-content {
  flex: 1;
  overflow-y: auto;
  background-color: #fff;
  border: 1px solid #ccc;
  border-top: none;
  border-radius: 0 0 8px 8px;
  padding: 18px 20px;
  color: #212529;
}

/* Card sections inside left panel ---------------------------*/
.param-section {
  background-color: #ffffff;
  border: 1px solid #ccc;
  border-radius: 8px;
  padding: 8px;
  margin-top: 0px;
  margin-bottom: 0px;
  box-shadow: 0 1px 2px rgba(0,0,0,0.05);
}
.param-section h4 {
  font-size: 18px;
  margin-top: 0px;
  color: #2a2a2a;
}

/* Inputs & Buttons ------------------------------------------*/
.form-control, select, input[type='text'], input[type='number'] {
  font-size: 16px !important;
  padding: 8px 10px !important;
  border-radius: 6px !important;
}

input[type='file'] {
  font-size: 15px;
}

.shiny-input-container {
  margin-bottom: 12px;
}

button, .btn {
  font-size: 16px !important;
  padding: 10px 18px !important;
  border-radius: 6px !important;
}

button:focus, select:focus, input:focus {
  outline: 3px solid #007bff;
  outline-offset: 2px;
}

/* Headings & Dividers --------------------------------------*/
h2, h3, h4, h5, label {
  color: #222;
}
hr {
  border-top: 1px solid #bbb;
  margin: 1em 0;
}
"

# ------------------------------------------------------------------
# UI Definition
# ------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css))
  ),

  # --- Main Container -----------------------------------------
  div(
    id = "main-container",

    # --- Left Panel -------------------------------------------
    div(
      id = "left-panel",
      h1("ShinyMappeR"),
      hr(),


      # Dataset section
      h3("Dataset"),
      div(
        class = "param-section",
        selectInput(
          "data",
          "Example Dataset:",
          choices = names(dataset_registry),
          selected = "circle"
        ),
        uiOutput("dynamic_dataset_ui"),
        hr(),
        fileInput(
          "upload",
          "Upload CSV:",
          accept = c(".csv", "text/csv", "text/plain")
        ),
        checkboxInput("header", "CSV has header", TRUE),
        helpText(
          "Upload a two-column CSV file (x and y). ",
          "If no file is uploaded, the example dataset is used."
        )
      ),

      # Lens section
      h3("Lens Function"),
      div(
        class = "param-section",
        selectInput(
          "lens",
          "",
          choices = names(lens_registry),
          selected = "project to x"
        ),
        uiOutput("dynamic_lens_ui")
      ),

      # Cover method section
      h3("Covering Method"),
      div(
        class = "param-section",
        selectInput(
          "cover_method",
          "",
          choices = c("Width-Balanced", "G‑Mapper", "Gaussian PDF"),
          selected = "Width-Balanced"
        ),
        conditionalPanel(
          condition = "input.cover_method == 'Width-Balanced'",
          sliderInput("num_patches", "Number of patches:", 1, 20, 10),
          sliderInput("percent_overlap", "Percent overlap:", 0, 100, 25)
        ),
        conditionalPanel(
          condition = "input.cover_method == 'G‑Mapper'",
          sliderInput("iterations", "Max iterations:", 1, 100, 20),
          sliderInput("ad_threshold", "A–D threshold:", 0.1, 100, 0.5, step = 0.1),
          sliderInput("g_overlap", "Gaussian overlap:", 0.05, 0.9, 0.3, step = 0.05),
          sliderInput("max_intervals", "Maximum number of intervals:", 5, 50, 10)
        ),
        conditionalPanel(
          condition = "input.cover_method == 'Gaussian PDF'",
          sliderInput("n_components", "Number of Gaussians:", 1, 10, 3),
          sliderInput(
            "z_threshold",
            "Responsibility threshold:",
            min = 0.01,
            max = 0.95,
            value = 0.30,
            step = 0.01
          ),
          sliderInput(
            "min_interval_size",
            "Minimum points per interval:",
            min = 1,
            max = 50,
            value = 8,
            step = 1
          ),
          helpText(
            "Responsibility threshold: minimum posterior probability for a ",
            "point to belong to a component. Higher values create stricter covers."
          )
        )
      ),

      # Clustering
      h3("Clusterer"),
      div(
        class = "param-section",
        selectInput(
          "method",
          "Linkage method:",
          choices = c("single", "complete", "average", "mcquitty")
        ),
        selectInput(
          "clusterer",
          "Cutting height method:",
          choices = c("global", "local")
        )
      )
    ),

    # --- Right Panel ------------------------------------------
    div(
      id = "right-panel",
      tabsetPanel(
        id = "viz-tabs",
        type = "tabs",
        tabPanel(
          "Global",

          # Original Data
          div(
            class = "viz-card",
            h4("Original Data"),
            div(
              class = "plot-container",
              textOutput("data_source"),
              plotOutput("filtered_data", height = "320px")
            )
          ),

          # Mapper Graph
          div(
            class = "viz-card",
            h4("Mapper Graph"),
            div(
              class = "plot-container",
              plotOutput("mapper", height = "500px")
            )
          ),

          # Global View
          div(
            class = "viz-card",
            h4("Global Dendrogram"),
            div(
              class = "plot-container",
              plotOutput("global_view", height = "300px")
            )
          ),
          div(
            class = "viz-card",
            h4("Global Histogram"),
            div(
              class = "plot-container",
              plotOutput("global_histogram", height = "900px"),
            ),
          ),
        ),
        tabPanel(
          "Local",
          # Patch Selection
          div(
            class = "param-section",
            sliderInput("display_patch", "Patch to display:",
              value = 1, min = 1, max = 2
            ),
          ),
          # Local Dendogram
          div(
            class = "viz-card",
            h4("Patch View (Local Dendrogram)"),
            div(
              class = "plot-container",
              plotOutput("patch_view", height = "300px")
            )
          ),
          # Patch View
          div(
            class = "viz-card",
            h4("Cluster View"),
            div(
              class = "plot-container",
              plotOutput("cluster_histograms", height = "600px")
            ),
          ),
          div(
            class = "viz-card",
            h4("Patch Histogram"),
            div(
              class = "plot-container",
              plotOutput("patch_histogram", height = "300px")
            )
          ),
        ),
        tabPanel(
          "Cover",
          div(
            class = "viz-card",
            h4("Cover Visualization"),
            div(
              class = "plot-container",
              plotOutput("cover_histogram", height = "600px"),
              plotOutput("mapper_cover_splits", height = "300px"),
              plotOutput("staggered_data", height = "300px")
            )
          ),
        )
      )
    )
  )
)
