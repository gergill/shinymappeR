---
output:
  pdf_document: default
  html_document: default
---
## Shiny Web Application: 1D MappeR

This repository contains a Shiny web application designed to visualize 1D mapper graph construction. The goal is to allow users to interactively explore different datasets/clustering methods and how they effect the mapper graph.

### Getting Started:
To easiest way to run this application is to download the contents of this repo to a folder, then open it as a new project in RStudio. 
You can also cd into the folder and run `Rscript app.R`, assuming you have the prequisite packages installed.

### Prerequisites:
Ensure you have the following R packages:

- `shiny`: to build the web application interface

- `mappeR`: for Mapper

- `dendextend`: for visualization of dendrograms

- `igraph`: for visualization of graphs

You can install these packages using the following commands in the R Console:
```R
install.packages("shiny")
install.packages("mappeR")
install.packages("dendextend")
install.packages("igraph")
```

### How to Contribute:

#### 1. Fork the repository to your GitHub Account

#### 2. Clone the forked repository to your machine

#### 3. Make changes and submit a pull request!
