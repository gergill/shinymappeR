---
output:
  pdf_document: default
  html_document: default
---
## Shiny Web Application: 1D MappeR

This repository contains a Shiny web application designed to visualize 1D mapper graph construction. The goal is to allow users to interactively explore different datasets/clustering methods and how they effect the mapper graph.

### Getting Started:
To run the application you will need R and the Shiny package installed. You can start the application by opening the R script in RStudio and clicking the Run App button.

### Prerequisites:
Ensure you have the following R packages:
- `shiny`: To build the web application interface
- `mappeR`: for mapper of course!
- `dendextend`: for visualization of dendrograms
- `igraph`: for visualization of graphs

you can install these packages using the following commands in the R Console:
```R
install.packages("shiny")
install.packages("mappeR")
install.packages("dendextend")
install.packages("igraph")
```
### How to Run:
Open the app.R file in RStudio, click Run App at the top.

### How to Contribute:

#### 1. _Fork the repository_ to your GitHub Account

![image](https://github.com/user-attachments/assets/1ef9f591-55b8-487b-95b7-2ab8af6e9783)

  - Forks have the same name as the original repository by default. You can optionally change the name so that it is distinguishable.
  - Make sure to add a good description for what your fork is doing.

#### 2. _Clone the forked repository_ to your machine

- Go to the forked repository you are an owner of and get the link

![image](https://github.com/user-attachments/assets/efe8c6a8-2741-4933-8c56-39c3b5921502)

- In your terminal run:
```shell
git clone https://github.com/georgeck612/shinymappeR.git
``` 
