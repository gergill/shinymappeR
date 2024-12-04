## Shiny Web Application: 1D MappeR

This repository contains a Shiny web application designed to visualize 1D mapper graph construction. The goal is to allow users to interactively explore different datasets/clustering methods and how they effect the mapper graph.

### Getting Started:
To run the application you will need R and the Shiny package installed. You can start the application by opening the R script in RStudio and clicking the Run App button.

### Prerequisites:
Ensure you have the following R packages:
- shiny: To build the web application interface
- mappeR: for mapper of course!

you can install these packages using the following commands in the R terminal:
```R
install.packages("shiny")
install.packages("mappeR")
```
### How to Run:
Open the app.R file in RStudio, click Run App at the top.

### Features:
- Dataset Selection:
  - circle
  - figure 8
  - spiral
  - barbell
- Parameter Adjustments:
  - number of points
  - projection coordinate
  - number of bins
  - percent overlap
  - clustering method
  - clustering scope
- Visualizations:
  - input data plot
    - shows the selected dataset with color coded bins
  - Mapper graph plot (also color coded)


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
git clone https://github.com/[your username]/zen-mapper[or whatever title you gave].git
``` 

#### 3. Create a branch
```shell
git checkout -b [change]/[name]
``` 
You can also do [this](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-and-deleting-branches-within-your-repository) on the GitHub website

#### 4. Make Changes
- You are free to make whatever changes or additions you'd like on the new branch

#### 5. Commit Changes to your branch
- Make sure to add a description
```shell
git commit -m "Description of your changes"
``` 

#### 6. Push changes to your fork

```shell
git push origin [change]/[name]
```

#### 7. Submit a Pull Request:

- Open a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request#creating-the-pull-request) to the main repository with a clear description of your changes and the purpose of the contribution.
