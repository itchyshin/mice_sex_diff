# Code and Data for "Sex and Power: Sexual dimorphism in trait variability and its eco-evolutionary and statistical implications"

# Introduction
This repository is for analyising a WT mice dataset to test for sex differences in mean and variance for over 200 traits. Our intial questions were two-fold: 

1) Do males and females show consistent sex differences / biases in traits?
2) Do males and females show similar or different biases in trait variance? 

The html file (preview [here](https://htmlpreview.github.io/?https://github.com/itchyshin/mice_sex_diff/blob/master/Supplement.html), or [download](https://github.com/itchyshin/mice_sex_diff/blob/master/Supplement.html) (and "save file") to open locally)) provides the most comprehensive overview of our workflow and analyses without having to rerun the analyses yourself.

# Datasets
 Unfortunately, the raw data file can not be provided via Github, as it is too large (274MB). However, it is freely accessible and uploaded to [zenodo.org](https://doi.org/10.5281/zenodo.3759701). As such, we have already processed the raw data file and provide a cleaned up file which is less computationally intensive to deal with. The file has been saved in a folder called `export`. This file is used for all further processing and analysis.

# Re-running analyses
## Step 1.
If users do not already have `R` and `RStudio` they will need to download both from the [CRAN mirrors](https://cran.r-project.org/mirrors.html) page. Choose the mirror closest to your locatioon and it will link to the `R` download page. Click the version and operating system that is relevant to you. `RStudio` can be downloaded from the [RStudio](https://rstudio.com/products/rstudio/download/) download page. 

## Step 2.
To re-run the analysis users can clone or download the entire respository to their local machine (un-zip the file if downloaded). Click on the 
`mice_sex_diff.Rproj`. This will open up `RStudio` and set the working directory to the respository. 

## Step 3. 
Once `Rstudio` is open, navigate the `MouseSexDiffApril20_2020.Rmd` file in the `scripts/` folder. This file has all the relevant code for analyses and provides detailed annotation on how things were done. One can simply walk through the code chunks as they appear, or more easily, just knit the entire document to `html` using `knit to html_document2` in the tab `knit`. Note that, to do this, you need to install the `bookdown` package from CRAN using `install.packages("bookdown")`. Other packages necessary for the scrpt will be loaded using `pacman`. 

# Questions or issues
If user have any questions or issues in running or understanding the code please contact the authors:
[Susi Zajitschek](susi.zajitschek@gmail.com) or [Shinichi Nakagawa](s.nakagawa@unsw.edu.au)
