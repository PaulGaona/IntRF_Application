# IntRf_Application

## 1 Introduction

The goal of IntRF_Application is to run tree-based regression for interval-valued data and provide a more detailed explanation on how to use the code for simulations and a real data example. I plan on providing a step-by-step process for each file.

All files were used at one point for my simulation and real data analysis of my Ms Thesis at Utah State University (INSERT LINK HERE). There is no guarantee that every package will be cran compliant, for new versions of R (2022.12.0).

As the project title states this is a project based on the application of a package I developed IntRf.

Please refer to the README of IntRF for details about ***installation***, ***minimal example***, and ***source code*** at
[PaulGaona/IntRF](https://github.com/PaulGaona/IntRF)

## 2 Motivation

We are motivated in implementing a new machine learning model for interval-valued data. An introduction to interval-valued data can be found at [Diamond 1990](https://www.sciencedirect.com/science/article/pii/0022247X9090353H) and [Billard 2000](https://link.springer.com/chapter/10.1007/978-3-642-59789-3_58). As well as a more recent journal introducing machine learning for interval-valued data [Chacon 2021](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8067438/).

Our aim is to develop a new model that builds on the natural structure of an interval $\[x^{L}, x^{U}\]$ = $\[x^{C} \pm x^{r} \]$. Please see (INSERT LINKE HERE) for a more detailed discussion on the preliminaries and the application of the model.
