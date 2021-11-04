---
title: "Areal Data Group Project"
author: "Kayla, Ingmar, Hanmo, Will"
date: "`11/2/20212021-11-04"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    toc_depth: 3
---



# Set up

**resources we are referencing:  

- http://www.css.cornell.edu/faculty/dgr2/_static/files/ov/ov_ADSA_Handout.pdf  
- Banerjee, Carlin and Gelfand, Hierarchical Modeling and Analysis for Spatial Data, 1st Edition, Ch 3  
- Bivand et al 2013 Ch 9  
- rspatial.org  

The packages we used are:  

```r
# install.packages(c("sp", "raster", "spdep"))
library(sp)
library(raster) # or terra
library(spdep)
## more...
```

The dataset(s) we are using are published in `package name(s)`.

```r
## load dataset(s)
```

# Exploratory Data Analysis  

## Measures of spatial association  

### Neighbors  
adjecency matrix construction - nearest neighbors, categories, 1/dist  

### Moran's I  

### Geary's C  

## Spatial smoothers  
Could delve further into this for the final project eg smoothing in image classification of fragmented habitats is problematic.  

### Clustering  

### Markov random fields  

# Spatial regression models  

## Zero means  

## Simultaneous Autoregressive Models  

### SAR error model  

### SAR lag model  

### SAR Durbin model  

### Likelihood Ratio  

### Model comparisons  

## Conditional Autoregressive Models  

### Gaussian case  

### non-Gaussian case  

### Model comparisons  
