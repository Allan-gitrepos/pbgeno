Here is a draft README.md file for the pbgeno R package:

# pbgeno

pbgeno is an R package to streamline plant breeding data analysis and visualization. It provides functions to easily generate publication-quality tables and figures for genotype datasets.

## Installation

You can install the latest version of pbgeno from GitHub:

```r 
#install.packages("devtools")
devtools::install_github("username/pbgeno")
```

## Usage

pbgeno contains functions for common plant breeding workflows:

```r
library(pbgeno)

# Jaccard distance matrix
dists <- jaccard_dist(genotype_data)

# Polymorphism information content
results <- calc_pic(marker_data)

# GGE biplot 
biplot(phenotype_data)
```

## Example data

pbgeno contains some example genotype and phenotype datasets:  

- `genotype_data`: SSR markers for 30 rice lines 
- `marker_data`: Dominant markers scored across 25 maize lines  
- `phenotype_data`: Grain yield for 40 wheat breeding lines 

## Key functions

* `jaccard_dist` - Generate Jaccard distance matrix
* `calc_pic` - Calculate PIC value for each marker  
* `biplot` - GGE biplot based on genotype x environment data
* `cluster_genotypes` - Cluster analysis and dendrogram plot 
* `genotype_table` - Publication-ready genotype table with custom formatting

## Getting help

If you encounter any issues or bugs with pbgeno, please file an issue on the GitHub repository. Ideas for package improvements and contributions are also welcome!
