Here is a draft README.md file for the pbgeno R package:

# pbgeno

pbgeno is an R package to streamline plant breeding data analysis and visualization. It provides functions to easily generate publication-quality tables and figures for genotype datasets.

## Installation

You can install the latest version of pbgeno from GitHub:

```r 
#install.packages("devtools")
devtools::install_github("Allan-gitrepos/pbgeno")
```

## Usage

pbgeno contains functions for common plant breeding workflows:

```r
library(pbgeno)

# Jaccard distance matrix
dists <- ggtree_jaccard(Jaccard_example)

# Polymorphism information content
results <- pic_calc(PIC_example)

# Convert structure foramt to tassel format data
tassel_data <- str2tassel(Structure_example, metadata)

```

## Example data

pbgeno contains some example genotype and phenotype datasets:  

- `Jaccard_example`: SSR markers data for calculating jaccard distances
- `PIC_example`: SSR marker data for calculating PIC_values
- `Structure_example`: SSR marker data example in structure format
- `metadata`: Meta data for the tassel data. 

## Key functions

* `ggtree_jaccard` - Generate Jaccard distance matrix
* `pic_calc` - Calculate PIC value for each marker  
* `str2tassel` - Converts Structure format data to tassel hapmap format


## Getting help

If you encounter any issues or bugs with pbgeno, please file an issue on the GitHub repository. Ideas for package improvements and contributions are also welcome!
