# steffanossaR
A tiny R package that probably won't survive that long :)
# Installation
```r
library(devtools)
install_github("steffanossa/steffanossaR")
```

## Step-functions to be used with *recipes*
### step_impute_constant()
Impute constant values.

### step_outliers_iqr_to_limits()
Identify outliers with the *IQR rule* and replace them with the limits of said rule.

### step_unwanted2NA()
Turn values into NAs. Either values that are specified or values that are not specified.

## Utility
### ex_factanal()
Extract *SS loadings*, *Proportion Var* and *Cumulative Var* from *factanal()* output.

### load2()
Load data and set its variable's name in one go.
<details>
  <summary>(<i>click to show/hide the characteristics of the data set</i>)</summary>
  <!-- have to be followed by an empty line! -->
  
  ```r
  datathing <- load2("folder/file.RData")
  ```
</details>
