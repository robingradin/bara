# bara

Data obtained using biological data acquisition techniques are often associated with batch effects.
Batch effects can arise from a large variety of sources and can sometimes have
detrimental impacts on subsequent analyses. Batch effects are especially troublesome
in prediction problems where it is common to have a fixed prediction model for classifying
subsequently acquired test samples. Thus, training sets and test sets are
often completely correlated with batch, which can result in poor predictive performance.

BARA is a method developed specifically for adjusting batch effects between 
training sets and test sets in predictive settings using reference samples.
Further, the method assumes that the variance important for making accurate predictions
can be captured in a lower dimensional space defined by the singular vectors of
the training set. Using a subset of the singular vectors, containing the largest 
fraction of variance, the adjustments between the training set and the test set
is made in the compressed space spanned by the vectors. 

For a more detailed descrition of the method, see the [article](https://doi.org/10.1371/journal.pone.0212669).

To install the package and get an introduction to the main functions of the package, run:

```r
devtools::install_github('robingradin/bara', build_opts = c('--no-resave-data', '--no-manual'))
vignette('introduction', 'bara')
```
