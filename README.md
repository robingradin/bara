# bara

Data obtained using biological data acquisition techniques are often associated with batch effects.
Batch effects can arise from a large variety of sources and can sometimes have
detrimental impacts on subsequent analyses. Batch effects are especially troublesome
in prediction problems where it is common to have a fixed prediction model for classifying
subsequently acquired test samples. Thus, the training set and the test set are
often completely correlated with batch, which can have a negative impact on the
model's prediction performance.

BARA is a method developed specifically for adjusting batch effects between 
training sets and test sets in predictive settings using reference samples.
Further, the method assumes that the variance important for making accurate predictions
can be captured in a lower dimensional space defined by the singular vectors of
the training set. Using a subset of the singular vectors containing the largest 
fraction of variance, the adjustments between the training set and the test set
is made in the compressed space spanned by the vectors. 

See the vignette for a description of the included functions.