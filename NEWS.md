# v0.4.0

## Add ordinal methods to the package

* Add `omisvm()` for ordinal multiple instance support vector machine
* Add `mior()` for multiple instance ordinal regression
* Add `misvm_orova()` for MI-SVM reducing ordinal to binary one-vs-all classification
* Add `svor_exc()` for support vector ordinal regression with explicit constraints

## Other changes

* Breaking: change `generate_mild_df()` to a new interface
* Breaking: change `mildsvm()` to `mismm()`
* Breaking: fix S3 method issue, affects `mi_df` and `mild_df` methods parameter
* Add `mi_df()` class and methods, including `as_mi_df()` 
* Add method for `mi_df` objects for `misvm()`, `cv_misvm()` and all new ordinal methods
* Add `ordmvnorm` data for examples
* Add print methods for `kfm_exact`, `kfm_nystrom`, `mild_df`, `mior`, `misvm`, `mismm`, `misvm_orova`, `omisvm`, `smm`, `svor_exc`
* Package now depends on R > 3.5.0, new imports of pillar, utils
* fix warning when `misvm()` has matrix passed
* fix `.reorder()` ambiguity
* pass lintr checks
* re-work internals for easier testing

# v0.3.1

* Fix bug where NaN columns passed to mildsvm() would fail
* Fix bug where columns with identical values passed to mildsvm() would fail

# v0.3.0

* Add new method to mildsvm(): method = 'qp-heuristic'. This works similar to the method of the same name in misvm(), but uses the SMM kernel from kme() in the underlying calculations.
* Fix bug in classify_bags() when using factors

# v0.2.0

* The main modeling functions (misvm(), mildsvm(), and smm()) now have three methods:
  * Formula method (i.e. misvm(mi(y, bags) ~ x1 + x2, data = df, ...))
  * Data-frame method (i.e. misvm(x, y, bags, ...))
  * Method for the mild_df class (I.e. misvm(mil_data, ...)). This method often performs non-trivial aggregation or transformation since misvm() and smm() work naturally on MIL data and supervised data, respectively.
* Prediction on main modeling functions always returns a tibble with a single column depending on the type argument
* Kernel feature maps functions are now organized as kfm_nystrom(), kfm_exact() with a build_fm() method.
* Update MilData class to mild_df class, and improve the class methods and constructors.
* Many internal methods removed and restructured.

# v0.1.0

* Initial release. This release has several known bugs and an early input/output scheme that has since been revised. This represents a mostly working starting point.