# clumpr 0.1.1

* Included italian data for 2015 and 2016
* Added dependency from the **svDialogs** package
* Added button to save data in RDA on the hard drive
* Interface updated with explicit number instead of sliders

# clumpr 0.1.0

* shinyapp

# clumpr 0.0.5

* Added strip support to state
* Changed strip for macroarea from region level to macroregion level
* Added tests dor state
* Updated `README`
* Added tests for state constructor.
* Added `get_macroareas()`, `get_macroregion()`, `get_region()`,
    `get_all_region()`, `get_state()`, `get_centers()`, `get_p_accept()`,
    `get_offered()`, `get_time()` methods for `state` class;
* Added `print()` method for `state`;
* Added `state()` and `set_macroareas` to create object of class `state`.

# clumpr 0.0.4

* Updated `LICENCE` file;
* Added lifecycle badge to the `README` file;
* Added `CODE_OF_CONDUCT.md` file;
* Reshaped `DESCRIPTION` accordingly to the tidy standards;
* Added support for AppVeyor.

# clumpr 0.0.3.063

* Updated `README`
* Added tests for macroareas.
* Added `get_center` generic and methods for `center` and `set_centers`
    classes;
* Added `get_p_accept()` method for `macroarea`;
* Added `get_*_strip()` (* = current or initial) generic funcitons and 
  methods for `macroarea`;
* Added `get_*_time()` (* = initial, current or final) generic and methods
  for `macroarea`;
* Added utility function `unlisit_1` to estract list components from a list
  vithout losing attributes (as happen with `unlist()`);
* Added `is_macroregion` function to check for inherit `macroregion` class;
* Added `get_macroregion()` generic function and method for `macroarea`;
* Added tests for `macroarea` and `set_macroregions`;
* Added `get_all_region` generic function and methods for `region`s,
  `macroregion`s and `set_macroregion`s, to access to the name of all the
  regions into the object;
* Added `get_regions` method for `region`s (identity function);
* Added `macroarea()` and `set_macroregions()`;
* Changed `time_period` to `current_time` in `macroregion` details and in
  the corresponding `get_time()` method;
* Added `get_region()` method for class `region`;
* Added `get_state` method for `macroarea`;
* Including `state` into regions' and macroregions' properties;
* Fixed an inconsistency in `center()`;
* Fixed mention to `viper` in `README`.

# clumpr 0.0.2.0082

* Added MS to authors;;
* Updated badges for `structures` branch;
* Added tests for correctness of output in macroregions;
* Added tests for `get_regions`, `get_centers`, `p_accept`, `set_regions`
  mehods for macroregions;
* Added tests for `print` method for `macroregion`s;
* Updatad `README`'s example;
* Added tests and check for unique region in a macroregion and unique
  centers on a region;
* Added `get_center`, `get_region`, `get_state`, `get_p_accept`,
  `get_offered`, `get_initial_strip` ,`get_current_strip`, `get_time`
  methods for `macroregion` (and generic function when needed);
* Added `print()` method for `macroregion` and `set_macroregion`;
* Added `macroregion()` to create object of class `macroregion`.

# clumpr 0.0.2.0002

* Organized `get_*()` in folder according to class.

# clumpr 0.0.1.00061

* Fixed bug of `at_least_one()` which leaded to a wrong probability
  computation;
* Fixed bug of `print.set_centers()` which didn't returned the imput object
  as invisible output;
* Imprved tests for regions.


# clumpr 0.0.1.00054

* Updated `README` with starting setup examples;
* Added `get_state()` generic, `get_state.center()` and `get_state.region()`
  methods to access to the state of a center or a region;
* Added `get_p_default()` generic and `get_p_default.region()` mothod to 
  access to the default probablity of acceptance for a center in a region 
  without an explicit one defined;
* Added `get_offered()` generic, `get_offered.center()` and
  `get_offered.region()` methods to access to the number of offered organs
  by centers and regions;
* Added `get_p_accept()` generic, `get_p_accept.center()` and
  `get_p_accept.region()` methods to access to the probability of
  acceptance for centers and regions;
* Added `print.region()` method;
* Added `get_centers.region()` method;
* Added `get_centers()` generic function to access to centers details;
* Provided storing of regions and states in lowercase while visualize them
  in Titlecase;
* Updated `regions` database in lowercase;
* Added import dependency from package `purrr`;
* Added tests for `set_centers()`;
* Added `set_centers()` to pass correct arguments to `region()`;
* Added `region()` constructor.


# clumpr 0.0.1.0007

* Added test for `zzz.R`;
* Defined generic function `get_region()` with method for class `center`;
* Reshaped badges in `README`.


# clumpr 0.0.0.9063

* Added support for Codecov
* Added support for Travis-CI
* Added dependecy from `R (>= 3.2)`
* Added tests for the constructor function `center()`;
* Provided `print.center()` methods;
* Created class `center` and its constructor function `center()`.


# clumpr 0.0.0.9006

* Added function `cat_line()` to `utils.R`;
* Added `.onAttach`, `.onLoad` `.onUnload` to `zzz.R` to provide package
  setup;
* Changed name in `clumper` according to "English" meanign.


# viper 0.0.0.9000

* Added support for `%>%` (pipe) operator to the package;
* Added a `NEWS.md` file to track changes to the package;
* Added a `README.rmd` and `README.md` files as welcome homepage to the 
  package;
* Added support fot test with `testthat` to the package;
* Provided basic structure to the package.
