NEWS
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor
  and patch)
* New additions without breaking backward compatibility bumps the minor
  (and resets the patch)
* Bug fixes and misc changes bumps the patch



termco 0.1.0
----------------------------------------------------------------

**BUG FIXES**

* `plot.term_count` did not properly handle weighting.  Ths has been fixed and
  allows for `"count"` as a choice.

**NEW FEATURES**

* `split_data` added for easy creationg of training and testing data.

* `classification_project` added to make a classification modeling project
  template.

**MINOR FEATURES**

IMPROVEMENTS

**CHANGES**



termco 0.0.1
----------------------------------------------------------------

This package is a small suite of functions used to count terms and substrings
  in strings.