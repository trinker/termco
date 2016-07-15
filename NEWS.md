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


termco 0.4.0 -
----------------------------------------------------------------

**BUG FIXES**

**NEW FEATURES**

* `term_cols` & `group_cols` added to quickly grab just term or grouping
  variable columns.

* `as_dtm` & `as_tdm` added to convert a `term_count` object into a
  `tm::DocumentTermMatrix` or `tm::TermDocumentMatrix` object.

* `update_names` added to allow for safe renaming of a `term_count` object's
  columns while also updating its attributes as well.

**MINOR FEATURES**

**IMPROVEMENTS**

* `classify` picks up a new default `ties.method` type of `"probabilities"`.
  This used the probability distribution from all tags assigned to randomly
  break ties based on that distribution.

* `term_count` gets an auto-collapse feature for hierarchical `term.list`s with
  duplicate names.  A message is printed telling the user this is happening.  To
  get the hierarchical coverage use `attributes(x2)[['pre_collapse_coverage']]`.


**CHANGES**


termco 0.3.0 - 0.3.6
----------------------------------------------------------------

**BUG FIXES**

* `colo` returned list rather than string if a single term was passed.  Spotted
  by Steve Simpson.  See <a href="https://github.com/trinker/termco/issues/12">issue #12</a>.

* `term_count` did not handle hierarchical `term.list` correctly due to a
  reordering done by **data.table** (when `group.vars` not `= TRUE`).  This
  has been corrected.

* Column ordering was not respected by `print.term_count`.

* `colo` did not copy to the clip board when `copy2clip` was `TRUE` and a single
  expression was passed to `...`.

**NEW FEATURES**

* `important_terms` added to compliment `frequent_terms` allowing tf-idf
  weighted terms to rise to the top.

* `collapse_tags` added to combine tags/columns from `term_count` object without
  stripping the `term_count` class and attributes.

**MINOR FEATURES**

* `plot_counts` picks up a `drop` argument to enable terms not found (if `x` is
  a `as_terms` object created from a `term_count` object) to be retained in the
  bar plot.  Suggested by Steve Simpson.  See <a href="https://github.com/trinker/termco/issues/18">issue #18</a>.

**IMPROVEMENTS**

* `colo` automatically adds a group parenthesis around `...` regexes to protect
  the grouping explicitly.  This is useful when a regex used or pipes (`|`).
  This would create an unintended expression that was overly aggressive (see #20).




termco 0.2.0
----------------------------------------------------------------

**NEW FEATURES**

* `validate_model` and `assign_validation_task` added to allow for human
  assessment of how accurate a model is functioning.

**CHANGES**

* `probe_colo_list`,`probe_colo_plot_list`, & `probe_colo_plot` all use
  `search_term_collocations` under the hood rather than `search_term` + `
  frequent_terms`.


termco 0.1.0
----------------------------------------------------------------

**BUG FIXES**

* `plot.term_count` did not properly handle weighting.  This has been fixed and
  allows for `"count"` as a choice.

* `search_term_which` (also `search_term`) did not treat te `and` argument
  correctly.  `and` was treated identical to the `not` argument.

**NEW FEATURES**

* `split_data` added for easy creation of training and testing data.

* `classification_project` added to make a classification modeling project
  template.

* `plot_cum_percent` added for cumulative percent plot of frequent terms.

* `probe_` family of functions added to easily make lists of function calls for
  exploration of the frequent terms in the context of the data.  Functions include:
  `probe_list`, `probe_colo_list`, `probe_colo_plot_list`, & `probe_colo_plot`.

* `hierarchical_coverage` added to allow exploration of the unique coverage of a
  text vector by a term after partitioning out the elements matched by previous
  terms.

* `tag_co_occurrence` added to explore tag co-occurrences.

* `search_term_collocations`   added as a convenience wrapper for `search_term`
  + `frequent_terms`. (Thanks to Steve Simpson)


**MINOR FEATURES**

* `plot_freq` picks up a `size` argument.

**IMPROVEMENTS**

* `term_count` now can be used in a hierarchical fashion.  A list of regexes can
  be passed and counted and then a second (or more) pass can be taken wit a new
  set of regexes on only those rows/text elements that were left untagged
  (count `rowSums` is zero).  This is accomplished by passing a `list` of
  `list`s of regexes.  Thanks to Steve Simpson for suggesting this feature.


termco 0.0.1
----------------------------------------------------------------

This package is a small suite of functions used to count terms and substrings
  in strings.