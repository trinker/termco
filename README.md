termco   
============


[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Build
Status](https://travis-ci.org/trinker/termco.svg?branch=master)](https://travis-ci.org/trinker/termco)
[![Coverage
Status](https://coveralls.io/repos/trinker/termco/badge.svg?branch=master)](https://coveralls.io/r/trinker/termco?branch=master)
[![DOI](https://zenodo.org/badge/5398/trinker/termco.svg)](https://zenodo.org/badge/latestdoi/5398/trinker/termco)

![](tools/termco_logo/r_termco.png)

**termco** is a small suite of functions used to count and find terms
and substrings in strings. The tools can be used to build an expert
rules, regular expression based text classification model. The package
wraps the
[**data.table**](https://cran.r-project.org/package=data.table) and
[**stringi**](https://cran.r-project.org/package=stringi) packages to
create fast data frame counts of regular expression terms and
substrings.


Table of Contents
============

-   [Functions](#functions)
-   [Installation](#installation)
-   [Contact](#contact)
-   [Examples](#examples)
    -   [Load the Tools/Data](#load-the-toolsdata)
    -   [Build Counts Dataframe](#build-counts-dataframe)
    -   [Printing](#printing)
    -   [Plotting](#plotting)
    -   [Ngram Collocations](#ngram-collocations)
        -   [Collocation Plotting](#collocation-plotting)
    -   [Converting to Document Term Matrix](#converting-to-document-term-matrix)
-   [Building an Expert Rules, Regex Classifier Model](#building-an-expert-rules-regex-classifier-model)
    -   [Load the Tools/Data](#load-the-toolsdata-1)
    -   [Splitting Data](#splitting-data)
    -   [Understanding Term Use](#understanding-term-use)
        -   [View Most Used Words](#view-most-used-words)
        -   [View Most Used Words in Context](#view-most-used-words-in-context)
        -   [View Important Words](#view-important-words)
    -   [Building the Model](#building-the-model)
    -   [Testing the Model](#testing-the-model)
    -   [Improving the Model](#improving-the-model)
        -   [Improving Coverage](#improving-coverage)
        -   [Improving Discrimination](#improving-discrimination)
    -   [Categorizing/Tagging](#categorizingtagging)
    -   [Evaluation: Accuracy](#evaluation-accuracy)
        -   [Pre Coded Data](#pre-coded-data)
        -   [Post Coding Data](#post-coding-data)

Functions
============


The main function of **termco** is `term_count`. It is used to extract
regex term counts by grouping variable(s) as well as to generate
classification models.

Most of the functions *count*, *search*, *plot* terms, and *covert*
between output types, while a few remaining functions are used to train,
test and interpret *model*s. Additionally, the `probe_` family of
function generate lists of function calls or plots for given search
terms. The table below describes the functions, category of use, and
their description:

<table>
<colgroup>
<col width="31%" />
<col width="15%" />
<col width="53%" />
</colgroup>
<thead>
<tr class="header">
<th>Function</th>
<th>Use Category</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>term_count</code></td>
<td>count</td>
<td>Count regex term occurrence; modeling</td>
</tr>
<tr class="even">
<td><code>token_count</code></td>
<td>count</td>
<td>Count fixed token occurrence; modeling</td>
</tr>
<tr class="odd">
<td><code>frequent_terms</code>/<code>all_words</code></td>
<td>count</td>
<td>Frequent terms</td>
</tr>
<tr class="even">
<td><code>important_terms</code></td>
<td>count</td>
<td>Important terms</td>
</tr>
<tr class="odd">
<td><code>hierarchical_coverage_term</code></td>
<td>count</td>
<td>Unique coverage of a text vector by terms</td>
</tr>
<tr class="even">
<td><code>hierarchical_coverage_regex</code></td>
<td>count</td>
<td>Unique coverage of a text vector by regex</td>
</tr>
<tr class="odd">
<td><code>ngram_collocations</code></td>
<td>count</td>
<td>Weighted frequent ngram (2 &amp; 3) collocations</td>
</tr>
<tr class="even">
<td><code>word_count</code></td>
<td>count</td>
<td>Count words</td>
</tr>
<tr class="odd">
<td><code>term_before</code>/<code>term_after</code></td>
<td>count</td>
<td>Frequency of words before/after a regex term</td>
</tr>
<tr class="even">
<td><code>term_first</code></td>
<td>count</td>
<td>Frequency of words at the begining of strings</td>
</tr>
<tr class="odd">
<td><code>colo</code></td>
<td>search</td>
<td>Regex output to find term collocations</td>
</tr>
<tr class="even">
<td><code>search_term</code></td>
<td>search</td>
<td>Search for regex terms</td>
</tr>
<tr class="odd">
<td><code>search_term_collocations</code></td>
<td>search</td>
<td>Wrapper for <code>search_term</code> + <code>frequent_terms</code></td>
</tr>
<tr class="even">
<td><code>classification_project</code></td>
<td>modeling</td>
<td>Make a classification modeling project template</td>
</tr>
<tr class="odd">
<td><code>as_dtm</code>/<code>as_tdm</code></td>
<td>modeling</td>
<td>Coerce <code>term_count</code> object into <code>tm::DocumentTermMatrix</code>/<code>tm::TermDocumentMatrix</code></td>
</tr>
<tr class="even">
<td><code>split_data</code></td>
<td>modeling</td>
<td>Split data into <code>train</code> &amp; <code>test</code> sets</td>
</tr>
<tr class="odd">
<td><code>evaluate</code></td>
<td>modeling</td>
<td>Check accuracy of model against human coder</td>
</tr>
<tr class="even">
<td><code>classify</code></td>
<td>modeling</td>
<td>Assign n tags to text from a model</td>
</tr>
<tr class="odd">
<td><code>get_text</code></td>
<td>modeling</td>
<td>Get the original text for model tags</td>
</tr>
<tr class="even">
<td><code>coverage</code></td>
<td>modeling</td>
<td>Coverage for <code>term_count</code> or <code>search_term</code> object</td>
</tr>
<tr class="odd">
<td><code>uncovered</code>/<code>get_uncovered</code></td>
<td>modeling</td>
<td>Get the uncovered text from a model</td>
</tr>
<tr class="even">
<td><code>tag_co_occurrence</code></td>
<td>modeling</td>
<td>Explor co-occurrence of tags from a model</td>
</tr>
<tr class="odd">
<td><code>validate_model</code>/<code>assign_validation_task</code></td>
<td>modeling</td>
<td>Human validation of a <code>term_count</code> model</td>
</tr>
<tr class="even">
<td><code>as_count</code></td>
<td>convert</td>
<td>Strip pretty printing from <code>term_count</code> object</td>
</tr>
<tr class="odd">
<td><code>as_terms</code></td>
<td>convert</td>
<td>Convert a count matrix to list of term vectors</td>
</tr>
<tr class="even">
<td><code>as_term_list</code></td>
<td>convert</td>
<td>Convert a vector of terms into a named term list</td>
</tr>
<tr class="odd">
<td><code>weight</code></td>
<td>convert</td>
<td>Weight a <code>term_count</code> object proportion/percent</td>
</tr>
<tr class="even">
<td><code>plot_ca</code></td>
<td>plot</td>
<td>Plot <code>term_count</code> object as 3-D correspondence analysis map</td>
</tr>
<tr class="odd">
<td><code>plot_counts</code></td>
<td>plot</td>
<td>Horizontal bar plot of group counts</td>
</tr>
<tr class="even">
<td><code>plot_freq</code></td>
<td>plot</td>
<td>Vertical bar plot of frequencies of counts</td>
</tr>
<tr class="odd">
<td><code>plot_cum_percent</code></td>
<td>plot</td>
<td>Plot <code>frequent_terms</code> object as cumulative percent</td>
</tr>
<tr class="even">
<td><code>probe_list</code></td>
<td>probe</td>
<td>Generate list of <code>search_term</code> function calls</td>
</tr>
<tr class="odd">
<td><code>probe_colo_list</code></td>
<td>probe</td>
<td>Generate list of <code>search_term_collocations</code> function calls</td>
</tr>
<tr class="even">
<td><code>probe_colo_plot_list</code></td>
<td>probe</td>
<td>Generate list of <code>search_term_collocationss</code> + <code>plot</code> function calls</td>
</tr>
<tr class="odd">
<td><code>probe_colo_plot</code></td>
<td>probe</td>
<td>Plot <code>probe_colo_plot_list</code> directly</td>
</tr>
</tbody>
</table>

Installation
============

To download the development version of **termco**:

Download the [zip
ball](https://github.com/trinker/termco/zipball/master) or [tar
ball](https://github.com/trinker/termco/tarball/master), decompress and
run `R CMD INSTALL` on it, or use the **pacman** package to install the
development version:

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_gh(
        "trinker/gofastr",
        "trinker/termco"
    )

Contact
=======

You are welcome to:    
- submit suggestions and bug-reports at: <https://github.com/trinker/termco/issues>    
- send a pull request on: <https://github.com/trinker/termco/>    
- compose a friendly e-mail to: <tyler.rinker@gmail.com>    

Examples
========

The following examples demonstrate some of the functionality of
**termco**.

Load the Tools/Data
-------------------

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(dplyr, ggplot2, termco)

    data(presidential_debates_2012)

Build Counts Dataframe
----------------------

    discoure_markers <- list(
        response_cries = c("\\boh", "\\bah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    counts <- presidential_debates_2012 %>%
        with(term_count(dialogue, grouping.var = list(person, time), discoure_markers))

    counts

    ## Coverage: 100% 
    ## # A tibble: 10 × 7
    ##       person   time n.words response_cries back_channels   summons
    ##       <fctr> <fctr>   <int>          <chr>         <chr>     <chr>
    ## 1      OBAMA time 1    3599        3(.08%)             0 43(1.19%)
    ## 2      OBAMA time 2    7477        2(.03%)             0  42(.56%)
    ## 3      OBAMA time 3    7243        1(.01%)       1(.01%)  58(.80%)
    ## 4     ROMNEY time 1    4085              0             0  27(.66%)
    ## 5     ROMNEY time 2    7536        1(.01%)       3(.04%)  49(.65%)
    ## 6     ROMNEY time 3    8303        5(.06%)             0 84(1.01%)
    ## 7    CROWLEY time 2    1672        2(.12%)             0   4(.24%)
    ## 8     LEHRER time 1     765        3(.39%)       3(.39%)         0
    ## 9   QUESTION time 2     583        2(.34%)             0         0
    ## 10 SCHIEFFER time 3    1445              0             0   2(.14%)
    ## # ... with 1 more variables: justification <chr>

Printing
--------

    print(counts, pretty = FALSE)

    ## Coverage: 100% 
    ## # A tibble: 10 × 7
    ##       person   time n.words response_cries back_channels summons
    ##       <fctr> <fctr>   <int>          <int>         <int>   <int>
    ## 1      OBAMA time 1    3599              3             0      43
    ## 2      OBAMA time 2    7477              2             0      42
    ## 3      OBAMA time 3    7243              1             1      58
    ## 4     ROMNEY time 1    4085              0             0      27
    ## 5     ROMNEY time 2    7536              1             3      49
    ## 6     ROMNEY time 3    8303              5             0      84
    ## 7    CROWLEY time 2    1672              2             0       4
    ## 8     LEHRER time 1     765              3             3       0
    ## 9   QUESTION time 2     583              2             0       0
    ## 10 SCHIEFFER time 3    1445              0             0       2
    ## # ... with 1 more variables: justification <int>

    print(counts, zero.replace = "_")

    ## Coverage: 100% 
    ## # A tibble: 10 × 7
    ##       person   time n.words response_cries back_channels   summons
    ##       <fctr> <fctr>   <int>          <chr>         <chr>     <chr>
    ## 1      OBAMA time 1    3599        3(.08%)             _ 43(1.19%)
    ## 2      OBAMA time 2    7477        2(.03%)             _  42(.56%)
    ## 3      OBAMA time 3    7243        1(.01%)       1(.01%)  58(.80%)
    ## 4     ROMNEY time 1    4085              _             _  27(.66%)
    ## 5     ROMNEY time 2    7536        1(.01%)       3(.04%)  49(.65%)
    ## 6     ROMNEY time 3    8303        5(.06%)             _ 84(1.01%)
    ## 7    CROWLEY time 2    1672        2(.12%)             _   4(.24%)
    ## 8     LEHRER time 1     765        3(.39%)       3(.39%)         _
    ## 9   QUESTION time 2     583        2(.34%)             _         _
    ## 10 SCHIEFFER time 3    1445              _             _   2(.14%)
    ## # ... with 1 more variables: justification <chr>

Plotting
--------

    plot(counts)

![](tools/figure/unnamed-chunk-6-1.png)

    plot(counts, labels=TRUE)

![](tools/figure/unnamed-chunk-6-2.png)

    plot_ca(counts, FALSE)

![](tools/figure/unnamed-chunk-6-3.png)

Ngram Collocations
------------------

**termco** wraps the [**quanteda**](https://github.com/kbenoit/quanteda)
package to examine important ngram collocations. **quanteda**'s
`collocation` function provides measures of: `"G2"`, `"X2"`, `"pmi"`,
and `"dice"` to examine the strength of relationship between ngrams.
**termco** adds stopword removal, min/max character filtering, and
stemming to **quanteda**'s `collocation` as well as a generic `plot`
method.

    x <- presidential_debates_2012[["dialogue"]]

    ngram_collocations(x)

    ##            collocation frequency        G2          X2       pmi      dice
    ##  1:          make sure       127 1271.0126   18973.589  5.010625 0.6827957
    ##  2:    governor romney       105 1178.5814   22616.632  5.374568 0.7342657
    ##  3:         four years        63  660.7728   14501.009  5.442233 0.6000000
    ##  4:   mister president        61  629.7792   11474.923  5.241191 0.4747082
    ##  5:      united states        31  422.7861   23741.541  6.641570 0.7654321
    ##  6:       middle class        30  370.0621   14816.826  6.203729 0.5714286
    ##  7:          last four        27  250.7039    5000.537  5.228369 0.3529412
    ##  8:    last four years        27  971.5841 1724704.275 11.062062 0.2084942
    ##  9:        middle east        26  340.0439   14557.426  6.328892 0.5360825
    ## 10:        health care        26  319.7309   14729.556  6.340881 0.6046512
    ## 11:    american people        26  195.0577    2128.716  4.422605 0.1947566
    ## 12:   small businesses        22  260.7927   11438.391  6.255470 0.5365854
    ## 13:        making sure        19  151.8800    2018.957  4.679492 0.1890547
    ## 14:     million people        17  119.0044    1155.814  4.241919 0.1338583
    ## 15:         dodd frank        15  266.5001   39798.000  7.883522 1.0000000
    ## 16: federal government        15  151.0503    4156.951  5.629194 0.3030303
    ## 17:       young people        15  123.5549    1643.548  4.708807 0.1287554
    ## 18:     small business        13  144.8151    5842.920  6.110745 0.3768116
    ## 19:  governor romney's        13  141.9289    2927.581  5.421088 0.1375661
    ## 20:      middle income        13  137.3467    4289.989  5.802799 0.2795699

    ngram_collocations(x, gram.length = 3)

    ##                 collocation frequency        G2         X2       pmi
    ##  1:         last four years        27  971.5841 1724704.28 11.062062
    ##  2:   thousand nine hundred        11  397.1371 3072410.33 12.539329
    ##  3:    twenty three million        11  259.6493 2513947.30 12.339475
    ##  4:   middle class families        10  449.7899 3048521.22 12.626088
    ##  5:    governor romney says         8 1158.2886  192222.97  9.991399
    ##  6:   thousand five hundred         8  347.5014  694552.92 11.366460
    ##  7:    governor romney said         6 1124.7911   39138.23  8.149088
    ##  8:         next four years         6  693.2538  175953.18 10.218342
    ##  9:  middle income families         6  317.3088 1360460.91 12.328836
    ## 10:    three million people         6  219.8922  119729.34  9.893721
    ## 11:       four years closer         5  522.4227  875348.03 12.061195
    ## 12:    dollar five trillion         5  313.6099  397529.29 11.267278
    ## 13:    dollar seven hundred         5  158.7751  493914.84 11.499557
    ## 14: hundred sixteen billion         5  138.6161 7274433.62 14.190439
    ## 15:   seven hundred sixteen         5  135.3639 2932291.59 13.281838
    ## 16:         five point plan         5  113.9890  567955.03 11.640329
    ## 17:       five million jobs         5  112.5139  145903.55 10.280819
    ## 18:    dollar four thousand         4  192.7733   64472.06  9.663346
    ## 19:    american people safe         4  192.0857  325883.97 11.305044
    ## 20: three million americans         4  150.8102  417336.21 11.553169
    ##           dice
    ##  1: 0.20849421
    ##  2: 0.17741935
    ##  3: 0.18032787
    ##  4: 0.17857143
    ##  5: 0.05387205
    ##  6: 0.10596026
    ##  7: 0.02933985
    ##  8: 0.05217391
    ##  9: 0.11214953
    ## 10: 0.04067797
    ## 11: 0.06711409
    ## 12: 0.07246377
    ## 13: 0.07352941
    ## 14: 0.14925373
    ## 15: 0.11904762
    ## 16: 0.08064516
    ## 17: 0.04854369
    ## 18: 0.03755869
    ## 19: 0.03375527
    ## 20: 0.06896552

    ngram_collocations(x, order.by = "dice")

    ##                 collocation frequency        G2       X2      pmi
    ##  1:              dodd frank        15 266.50007 39798.00 7.883522
    ##  2:         standard bearer         4  81.64188 39798.00 9.205278
    ##  3:            apology tour         3  62.95758 39798.00 9.492960
    ##  4:   intellectual property         3  62.95758 39798.00 9.492960
    ##  5:            joint chiefs         3  62.95758 39798.00 9.492960
    ##  6:           onest century         3  62.95758 39798.00 9.492960
    ##  7: unintended consequences         2  43.59364 39798.00 9.898425
    ##  8:      appleton wisconsin         2  43.59364 39798.00 9.898425
    ##  9:         abraham lincoln         2  43.59364 39798.00 9.898425
    ## 10:      permanent resident         2  43.59364 39798.00 9.898425
    ## 11:              boca raton         2  43.59364 39798.00 9.898425
    ## 12:           raton florida         2  43.59364 39798.00 9.898425
    ## 13:          prime minister         2  43.59364 39798.00 9.898425
    ## 14:         haqqani network         2  43.59364 39798.00 9.898425
    ## 15:             wall street         9 162.59463 35817.30 8.288987
    ## 16:      planned parenthood         5  94.41404 33164.17 8.799812
    ## 17:             food stamps         9 158.66522 32560.36 8.193677
    ## 18:        self deportation         4  76.63786 31837.60 8.982134
    ## 19:        cleveland clinic         3  58.45891 29847.75 9.205278
    ## 20:    religious minorities         3  58.45891 29847.75 9.205278
    ##          dice
    ##  1: 1.0000000
    ##  2: 1.0000000
    ##  3: 1.0000000
    ##  4: 1.0000000
    ##  5: 1.0000000
    ##  6: 1.0000000
    ##  7: 1.0000000
    ##  8: 1.0000000
    ##  9: 1.0000000
    ## 10: 1.0000000
    ## 11: 1.0000000
    ## 12: 1.0000000
    ## 13: 1.0000000
    ## 14: 1.0000000
    ## 15: 0.9473684
    ## 16: 0.9090909
    ## 17: 0.9000000
    ## 18: 0.8888889
    ## 19: 0.8571429
    ## 20: 0.8571429

### Collocation Plotting

    plot(ngram_collocations(x))

![](tools/figure/unnamed-chunk-8-1.png)

    plot(ngram_collocations(x), drop.redundant.yaxis.text = FALSE)

![](tools/figure/unnamed-chunk-8-2.png)

    plot(ngram_collocations(x, gram.length = 3))

![](tools/figure/unnamed-chunk-8-3.png)

    plot(ngram_collocations(x, order.by = "dice"))

![](tools/figure/unnamed-chunk-8-4.png)

Converting to Document Term Matrix
----------------------------------

Regular expression counts can be useful features in machine learning
models. The **tm** package's `DocumentTermMatrix` is a popular data
structure for machine learning in **R**. The `as_dtm` and `as_tdm`
functions are useful for coercing the count `data.table` structure of a
`term_count` object into a `DocumentTermMatrix`/`TermDocumentMatrix`.
The result can be combined with token/word only `DocumentTermMatrix`
structures using `cbind` & `rbind`.

    as_dtm(markers)

    ## <<DocumentTermMatrix (documents: 10, terms: 4)>>
    ## Non-/sparse entries: 27/13
    ## Sparsity           : 32%
    ## Maximal term length: 14
    ## Weighting          : term frequency (tf)

    cosine_distance <- function (x, ...) {
        x <- t(slam::as.simple_triplet_matrix(x))
        stats::as.dist(1 - slam::crossprod_simple_triplet_matrix(x)/(sqrt(slam::col_sums(x^2) %*% 
            t(slam::col_sums(x^2)))))
    }


    mod <- hclust(cosine_distance(as_dtm(markers)))
    plot(mod)
    rect.hclust(mod, k = 5, border = "red")

![](tools/figure/unnamed-chunk-9-1.png)

    (clusters <- cutree(mod, 5))

    ##     OBAMA.time 1     OBAMA.time 2     OBAMA.time 3    ROMNEY.time 1 
    ##                1                1                1                2 
    ##    ROMNEY.time 2    ROMNEY.time 3   CROWLEY.time 2    LEHRER.time 1 
    ##                2                2                3                4 
    ##  QUESTION.time 2 SCHIEFFER.time 3 
    ##                5                3

Building an Expert Rules, Regex Classifier Model
================================================

Machine learning models of classification are great when you have known
tags to train with because the model scales. Qualitative, expert based
human coding is terrific for when you have no tagged data. However, when
you have a larger, untagged data set the machine learning approaches
have no outcome to learn from and the data is too large to classify by
hand. One solution is to use a expert rules, regular expression approach
that is somewhere between machine learning and hand coding. This is one
solution for tagging larger, untagged data sets. Additionally, when each
text element contains larger chunks of text, [unsupervised clustering
type algorithms](https://github.com/trinker/clustext) such as k-means,
non-negative matrix factorization, hierarchical clustering, or [topic
modeling](https://github.com/trinker/topicmodels_learning) may be of use
for creating clusters that could be interpreted and treated as
categories.

This example section highlights the types of function combinations and
order for a typical expert rules classification. This task typically
involves the combined use of available literature, close examinations of
term usage within text, and researcher experience. Building a classifier
model requires the researcher to build a list of regular expressions
that map to a category or tag. Below I outline minimal work flow for
classification.

Note that the user may want to begin with a classification model
template that contains subdirectories and files for a classification
project. The `classification_project` generates this template with a
pre-populated *'classification.R'* script that can guide the user
through the modeling process. The directory tree looks like the
following:

    template
        |
        |   .Rproj
        |   
        +---categories
        |       categories.R
        |       
        +---data
        +---output
        +---plots
        +---reports
        \---scripts
                01_data_cleaning.R
                02_classification.R

Load the Tools/Data
-------------------

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(dplyr, ggplot2, termco)

    data(presidential_debates_2012)

Splitting Data
--------------

Many classification techniques require the data to be split into a
training and test set to allow the researcher to observe how a model
will perform on a new data set. This also prevents over-fitting the
data. The `split_data` function allows easy splitting of `data.frame` or
`vector` data by integer or proportion. The function returns a named
list of the data set into a `train` and `test` set. The printed view is
a truncated version of the returned list with `|...` indicating there
are additional observations.

    set.seed(111)
    (pres_deb_split <- split_data(presidential_debates_2012, .75))

    ## split_data:
    ## 
    ## train: n = 2184
    ## # A tibble: 6 × 5
    ##      person    tot   time      role
    ##      <fctr>  <chr> <fctr>    <fctr>
    ## 1   CROWLEY  230.2 time 2 moderator
    ## 2 SCHIEFFER   48.1 time 3 moderator
    ## 3    ROMNEY  98.15 time 2 candidate
    ## 4    ROMNEY 173.12 time 2 candidate
    ## 5     OBAMA  102.6 time 2 candidate
    ## 6     OBAMA 120.16 time 2 candidate
    ## # ... with 1 more variables: dialogue <chr>
    ## |...
    ## 
    ## test: n = 728
    ## # A tibble: 6 × 5
    ##   person   tot   time      role
    ##   <fctr> <chr> <fctr>    <fctr>
    ## 1 LEHRER   1.1 time 1 moderator
    ## 2 ROMNEY   2.2 time 1 candidate
    ## 3 ROMNEY   4.4 time 1 candidate
    ## 4 ROMNEY   4.5 time 1 candidate
    ## 5 ROMNEY   4.7 time 1 candidate
    ## 6 ROMNEY  4.17 time 1 candidate
    ## # ... with 1 more variables: dialogue <chr>
    ## |...

The training set can be accessed via `pres_deb_split$train`; likewise,
the test set can be accessed by way of `pres_deb_split$test`.

Here I show splitting by integer.

    split_data(presidential_debates_2012, 100)

    ## split_data:
    ## 
    ## train: n = 100
    ## # A tibble: 6 × 5
    ##   person    tot   time      role
    ##   <fctr>  <chr> <fctr>    <fctr>
    ## 1  OBAMA  102.4 time 2 candidate
    ## 2 ROMNEY 122.26 time 3 candidate
    ## 3 ROMNEY 166.16 time 3 candidate
    ## 4 ROMNEY 162.18 time 3 candidate
    ## 5  OBAMA   20.3 time 2 candidate
    ## 6 ROMNEY  59.12 time 1 candidate
    ## # ... with 1 more variables: dialogue <chr>
    ## |...
    ## 
    ## test: n = 2812
    ## # A tibble: 6 × 5
    ##   person   tot   time      role
    ##   <fctr> <chr> <fctr>    <fctr>
    ## 1 LEHRER   1.1 time 1 moderator
    ## 2 LEHRER   1.2 time 1 moderator
    ## 3 ROMNEY   2.1 time 1 candidate
    ## 4 ROMNEY   2.2 time 1 candidate
    ## 5 LEHRER   3.1 time 1 moderator
    ## 6 ROMNEY   4.1 time 1 candidate
    ## # ... with 1 more variables: dialogue <chr>
    ## |...

I could have trained on the training set and tested on the testing set
in the following examples around modeling but have chosen not to for
simplicity.

Understanding Term Use
----------------------

In order to build the named list of regular expressions that map to a
category/tag the researcher must understand the terms (particularly
information salient terms) in context. The understanding of term use
helps the researcher to begin to build a mental model of the topics
being used in a fashion similar to qualitative coding techniques. Broad
categories will begin to coalesce as word use is elucidated. It forms
the initial names of the "named list of regular expressions". Of course
building the regular expressions in the regex model building step will
allow the researcher to see new ways in which terms are used as well as
new important terms. This in turn will reshape, remove, and add names to
the "named list of regular expressions". This recursive process is
captured in the model below.

<img src="tools/figure/model2.png" width="400" alt="model">

### View Most Used Words

A common task in building a model is to understand the most frequent
words while excluding less information rich function words. The
`frequnt_terms` function produces an ordered data frame of counts. The
researcher can exclude stop words and limit the terms to contain n
characters between set thresholds. The output is ordered by most to
least frequent n terms but can be rearranged alphabetically.

    presidential_debates_2012 %>%
        with(frequent_terms(dialogue))

    ##    term      frequency
    ## 1  going     271      
    ## 2  make      217      
    ## 3  people    214      
    ## 4  governor  204      
    ## 5  president 194      
    ## 6  said      178      
    ## 7  want      173      
    ## 8  sure      156      
    ## 9  just      134      
    ## 10 will      125      
    ## 11 years     118      
    ## 12 jobs      116      
    ## 13 romney    110      
    ## 14 also      102      
    ## 15 know       97      
    ## 16 four       94      
    ## 17 world      92      
    ## 18 well       91      
    ## 19 right      88      
    ## 20 think      88

    presidential_debates_2012 %>%
        with(frequent_terms(dialogue, 40)) %>%
        plot()

![](tools/figure/unnamed-chunk-14-1.png)

A cumulative percent can give a different view of the term usage. The
`plot_cum_percent` function converts a `frequent_terms` output into a
cumulative percent plot. Additionally, `ngram_collocations` + `plot` can
give insight into the frequently occurring ngrams.

    presidential_debates_2012 %>%
        with(frequent_terms(dialogue, 40)) %>%
        plot_cum_percent()

![](tools/figure/unnamed-chunk-15-1.png)

It may also be helpful to view the unique contribution of terms on the
coverage excluding all elements from the match vector that were
previously matched by another term. The `hierarchical_coverage_term` and
accompanying `plot` method allows for hierarchical exploration of the
unique coverage of terms.

    terms <- presidential_debates_2012 %>%
        with(frequent_terms(dialogue, 30)) %>%
        `[[`("term")

    presidential_debates_2012 %>%
        with(hierarchical_coverage_term(dialogue, terms))

    ##          term       unique cumulative
    ## 1       going 0.0834478022  0.0834478
    ## 2        make 0.0576923077  0.1411401
    ## 3      people 0.0515109890  0.1926511
    ## 4    governor 0.0583791209  0.2510302
    ## 5   president 0.0480769231  0.2991071
    ## 6        said 0.0295329670  0.3286401
    ## 7        want 0.0305631868  0.3592033
    ## 8        sure 0.0058379121  0.3650412
    ## 9        just 0.0223214286  0.3873626
    ## 10       will 0.0212912088  0.4086538
    ## 11      years 0.0240384615  0.4326923
    ## 12       jobs 0.0164835165  0.4491758
    ## 13     romney 0.0003434066  0.4495192
    ## 14       also 0.0127060440  0.4622253
    ## 15       know 0.0106456044  0.4728709
    ## 16       four 0.0051510989  0.4780220
    ## 17      world 0.0116758242  0.4896978
    ## 18       well 0.0144230769  0.5041209
    ## 19      right 0.0157967033  0.5199176
    ## 20      think 0.0113324176  0.5312500
    ## 21    america 0.0099587912  0.5412088
    ## 22     number 0.0109890110  0.5521978
    ## 23       back 0.0058379121  0.5580357
    ## 24       need 0.0082417582  0.5662775
    ## 25      first 0.0065247253  0.5728022
    ## 26     middle 0.0054945055  0.5782967
    ## 27   thousand 0.0085851648  0.5868819
    ## 28       time 0.0082417582  0.5951236
    ## 29    economy 0.0075549451  0.6026786
    ## 30 government 0.0082417582  0.6109203
    ## 31       work 0.0068681319  0.6177885

    presidential_debates_2012 %>%
        with(hierarchical_coverage_term(dialogue, terms)) %>%
        plot(use.terms = TRUE)

![](tools/figure/unnamed-chunk-16-1.png)

### View Most Used Words in Context

Much of the exploration of terms in context in effort to build the named
list of regular expressions that map to a category/tag involves
recursive views of frequent terms in context. The `probe` family of
functions can generate lists of function calls (and copy them to the
clipboard for easy transfer) allowing the user to circulate through term
lists generated from other **termco** tools such as `frequent_terms`.
This is meant to standardize and speed up the process.

The first `probe_` tool makes a list of function calls for `search_term`
using a term list. Here I show just 10 terms from `frequent_terms`. This
can be pasted into a script and then run line by line to explore the
frequent terms in context.

    presidential_debates_2012 %>%
        with(frequent_terms(dialogue, 10)) %>%
        select(term) %>%
        unlist() %>%
        probe_list("presidential_debates_2012$dialogue") 

    ## search_term(presidential_debates_2012$dialogue, "going")
    ## search_term(presidential_debates_2012$dialogue, "make")
    ## search_term(presidential_debates_2012$dialogue, "people")
    ## search_term(presidential_debates_2012$dialogue, "governor")
    ## search_term(presidential_debates_2012$dialogue, "president")
    ## search_term(presidential_debates_2012$dialogue, "said")
    ## search_term(presidential_debates_2012$dialogue, "want")
    ## search_term(presidential_debates_2012$dialogue, "sure")
    ## search_term(presidential_debates_2012$dialogue, "just")
    ## search_term(presidential_debates_2012$dialogue, "will")

The next `probe_` function generates a list of
`search_term_collocations` function calls (`search_term_collocations`
wraps `search_term` with `frequent_terms` and eliminates the search term
from the output). This allows the user to systematically explore the
words that frequently collocate with the original terms.

    presidential_debates_2012 %>%
        with(frequent_terms(dialogue, 5)) %>%
        select(term) %>%
        unlist() %>%
        probe_colo_list("presidential_debates_2012$dialogue") 

    ## search_term_collocations(presidential_debates_2012$dialogue, "going")
    ## search_term_collocations(presidential_debates_2012$dialogue, "make")
    ## search_term_collocations(presidential_debates_2012$dialogue, "people")
    ## search_term_collocations(presidential_debates_2012$dialogue, "governor")
    ## search_term_collocations(presidential_debates_2012$dialogue, "president")

As `search_term_collocations` has a `plot` method the user may wish to
generate function calls similar to `probe_colo_list` but wrapped with
`plot` for a visual exploration of the data. The `probe_colo_plot_list`
makes a list of such function calls, whereas the `probe_colo_plot` plots
the output directly to a single external .pdf file.

    presidential_debates_2012 %>%
        with(frequent_terms(dialogue, 5)) %>%
        select(term) %>%
        unlist() %>%
        probe_colo_plot_list("presidential_debates_2012$dialogue") 

    ## plot(search_term_collocations(presidential_debates_2012$dialogue, "going"))
    ## plot(search_term_collocations(presidential_debates_2012$dialogue, "make"))
    ## plot(search_term_collocations(presidential_debates_2012$dialogue, "people"))
    ## plot(search_term_collocations(presidential_debates_2012$dialogue, "governor"))
    ## plot(search_term_collocations(presidential_debates_2012$dialogue, "president"))

The plots can be generated externally with the `probe_colo_plot`
function which makes multi-page .pdf of frequent terms bar plots; one
plot for each term.

    presidential_debates_2012 %>%
        with(frequent_terms(dialogue, 5)) %>%
        select(term) %>%
        unlist() %>%
        probe_colo_plot("presidential_debates_2012$dialogue") 

### View Important Words

It may also be useful to view top
[min-max](http://stats.stackexchange.com/a/70807/7482) scaled tf-idf
weighted terms to allow the more information rich terms to bubble to the
top. The `important_terms` function allows the user to do exactly this.
The function works similar to `term_count` but with an information
weight.

    presidential_debates_2012 %>%
        with(important_terms(dialogue, 10))

    ##         term    tf_idf
    ## 1   governor 1.0000000
    ## 2      right 0.8061417
    ## 3      going 0.7180599
    ## 4  president 0.6906233
    ## 5        get 0.6435831
    ## 6       want 0.6361092
    ## 7       said 0.6349331
    ## 8      thank 0.5509088
    ## 9        one 0.5338552
    ## 10      well 0.5163518

Building the Model
------------------

To build a model the researcher created a named list of regular
expressions that map to a category/tag. This is fed to the `term_count`
function. `term_count` allows for aggregation by grouping variables but
for building the model we usually want to get observation level counts.
Set `grouping.var = TRUE` to generate an `id` column of 1 through number
of observation which gives the researcher the observation level counts.

    discoure_markers <- list(
        response_cries = c("\\boh", "\\bah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    model <- presidential_debates_2012 %>%
        with(term_count(dialogue, grouping.var = TRUE, discoure_markers))

    model

    ## Coverage: 13.02% 
    ## # A tibble: 2,912 × 6
    ##       id n.words response_cries back_channels summons justification
    ##    <int>   <int>          <int>         <int>   <int>         <int>
    ## 1      1      10              0             0       0             0
    ## 2      2       9              1             0       0             0
    ## 3      3      14              0             0       0             0
    ## 4      4      14              0             0       0             0
    ## 5      5       5              1             0       0             0
    ## 6      6       5              0             0       0             0
    ## 7      7      40              0             0       0             0
    ## 8      8       2              0             0       0             0
    ## 9      9      20              0             0       2             0
    ## 10    10      13              0             0       1             0
    ## # ... with 2,902 more rows

Testing the Model
-----------------

In building a classifier the researcher is typically concerned with
coverage, discrimination, and accuracy. The first two are easier to
obtain while accuracy is not possible to compute without a comparison
sample of expertly tagged data.

We want our model to be assigning tags to as many of the text elements
as possible. The `coverage` function can provide an understanding of
what percent of the data is tagged. Our model has relatively low
coverage, indicating the regular expression model needs to be improved.

    model %>%
        coverage()

    ## Coverage    : 13.0%
    ## Coverered   :   379
    ## Not Covered : 2,533

Understanding how well our model discriminates is important as well. We
want the model to cover as close to 100% of the data as possible, but
likely want fewer tags assigned to each element. If the model is tagging
many tags to each element it is not able to discriminate well. The
`as_terms` + `plot_freq` function provides a visual representation of
the model's ability to discriminate. The output is a bar plot showing
the distribution of the number of tags at the element level. The goal is
to have a larger density at 1 tag. Note that the plot also gives a view
of coverage, as the zero bar shows the frequency of elements that could
not be tagged. Our model has a larger distribution of 1 tag compared to
the &gt;1 tag distributions, though the coverage is very poor. As the
number of tags increases the ability of the model to discriminate
typically lessens. There is often a trade off between model coverage and
discrimination.

    model %>%
        as_terms() %>%
        plot_freq(size=3) + xlab("Number of Tags")

![](tools/figure/unnamed-chunk-24-1.png)

We may also want to see the distribution of the tags as well. The
combination of `as_terms` + `plot_counts` gives the distribution of the
tags. In our model the majority of tags are applied to the **summons**
category.

    model %>%
        as_terms() %>%
        plot_counts() + xlab("Tags")

![](tools/figure/unnamed-chunk-25-1.png)

Improving the Model
-------------------

### Improving Coverage

The model does not have very good coverage. To improve this the
researcher will want to look at the data with no coverage to try to
build additional regular expressions and categories. This requires
understanding language, noticing additional features of the data with no
coverage that may map to categories, and building regular expressions to
model these features. This section will outline some of the tools that
can be used to detect features and build regular expressions to model
these language features.

We first want to view the untagged data. The `uncovered` function
provides a logical vector that can be used to extract the text with no
tags.

    untagged <- get_uncovered(model)

    head(untagged)

    ## [1] "We'll talk about specifically about health care in a moment."                                                                                                                                              
    ## [2] "What I support is no change for current retirees and near retirees to Medicare."                                                                                                                           
    ## [3] "And the president supports taking dollar seven hundred sixteen billion out of that program."                                                                                                               
    ## [4] "So that's that's number one."                                                                                                                                                                              
    ## [5] "Number two is for people coming along that are young, what I do to make sure that we can keep Medicare in place for them is to allow them either to choose the current Medicare program or a private plan."
    ## [6] "Their choice."

The `frequent_terms` function can be used again to understand common
features of the untagged data.

    untagged %>%
        frequent_terms()

    ##    term      frequency
    ## 1  going     211      
    ## 2  governor  177      
    ## 3  president 172      
    ## 4  people    169      
    ## 5  make      166      
    ## 6  said      149      
    ## 7  want      130      
    ## 8  sure      110      
    ## 9  just      107      
    ## 10 will      103      
    ## 11 years     101      
    ## 12 jobs       96      
    ## 13 romney     95      
    ## 14 know       82      
    ## 15 four       81      
    ## 16 also       78      
    ## 17 america    77      
    ## 18 right      76      
    ## 19 well       74      
    ## 20 world      72

We may see a common term such as the word *right* and want to see what
other terms collocate with it. Using a regular expression that searches
for multiple terms can improve a model's accuracy and ability to
discriminate. Using `search_term` in combination with `frequent_terms`
can be a powerful way to see which words tend to collocate. Here I pass
a regex for *right* (`\\bright`) to `search_term`. This pulls up the
text that contains this term. I then use `frequent_terms` to see what
words frequently occur with the word *right*. We notice the word
*people* tends to occur with *right*.

    untagged %>%
        search_term("\\bright") %>%
        frequent_terms(10, stopwords = "right")

    ##    term       frequency
    ## 1  that       32       
    ## 2  have       12       
    ## 3  people     10       
    ## 4  with        9       
    ## 5  this        8       
    ## 6  government  7       
    ## 7  course      6       
    ## 8  going       6       
    ## 9  it's        6       
    ## 10 president   6       
    ## 11 that's      6       
    ## 12 want        6       
    ## 13 you're      6

The `search_term_collocations` function provides a convenient wrapper
for `search_term` + `frequent_terms` which also removes the search term
from the output.

    untagged %>%
        search_term_collocations("\\bright", n=10)

    ##    term       frequency
    ## 1  people     10       
    ## 2  government  7       
    ## 3  course      6       
    ## 4  going       6       
    ## 5  president   6       
    ## 6  want        6       
    ## 7  also        5       
    ## 8  governor    5       
    ## 9  jobs        5       
    ## 10 make        5

This is an exploratory act. Finding the right combination of features
that occur together requires lots of recursive noticing, trialling,
testing, reading, interpreting, and deciding. After we noticed that the
terms *people* and *course* appear with the term *right* above we will
want to see these text elements. We can use a grouped-or expression with
`colo` to build a regular expression that will search for any text
elements that contain these two terms anywhere. `colo` is more powerful
than initially shown here; I demonstrate further functionality below.
Here is the regex produced.

    colo("\\bright", "(people|course)")

    ## [1] "((\\bright.*(people|course))|((people|course).*\\bright))"

This is extremely powerful when used inside of `search_term` as the text
containing this regular expression will be returned along with the
coverage proportion on the uncovered data.

    search_term(untagged, colo("\\bright", "(people|course)"))

    ##  [1] "Right now, the CBO says up to twenty million people will lose their insurance as Obamacare goes into effect next year."                                                                                                                                                                                                  
    ##  [2] "The federal government taking over health care for the entire nation and whisking aside the tenth Amendment, which gives states the rights for these kinds of things, is not the course for America to have a stronger, more vibrant economy."                                                                           
    ##  [3] "And what we're seeing right now is, in my view, a a trickle down government approach, which has government thinking it can do a better job than free people pursuing their drea Miss And it's not working."                                                                                                              
    ##  [4] "And the challenges America faces right now look, the reason I'm in this race is there are people that are really hurting today in this country."                                                                                                                                                                         
    ##  [5] "It's going to help people across the country that are unemployed right now."                                                                                                                                                                                                                                             
    ##  [6] "That's not the right course for America."                                                                                                                                                                                                                                                                                
    ##  [7] "The right course for America is to have a true all of the above policy."                                                                                                                                                                                                                                                 
    ##  [8] "When you've got thousands of people right now in Iowa, right now in Colorado, who are working, creating wind power with good paying manufacturing jobs, and the Republican senator in that in Iowa is all for it, providing tax breaks to help this work and Governor Romney says I'm opposed."                          
    ##  [9] "When it comes to community colleges, we are setting up programs, including with Nassau Community College, to retrain workers, including young people who may have dropped out of school but now are getting another chance, training them for the jobs that exist right now."                                            
    ## [10] "That's not the right course for us."                                                                                                                                                                                                                                                                                     
    ## [11] "The right course for us is to make sure that we go after the the people who are leaders of these various anti American groups and these these jihadists, but also help the Muslim world."                                                                                                                                
    ## [12] "And so the right course for us, is working through our partners and with our own resources, to identify responsible parties within Syria, organize them, bring them together in a in a form of if not government, a form of of of council that can take the lead in Syria."                                              
    ## [13] "And it's widely reported that drones are being used in drone strikes, and I support that and entirely, and feel the president was right to up the usage of that technology, and believe that we should continue to use it, to continue to go after the people that represent a threat to this nation and to our friends."
    ## [14] "People can look it up, you're right."                                                                                                                                                                                                                                                                                    
    ## [15] "Those are the kinds of choices that the American people face right now."                                                                                                                                                                                                                                                 
    ## attr(,"coverage")
    ## [1] 0.005921832

We notice right away that the phrase *right course* appears often. We
can create a search with just this expression.

***Note*** *that the decision to include a regular expression in the
model is up to the researcher. We must guard against over-fitting the
model, making it not transferable to new, similar contexts.*

    search_term(untagged, "right course")

    ## [1] "That's not the right course for America."                                                                                                                                                                                                                                  
    ## [2] "The right course for America is to have a true all of the above policy."                                                                                                                                                                                                   
    ## [3] "That's not the right course for us."                                                                                                                                                                                                                                       
    ## [4] "The right course for us is to make sure that we go after the the people who are leaders of these various anti American groups and these these jihadists, but also help the Muslim world."                                                                                  
    ## [5] "And so the right course for us, is working through our partners and with our own resources, to identify responsible parties within Syria, organize them, bring them together in a in a form of if not government, a form of of of council that can take the lead in Syria."
    ## attr(,"coverage")
    ## [1] 0.001973944

Based on the `frequent_terms` output above, the word *jobs* also seems
important. Again, we use the `search_term` + `frequent_terms` combo to
extract words collocating with *jobs*.

    search_term_collocations(untagged, "jobs", n=15)

    ##    term          frequency
    ## 1  million       17       
    ## 2  create        15       
    ## 3  going         15       
    ## 4  back          12       
    ## 5  country       11       
    ## 6  people        10       
    ## 7  make           9       
    ## 8  sure           9       
    ## 9  five           8       
    ## 10 hundred        8       
    ## 11 overseas       8       
    ## 12 want           8       
    ## 13 years          8       
    ## 14 businesses     7       
    ## 15 companies      7       
    ## 16 creating       7       
    ## 17 energy         7       
    ## 18 good           7       
    ## 19 just           7       
    ## 20 manufacturing  7       
    ## 21 thousand       7

As stated above, `colo` is a powerful search tool as it can take
multiple regular expressions as well as allowing for multiple negations
(i.e., find x but not if y). To include multiple negations use a
grouped-or regex as shown below.

    ## Where do `jobs` and `create` collocate?
    search_term(untagged, colo("jobs", "create")) 

    ##  [1] "If I'm president I will create help create twelve million new jobs in this country with rising incomes."                                                                                                                                                                     
    ##  [2] "I know what it takes to create good jobs again."                                                                                                                                                                                                                             
    ##  [3] "And what I want to do, is build on the five million jobs that we've created over the last thirty months in the private sector alone."                                                                                                                                        
    ##  [4] "It's going to help those families, and it's going to create incentives to start growing jobs again in this country."                                                                                                                                                         
    ##  [5] "We created twenty three million new jobs."                                                                                                                                                                                                                                   
    ##  [6] "two million new jobs created."                                                                                                                                                                                                                                               
    ##  [7] "We've created five million jobs, and gone from eight hundred jobs a month being lost, and we are making progress."                                                                                                                                                           
    ##  [8] "He keeps saying, Look, I've created five million jobs."                                                                                                                                                                                                                      
    ##  [9] "eight percent, between that period the end of that recession and the equivalent of time to today, Ronald Reagan's recovery created twice as many jobs as this president's recovery."                                                                                         
    ## [10] "This is the way we're going to create jobs in this country."                                                                                                                                                                                                                 
    ## [11] "We have to be competitive if we're going to create more jobs here."                                                                                                                                                                                                          
    ## [12] "We need to create jobs here."                                                                                                                                                                                                                                                
    ## [13] "And it's estimated that that will create eight hundred thousand new jobs."                                                                                                                                                                                                   
    ## [14] "That's not the way we're going to create jobs here."                                                                                                                                                                                                                         
    ## [15] "The way we're going to create jobs here is not just to change our tax code, but also to double our exports."                                                                                                                                                                 
    ## [16] "That's going to help to create jobs here."                                                                                                                                                                                                                                   
    ## [17] "Government does not create jobs."                                                                                                                                                                                                                                            
    ## [18] "Government does not create jobs."                                                                                                                                                                                                                                            
    ## [19] "Barry, I think a lot of this campaign, maybe over the last four years, has been devoted to this nation that I think government creates jobs, that that somehow is the answer."                                                                                               
    ## [20] "And when it comes to our economy here at home, I know what it takes to create twelve million new jobs and rising take home pay."                                                                                                                                             
    ## [21] "And Governor Romney wants to take us back to those policies, a foreign policy that's wrong and reckless, economic policies that won't create jobs, won't reduce our deficit, but will make sure that folks at the very top don't have to play by the same rules that you do."
    ## attr(,"coverage")
    ## [1] 0.008290565

    ## Where do `jobs`, `create`,  and the word `not` collocate?
    search_term(untagged, colo("jobs", "create", "(not|'nt)")) 

    ## [1] "That's not the way we're going to create jobs here."                                                        
    ## [2] "The way we're going to create jobs here is not just to change our tax code, but also to double our exports."
    ## [3] "Government does not create jobs."                                                                           
    ## [4] "Government does not create jobs."                                                                           
    ## attr(,"coverage")
    ## [1] 0.001579155

    ## Where do `jobs` and`create` collocate without a `not` word?
    search_term(untagged, colo("jobs", "create", not = "(not|'nt)")) 

    ##  [1] "If I'm president I will create help create twelve million new jobs in this country with rising incomes."                                                                                                                                                                     
    ##  [2] "I know what it takes to create good jobs again."                                                                                                                                                                                                                             
    ##  [3] "And what I want to do, is build on the five million jobs that we've created over the last thirty months in the private sector alone."                                                                                                                                        
    ##  [4] "It's going to help those families, and it's going to create incentives to start growing jobs again in this country."                                                                                                                                                         
    ##  [5] "We created twenty three million new jobs."                                                                                                                                                                                                                                   
    ##  [6] "two million new jobs created."                                                                                                                                                                                                                                               
    ##  [7] "We've created five million jobs, and gone from eight hundred jobs a month being lost, and we are making progress."                                                                                                                                                           
    ##  [8] "He keeps saying, Look, I've created five million jobs."                                                                                                                                                                                                                      
    ##  [9] "eight percent, between that period the end of that recession and the equivalent of time to today, Ronald Reagan's recovery created twice as many jobs as this president's recovery."                                                                                         
    ## [10] "This is the way we're going to create jobs in this country."                                                                                                                                                                                                                 
    ## [11] "We have to be competitive if we're going to create more jobs here."                                                                                                                                                                                                          
    ## [12] "We need to create jobs here."                                                                                                                                                                                                                                                
    ## [13] "And it's estimated that that will create eight hundred thousand new jobs."                                                                                                                                                                                                   
    ## [14] "That's going to help to create jobs here."                                                                                                                                                                                                                                   
    ## [15] "Barry, I think a lot of this campaign, maybe over the last four years, has been devoted to this nation that I think government creates jobs, that that somehow is the answer."                                                                                               
    ## [16] "And when it comes to our economy here at home, I know what it takes to create twelve million new jobs and rising take home pay."                                                                                                                                             
    ## [17] "And Governor Romney wants to take us back to those policies, a foreign policy that's wrong and reckless, economic policies that won't create jobs, won't reduce our deficit, but will make sure that folks at the very top don't have to play by the same rules that you do."
    ## attr(,"coverage")
    ## [1] 0.006711409

    ## Where do `jobs`, `romney`, and `create` collocate?
    search_term(untagged, colo("jobs", "create", "romney")) 

    ## [1] "And Governor Romney wants to take us back to those policies, a foreign policy that's wrong and reckless, economic policies that won't create jobs, won't reduce our deficit, but will make sure that folks at the very top don't have to play by the same rules that you do."
    ## attr(,"coverage")
    ## [1] 0.0003947888

Here is one more example with `colo` for the words *jobs* and
*overseas*. The user may want to quickly test and then transfer the
regex created by `colo` to the regular expression list. By setting
`options(termco.copy2clip = TRUE)` the user globally sets `colo` to use
the **clipr** package to copy the regex to the clipboard for better work
flow.

    search_term(untagged, colo("jobs", "overseas")) 

    ## [1] "And everything that I've tried to do, and everything that I'm now proposing for the next four years in terms of improving our education system or developing American energy or making sure that we're closing loopholes for companies that are shipping jobs overseas and focusing on small businesses and companies that are creating jobs here in the United States, or closing our deficit in a responsible, balanced way that allows us to invest in our future."
    ## [2] "You can ship jobs overseas and get tax breaks for it."                                                                                                                                                                                                                                                                                                                                                                                                                
    ## [3] "The outsourcing of American jobs overseas has taken a toll on our economy."                                                                                                                                                                                                                                                                                                                                                                                           
    ## [4] "Making sure that we're bringing manufacturing back to our shores so that we're creating jobs here, as we've done with the auto industry, not rewarding companies that are shipping jobs overseas."                                                                                                                                                                                                                                                                    
    ## [5] "I know Americans had seen jobs being shipped overseas; businesses and workers not getting a level playing field when it came to trade."                                                                                                                                                                                                                                                                                                                               
    ## [6] "Having a tax code that rewards companies that are shipping jobs overseas instead of companies that are investing here in the United States, that will not make us more competitive."                                                                                                                                                                                                                                                                                  
    ## [7] "And the one thing that I'm absolutely clear about is that after a decade in which we saw drift, jobs being shipped overseas, nobody championing American workers and American businesses, we've now begun to make some real progress."                                                                                                                                                                                                                                
    ## [8] "And I've put forward a plan to make sure that we're bringing manufacturing jobs back to our shores by rewarding companies and small businesses that are investing here, not overseas."                                                                                                                                                                                                                                                                                
    ## attr(,"coverage")
    ## [1] 0.00315831

The researcher uses an iterative process to continue to build the
regular expression list. The `term_count` function builds the matrix of
counts to further test the model. The use of (a) `coverage`, (b)
`as_terms` + `plot_counts`, and (c) `as_terms` + `freq_counts` will
allow for continued testing of model functioning.

### Improving Discrimination

It is often desirable to improve discrimination. While the bar plot
highlighting the distribution of the number of tags is useful, it only
indicates if there is a problem, not where the problem lies. The
`tag_co_occurrence` function produces a list of `data.frame` and
`matrices` that aide in understanding how to improve discrimination.
This list is useful, but the `plot` method provides an improved visual
view of the co-occurrences of tags.

The network plot on the left shows the strength of relationships between
tags, while the plot on the right shows the average number of other tags
that co-occur with each regex tag. In this particular case the plot
combo is not complex because of the limited number of regex tags. Note
that the edge strength is relative to all other edges. The strength has
to be considered in the context of the average number of other tags that
co-occur with each regex tag bar/dot plot on the right. As the number of
tags increases the plot increases in complexity. The unconnected nodes
and shorter bars represent the tags that provide the best discriminatory
power, whereas the other tags have the potential to be redundant.

    tag_co_occurrence(model) %>%
        plot(min.edge.cutoff = .01)

![](tools/figure/impr_disc-1.png)

Categorizing/Tagging
--------------------

The `classify` function enables the researcher to apply *n* tags to each
text element. Depending on the text and the regular expression list's
ability, multiple tags may be applied to a text. The `n` argument allows
the maximum number of tags to be set though the function does not
guarantee this many (or any) tags will be assigned.

Here I show the `head` of the returned vector (if `n` &gt; 1 a `list`
may be returned) as well as a `table` and plot of the counts. Use
`n = Inf` to return all tags.

    classify(model) %>%
        head()

    ## [1] NA               "response_cries" NA               NA              
    ## [5] "response_cries" NA

    classify(model) %>%
        unlist() %>%
        table()

    ## .
    ##  back_channels  justification response_cries        summons 
    ##              6            121             16            236

    classify(model) %>%
        unlist() %>%
        plot_counts() + xlab("Tags")

![](tools/figure/unnamed-chunk-36-1.png)

Evaluation: Accuracy
--------------------

### Pre Coded Data

The `evaluate` function is a more formal method of evaluation than
`validate_model`. The `evaluate` function yields a test a model's
accuracy, precision, and recall using macro and micro averages of the
confusion matrices for each tag as outlined by [Dan Jurafsky & Chris
Manning](https://www.youtube.com/watch?v=OwwdYHWRB5E&index=31&list=PL6397E4B26D00A269).
The function requires a known, human coded sample. In the example below
I randomly generate "known human coded tagged" vector. Obviously, this
is for demonstration purposes. The model outputs a pretty printing of a
list. Note that if a larger, known tagging set of data is available the
user may want to strongly consider machine learning models (see:
[**RTextTools**](https://cran.r-project.org/package=RTextTools)).

This minimal example will provide insight into the way the evaluate
scores behave:

    known <- list(1:3, 3, NA, 4:5, 2:4, 5, integer(0))
    tagged <- list(1:3, 3, 4, 5:4, c(2, 4:3), 5, integer(0))
    evaluate(tagged, known)

    ## ----------------------------------------------- 
    ## Tag Level Measures
    ## ----------------------------------------------- 
    ##           tag precision recall F_score accuracy
    ##             1     1.000  1.000   1.000    1.000
    ##             2     1.000  1.000   1.000    1.000
    ##             3     1.000  1.000   1.000    1.000
    ##             4      .667  1.000    .800     .857
    ##             5     1.000  1.000   1.000    1.000
    ## No_Code_Given      .000   .000    .000     .857
    ## 
    ## -------------------- 
    ## Summary Measures
    ## -------------------- 
    ## N:                 7
    ## 
    ## Macro-Averaged  
    ##   Accuracy:     .952
    ##   F-score:      .800
    ##   Precision:    .778
    ##   Recall:       .833
    ## 
    ## Micro-Averaged  
    ##   Accuracy:     .952
    ##   F-score:      .909
    ##   Precision:    .909
    ##   Recall:       .909

Below we create fake "known" tags to test `evaluate` with real data
(though the comparison is fabricated).

    mod1 <- presidential_debates_2012 %>%
        with(term_count(dialogue, TRUE, discoure_markers)) %>%
        classify()

    fake_known <- mod1
    set.seed(1)
    fake_known[sample(1:length(fake_known), 300)] <- "random noise"

    evaluate(mod1, fake_known)

    ## ------------------------------------------------ 
    ## Tag Level Measures
    ## ------------------------------------------------ 
    ##            tag precision recall F_score accuracy
    ##  back_channels     1.000  1.000   1.000    1.000
    ##  justification      .917  1.000    .957     .997
    ##  No_Code_Given      .896  1.000    .945     .909
    ##   random noise      .000   .000    .000     .897
    ## response_cries      .812  1.000    .897     .999
    ##        summons      .903  1.000    .949     .992
    ## 
    ## -------------------- 
    ## Summary Measures
    ## -------------------- 
    ## N:             2,912
    ## 
    ## Macro-Averaged  
    ##   Accuracy:     .966
    ##   F-score:      .791
    ##   Precision:    .755
    ##   Recall:       .833
    ## 
    ## Micro-Averaged  
    ##   Accuracy:     .966
    ##   F-score:      .897
    ##   Precision:    .897
    ##   Recall:       .897

### Post Coding Data

It is often useful to less formally, validate a model via human
evaluation; checking that text is being tagged as expected. This
approach is more formative and less rigorous than `evaluate`, intended
to be used to assess model functioning in order to improve it. The
`validate_model` provides an interactive interface for a single
evaluator to sample n tags and corresponding texts and assess the
accuracy of the tag to the text. The `assign_validation_task` generates
an external file(s) for n coders for redundancy of code assessments.
This may be of use in [Mechanical
Turk](https://www.mturk.com/mturk/welcome) type applications. The
example below demonstrates `validate_model`'s `print`/`summary` and
`plot` outputs.

    validated <- model %>%
        validate_model()

After `validate_model` has been run the `print`/`summary` and `plot`
provides an accuracy of each tag and a confidence level (note that the
confidence band is highly affected by the number of samples per tag).

    validated

    ## -------
    ## Overall:
    ## -------
    ##    accuracy   n sampled  se lower upper
    ## 1:    59.6% 484      57 .06 46.9% 72.4%
    ## 
    ## 
    ## ---------------
    ## Individual Tags:
    ## ---------------
    ##               tag accuracy   n sampled  se lower  upper
    ## 1:  back_channels    83.3%   7       6 .15 53.5% 100.0%
    ## 2: response_cries    72.7%  13      11 .13 46.4%  99.0%
    ## 3:  justification    55.0% 155      20 .11 33.2%  76.8%
    ## 4:        summons    50.0% 309      20 .11 28.1%  71.9%

    plot(validated)

![](tools/figure/unnamed-chunk-41-1.png)

These examples give guidance on how to use the tools in the **termco**
package to build an expert rules, regular expression text classification
model.