termco
============


[![Project Status: Wip - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/0.1.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.org/trinker/termco.svg?branch=master)](https://travis-ci.org/trinker/termco)
[![Coverage
Status](https://coveralls.io/repos/trinker/termco/badge.svg?branch=master)](https://coveralls.io/r/trinker/termco?branch=master)
<a href="https://img.shields.io/badge/Version-0.0.1-orange.svg"><img src="https://img.shields.io/badge/Version-0.0.1-orange.svg" alt="Version"/></a>
</p>
<img src="inst/termco_logo/r_termco.png" width="200" alt="qdapRegex Logo">

**termco** is A small suite of functions used to count terms and
substrings in strings. The package wraps the
[**data.table**](https://cran.r-project.org/web/packages/data.table/index.html)
and
[**stringi**](https://cran.r-project.org/web/packages/stringi/index.html)
packages to create fast data frame counts of regular expression terms
and substrings.


Table of Contents
============

-   [Installation](#installation)
-   [Contact](#contact)
    -   [Examples](#examples)
        -   [Print Method](#print-method)
        -   [Plot Method](#plot-method)

Installation
============


To download the development version of **termco**:

Download the [zip
ball](https://github.com/trinker/termco/zipball/master) or [tar
ball](https://github.com/trinker/termco/tarball/master), decompress and
run `R CMD INSTALL` on it, or use the **pacman** package to install the
development version:

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_gh("trinker/termco")

Contact
=======

You are welcome to: 
* submit suggestions and bug-reports at: <https://github.com/trinker/termco/issues> 
* send a pull request on: <https://github.com/trinker/termco/> 
* compose a friendly e-mail to: <tyler.rinker@gmail.com>


Examples
--------

The following examples demonstrate some of the functionality of
**termco**.

    library(termco); library(qdapRegex)

    data(pres_debates2012)

    discoure_markers <- list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    )

    with(pres_debates2012, term_count(dialogue, list(person, time), discoure_markers))

    ## Coverage: 100% 
    ## Source: local data frame [10 x 7]
    ## 
    ##       person   time n.words response_cries back_channels   summons
    ## 1      OBAMA time 1    3599        4(.11%)             0 43(1.19%)
    ## 2      OBAMA time 2    7477        2(.03%)             0  42(.56%)
    ## 3      OBAMA time 3    7243        4(.06%)       1(.01%)  58(.80%)
    ## 4     ROMNEY time 1    4085        1(.02%)             0  27(.66%)
    ## 5     ROMNEY time 2    7536        6(.08%)       3(.04%)  49(.65%)
    ## 6     ROMNEY time 3    8303        8(.10%)             0 84(1.01%)
    ## 7    CROWLEY time 2    1672        2(.12%)             0   4(.24%)
    ## 8     LEHRER time 1     765        6(.78%)       3(.39%)         0
    ## 9   QUESTION time 2     583        2(.34%)             0         0
    ## 10 SCHIEFFER time 3    1445              0             0   2(.14%)
    ## Variables not shown: justification (chr)

### Print Method

    print(markers, pretty = FALSE)

    ## Coverage: 100% 
    ## Source: local data frame [10 x 7]
    ## 
    ##       person   time n.words response_cries back_channels summons
    ## 1      OBAMA time 1    3599              0             0       0
    ## 2      OBAMA time 2    7477              1             0       0
    ## 3      OBAMA time 3    7243              0             1       0
    ## 4     ROMNEY time 1    4085              0             0       1
    ## 5     ROMNEY time 2    7536              0             3       0
    ## 6     ROMNEY time 3    8303              1             0       1
    ## 7    CROWLEY time 2    1672              0             0       0
    ## 8     LEHRER time 1     765              1             3       0
    ## 9   QUESTION time 2     583              2             0       0
    ## 10 SCHIEFFER time 3    1445              0             0       0
    ## Variables not shown: justification (int)

    print(markers, zero.replace = "_")

    ## Coverage: 100% 
    ## Source: local data frame [10 x 7]
    ## 
    ##       person   time n.words response_cries back_channels summons
    ## 1      OBAMA time 1    3599              _             _       _
    ## 2      OBAMA time 2    7477        1(.01%)             _       _
    ## 3      OBAMA time 3    7243              _       1(.01%)       _
    ## 4     ROMNEY time 1    4085              _             _ 1(.02%)
    ## 5     ROMNEY time 2    7536              _       3(.04%)       _
    ## 6     ROMNEY time 3    8303        1(.01%)             _ 1(.01%)
    ## 7    CROWLEY time 2    1672              _             _       _
    ## 8     LEHRER time 1     765        1(.13%)       3(.39%)       _
    ## 9   QUESTION time 2     583        2(.34%)             _       _
    ## 10 SCHIEFFER time 3    1445              _             _       _
    ## Variables not shown: justification (chr)

### Plot Method

    plot(markers)

![](inst/figure/unnamed-chunk-5-1.png)

    plot(markers, labels=TRUE)

![](inst/figure/unnamed-chunk-5-2.png)