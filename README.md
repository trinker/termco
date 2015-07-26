termco
============


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

    ## 
    ## Attaching package: 'termco'
    ## 
    ## The following object is masked from 'package:qdap':
    ## 
    ##     weight

    data(pres_debates2012)

    discoure_markers <- lapply(list(
        response_cries = c("oh", "ah", "aha", "ouch", "yuk"),
        back_channels = c("uh[- ]huh", "uhuh", "yeah"),
        summons = "hey",
        justification = "because"
    ), qdapRegex::bind)

    with(pres_debates2012, term_count(dialogue, list(person, time), discoure_markers))

    ## Source: local data frame [10 x 7]
    ## 
    ##       person   time n.words response_cries back_channels summons
    ## 1      OBAMA time 1    3599              0             0       0
    ## 2      OBAMA time 2    7477        1(.01%)             0       0
    ## 3      OBAMA time 3    7243              0       1(.01%)       0
    ## 4     ROMNEY time 1    4085              0             0 1(.02%)
    ## 5     ROMNEY time 2    7536              0       3(.04%)       0
    ## 6     ROMNEY time 3    8303        1(.01%)             0 1(.01%)
    ## 7    CROWLEY time 2    1672              0             0       0
    ## 8     LEHRER time 1     765        1(.13%)       3(.39%)       0
    ## 9   QUESTION time 2     583        2(.34%)             0       0
    ## 10 SCHIEFFER time 3    1445              0             0       0
    ## Variables not shown: justification (chr)