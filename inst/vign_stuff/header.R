---
title: "termco Introduction"
author: "Tyler Rinker"
date: "2015-09-27"
output:
  html_document:
    toc: true
---

<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{termco Introduction}
-->

```{r, echo = FALSE, message = FALSE}
library(knitr);library(termco)
knit_hooks$set(htmlcap = function(before, options, envir) {
  if(!before) {
    paste('<p class="caption"><b><em>',options$htmlcap,"</em></b></p>",sep="")
    }
    })
knitr::opts_knit$set(self.contained = TRUE, cache = FALSE)
knitr::opts_chunk$set(fig.path = "inst/figure/")
```
