header <- readLines('inst/vign_stuff/header.R')
rmd <- readLines('README.Rmd')

rmd <- rmd[grep("^\\*\\*termco\\*\\* is", rmd):length(rmd)]
rmd <- rmd[!seq_along(rmd) %in% grep("^# Installation", rmd):(1 + grep("^## Examples", rmd))]
rmd <- gsub("(^#)(#+)", "\\2", rmd)

ind <- grep("The following examples", rmd)

img <- c("```{r, echo=FALSE, results='asis'}", "uri_embed(\"r_termco.png\",",
"    \"width=\\\"250\\\", height=\\\"150\\\" style=\\\"display:block; margin-left:auto; margin-right:auto;\\\"\")",
"```\n\n")

rmd[ind] <- paste0(paste(img, collapse="\n"), "# The Main Function: `termc_count`\n\n", rmd[ind])

cat(paste(c(header, "", rmd), collapse="\n"), file="vignettes/intro-termco.Rmd")

