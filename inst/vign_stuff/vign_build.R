header <- readLines('inst/vign_stuff/header.R')
rmd <- readLines('README.Rmd')

rmd <- rmd[grep("^\\*\\*termco\\*\\* is", rmd):length(rmd)]
rmd <- rmd[!seq_along(rmd) %in% grep("^# Installation", rmd):(1 + grep("^## Examples", rmd))]
rmd <- gsub("(^#)(#+)", "\\2", rmd)

ind <- grep("The following examples", rmd)


rmd[ind] <- paste0("# Examples\n\n", rmd[ind])

cat(paste(c(header, "", rmd), collapse="\n"), file="vignettes/intro-termco.Rmd")

