######################################
## Exper Rules Regex Categorization ##
######################################
##==================
## Load Dependencies
##==================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(termco, qdapRegex, stringi, dplyr, ggplot2, magrittr, {{read_package}})

## Setup
options(termco.copy2clip = TRUE)
## It is recommended that the user does a search and replace  on this file for
##    the following term, replacing it with the name of the text variable column
##    name in your data set:
##        '<<INSERT_TEXT_VAR_NAME_HERE>>'

##=====
## Data
##=====
## Load Data
dat <- {{read_package}}::read_{{read_file_type}}("data/<<INSERT_DATA_FILE_NAME_HERE>>.{{read_file_type}}")

## Split Data Into Training & Test Sets
set.seed(111)
(split_dat <- split_data(dat, .5))
train <- split_dat$train
testing <- split_dat$test

##=========
## Modeling
##=========
## Inspect the Data (Frequent Terms)
n <- 100
(freqs <- train %$%
    frequent_terms(<<INSERT_TEXT_VAR_NAME_HERE>>, n = n))

train %$%
    frequent_terms(<<INSERT_TEXT_VAR_NAME_HERE>>, n = n) %>%
    plot()

train %$%
    frequent_terms(<<INSERT_TEXT_VAR_NAME_HERE>>, n = n) %>%
    plot(as.cloud=TRUE)

# Frequent Ngram
train %$%
    frequent_ngrams(<<INSERT_TEXT_VAR_NAME_HERE>>, n = 50) %>%
    plot()

## Systematically View Frequent Terms in Context
probe_colo_list(freqs[[1]], 'train$<<INSERT_TEXT_VAR_NAME_HERE>>')

## Systematically View Frequent Term Collocations
probe_colo_plot_list(freqs[[1]], 'train$<<INSERT_TEXT_VAR_NAME_HERE>>')




## Generate Model (term_list) File
## if (!file.exists("{{categories_file}}")) {term_list_template(path = "{{categories_file}}")}
file.edit("{{categories_file}}")
cats <- read_term_list("{{categories_file}}")

##================
## Build the Model
##================
## Apply Model (term list)
model <- train %$%
    term_count(<<INSERT_TEXT_VAR_NAME_HERE>>, grouping.var = TRUE,  term.list = cats)


## Coverage
model %>%
    coverage()

# Discrimination
model %>%
    as_terms() %>%
    plot_freq(size=3) +
        xlab("Number of Tags")

tag_co_occurrence(model) %>%
    plot()

# Category Loadings
model %>%
    as_terms() %>%
    plot_counts() +
        xlab("Tags")

##--------------------
## Improving the Model
##--------------------
untagged <- get_uncovered(model)


untagged %>%
    frequent_terms()


## Terms That Collocate with a Frequent Term
untagged %>%
    search_term("termA") %>%
    frequent_terms(10, stopwords = tm::stopwords("en"))

untagged %>%
    frequent_terms(5) %>%
    select(term) %>%
    unlist() %>%
    probe_colo_plot(untagged)

## Collocation Regex
colo("\\btermA", "(termB|termC)")


## Classification
classify(model) %>%
    plot()

## Inspect the Data (Frequent Terms)
n <- 100
(freqs <- untagged %>%
    frequent_terms(n = n))

untagged %>%
    frequent_terms(n = n) %>%
    plot()

untagged %>%
    frequent_terms(n = n) %>%
    plot(as.cloud=TRUE)

# Frequent Ngram
untagged %>%
    frequent_ngrams(n = 50) %>%
    plot()

## Systematically View Frequent Terms in Context
probe_colo_list(freqs[[1]], 'untagged')

## Systematically View Frequent Term Collocations
probe_colo_plot_list(freqs[[1]], 'untagged'>)



## Coverage on Testing Data
testing %$%
    term_count(<<INSERT_TEXT_VAR_NAME_HERE>>, grouping.var = TRUE, cats) %>%
    coverage()


## Model Validation
validated <- model %>%
    validate_model()


