library(stringi)

# Read test data
nonres <- read.csv('names-non-residential.csv')
res <- read.csv('names-residential.csv')
test_set <- c(nonres$name, res$name)

# Apply regex
matched <- stri_detect(test_set, regex = pattern)

# Check
cat('Successfully matching:', paste0(sum(matched), '/', nrow(nonres)), 'non-residential names\n',
    'Successfully skipping:', paste0(sum(matched == F), '/', nrow(res)), 'residential names\n',
    'Unmatched by the pattern:', paste0(test_set[!matched], collapse = ', '))