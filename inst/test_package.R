system("R CMD Rd2pdf --pdf ../PCAmixdata")
system("R CMD build ../PCAmixdata")
system("R CMD check --as-cran ../PCAmixdata")
library(PCAmixdata)
devtools::check(,cran=TRUE)
#http://xmpalantir.wu.ac.at/cransubmit/