#Get the devtools package
install.packages("devtools")

library(devtools)
library(knitr)
library(kableExtra)
library(prettydoc)


#install the geosparr package and browse its cxontents on a browser

install_github("agroimpacts/naturewave", build_vignettes = TRUE)

browseVignettes("naturewave")



