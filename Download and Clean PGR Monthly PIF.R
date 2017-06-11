
## Parse Progressive's monthly policyholder growth release
monthly.URL <- "http://investors.progressive.com/phoenix.zhtml?c=81824&p=irol-newsArticle&ID=2211492"
results.year <- 2016
results.month <- 9

library(XML)
library(RCurl)
library(rlist)
library(dplyr)

use.url <- getURL(monthly.URL, .opts = list(ssl.verifypeer = FALSE))
tables <- readHTMLTable(use.url)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)

policy.data <- as.data.frame(tables[[3]]) #3rd table is the one we want
policy.data[2, 1] <- 'Policies in Force' #fill in row 2, column 1 with dummy
policy.data <- policy.data[-c(1, 3, 4, 12), ] #remove useless rows

## define a function to take blanks or emptys into nas
empty_as_na <- function(x){
    if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
    ifelse(x %in% c(""," ","NA"), NA, x)
}

## transform all columns
policy.data %>% mutate_each(funs(empty_as_na)) -> policy.data

# define funx to turn a row into a vector and remove the nas
row.collapse <- function(data.set, row.nbr) {
    x <- data.set[row.nbr, ]
    x <- x[!is.na(x)]
}

# Create empty data frame with 4 columns
month.output <- data.frame(v1 = factor(),
                           v2 = factor(),
                           v3 = factor(),
                           v4 = factor())
for (i in 1:nrow(policy.data)) {
    y <- row.collapse(policy.data, i)
    if (i == 1) {
        month.output <- (t(y))
    }
    else {
        month.output <- rbind(month.output, t(y))
    }
}

# Transpose and properly name the output
month.output <- as.data.frame(t(month.output))
colnames(month.output) <- unlist(month.output[1, ])
month.output <- month.output[-1, ]
month.output <- cbind(results.year = rep(results.year, nrow(month.output)),
                      results.month = rep(results.month, nrow(month.output)),
                      month.output)
month.output

#Write output to a csv file
write.location <- paste0('C:/Users/Colin/Desktop/Progressive Results/PIF/', results.year,
                         results.month, '.csv')
write.csv(month.output, write.location)
