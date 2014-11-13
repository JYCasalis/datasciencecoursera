corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## 
    ## Expected results:
    ##
    ## source("corr.R")
    ## source("complete.R")
    ## cr <- corr("specdata", 150)
    ## head(cr)
    ## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
    ## summary(cr)
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
    ## cr <- corr("specdata", 400)
    ## head(cr)
    ## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
    ## summary(cr)
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
    ## cr <- corr("specdata", 5000)
    ## summary(cr)
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 
    ## length(cr)
    ## [1] 0
    ## cr <- corr("specdata")
    ## summary(cr)
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
    ## length(cr)
    ## [1] 323
    
    correlate <- function(fname) {
        # Read data from file
        data <- read.csv(file.path(directory, fname))
        
        # Retrieve number of complete observations
        nobs <- sum(complete.cases(data))
        
        # If enough observations
        if (nobs > threshold) {
            # Compute correlation between nitrate & sulfate
            cor(data$nitrate, data$sulfate, use="complete.obs")
        }
    }
    
    tcorrs <- sapply(list.files(directory), correlate) 
    # Remove NULL elements returned by correlate
    tcorrs[!is.null(tcorrs)]
}
