import <- function(package, devtools = F, version = NA)
{
    if(devtools == T)
    {
        uri <- package
        package <- tail(strsplit(package, '/')[[1]], 1)

        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : GHUB :', uri))
            supressWarnings(devtools::install_github(uri))
        }
        else
        {
            if(packageVersion(package) < version)
            {
                print(paste( ' Upgrading Package : GHUB :', uri))
                supressWarnings(devtools::install_github(uri))
            }
        }
    }
    else
    {
        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : CRAN :', package))
            supressWarnings(install.packages(package, dep = T))
        }
    }

    suppressWarnings(library(package, character.only = T))
}

pHist <- function(x, breaks, label)
{
    ggplot() +
        geom_histogram(aes(y=..density..,x=x), bins=breaks, fill="yellow", colour="black") +
        geom_vline(xintercept=mean(x), linetype="dashed", size=1, colour="blue") +
        stat_function(aes(x=x), fun = dnorm, colour = "red", size = 1,
                      args=list(mean=mean(x), sd=sd(x))) +
        xlab(label) +
        theme_economist()
}
