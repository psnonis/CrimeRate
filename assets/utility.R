import <- function(package, devtools = F, version = NA)
{
    if(devtools == T)
    {
        uri <- package
        package <- tail(strsplit(package, '/')[[1]], 1)

        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : GHUB :', uri))
            devtools::install_github(uri)
        }
        else
        {
            if(packageVersion(package) < version)
            {
                print(paste( ' Upgrading Package : GHUB :', uri))
                devtools::install_github(uri)
            }
        }
    }
    else
    {
        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : CRAN :', package))

            install.packages(package, dep = T)
        }
    }

    suppressWarnings(library(package, character.only = T))
}
