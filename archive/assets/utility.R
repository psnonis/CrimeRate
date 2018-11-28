import <- function(package, devtools = F, version = NA)
{
    if(devtools == T)
    {
        uri <- package
        package <- tail(strsplit(package, '/')[[1]], 1)

        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : GHUB :', uri))
            suppressWarnings(devtools::install_github(uri))
        }
        else
        {
            if(packageVersion(package) < version)
            {
                print(paste( ' Upgrading Package : GHUB :', uri))
                suppressWarnings(devtools::install_github(uri))
            }
        }
    }
    else
    {
        if(!require(package, character.only = T))
        {
            print(paste( 'Installing Package : CRAN :', package))
            suppressWarnings(install.packages(package, dep = T))
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
        theme_economist() +
        scale_colour_economist()
}

mSetup <- function()
{
    cfips           <<- county.fips[grepl('north carolina', county.fips$polyname),] # fips for NC counties
    cfips           <<- rename(cfips, county = fips, subregion = polyname)          # conform column names
    cfips$county    <<- cfips$county - 37000                                        # conform to crime_v2
    cfips$subregion <<- gsub('north carolina,(.*?)', '\\1', cfips$subregion)        # remove state name
    cfips$subregion <<- gsub(':.*','',cfips$subregion)                              # fix currituck county
    cfips           <<- cfips[!duplicated(cfips$subregion),]                        # del currituck duplicates
    rownames(cfips) <<- NULL                                                        # reset row names

    ncmap           <<- subset(map_data('county'), region == 'north carolina') # geo locations for NC counties
    ncmap           <<- left_join(ncmap, cfips, by = 'subregion')              # append county fips codes
    rownames(ncmap) <<- NULL                                                   # reset row names
    ncmap           <<- left_join(ncmap, crime, by = 'county')                 # append crime data
}

mMapNC <- function(column, title)
{
    ggplot(data = ncmap, mapping = aes_string(x = 'long', y = 'lat', group = 'group', fill = column)) +
        geom_polygon(color = 'black', size = 0.025) + theme_wsj() +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        theme(legend.position = 'bottom',
              text = element_text(size = 12, family = 'sans'),
              title = element_text(size = 16, family = 'sans'),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle(title, subtitle = 'North Carolina') +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')))
}

sBuild <- function(dat,out){
    tbl_df(t(sapply(1:ncol(dat), function(n){
        x <- crime[,n]
        c(  Variable =   colnames(dat)[n], CrimeCOR  = round(cor(out,x), 2),
            Mean     = round( mean(x), 2), Median    = round( median(x), 2),
            Min      = round(  min(x), 2), Max       = round(    max(x), 2),
            SD       = round(   sd(x), 2),
            Spread   = list(x),            Histogram = list(hist(x, plot = F)$counts))
    })))
}

sTable <- function(tab){
    formattable(tab, list(CrimeCOR = color_bar("powderblue"), SD = color_bar("orange"),
                          Spread = function(z){
                              sapply(z, function(zz){
                                  knit(text = sprintf("`r sparkline(c(%s), type='box')`",
                                                      paste0(zz, collapse = ",")), quiet = T)})},
                          Histogram = function(z){
                              sapply(z, function(zz){
                                  knit(text = sprintf("`r sparkline(c(%s), type='bar')`",
                                                      paste0(zz, collapse = ",")), quiet = T)})}
    ))
}
