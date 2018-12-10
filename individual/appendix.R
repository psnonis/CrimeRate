# appendix.R

pModel <- function(m, title)
{
    autoplot(m, smooth.colour = "#dd4814", smooth.size = 1, alpha = 0.8, size = 2) +
    theme(title = element_text(size = 10, face = 'bold', family = 'sans'),
          plot.background = element_rect(fill = "#f8f2e4"))
}

pDiagnostics <- function(m, which)
{
    par(mfrow = c(1, 1))
    autoplot(m, which = which, ncol = 1, smooth.colour = "#dd4814", smooth.size = 1, alpha = 0.8, size = 2) +
        theme(title = element_text(size = 10, face = 'bold', family = 'sans'),
              plot.background = element_rect(fill = "#f8f2e4"))
}

pHist <- function(x, breaks, label)
{
    ggplot() +
    geom_histogram(aes(y = ..density.., x = x),
                   bins = breaks, fill = '#ffbb33', colour = '#000000') +
    geom_vline(xintercept = mean(x),
               linetype = 'dashed', size = 1.5, colour = '#0d47a1') +
    stat_function(aes(x = x), fun = dnorm, colour = '#dd4814',
                  size = 1.5, args = list(mean = mean(x), sd = sd(x))) +
    ggtitle(label) + theme_wsj() +
    theme(title = element_text( size = 10, face = 'bold', family = 'sans'),
          plot.background = element_rect(fill = "#f8f2e4"))
}

pScatterMatrix <- function(data, columns)
{
    ggscatmat(data, columns = columns, alpha = 0.5) +
    geom_smooth(method = 'lm', colour = '#dd4814') + theme_wsj()
}

mSetup <- function()
{
    cfips           <<- county.fips[grepl('north carolina', county.fips$polyname),]
    cfips           <<- rename(cfips, county = fips, subregion = polyname)
    cfips$county    <<- cfips$county - 37000
    cfips$subregion <<- gsub('north carolina,(.*?)', '\\1', cfips$subregion)
    cfips$subregion <<- gsub(':.*','',cfips$subregion)
    cfips           <<- cfips[!duplicated(cfips$subregion),]
    rownames(cfips) <<- NULL

    ncmap           <<- subset(map_data('county'), region == 'north carolina')
    ncmap           <<- left_join(ncmap, cfips, by = 'subregion')
    rownames(ncmap) <<- NULL
    ncmap           <<- left_join(ncmap, crime, by = 'county')
}

mMapNC <- function(column, title)
{
    ggplot(data = ncmap,
           mapping = aes_string(x = 'long', y = 'lat', group = 'group', fill = column)) +
    geom_polygon(color = 'black', size = 0.025) +
    coord_map(projection = 'albers', lat0 = 39, lat1 = 45) + theme_wsj() +
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
            Spread   = list(x),
            Histogram = list(hist(x, plot = F)$counts))
    })))
}

sTable <- function(tab){
    formattable(tab, list(CrimeCOR = color_bar('#33b5e5'), SD = color_bar('#ff8800'),
                          Spread = function(z){
                              sapply(z, function(zz){
                                  knit(text = sprintf('`r sparkline(c(%s), type="box")`',
                                                      paste0(zz, collapse = ',')), quiet = T)})},
                          Scatter = function(z){
                              sapply(z, function(zz){
                                  knit(text = sprintf('`r sparkline(c(%s), type="line", fillColor=F)`',
                                                      paste0(zz, collapse = ',')), quiet = T)})},
                          Histogram = function(z){
                              sapply(z, function(zz){
                                  knit(text = sprintf('`r sparkline(c(%s), type="bar")`',
                                                      paste0(zz, collapse = ',')), quiet = T)})}
    ))
}

import <- function(package)
{
    if(!require(package, character.only = T))
        suppressWarnings(install.packages(package, dep = T))
    suppressWarnings(library(package, character.only = T))
}
