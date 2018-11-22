crime <- read.csv('crime_v2.csv')
crime <- na.omit(crime)
crime <- crime[!duplicated(crime$county),]
crime$prbconv <- as.numeric(as.character(crime$prbconv))

library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(maps)

cfips           <- county.fips[grepl('north carolina', county.fips$polyname),] # fips for NC counties
cfips           <- rename(cfips, county = fips, subregion = polyname)          # conform column names
cfips$county    <- cfips$county - 37000                                        # conform to crime_v2
cfips$subregion <- gsub('north carolina,(.*?)', '\\1', cfips$subregion)        # remove state name
cfips$subregion <- gsub(':.*','',cfips$subregion)                              # fix currituck county
cfips           <- cfips[!duplicated(cfips$subregion),]                        # del currituck duplicates

ncmap <- subset(map_data('county'), region == 'north carolina') # geo locations for NC counties
ncmap <- left_join(ncmap, cfips, by = 'subregion')              # append county fips codes
rownames(ncmap) <- NULL                                         # reset row names

ncmap <- left_join(ncmap, crime, by = 'county') # append crime data

map <- function(column, title)
{
    ggplot(data = ncmap, mapping = aes_string(x = 'long', y = 'lat', group = 'group', fill = column)) +
        geom_polygon(color = 'black', size = 0.05) + theme_wsj() +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        theme(legend.position = 'bottom',
              text = element_text(size = 12, family = 'sans'),
              title = element_text(size = 16, family = 'sans'),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle(title, subtitle = 'North Carolina') +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')))
}

map('crmrte', 'Crime Rate per Capita')

library(corrplot)

crime.numeric     <- crime[, !names(crime) %in% c('county','year','west','central','urban')]
crime.correlation <- round(cor(crime.numeric, use = 'pairwise.complete.obs'), 2 )
crime.correlation.sqrt <- round(cor(sqrt(crime.numeric), use = 'pairwise.complete.obs'), 2)

?cor

corrplot(crime.correlation,         method = 'circle', type = 'lower',  order = 'FPC', diag = F)
corrplot(crime.correlation.log_log, method = 'circle', type = 'lower',  order = 'FPC', diag = F)
corrplot(crime.correlation.sqrt, method = 'circle', type = 'lower',  order = 'FPC', diag = F)


qqnorm(sqrt(crime$crmrte))
qqline(sqrt(crime$crmrte), col='red')
hist(sqrt(crime$crmrte))

library(knitr)

crime.summary <- data.frame(t(mapply(function(x)
{
    list(
        min  = round(min(x),3),
        mean = round(mean(x),3),
        max  = round(max(x),3),
        cor  = round(cor(x,crime$crmrte),3)
    )
}, crime)))

tab <- kable(crime.summary)

knit(text=tab)

mo<-function(x,y,xt,yt)
{
    x<-xt(x)
    y<-yt(y)

    plot(y~x)
    abline(lm(y~x))
}

mo(crime$density,crime$crmrte,log,)
