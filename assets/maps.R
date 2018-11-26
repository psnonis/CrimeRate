cfips           <- county.fips[grepl('north carolina', county.fips$polyname),] # fips for NC counties
cfips           <- rename(cfips, county = fips, subregion = polyname)          # conform column names
cfips$county    <- cfips$county - 37000                                        # conform to crime_v2
cfips$subregion <- gsub('north carolina,(.*?)', '\\1', cfips$subregion)        # remove state name
cfips$subregion <- gsub(':.*','',cfips$subregion)                              # fix currituck county
cfips           <- cfips[!duplicated(cfips$subregion),]                        # del currituck duplicates
rownames(cfips) <- NULL

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
