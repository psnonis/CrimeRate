
+

crime$population85  <- cpops$population85

library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(maps)
library(stargazer)

cpops           <- read.csv('population_1985.csv')

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

map('crmrte', 'Crime Rate per Capita')

cfips

library(corrplot)

crime.numeric     <- crime[, !names(crime) %in% c('county','year','west','central','urban')]
crime.correlation <- round(cor(crime.numeric, use = 'pairwise.complete.obs'), 2 )
crime.correlation.sqrt <- round(cor(sqrt(crime.numeric), use = 'pairwise.complete.obs'), 2)


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

qq<-function(x,y)
{
    qqnorm(lm(y,x))
    qqline(lm(y,x))
}

mo<-function(x,y,xt,yt)
{
    x<-xt(x)
    y<-yt(y)

    plot(y~x)
    abline(lm(y~x))
}

mo(crime$density,crime$crmrte,log,log)
qq(crime$density,crime$crmrte)
skewness(crime$crmrte)

qqnorm(residuals(lm(log(crmrte)~density, data = crime)))
stargazer(crime)


nrow(crime[crime$prbconv<0 | crime$prbconv>1,])

f1 <- log(crime$crmrte) ~ crime$prbarr + crime$prbconv + crime$prbpris + crime$avgsen + crime$density
m1 <- lm(f1)
s1 <- summary(m1)

s1$r.squared

summary(lm(crmrte~taxpc, data = crime))$r.squared

crime.explanatory <- c('density','prbarr','prbconv','prbpris','avgsen','mix','polpc','taxpc','pctmin80','pctymle','wcon','wfed','wfir','wloc','wmfg','wser','wsta','wtrd','wtuc')

m = vector()

for (x in names(crime.numeric)){

    y1 <- paste(       'crime$crmrte'    )
    y2 <- paste('log(','crime$crmrte',')')

    x1 <- paste(       'crime$', x     )
    x2 <- paste('log(','crime$', x, ')')

    r1 <- summary(lm(paste(y1, '~', x1)))$r.squared
    r2 <- summary(lm(paste(y2, '~', x1)))$r.squared
    r3 <- summary(lm(paste(y2, '~', x2)))$r.squared

    print(sprintf('%8s : %.2f %.2f %.2f', x, r1, r2, r3))

    m[x] = lm(paste(y1, '~', x1))
}

pHist <- function(x)
{
    ggplot() + geom_histogram(aes(y=..density..,x=x), bins=20, fill="yellow", colour="black") +
        geom_vline(xintercept=mean(x),linetype="dashed",size=1,colour="blue") +
        stat_function(aes(x=x), fun = dnorm, colour = "red", size = 1, args = list(mean = mean(x), sd = sd(x)))
}


pHist(log(crime$crmrte))

crime.correlation <- as.data.frame(as.table(cor(crime.numeric)))
crime.correlation[crime.correlation$Var1=='crmrte',]

plot(crime$density)

crime$taxpc_1k <- crime$taxpc*10000
summary(lm(log(crime$crmrte)~crime$prbarr+crime$prbconv+crime$prbpris))$r.squared
coef(lm(log(crime$crmrte)~crime$prbarr+crime$prbconv+crime$prbpris+crime$avgsen+crime$density+crime$taxpc+crime$pctmin80+crime$pctymle))

coef(lm(log(crime$crmrte)~crime$density))
plot(crime$density,log(crime$crmrte));abline(lm(log(crime$crmrte)~crime$density))

