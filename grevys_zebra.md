grevy’s\_zebra
================
Ilya
12/15/2018

Mapping preferred habitat of Grevy's zebra
==========================================

Objective: Map habitat quality for Grevy's zebra based on radiocollar and remote sensing data.
----------------------------------------------------------------------------------------------

Background
----------

Grevy's zebra are critically endangered. Grevy's zebra depend on the rangelands of Northern Kenya. To prioritize conservation interventions in this area, it is important to identify the environmental features preferred by Grevy's zebra. We expect land cover, woody biomass, and livestock density to be important influences on the locations chosen by Grevy's zebra. Land cover affects forage availability for Grevy's zebra, which are primarily grazers while also infrequently browsing. Woody biomass, in addition to affecting forage, is a proxy for visibility, which drives predation danger. Livestock represent competition and human activity.

Strategy
--------

Assemble and integrate Grevy's zebra GPS radiocollar and environmental data. Generate "false" Grevy's zebra locations by drawing random moves from observed distributions of step lengths and turning angles. Use machine learning (generalized boosted models) to identify environmental features that predict whether a Grevy's zebra location is true (observed locations) or false. Use model to map habitat suitability based on environmental features.

Data sources
------------

### Grevy's zebra GPS collar locations

### Environmental data

#### Global land cover dataset (European Space Agency)

#### Woody biomass dataset (Bouvet et al.)

#### Livestock density (Food and Agriculture Organization)

Study design
------------

#### install packages needed for study

``` r
# pkgTest is a helper function to load packages and install packages only when they are not installed yet.
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)    
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("raster", "rgdal", "dplyr", "lubridate", "ggplot2",  "geojsonio", 
                    "tmaptools", "geojson", "stplanr", "sf", "tidyverse", "amt", "tibble", "survival", "sp", "lubridate" ,
                    "MuMIn", "lme4", "gamm4", "lmerTest", "ggplot2", "onehot", "scales")

for (package in neededPackages){pkgTest(package)}
```

    ## Loading required package: sp

    ## rgdal: version: 1.3-3, (SVN revision 759)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
    ##  Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/rgdal/gdal
    ##  GDAL binary built with GEOS: FALSE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/rgdal/proj
    ##  Linking to sp version: 1.2-7

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:raster':
    ## 
    ##     intersect, select, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## 
    ## Attaching package: 'geojsonio'

    ## The following object is masked from 'package:base':
    ## 
    ##     pretty

    ## 
    ## Attaching package: 'geojson'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     polygon

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, proj.4 4.9.3

    ## ── Attaching packages ───────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ tibble  1.4.2     ✔ purrr   0.2.4
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ tidyr::extract()         masks raster::extract()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks raster::intersect(), base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ dplyr::select()          masks raster::select()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks raster::union(), base::union()

    ## Loading required package: survival

    ## 
    ## Attaching package: 'amt'

    ## The following object is masked from 'package:geojsonio':
    ## 
    ##     centroid

    ## The following object is masked from 'package:sp':
    ## 
    ##     bbox

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loading required package: mgcv

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmList

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## The following object is masked from 'package:raster':
    ## 
    ##     getData

    ## This is mgcv 1.8-23. For overview type 'help("mgcv-package")'.

    ## This is gamm4 0.2-5

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

### Read in and merge Grevy's zebra GPS radiocollar data.

### GZT 2006-2008

#### read in collar data 2006-2007 from GZT

``` r
#need path name to match data organization  
path <- "GZ collar data/GZ-Collar-Data-2006-2007"
#get list of file names
fns <- list.files(path)
#find the files that have .shp extension
shps <- fns[grepl(".shp", fns)]
#find the locations files (vs. tracks)
shps <- shps[grepl("locations", shps)]
#remove the xml files
shps.xml <- shps[grepl(".xml", shps)]
shps <- setdiff(shps, shps.xml)

#check merge locations file to see what's in it
m <- shapefile(paste(path, "/", "merge_locations.shp", sep = ""))
m.df = data.frame(m)
unique(m$COLLAR_ID)
```

    ##  [1] "T5H-1429" "T5H-1529" "T5H-1427" "T5H-1528" "T5H-1538" "T5H-1536"
    ##  [7] "T5H-1535" "T5H-1534" "T5H-1533" "T5H-1532" "T5H-1531" "T5H-1530"
    ## [13] "T5H-1431" "T5H-1430" "T5H-1428"

``` r
#get list of shpfiles w/o merge file
shps.original = setdiff(shps, "merge_locations.shp")
#get names of zebras 
id.names <- sapply(strsplit(shps.original, "_"), `[`, 1)
#get chronofile name of ids
chrono.names <- sapply(strsplit(shps.original, "_"), `[`, 2)
out = NULL

for (a in 1:length(shps.original)){
  print(a)
  #read in shapefile
  tmp <- shapefile(paste(path, "/", shps.original[a], sep = ""))
  #make into dataframe from shapefile
  tmp<- data.frame(tmp)
  #make names lower case
  names(tmp)<- tolower(names(tmp))
  #assign zebra name
  tmp$id <- id.names[a]
  #assign chronofile.chk (to be checked against chronofile field)
  tmp$chronofile.chk <-chrono.names[a]
  #add records to out
  out = rbind(out, tmp)
}
```

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6
    ## [1] 7
    ## [1] 8
    ## [1] 9
    ## [1] 10
    ## [1] 11
    ## [1] 12
    ## [1] 13
    ## [1] 14
    ## [1] 15
    ## [1] 16
    ## [1] 17
    ## [1] 18

``` r
#confirm that data read into "out"" have same number of rows as in merge file
print(dim(out)[1])==print(dim(m.df)[1])#
```

    ## [1] 60210
    ## [1] 60210

    ## [1] TRUE

``` r
#confirm chronofile matches chronofile.chk
print(unique(out$chronofile==out$chronofile.chk))
```

    ## [1] TRUE

``` r
m0607 <- out
save(m0607,file="m0607.Rdata") 
```

#### get summary (metadata) of 06-07 data

``` r
load("m0607.Rdata")

m0607$date = as.Date(m0607$fixtime, "%Y/%m/%d")
m0607sum <- m0607 %>% 
  group_by(id) %>%
    summarise(
    first.date.GZT = min(date),
    last.date.GZT = max(date),
    count.GZT = n(),
    collar_id = collar_id[1],
    chronofile = chronofile[1])
print(m0607sum)
```

    ## # A tibble: 18 x 6
    ##    id        first.date.GZT last.date.GZT count.GZT collar_id chronofile
    ##    <chr>     <date>         <date>            <int> <chr>          <dbl>
    ##  1 Belinda   2006-09-15     2007-11-30         8083 T5H-1532         125
    ##  2 Dableya   2007-02-25     2007-11-10         6490 T5H-1538         131
    ##  3 Hiroya    2007-02-25     2007-05-31         1225 T5H-1528         141
    ##  4 Jeff      2006-09-17     2006-11-06          885 T5H-1529         122
    ##  5 Johnna    2006-11-16     2007-05-02         4205 T5H-1531         124
    ##  6 Kiana     2006-12-17     2007-01-19          784 T5H-1533         126
    ##  7 Kobosa    2007-02-25     2007-04-21         1396 T5H-1536         129
    ##  8 Lepere    2006-12-17     2007-06-11         4444 T5H-1534         127
    ##  9 Liz       2006-06-16     2007-06-20         6885 T5H-1430         119
    ## 10 Loijuk    2006-06-16     2007-11-30         9773 T5H-1431         120
    ## 11 Martha    2007-07-29     2007-11-30         3248 T5H-1529         163
    ## 12 njeri     2006-06-16     2006-09-07          818 T5H-1428         117
    ## 13 Petra     2006-12-17     2007-04-19         3133 T5H-1535         128
    ## 14 Rose      2006-11-16     2006-11-23          200 T5H-1530         123
    ## 15 Samburu   2006-06-19     2007-02-04         3761 T5H-1429         118
    ## 16 Samburu2  2007-06-21     2007-11-30         3931 T5H-1429         164
    ## 17 Silurian1 2006-06-19     2006-09-17          783 T5H-1427         116
    ## 18 Silurian2 2007-03-01     2007-03-08          166 T5H-1427         142

``` r
m0607sum = data.frame(m0607sum)
save(m0607sum, file = "m0607sum.Rdata") 
```

#### Integrate three sources of metadata for 2006-2008 and add info to location data.

GZT 2006-2007 data have date but not time of day. Read in collar data 2006-2007 from MBB thesis files; these actually go into 2008. Merge metadata extracted from GZT version (m0607sum), with metadata extracted from MBB thesis files (m0606MBBsum), and metadata from Henrik's report (H). Add metadata from Henrik report to location data (m0608MBB)

``` r
load("m0607sum.Rdata")
path <- "Michael/MBB_Thesis/MBB_SCRIPTS/Zebra_csv"

m0608MBB <- read.csv(paste(path, "/", "master.csv", sep = ""))

m0608MBB$name = m0608MBB$id
m0608MBB$Fixtime = as.character(m0608MBB$Fixtime)
m0608MBB$Fixtime <- strptime(m0608MBB$Fixtime, format = "%m/%d/%Y %H:%M", tz = "Africa/Nairobi")
#now get strftime which can be used to get min
m0608MBB$Fixtime <-strftime(m0608MBB$Fixtime)

#MBB data on Silurian are missing from "master", so read those in separately
silurian = "silurian.txt"
sil = read.csv(silurian)
sil$id = "Silurian"
sil$name = "Silurian"
sil$Fixtime <- strptime(sil$Fixtime, format = "%m/%d/%Y %H:%M", tz = "Africa/Nairobi")
#now get strftime which can be used to get min
sil$Fixtime <-strftime(sil$Fixtime)

int.col = intersect(names(sil), names(m0608MBB))
sil = sil[,int.col]
setdiff(names(m0608MBB), names(sil))
```

    ## character(0)

``` r
setdiff(names(sil), names(m0608MBB))
```

    ## character(0)

``` r
m0608MBB = rbind(m0608MBB, sil)

#MBB data on jeff are missing from "master", so read those in separately
jeff = "jeff.csv"
jeff = read.csv(jeff)
jeff$id = "Jeff"
jeff$name = "Jeff"
jeff$Fixtime <- strptime(jeff$Fixtime, format = "%m/%d/%Y %H:%M", tz = "Africa/Nairobi")
#now get strftime which can be used to get min
jeff$Fixtime <-strftime(jeff$Fixtime)
int.col = intersect(names(jeff), names(m0608MBB))
jeff = jeff[,int.col]
setdiff(names(m0608MBB), names(jeff))
```

    ## character(0)

``` r
setdiff(names(jeff), names(m0608MBB))
```

    ## character(0)

``` r
m0608MBB = rbind(m0608MBB, jeff)

m0608MBBsum <- m0608MBB %>% 
  group_by(id) %>%
    summarise(
    collar_id = collar_id[1],
    chronofile = Chronofile[1],
      first.date.MBB = min(Fixtime),
    last.date.MBB = max(Fixtime),
    count.MBB = n()
    )
m0608MBBsum = data.frame(m0608MBBsum)
m0608MBBsum = m0608MBBsum[order(m0608MBBsum$collar_id),]
print(m0608MBBsum)#seems to be missing Silurian
```

    ##           id collar_id chronofile      first.date.MBB       last.date.MBB
    ## 16 Silurian2  T5H-1427        142 2007-03-01 02:00:00 2007-03-08 19:03:00
    ## 17  Silurian  T5H-1427        116 2006-06-19 16:00:00 2006-09-17 14:00:00
    ## 11     Njeri  T5H-1428        117 2006-06-16 12:00:00 2006-09-07 20:00:00
    ## 14   Samburu  T5H-1429        118 2006-06-19 04:00:00 2007-02-04 02:00:00
    ## 15  Samburu2  T5H-1429        164 2007-06-21 14:00:00 2008-02-21 11:00:00
    ## 8        Liz  T5H-1430        119 2006-06-16 15:00:00 2007-06-20 06:00:00
    ## 9     Loijuk  T5H-1431        120 2006-06-16 12:00:00 2009-11-01 00:00:00
    ## 3     Hiroya  T5H-1528        141 2007-02-25 05:48:00 2007-05-31 00:00:00
    ## 10    Martha  T5H-1529        163 2007-07-29 12:00:00 2008-10-13 05:00:00
    ## 18      Jeff  T5H-1529        122 2006-09-17 08:00:00 2006-11-06 06:00:00
    ## 13      Rose  T5H-1530        123 2006-11-16 15:17:00 2006-11-23 03:00:00
    ## 4     Johnna  T5H-1531        124 2006-11-16 13:23:00 2007-05-02 07:00:00
    ## 1    Belinda  T5H-1532        125 2006-09-17 06:00:00 2008-07-27 10:00:00
    ## 5      Kiana  T5H-1533        126 2006-12-17 12:00:00 2008-05-07 04:00:00
    ## 7     Lepere  T5H-1534        127 2006-12-17 13:00:00 2007-06-11 06:00:00
    ## 12     Petra  T5H-1535        128 2006-12-17 12:00:00 2007-04-19 00:00:00
    ## 6     Kobosa  T5H-1536        129 2007-02-25 12:00:00 2007-04-21 05:30:00
    ## 2    Dableya  T5H-1538        131 2007-02-25 12:00:00 2007-11-10 17:00:00
    ##    count.MBB
    ## 16       168
    ## 17       784
    ## 11       822
    ## 14      3780
    ## 15      6121
    ## 8       6980
    ## 9      10973
    ## 3       1236
    ## 10     11697
    ## 18       888
    ## 13       201
    ## 4       4234
    ## 1      10690
    ## 5        800
    ## 7       4530
    ## 12      3173
    ## 6       1413
    ## 2       6558

``` r
#put together m0607sum and m0608MBBsum
intersect(names(m0607sum), names(m0608MBBsum))
```

    ## [1] "id"         "collar_id"  "chronofile"

``` r
setdiff(m0607sum$id, m0608MBBsum$id)
```

    ## [1] "njeri"     "Silurian1"

``` r
setdiff( m0608MBBsum$id, m0607sum$id)
```

    ## [1] "Silurian" "Njeri"

``` r
m0607sum$id[m0607sum$id=="njeri"]="Njeri"
#assume that Silurian = Silurian1
m0608MBBsum$id = as.character(m0608MBBsum$id)
m0608MBBsum$id[m0608MBBsum$id=="Silurian"]="Silurian1"
m0608GZT_MBB = merge(m0607sum, m0608MBBsum)
m0608GZT_MBB$first.data.match = m0608GZT_MBB$first.date.GZT==m0608GZT_MBB$first.date.MBB#all except belind match, and that is only two days off
m0608GZT_MBB$count.MBB>m0608GZT_MBB$count.GZT#check that there are always more data in MBB files -- all TRUE
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [15] TRUE TRUE TRUE TRUE

``` r
#now read in metadata from Henrik
H = read.csv("metadata_STE_06_07_Henrik.csv", blank.lines.skip = TRUE)
H = subset(H, GZ.ID.final!="")
names(H)[names(H)=="Serial.No."]="collar_id"
names(H)[names(H)=="GZ.ID.final"]="id"
H$id = as.character(H$id)
H$id[H$id == "Silurian"]="Silurian1"
setdiff(H$id, m0608GZT_MBB$id)#Laisamis2 -- active, not reporting
```

    ## [1] "Laisamis2"

``` r
setdiff(m0608GZT_MBB$id, H$id)
```

    ## [1] "Martha"   "Samburu2"

``` r
Martha = subset(m0608GZT_MBB,id=="Martha")#makes sense that Martha would be missing from Henrik because it was put on after Henrik's report (made in March '07)

Samburu2 = subset(m0608GZT_MBB,id=="Samburu2")#Samburu2 also put on after Henrik's report 
#take certain fields from Henrik
keep.col= c("id", "collar_id","place", "Area.Name", "Sex", "Age..yrs.")
H = H[,keep.col]
#add rows in Henrik dataset for Martha and Samburu2
tmp = data.frame(id = "Martha",
                 collar_id = "T5H-1529",
                 place = "unknown",
                 Area.Name = "unknown",
              Sex = "F",
              Age..yrs. = "unknown")
H = rbind(H, tmp)
tmp = data.frame(id = "Samburu2",
                 collar_id = "T5H-1429",
                 place = "unknown",
                 Area.Name = "unknown",
              Sex = "unknown",
              Age..yrs. = "unknown")
H = rbind(H, tmp)

#merge H with m0608GZT_MBB
m0608GZT_MBB_H = merge(m0608GZT_MBB, H)
#save metadata file
keep.col = c("id", "collar_id", "chronofile", "place", "Area.Name", "Sex", "Age..yrs.")
m0608GZT_MBB_H = m0608GZT_MBB_H[,keep.col]

intersect(names(m0608MBB), names(m0608GZT_MBB_H))
```

    ## [1] "id"        "collar_id"

``` r
#change Silurian to Silurian1 in m0608MBB
m0608MBB$id = as.character(m0608MBB$id)
m0608MBB$id[m0608MBB$id == "Silurian"]="Silurian1"
m0608_locs = merge(m0608MBB, H)
dim(m0608MBB)[1]==dim(m0608_locs)[1]#should be TRUE and it is 
```

    ## [1] TRUE

``` r
setdiff(unique(m0608MBB$id), unique(m0608_locs$id))#should be empty
```

    ## character(0)

``` r
setdiff(unique(m0608GZT_MBB_H$id), unique(m0608_locs$id))#should be empty
```

    ## character(0)

``` r
save(m0608_locs, file = "m0608_locs.Rdata")
write.csv(m0608_locs, file = "GZ_2006_2008.csv")
```

### 2011-2014 data

#### Read in collar data from 2011 to 2013 (output: z11\_13)

``` r
path <- "GZ collar data/"
z11_13 = read.csv(file =paste(path,"Collar data from Jan 2011_Dec 2013_PL.csv", sep =""))
z11_13$datetime = strptime(z11_13$acquisitiontime, format = "%d/%m/%Y %H:%M:%S", tz = "Africa/Nairobi")
#now get strftime which can be used to get min
z11_13$datetime <-strftime(z11_13$datetime)
save(z11_13, file = "z11_13.Rdata")
#z11_13$collar_id = z11_13$collarid
z11_13sum <- z11_13 %>%
  group_by(collarid) %>%
      summarise(
    first.date = min(datetime),
    last.date = max(datetime),
    count = n())

#get list of file names, for shapefiles that may overlap with z11_13
fns <- list.files(path)
#find the files that have .shp extension
shps <- fns[grepl(".shp", fns)]
#find the files w/ "11Sep2012"
shps <- shps[grepl("11Sep2012", shps)]
out= NULL
for (a in 1:length(shps)){
  #read in shapefile
  tmp <- shapefile(paste(path, "/", shps[a], sep = ""))
  #make into dataframe from shapefile
  tmp<- data.frame(tmp)
  #make names lower case
  names(tmp)<- tolower(names(tmp))
  #assign zebra name
#  tmp$id <- id.names[a]
  #assign chronofile.chk (to be checked against chronofile field)
#  tmp$chronofile.chk <-chrono.names[a]
  #add records to out
  out = rbind(out, tmp)
}
```

    ## Warning in .local(x, ...): .prj file is missing

    ## Warning in .local(x, ...): .prj file is missing

    ## Warning in .local(x, ...): .prj file is missing

    ## Warning in .local(x, ...): .prj file is missing

    ## Warning in .local(x, ...): .prj file is missing

    ## Warning in .local(x, ...): .prj file is missing

``` r
#summarize out
out$datetime <- strptime(out$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
#now get strftime which can be used to get min
out$datetime <-strftime(out$datetime)

out1 <-out %>% 
  group_by(collarid) %>%
    summarise(
    first.date.sep = min(datetime),
    last.date.sep = max(datetime),
    count = n())

#merge z11_13sum and out1
z11_13sum$collarid = as.character(z11_13sum$collarid)
intersect(names(z11_13sum), names(out1))
```

    ## [1] "collarid" "count"

``` r
intersect(unique(out1$collarid), unique(z11_13sum$collarid))
```

    ## [1] "st2010-1059" "st2010-1060" "st2010-1092" "st2010-1093" "st2010-1095"
    ## [6] "st2010-1185"

``` r
z11_13_out1 = merge(z11_13sum, out1, by = "collarid")
head(z11_13_out1)
```

    ##      collarid          first.date           last.date count.x
    ## 1 st2010-1059 2010-10-14 13:31:51 2012-10-21 06:00:09   16183
    ## 2 st2010-1060 2010-10-14 13:43:36 2012-12-05 06:01:57   17048
    ## 3 st2010-1092 2010-11-24 15:25:45 2012-12-07 00:00:16   14044
    ## 4 st2010-1093 2010-11-24 15:20:46 2012-11-26 06:00:17   13427
    ## 5 st2010-1095 2010-11-21 04:29:26 2012-12-04 18:00:53   14040
    ## 6 st2010-1185 2012-10-09 00:00:40 2013-11-22 00:00:37    7357
    ##        first.date.sep       last.date.sep count.y
    ## 1 2012-01-01 00:00:08 2012-08-20 15:00:09    5508
    ## 2 2012-01-01 00:00:08 2012-08-29 15:00:40    5740
    ## 3 2012-01-01 00:01:17 2012-09-11 09:00:29    6004
    ## 4 2012-01-01 00:00:28 2012-09-11 09:00:45    5650
    ## 5 2012-01-01 00:00:13 2012-09-11 03:01:36    5962
    ## 6 2012-08-13 08:59:47 2012-08-15 10:00:38      50

``` r
#confirmed that separate shapefiles (out) are subset of data in z11_13
```

### Combine all GZT data

#### Merge '06-'08 and '11-'14 data

Read in metadata for 2010-2014 data, merge with z11\_13. Output: Z.Rdata

``` r
#load 0608 data and change field names to match up with names in metadata for '10-'14
load("m0608_locs.Rdata")
names(m0608_locs)=tolower(names(m0608_locs))
names(m0608_locs)[names(m0608_locs)=="area.name"]="location.collared"
names(m0608_locs)[names(m0608_locs)=="fixtime"]="datetime"
m0608_locs$repro.status.at.collaring = NA
keep = c("id", "collar_id", "datetime", "lon", "lat", "height", "temp", "location.collared", "sex", "age..yrs.", "repro.status.at.collaring")
m0608_locs = m0608_locs[,keep]
names(m0608_locs)
```

    ##  [1] "id"                        "collar_id"                
    ##  [3] "datetime"                  "lon"                      
    ##  [5] "lat"                       "height"                   
    ##  [7] "temp"                      "location.collared"        
    ##  [9] "sex"                       "age..yrs."                
    ## [11] "repro.status.at.collaring"

``` r
#load '11-'13 data
load("z11_13.Rdata")
names(z11_13)[names(z11_13)=="collarid"]="collar_id"
names(z11_13)[names(z11_13)=="latitude"]="lat"
names(z11_13)[names(z11_13)=="longitude"]="lon"
names(z11_13)[names(z11_13)=="altitude"]="height"
names(z11_13)[names(z11_13)=="temperature"]="temp"
z11_13$id = z11_13$collar_id#id will be the same as collar_id
z11_13$age..yrs. = NA
names(z11_13)
```

    ##  [1] "record_id"         "collar_id"         "collartype"       
    ##  [4] "unit_record_index" "acquisitiontime"   "timetofix"        
    ##  [7] "lat"               "lon"               "hdop"             
    ## [10] "haccuracy"         "heading"           "speed"            
    ## [13] "speedaccuracy"     "height"            "temp"             
    ## [16] "datetime"          "id"                "age..yrs."

``` r
keep = c("id", "collar_id", "datetime", "lon", "lat", "height", "temp", "age..yrs.")
z11_13 = z11_13[,keep]
#left off here 20180427 -need to merge z11_13 with zmeta10_14, then merge with other dataset
#read in metadata for '11-'13
path <- "GZ collar data/"
zmeta_10_14 = read.csv(file = paste(path,"GZ Collar Tracking List July 2014.csv", sep = ""))
names(zmeta_10_14)
```

    ## [1] "Channel"            "Unit.ID"            "Collar.Frequency"  
    ## [4] "Receiver.Frequency" "Location.collared"  "GZ.class.collared" 
    ## [7] "Year.deployed"      "Month.Deployed"     "X"

``` r
head(zmeta_10_14)
```

    ##   Channel     Unit.ID Collar.Frequency Receiver.Frequency
    ## 1      15 ST2010-1062          160.152             160.15
    ## 2      48 ST2010-1185          160.480             160.48
    ## 3      50 ST2010-1186          160.500             160.50
    ## 4      52 ST2010-1187          160.520             160.52
    ## 5      54 ST2010-1188          160.540             160.54
    ## 6      55 ST2010-1058          160.548             160.55
    ##   Location.collared GZ.class.collared Year.deployed Month.Deployed  X
    ## 1            Meibae   Pregnant female          2010            Nov NA
    ## 2          Laisamis            Female          2011            Nov NA
    ## 3          Laisamis            Female          2011            Nov NA
    ## 4          Laisamis            Female          2011            Nov NA
    ## 5          Laisamis            Female          2011            Nov NA
    ## 6            Meibae   Pregnant female          2010            Nov NA

``` r
names(zmeta_10_14) =tolower(names(zmeta_10_14))
names(zmeta_10_14)[names(zmeta_10_14)=="unit.id"]= "collar_id"
names(zmeta_10_14)[names(zmeta_10_14)=="gz.class.collared"]= "repro.status.at.collaring"
unique(zmeta_10_14$gz.class.collared)#all are female
```

    ## NULL

``` r
zmeta_10_14$sex = "F"
names(zmeta_10_14)
```

    ##  [1] "channel"                   "collar_id"                
    ##  [3] "collar.frequency"          "receiver.frequency"       
    ##  [5] "location.collared"         "repro.status.at.collaring"
    ##  [7] "year.deployed"             "month.deployed"           
    ##  [9] "x"                         "sex"

``` r
keep = c("collar_id", "location.collared", "repro.status.at.collaring", "sex")
zmeta_10_14 = zmeta_10_14[,keep]#keep only some columns in metadata
zmeta_10_14$collar_id = tolower(zmeta_10_14$collar_id)#make collar_id lower case
intersect(names(z11_13), names(zmeta_10_14))
```

    ## [1] "collar_id"

``` r
z11_13 = merge(z11_13, zmeta_10_14)

setdiff(names(m0608_locs), names(z11_13))#confirm there are no differences in columns
```

    ## character(0)

``` r
setdiff(names(z11_13),names(m0608_locs))
```

    ## character(0)

``` r
intersect(unique(z11_13$collar_id), unique(m0608_locs$collar_id))#confirm there are no ids in common
```

    ## character(0)

``` r
intersect(names(z11_13), names(m0608_locs))#check names in common
```

    ##  [1] "collar_id"                 "id"                       
    ##  [3] "datetime"                  "lon"                      
    ##  [5] "lat"                       "height"                   
    ##  [7] "temp"                      "age..yrs."                
    ##  [9] "location.collared"         "repro.status.at.collaring"
    ## [11] "sex"

``` r
Z = rbind(m0608_locs, z11_13)
save(Z, file = "Z.Rdata")
write.csv(Z, file = "GZcollar_06_13.csv")
```

#### Summarize collar data

summary: date range, location collared, sample size

``` r
library(dplyr)
load("Z.Rdata")

Zsum <- Z %>%
  group_by(id) %>%
  summarise(min.datetime = min(datetime),
            max.datetime = max(datetime),
            location.collared = location.collared[1],
            count = n())

Zsum = data.frame(Zsum)
Zsum = Zsum[order(Zsum$max.datetime),]
Zsum
```

    ##             id        min.datetime        max.datetime location.collared
    ## 12       Njeri 2006-06-16 12:00:00 2006-09-07 20:00:00       Naibelibeli
    ## 17   Silurian1 2006-06-19 16:00:00 2006-09-17 14:00:00           Samburu
    ## 4         Jeff 2006-09-17 08:00:00 2006-11-06 06:00:00            Kalama
    ## 14        Rose 2006-11-16 15:17:00 2006-11-23 03:00:00         Buffalo S
    ## 15     Samburu 2006-06-19 04:00:00 2007-02-04 02:00:00           Samburu
    ## 18   Silurian2 2007-03-01 02:00:00 2007-03-08 19:03:00    5 km N of Lewa
    ## 13       Petra 2006-12-17 12:00:00 2007-04-19 00:00:00           Ngaroni
    ## 7       Kobosa 2007-02-25 12:00:00 2007-04-21 05:30:00          Laisamis
    ## 5       Johnna 2006-11-16 13:23:00 2007-05-02 07:00:00         Buffalo S
    ## 3       Hiroya 2007-02-25 05:48:00 2007-05-31 00:00:00          Laisamis
    ## 8       Lepere 2006-12-17 13:00:00 2007-06-11 06:00:00           Ngaroni
    ## 9          Liz 2006-06-16 15:00:00 2007-06-20 06:00:00            Loijuk
    ## 2      Dableya 2007-02-25 12:00:00 2007-11-10 17:00:00          Laisamis
    ## 16    Samburu2 2007-06-21 14:00:00 2008-02-21 11:00:00           unknown
    ## 6        Kiana 2006-12-17 12:00:00 2008-05-07 04:00:00           Ngaroni
    ## 1      Belinda 2006-09-17 06:00:00 2008-07-27 10:00:00            Kalama
    ## 11      Martha 2007-07-29 12:00:00 2008-10-13 05:00:00           unknown
    ## 10      Loijuk 2006-06-16 12:00:00 2009-11-01 00:00:00            Loijuk
    ## 19 st2010-1058 2010-10-14 13:00:10 2010-12-28 00:00:32            Meibae
    ## 22 st2010-1061 2010-10-14 14:42:39 2011-08-17 18:00:09            Meibae
    ## 27 st2010-1094 2010-11-21 04:27:49 2011-08-29 18:00:15               SNR
    ## 23 st2010-1062 2010-10-14 15:30:53 2011-11-04 12:00:12            Meibae
    ## 24 st2010-1063 2010-10-13 09:07:36 2012-06-03 00:00:42            Meibae
    ## 20 st2010-1059 2010-10-14 13:31:51 2012-10-21 06:00:09            Meibae
    ## 26 st2010-1093 2010-11-24 15:20:46 2012-11-26 06:00:17            Kalama
    ## 28 st2010-1095 2010-11-21 04:29:26 2012-12-04 18:00:53               SNR
    ## 21 st2010-1060 2010-10-14 13:43:36 2012-12-05 06:01:57          Westgate
    ## 25 st2010-1092 2010-11-24 15:25:45 2012-12-07 00:00:16            Kalama
    ## 31 st2010-1187 2012-10-09 00:00:10 2013-03-28 00:00:12          Laisamis
    ## 30 st2010-1186 2012-10-09 00:00:17 2013-11-09 06:00:10          Laisamis
    ## 29 st2010-1185 2012-10-09 00:00:40 2013-11-22 00:00:37          Laisamis
    ## 34 st2010-1224 2013-06-03 14:15:27 2013-12-04 12:02:37            Mchale
    ## 33 st2010-1189 2012-10-09 00:00:41 2013-12-05 06:00:11          Laisamis
    ## 32 st2010-1188 2012-10-09 00:00:18 2013-12-05 06:00:24          Laisamis
    ##    count
    ## 12   822
    ## 17   784
    ## 4    888
    ## 14   201
    ## 15  3780
    ## 18   168
    ## 13  3173
    ## 7   1413
    ## 5   4234
    ## 3   1236
    ## 8   4530
    ## 9   6980
    ## 2   6558
    ## 16  6121
    ## 6    800
    ## 1  10690
    ## 11 11697
    ## 10 10973
    ## 19  1016
    ## 22  5149
    ## 27  3043
    ## 23  8257
    ## 24 13183
    ## 20 16183
    ## 26 13427
    ## 28 14040
    ## 21 17048
    ## 25 14044
    ## 31  2251
    ## 30  5403
    ## 29  7357
    ## 34  4378
    ## 33  6980
    ## 32  6091

``` r
write.csv(Zsum, file = "Z.summary.csv")
```

### Clean GZ data

#### Subset by bounding coordinates matching range of GZ in Kenya.

This removes implausible points.

``` r
load("Z.Rdata")

df = Z
proj  = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
xy <- cbind(df$lon,df$lat)#package sp

df <-SpatialPointsDataFrame(coords = xy, data = df,
                            proj4string = CRS(proj))

m <- raster("GZ_2010_AGB_watermask_byte_LZW.tif")

projection(m)==projection(df)#check this is true
```

    ## [1] TRUE

``` r
dim(df)
```

    ## [1] 212898     11

``` r
ZCrop <- raster::crop(df, m)
dim(ZCrop)
```

    ## [1] 211631     11

``` r
Z = ZCrop
save(Z, file = "Z.Rdata")
```

### Read in and merge environmental data.

### Generate false Grevy's zebra locations.

Apply amt package to zebra data

``` r
max_speed = 30#executive decision: cannot go faster than 30 km per hour
#following this vignette: https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html
load("Z.Rdata")
nrand =1#number of randomly selected moves chosen
Z$datetime =parse_date_time(Z$datetime,  "YmdHMS")

head(Z$datetime)
```

    ## [1] "2006-09-17 06:00:00 UTC" "2006-09-17 08:00:00 UTC"
    ## [3] "2006-09-17 14:00:00 UTC" "2006-09-17 18:00:00 UTC"
    ## [5] "2006-09-17 22:00:00 UTC" "2006-09-19 00:00:00 UTC"

``` r
ids = unique(Z$id)
a = 1#counter
out = NULL
real = NULL
for (a in 1:length(ids)){#start for loop through ids
  print(a)
  z_tmp = subset(Z, id == ids[a])
  z_tmp = z_tmp[!duplicated(z_tmp$datetime),]#remove duplicate datetimes

  #get one row
  z_tmp1 = z_tmp[1,]
  z_tmp <- mk_track(z_tmp, lon, lat, datetime, id = id)
  #this makes a data.frame
  
  #add burst field
  z_tmp<-z_tmp %>%
    mutate(burst_ = 1)
  #change to tibble
  z_tmp = as_tibble(z_tmp)
  #change to bursts
  z_tmp <- steps_by_burst(z_tmp, lonlat=TRUE)
  speed = rep(NA, dim(z_tmp)[1])
  speed= 60*(z_tmp$sl_/as.numeric(z_tmp$t2_-z_tmp$t1_)/1000)
  z_tmp$speed = speed 
  z_tmp = subset(z_tmp, speed<max_speed)
  #now get bursts again
  z_tmp <- z_tmp %>%
    mutate(x_ = x1_,
           y_ = y1_,
           t_ = t1_)
  
  z_tmp= z_tmp[,c("x_","y_", "t_")]
  z_tmp <- mk_track(z_tmp, x_, y_, t_)
  #this makes a data.frame
    #add burst field
  z_tmp<-z_tmp %>%
    mutate(burst_ = 1)

  #change to tibble
  z_tmp = as_tibble(z_tmp)
  #change to bursts
  z_tmp <- steps_by_burst(z_tmp, lonlat=TRUE)

  z_tmp_real = z_tmp
  z_tmp_real$id = as.character(ids[a])
  real = rbind(real, z_tmp_real)
  #get random steps
  z_tmp <- random_steps(z_tmp, n = nrand)
  #add id field
  z_tmp$id = as.character(ids[a])
  out = rbind(out, z_tmp)
}
```

    ## [1] 1

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 2

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 3

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 4

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 5

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 6

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 7

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 8

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 9

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 10

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 11

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 12

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 13

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 14

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 15

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 16

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 17

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 18

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 19

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 20

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 21

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 22

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 23

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 24

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 25

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 26

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 27

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 28

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 29

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 30

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 31

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 32

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 33

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

    ## [1] 34

    ## .t found, creating `track_xyt`.
    ## .t found, creating `track_xyt`.

    ## Warning in random_steps.steps(z_tmp, n = nrand): Step-lengths or turning
    ## angles contained NA, which were removed.

``` r
head(z_tmp)
```

    ## # A tibble: 6 x 13
    ##   burst_ step_id_ case_   x1_    y1_   x2_      y2_ t1_                
    ## *  <dbl>    <int> <lgl> <dbl>  <dbl> <dbl>    <dbl> <dttm>             
    ## 1      1        1 TRUE   37.1 0.0379  37.5    0.190 2013-06-04 12:05:41
    ## 2      1        1 FALSE  37.1 0.0379 384.  -955.    2013-06-04 12:05:41
    ## 3      1        2 TRUE   37.5 0.190   37.5    0.197 2013-06-04 14:05:40
    ## 4      1        2 FALSE  37.5 0.190   34.4 -185.    2013-06-04 14:05:40
    ## 5      1        3 TRUE   37.5 0.197   37.5    0.197 2013-06-04 15:05:44
    ## 6      1        3 FALSE  37.5 0.197   43.5   48.9   2013-06-04 15:05:44
    ## # ... with 5 more variables: t2_ <dttm>, dt_ <time>, sl_ <dbl>, ta_ <dbl>,
    ## #   id <chr>

``` r
out=data.frame(out)
out_false = subset(out, case_ == FALSE)
#https://stackoverflow.com/questions/7477003/calculating-new-longitude-latitude-from-old-n-meters
r_earth = 6371000.0 
out_false$new_latitude  = out_false$y1_  + (out_false$y2_ / r_earth) * (180 / pi)

out_false$new_longitude = out_false$x1_ + (out_false$x2_ / r_earth) * (180 / pi) / cos(out_false$y1_ * pi/180);
#check: https://www.nhc.noaa.gov/gccalc.shtml

#TRUE moves already have x2, y2 as new longitude, new latitude
out_true = subset(out, case_ == TRUE)
out_true$new_latitude = out_true$y2_
out_true$new_longitude = out_true$x2_

out = rbind(out_false, out_true)
out$datetime = parse_date_time(out$t1,  "YmdHMS")

#rename new_longitude as location-long to match format in movebank
names(out)[names(out)=="new_longitude"]="location-long"#use location they went to
names(out)[names(out)=="new_latitude"]="location-lat"
names(out)[names(out)=="t2_"]="timestamp"
#change time stamp to include fraction of second -- did this to use movebank environmental data annotation for generic data. but that has max size 100,000 rows. so commenting out as unnecessary and in case it causes problems reading into regular movebank. 
out$timestamp = as.character(out$timestamp)
# out$timestamp=paste(out$timestamp,".001", sep = "")
out$timestamp <- strptime(out$timestamp, "%Y-%m-%d %H:%M:%OS")#for some reason using strptime and then strftime works
out$timestamp <- strftime(out$timestamp, "%Y-%m-%d %H:%M:%OS", usetz=FALSE)
head(out$timestamp)
```

    ## [1] "2006-09-17 14:00:00" "2006-09-17 18:00:00" "2006-09-17 22:00:00"
    ## [4] "2006-09-19 00:00:00" "2006-09-19 04:00:00" "2006-09-19 06:00:00"

``` r
out = out[,c("case_", 
              "id","location-long",
             "location-lat",
             "timestamp",
             "sl_",
             "ta_",
             "dt_")]
unique(out$id)
```

    ##  [1] "Belinda"     "Dableya"     "Hiroya"      "Jeff"        "Johnna"     
    ##  [6] "Kiana"       "Kobosa"      "Lepere"      "Liz"         "Loijuk"     
    ## [11] "Martha"      "Njeri"       "Petra"       "Rose"        "Samburu"    
    ## [16] "Samburu2"    "Silurian1"   "Silurian2"   "st2010-1058" "st2010-1059"
    ## [21] "st2010-1060" "st2010-1061" "st2010-1062" "st2010-1063" "st2010-1092"
    ## [26] "st2010-1093" "st2010-1094" "st2010-1095" "st2010-1185" "st2010-1186"
    ## [31] "st2010-1187" "st2010-1188" "st2010-1189" "st2010-1224"

``` r
out_init = subset(out, id == ids[1])
out_rest = subset(out, id != ids[1])
write.csv(out_init, file = "out_init.csv")
write.csv(out_rest, file = "out_rest.csv")
save(out, file = "out.Rdata")

#work out cutoff based on horses canter top speed = ~30 km/hr
real = subset(out, case_ == "TRUE")
speed = rep(NA, dim(real)[1])
speed = 60*(real$sl_/as.numeric(real$dt)/1000)
real$speed = speed
save(real, file = "real.Rdata")
write.csv(real, file = "Zreal.csv")
h = hist(real$speed, plot = FALSE, breaks = 200)
```

#### Land cover

Read in Africa land cover map and subset for Kenya. Source: <http://2016africalandcover20m.esrin.esa.int/>

``` r
path = "ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0/"
esa <- raster(paste0(path,"ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.tif"))

path = "TM_WORLD_BORDERS-0.3/"
countries = shapefile(paste0(path,"TM_WORLD_BORDERS-0.3.shp"))

kenya = subset(countries, NAME=="Kenya")
proj <- projection(kenya)

esa_kenya = crop(esa, kenya)
plot(esa_kenya)
```

![](grevys_zebra_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
writeRaster(esa_kenya, "esa_kenya.tif", format="GTiff",
            overwrite=TRUE)
```

#### Woody biomass

Read in woody biomass dataset (source: Bouvet et al.) and ESA land cover and put them together. Result: r\_comb.tif

``` r
m <- raster("GZ_2010_AGB_watermask_byte_LZW.tif")

##Not using this global land cover map because we found one Africa.
# G = raster("Globcover2009_V2.3_Global_/GLOBCOVER_L4_200901_200912_V2.3.tif")

G = raster("esa_kenya.tif")

projection(m)==projection(G)#check this is true
```

    ## [1] TRUE

``` r
GCrop <- raster::crop(G, m)

dim(GCrop)
```

    ## [1] 26125 18404     1

``` r
#adapted from SO: https://stackoverflow.com/questions/37956422/resample-raster
Nhr_rows <- dim(GCrop)[1] # resolution of high-res raster
Nhr_cols <- dim(GCrop)[2] # resolution of high-res raster
Nlr_rows <- Nhr_rows/10 # resolution of low-res raster
Nlr_cols <- Nhr_cols/10 # resolution of low-res raster
Nratio <- as.integer(Nhr_rows/Nlr_rows) # ratio of high to low resolutions, to nearest integer value for aggregation

# create some rasters
r.hr <- raster(ncols=Nhr_cols, nrows=Nhr_rows)
r.lr <- raster(ncols=Nlr_cols, nrows=Nlr_rows)
#don't think next row is needed, think it is holdover from debugging
# r.hr[] <- sample(1:13, Nhr^2, replace=T)
r.hr<- GCrop

# aggregate high res raster to *almost* the same resolution as the low res one (to nearest integer number of cells)
# each resulting layerN contains the fraction of area within that cell in which value of original raster is N. 
layer1 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==1, na.rm=na.rm)})
layer2 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==2, na.rm=na.rm)})
layer3 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==3, na.rm=na.rm)})
layer4 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==4, na.rm=na.rm)})
layer5 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==5, na.rm=na.rm)})
layer6 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==6, na.rm=na.rm)})
layer7 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==7, na.rm=na.rm)})
layer8 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==8, na.rm=na.rm)})
layer10 <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x==10, na.rm=na.rm)})

# and resample low res raster to the desired resolution
layer1 <- raster::resample(layer1, r.lr, method = "ngb") # Note you may prefer method = 'bilinear', depending on purpose.
layer2 <- raster::resample(layer2, r.lr, method = "ngb")
layer3 <- raster::resample(layer3, r.lr, method = "ngb")
layer4 <- raster::resample(layer4, r.lr, method = "ngb")
layer5 <- raster::resample(layer5, r.lr, method = "ngb")
layer6 <- raster::resample(layer6, r.lr, method = "ngb")
layer7 <- raster::resample(layer7, r.lr, method = "ngb")
layer8 <- raster::resample(layer8, r.lr, method = "ngb")
layer10 <- raster::resample(layer10, r.lr, method = "bilinear")

layer_all = stack(layer1,layer2,
                  layer3,
                  layer4,
                  layer5,
                  layer6,
                  layer7,
                  layer8,
                  layer10)


#resample m (woody biomass) so  it as at same spatial scale as coarser-scaled layer_all (esa) 

Nhr_rows <- dim(m)[1] # resolution of high-res raster
Nhr_cols <- dim(m)[2] # resolution of high-res raster
Nlr_rows <- dim(layer1)[1] # resolution of low-res raster
Nlr_cols <- dim(layer1)[2] # resolution of low-res raster
# Nratio <- as.integer(Nhr_rows/Nlr_rows) # ratio of high to low resolutions, to nearest integer value for aggregation
Nratio <- Nhr_rows/Nlr_rows # ratio of high to low resolutions, to nearest integer value for aggregation

r.hr <- raster(ncols=Nhr_cols, nrows=Nhr_rows)
r.lr <- raster(ncols=Nlr_cols, nrows=Nlr_rows)
r.hr<- m

# each resulting layerN contains the fraction of area within that cell in which value of original raster is N. 
layerm <- raster::aggregate(r.hr, Nratio, fun=function(x, na.rm=T) {mean(x, na.rm=na.rm)})

layer_all_resample <- raster::resample(layer_all, layerm, method = "bilinear")

# m_res = resample(layerm,layer1, method = "bilinear")

#add the two layers
r_comb = addLayer(layer_all_resample,layerm)

#save combined layers 
# writeRaster(r_comb, "r_comb.grd", format="raster",
#             overwrite = TRUE)

outfile <- writeRaster(r_comb, filename='r_comb.tif', format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
```

#### Livestock

Read in cattle, sheep, and goat data and combine

``` r
#source for cattle data: http://www.fao.org/geonetwork/srv/en/metadata.show?id=47949&currTab=simple
r_comb = stack("r_comb.tif")

C = raster("AFCattle1km_AD_2010_GLW2_01_TIF/AF_Cattle1km_AD_2010_v2_1.tif")
#assign projection
projection(C)=projection(r_comb)#check this is true

#crop
CCrop <- raster::crop(C, r_comb)

#resample C (cattle) so  it as at same spatial scale as finer-scaled r_comb
C_res = resample(CCrop,r_comb, method = "ngb")

#add the two layers
r_comb = addLayer(r_comb,C_res)

S = raster("AF_Sheep1km_AD_2010_GLW2_1_TIF/AF_Sheep1km_AD_2010_v2_1.tif")
projection(S)=projection(r_comb)#check this is true

#crop
SCrop <- raster::crop(S, r_comb)

#resample C (cattle) so  it as at same spatial scale as finer-scaled r_comb
S_res = resample(SCrop,r_comb, method = "ngb")

#add the layer
r_comb = addLayer(r_comb,S_res)

#goats
G = raster("AFGoats1km_AD_2010_v2_1_TIF/AF_Goats1km_AD_2010_v2_1.tif")
projection(G)=projection(r_comb)#check this is true

#crop
GCrop <- raster::crop(G, r_comb)

#resample C (cattle) so  it as at same spatial scale as finer-scaled r_comb
G_res = resample(GCrop,r_comb, method = "ngb")

#add the layer
r_comb = addLayer(r_comb,G_res)

outfile <- writeRaster(r_comb, filename='r_comb1.tif', format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
```

### Assign environmental data to true and false Grevy's zebra locations.

load zebra data and environmental data and extract values

``` r
load("out.Rdata")
out$hour = hour(out$timestamp)
out$month = month(out$timestamp)
out$year = year(out$timestamp)

r_comb_in = stack("r_comb1.tif")

out_spdf = SpatialPointsDataFrame(coords = out[,c("location-long", "location-lat")], 
                               data = out)

 # tell R that out coordinates are in the same lat/lon reference system
# as the woody biomass and land cover 
projection(out_spdf) <- projection(r_comb_in)
locs.vals = raster::extract(r_comb_in, out_spdf,
                            df = TRUE)#specify 

locs.vals.df = data.frame(locs.vals)
locs.vals.df$row = seq(1, dim(locs.vals.df)[1])

#read in legend for GlobCover and add field with description in words
# G_legend = read.csv("/Users/fischhoff/outside_analytics/sdm/Globcover2009_V2.3_Global_/Globcover2009_Legend.csv")

G_legend = read.csv("ESACCI-LC_S2_Prototype_ColorLegend.csv")

####################
##changed this to work for ESA
#names(G_legend)[names(G_legend)=="NB_LAB"]="GLOBCOVER_L4_200901_200912_V2.3"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.1"]="Tree.cover.areas"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.2"]="Shrubs.cover.areas"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.3"]="Grassland"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.4"]="Cropland"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.5"]="Vegetation aquatic or regularly flooded"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.6"]="Lichens Mosses / Sparse vegetation"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.7"]="Bare areas"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.8"]="Built up areas"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.9"]="Open water"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.10"]="woody.biomass"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.11"]="cattle"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.12"]="sheep"
names(locs.vals.df)[names(locs.vals.df)=="r_comb1.13"]="goats"
# encoder <- onehot(locs.vals.df.l, stringsAsFactors=FALSE, 
#                   max_levels = length(unique(locs.vals.df.l$GLOBCOVER)))

####################
# locs.vals.df2 <- predict(encoder, locs.vals.df.l)
# locs.vals.df3 = as.data.frame(locs.vals.df2)

out$row = seq(1, dim(out)[1])

out = merge(out, locs.vals.df, by = "row")

out$case.numeric = as.numeric(out$case_)
summary(out)
```

    ##       row           case_              id            location-long  
    ##  Min.   :     1   Mode :logical   Length:422130      Min.   :36.64  
    ##  1st Qu.:105533   FALSE:211065    Class :character   1st Qu.:37.14  
    ##  Median :211066   TRUE :211065    Mode  :character   Median :37.34  
    ##  Mean   :211066                                      Mean   :37.40  
    ##  3rd Qu.:316598                                      3rd Qu.:37.63  
    ##  Max.   :422130                                      Max.   :38.42  
    ##   location-lat      timestamp              sl_           
    ##  Min.   :-0.1406   Length:422130      Min.   :     0.00  
    ##  1st Qu.: 0.6843   Class :character   1st Qu.:    96.99  
    ##  Median : 0.8232   Mode  :character   Median :   267.69  
    ##  Mean   : 0.9184                      Mean   :   506.08  
    ##  3rd Qu.: 0.9941                      3rd Qu.:   635.16  
    ##  Max.   : 2.4293                      Max.   :216744.26  
    ##       ta_                 dt_                hour           month       
    ##  Min.   :-179.99832   Length:422130     Min.   : 0.00   Min.   : 1.000  
    ##  1st Qu.: -63.54065   Class :difftime   1st Qu.: 5.00   1st Qu.: 4.000  
    ##  Median :   0.00000   Mode  :numeric    Median :12.00   Median : 7.000  
    ##  Mean   :  -0.00168                     Mean   :11.49   Mean   : 6.703  
    ##  3rd Qu.:  63.26255                     3rd Qu.:18.00   3rd Qu.:10.000  
    ##  Max.   : 180.00000                     Max.   :23.00   Max.   :12.000  
    ##       year            ID         Tree.cover.areas   Shrubs.cover.areas
    ##  Min.   :2006   Min.   :     1   Min.   :0.000000   Min.   :0.0000    
    ##  1st Qu.:2007   1st Qu.:105533   1st Qu.:0.000000   1st Qu.:0.0000    
    ##  Median :2011   Median :211066   Median :0.000000   Median :0.2750    
    ##  Mean   :2010   Mean   :211066   Mean   :0.009298   Mean   :0.3427    
    ##  3rd Qu.:2012   3rd Qu.:316598   3rd Qu.:0.000000   3rd Qu.:0.5972    
    ##  Max.   :2013   Max.   :422130   Max.   :0.812739   Max.   :1.0000    
    ##    Grassland          Cropland        
    ##  Min.   :0.00000   Min.   :0.0000000  
    ##  1st Qu.:0.00000   1st Qu.:0.0000000  
    ##  Median :0.02057   Median :0.0000000  
    ##  Mean   :0.28209   Mean   :0.0007197  
    ##  3rd Qu.:0.50493   3rd Qu.:0.0000000  
    ##  Max.   :1.00000   Max.   :1.0000000  
    ##  Vegetation aquatic or regularly flooded
    ##  Min.   :0                              
    ##  1st Qu.:0                              
    ##  Median :0                              
    ##  Mean   :0                              
    ##  3rd Qu.:0                              
    ##  Max.   :0                              
    ##  Lichens Mosses / Sparse vegetation   Bare areas Built up areas
    ##  Min.   :0                          Min.   :0    Min.   :0     
    ##  1st Qu.:0                          1st Qu.:0    1st Qu.:0     
    ##  Median :0                          Median :0    Median :0     
    ##  Mean   :0                          Mean   :0    Mean   :0     
    ##  3rd Qu.:0                          3rd Qu.:0    3rd Qu.:0     
    ##  Max.   :0                          Max.   :0    Max.   :0     
    ##    Open water        woody.biomass        cattle           sheep       
    ##  Min.   :0.000e+00   Min.   : 0.000   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:1.719e-06   1st Qu.: 4.812   1st Qu.: 15.45   1st Qu.: 40.15  
    ##  Median :3.801e-05   Median : 9.797   Median : 22.95   Median : 84.33  
    ##  Mean   :4.822e-04   Mean   :15.217   Mean   : 24.04   Mean   :125.56  
    ##  3rd Qu.:9.130e-04   3rd Qu.:20.500   3rd Qu.: 35.22   3rd Qu.:196.36  
    ##  Max.   :4.252e-03   Max.   :85.000   Max.   :265.40   Max.   :621.42  
    ##      goats         case.numeric
    ##  Min.   :  0.00   Min.   :0.0  
    ##  1st Qu.: 59.81   1st Qu.:0.0  
    ##  Median :104.82   Median :0.5  
    ##  Mean   :136.59   Mean   :0.5  
    ##  3rd Qu.:235.71   3rd Qu.:1.0  
    ##  Max.   :717.52   Max.   :1.0

``` r
out = out[,c(1:14,20:24)]#remove columns that are land covers not observed

out_env = out
save(out_env, file = "out_env.Rdata")
```

Analysis
--------

### Define generalized boosted model to predict true vs. false.

### Split Grevy's zebra data into training and test sets

### Fit model and determine training and test accuracy.

### Predict habitat suitability across landscape by applying fitted model to map of environmental features.
