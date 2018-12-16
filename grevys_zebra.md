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

Subset by bounding coordinates --
=================================

### Read in and merge environmental data.

### Generate false Grevy's zebra locations.

### Assign environmental data to true and false Grevy's zebra locations.

Analysis
--------

### Define generalized boosted model to predict true vs. false.

### Split Grevy's zebra data into training and test sets

### Fit model and determine training and test accuracy.

### Predict habitat suitability across landscape by applying fitted model to map of environmental features.
