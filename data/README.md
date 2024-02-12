## Codebook

## hf131-01-plot.csv
Rows: 150
Columns: 23

- `plot` : Unique identifier for plot.
- `site` : Unique identifier for site.
- `ft` : Forest type.  TS = hemlock (Tsuga canadensis) dominated (hemlock >50% basal area), MD = Mixed deciduous (hemlock <25% basal area).
- `asn` : Unique identifier for cover board station.
- `lat` : Latitude in degrees, decimal minutes for each cover board station.
- `long` : Longitude in degrees, decimal minutes for each cover board station.
- `datum` : Datum of coordinates.
- `ba` : Average basal area in 7.5m fixed radius subplot, in square meters/hectare.
- `st.ha` : Average number of stems per hectare with DBH >5cm in 7.5m fixed radius subplot.
- `ts` : Percent of basal area made up by hemlock.
- `bearing` : Compass bearing in degrees of each cover board transect.
- `aspect` : Slope aspect.
- `slope` : Slope (percent).
- `canopy.cover` : Percent canopy cover at each cover board station, measured with densitometer.
- `cwd.no` : Number of pieces of coarse woody debris with diameter >2.5cm along three 7.5m transects originating from each cover board station.
- `cwd.con` : Number of pieces of coarse woody debris with diameter >2.5cm in contact with forest floor along three 7.5m transects originating from each cover board station.
- `vcwd` : Volume of coarse woody debris with diameter >2.5cm measured along three 7.5m transects originating from each cover board station, in cubic meters.
- `vcwd.con` : Volume of coarse woody debris with diameter >2.5cm in contact with forest floor measured along three 7.5m transects originating from each cover board station, in cubic meters.
- `avg.dc` : Average decay class of coarse woody debris from 1 to 5.
    1: solid wood, recently fallen, bark and twigs present
    2: solid wood, significantly weathered, branches present
    3: wood not solid, bark may be sloughing but nail still must be pounded into the wood
    4: wood sloughing and/or friable, nail may be forcibly pushed into wood
    5: wood friable, barely holding shape, nail may be easily pushed into wood
- `tot.snags` : Total number of snags within 7.5m subplot sharing center with each cover board station.
- `avg.snag.ht` : Average snag height within 7.5m subplot, in meters.
- `avg.snag.dbh` : Average snag diameter at breast height within 7.5m subplot, in centimeters.
- `soil.ph` : pH of organic soil layer from random point within subplot.

## hf131-02-tree.csv
Rows: 3,206
Columns: 8

- `stem.master` : Unique identifier for each tree stem.
- `plot` : Unique identifier for each plot.
- `asn` : Unique identifier for each cover board station.
- `stem.asn` : Unique identifier for each tree stem within cover board station.
- `species` : Four-letter code for tree species; first two letters of genus and species -- e.g., "Acer rubrum" = "ACRU".
- `dbh` : Diameter at breast height in centimeters for all stems with DBH >5cm.
- `alive` : Tree was alive or dead at time of visit (Y/N).
- `sh` : Hieght of snag in meters (if dead).

## hf131-03-cwd.csv
Rows: 1,103
Columns: 10

- `cwd.no` : Unique identifier for each piece of coarse woody debris with diameter >2.5cm encountered in subplot.
- `site` : Unique identifier for site.
- `ft` : Forest type.  TS = hemlock (Tsuga canadensis) dominated (hemlock >50% basal area), MD = Mixed deciduous (hemlock <25% basal area).
- `plot` : Unique identifier for plot.
- `asn` : Unique identifier for cover board station.
- `tr` : Bearing of each 7.5m transect originating from cover board station.
- `species` : Species of coarse woody debris, using four-letter code.
- `diam` : Diameter of each piece of coarse woody debris.
- `dc` : Decay class on scale from 1 to 5.
    1: solid wood, recently fallen, bark and twigs present
    2: solid wood, significantly weathered, branches present
    3: wood not solid, bark may be sloughing but nail still must be pounded into the wood
    4: wood sloughing and/or friable, nail may be forcibly pushed into wood
    5: wood friable, barely holding shape, nail may be easily pushed into wood
- `c` : Whether piece of coarse woody debris was in contact with soil (Y/N).

## hf131-04-aco.csv
Rows: 2,380
Columns: 14

- `datetime` : Date and time in POSIXct format.
- `date` : Date in Date format.
- `aco` : Unique identifier for cover board.
- `plot` : Unique identifier for plot.
- `asn` : Unique identifier for cover board station.
- `airt` : Air temperature at soil surface at one cover board per station, in degrees Celsius.
- `rh` : Relative humidity (percent).
- `soilt` : Soil temperature at 5cm depth at one cover board per station, in degrees Celsius.
- `time` : Time of cover board monitoring and temperature measurements.
- `pca` : Number of red-backed salamanders (Plethodon cinereus) observed under each cover board.
- `pcn` : Specimen number assigned to each P. cinereus; unclear if this is a unique identifier that indicates recapture.
- `em.aco` : Specific cover board at which temperature measurements were sampled.
- `osn` : Specimen number of other herps.
- `other.sp` : Species of herp other than P. cinereus:
    NOVI: Notophthalmus viridescens viridescens
    THIS: Thamnophis sirtalis
    AMMA: Ambystoma maculatum
    RASY: Rana sylvatica
    STOC: Storeriea o. occipitomaculata
    EUBI: Eurycea bislineata


## hf131-05-pc-abund.csv
Rows: 250
Columns: 9

- `pc.no` : Number of P. cinereus observation.
- `date` : Date of observation.
- `plot` : Plot of observation.
- `aco` : Cover board of observation.
- `p` : Phase (R = redback, L = leadback).
- `svl` : Snout-vent length in millimeters.
- `tl` : Total length in millimeters.
- `wt` : Weight in centigrams
- `sex` : Sex of adults determined by snout shape.

## hf131-06-herps.csv
Rows: 48
Columns: 8

- `osn` : Specimen number of observation.
- `date` : Date of observation.
- `plot` : Plot of observation.
- `aco` : Cover board of observation.
- `species` :  Species of herp other than P. cinereus:
    NOVI: Notophthalmus viridescens viridescens
    THIS: Thamnophis sirtalis
    AMMA: Ambystoma maculatum
    RASY: Rana sylvatica
    STOC: Storeriea o. occipitomaculata
    EUBI: Eurycea bislineata
- `svl` : Snout-vent length in millimeters.
- `tl` : Total length in millimeters.
- `weight` : Weight in centigrams.

