# SpectrumUtils
Utilities for extracting inputs and estimates from Spectrum files

## Description
[Spectrum](https://avenirhealth.org/software-spectrum.php) is a suite of policy models to support decision making around demographic and public health questions. The `SpectrumUtils` package is an R interface for reading data from Spectrum modules into R for analysis, visualization, and development. National Spectrum files produced for annual HIV estimates can be requested from UNAIDS [via this link](https://www.unaids.org/en/dataanalysis/datatools/spectrum-epp).

## How to use this code
This package is not currently listed in CRAN. If you have the `devtools` package installed, you can install `SpectrumUtils` in R via

```
> devtools::install_github(rlglaubius/SpectrumUtils)
```

Suppose you have a national Spectrum file for Zimbabwe, `Zimbabwe.PJNZ`. Once you have installed `SpectrumUtils`, you can extract geographic metadata for that file:

```
> library(SpectrumUtils)
> pjn.raw = read.raw.pjn("Path/to/files/Zimbabwe.PJNZ")
> extract.geo.info(pjn.raw, direction="long")
  iso.code snu.name snu.code
1      716          0
```

Spectrum files are just zipped archives. We use `read.raw.pjn` to extract raw projection data embedded in a .PJN file inside the PJNZ. The return value from `extract.geo.info` tells us the ISO-3166 numeric country code for the file (716). You may wish to use the `ISOcodes` R package to look up other standard information associated with that code, like the official country name and alphabetic ISO-3166 codes. The return value from `extract.geo.info` also has fields for sub-national unit (SNU) name and numeric code. Since this is a national file, the SNU name is blank and SNU code is 0 (please note that, unlike ISO-3166 codes, SNU numeric codes are specific to Spectrum).

If your file was prepared using Spectrum's AIDS Impact Module (AIM), you can extract estimated numbers of people living with HIV by age, sex, and calendar year:

```
> dp.raw = read.raw.dp("Path/to/files/Zimbabwe.PJNZ")
> plhiv = dp.output.hivpop(dp.raw, direction="long")
```


This repository includes two key entry point scripts, nhfitter.R and visualize-fit.R. Other code files provide supporting functionality required by these entry point scripts. To run the analysis, point your R session working directory at this repository, then run

```
> fit.model = TRUE
> source("nhfitter.R")
> save.image("fit-ws.RData") # optional
```

We are actively adding support to access more contents of Spectrum files. To list the available functions in the package, please use

```
> lsf.str("package:SpectrumUtils")
```

You can access help in R for any of those functions.

