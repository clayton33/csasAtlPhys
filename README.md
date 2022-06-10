# csasAtlPhys
The csasAtlPhys R package supports DFO Atlantic region physical oceanography CSAS analysis.

## About csasAtlPhys

csasAtlPhys helps support data analysis to produce data products presented in the research document
that summarizes the meteorological and physical oceanographic conditions on the Scotian shelf and Gulf of Maine, 
[as seen here](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_016-eng.html).

This package mainly has functionality to download and read various data sources around the web.
It also has some basic plotting methods used to produce consistent plots required for the document. In addition, it had some functions for some types of calculations.
Various files that are critical for various aspects of the analysis also reside in this package to
insure that they are never lost.

Please note that this package *does not* have any figures or data output for
the analysis described above.

This package is in active development, and due to the nature of the package,
there is no plan at this time to submit to CRAN. For users to who wish to install the package, the can do so by

```
library(devtools)
install_github('clayton33/csasAtlPhys', ref = 'master')
```

this package is dependent on the `oce` package, so it is also best to have
the most up to date, the develop, version of the package. Instructions 
on updating that package is similar, and are summarized [here](https://github.com/dankelley/oce).
