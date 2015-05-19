[![Build Status](https://travis-ci.org/stewid/EpiContactTrace.svg)](https://travis-ci.org/stewid/EpiContactTrace)
[![Build status](https://ci.appveyor.com/api/projects/status/2xpa13qi4s2gxso4?svg=true)](https://ci.appveyor.com/project/stewid/epicontacttrace)
[![CRAN status](http://www.r-pkg.org/badges/version/EpiContactTrace)](http://cran.r-project.org/web/packages/EpiContactTrace/index.html)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/last-month/EpiContactTrace)](http://cran.r-project.org/web/packages/EpiContactTrace/index.html)

# EpiContactTrace

EpiContactTrace is a package for facilitating livestock contact tracing
and risk based surveillance.  EpiContactTrace uses the network
parameters: in-degree, out-degree, ingoing contact-chain and outgoing
contact-chain, which are relevant for forward- and backward
contact-tracing respectively. The package can generate reports to
visualize the contact structure on the farm level.

You can track (and contribute to) development of `EpiContactTrace` at
https://github.com/stewid/EpiContactTrace.

## Installation

To install the latest release on CRAN

```
install.packages("EpiContactTrace")
```

To install the development version of EpiContactTrace, it's easiest to
use the `devtools` package:

```
# install.packages("devtools")
library(devtools)
install_github("EpiContactTrace", "stewid")
```

License
-------

The `EpiContactTrace` package is licensed under the European Union
Public Licence (EUPL) http://ec.europa.eu/idabc/eupl
