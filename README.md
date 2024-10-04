[![Build Status](https://github.com/stewid/EpiContactTrace/actions/workflows/R-CI.yaml/badge.svg)](https://github.com/stewid/EpiContactTrace/actions/workflows/R-CI.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/EpiContactTrace)](https://cran.r-project.org/package=EpiContactTrace)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/last-month/EpiContactTrace)](https://cran.r-project.org/package=EpiContactTrace)
[![Coverage Status](https://coveralls.io/repos/github/stewid/EpiContactTrace/badge.svg?branch=master)](https://coveralls.io/github/stewid/EpiContactTrace?branch=master)

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
install_github("stewid/EpiContactTrace")
```

License
-------

The `EpiContactTrace` package is licensed under the European Union
Public Licence (EUPL) http://ec.europa.eu/idabc/eupl
