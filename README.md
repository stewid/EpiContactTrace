# EpiContactTrace

EpiContactTrace is a package for facilitating livstock
contact tracing and risk based surveillance.
EpiContactTrace uses the network parameters in-degree,
out-degree, ingoing contact-chain and outgoing contact-chain,
which are relevant for forward- and backward contact-tracing
respectively. The package can generate reports to visualize the
contact structure on farm level.
You can track (and contribute to) development of `EpiContactTrace`
at https://github.com/stewid/EpiContactTrace.

## Development

To install the development version of EpiContactTrace, it's easiest to use the `devtools` package:

    # install.packages("devtools")
    library(devtools)
    install_github("EpiContactTrace", "stewid")