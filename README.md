clumpr [![app](https://img.shields.io/badge/Live%20App-here-orange.svg)](https://r-ubesp.dctv.unipd.it/shiny/clumpr/)
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis Build
Status](https://travis-ci.com/UBESP-DCTV/clumpr.svg?token=wGyFLep97LHjNKfPGjkg&branch=structures)](https://travis-ci.org/UBESP-DCTV/clumpr)
[![Codecov
Status](https://codecov.io/gh/UBESP-DCTV/clumpr/branch/structures/graph/badge.svg?token=IY02gbLUth)](https://codecov.io/gh/UBESP-DCTV/clumpr)
[![Build
status](https://ci.appveyor.com/api/projects/status/2q6ylqbu14cu43am?svg=true)](https://ci.appveyor.com/project/CorradoLanera/clumpr)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/clumpr)](https://cran.r-project.org/package=clumpr)

**C**urrent transp**L**ant s**U**rplus **M**anagement **P**rotocol in
**R**

Last update: 2018-06-11

## Description

The `clumpr` package aims to provide a toolbox of function for model,
validate and visualize dynamics assessed by the “Protocollo Nazionale
per la Gestione delle Eccedenze di Tutti i Programmi di Trapianto” in
Italy.

The first implementation is focused on the lung-transplant centers
management.

## Example

This is a basic example which shows you how it works:

``` r
library(clumpr)

# setup centers and regions
pavia   <- center('Pavia',   'Lombardia', offered = 11, p_accept = 0.8)
bergamo <- center('Bergamo', 'Lombardia', offered =  7, p_accept = 0.5)
milano  <- center('Milano',  'Lombardia', offered =  3)

lombardia <- region(set_centers(pavia, bergamo, milano), default_p = 0.2)

# show data
lombardia
#>     Region  : Lombardia (Italy)
#>     Centers : Pavia, Bergamo, Milano (#3)
#>     Acceptance rate : 0.92 (at least one center)
#>     Offered organs  : 21 (from all the centers)
get_centers(lombardia)
#>     Center           : Pavia (Lombardia --- Italy)
#>     Acceptance rate  : 0.8
#>     Offered organs   : 11
#> 
#>     Center           : Bergamo (Lombardia --- Italy)
#>     Acceptance rate  : 0.5
#>     Offered organs   : 7
#> 
#>     Center           : Milano (Lombardia --- Italy)
#>     Acceptance rate  : <inherit from the region rate>
#>     Offered organs   : 3

# setup macroregions
padova <- center('Padova', 'Veneto', 10, 0.9)
veneto <- region(set_centers(padova))

nitp <- macroregion('NITp', set_regions(lombardia, veneto),
  initial_strip = c('lombardia',  'lombardia', 'veneto'))

# show data
nitp
#>     Macroregion : NITp (Italy)
#>     Regions     : Lombardia; Veneto (#2)
#>     Centers     : Pavia, Bergamo, Milano; Padova (#4)
#>     Acceptance rate : 0.992 (at least one center in some region)
#>     Offered organs  : 31 (from every the centers of every region)
#>     Initail strip   : Lombardia --> Lombardia --> Veneto
#>     Current strip   : Lombardia --> Lombardia --> Veneto
#>     Time period     : 0
get_regions(nitp)
#>     Region  : Lombardia (Italy)
#>     Centers : Pavia, Bergamo, Milano (#3)
#>     Acceptance rate : 0.92 (at least one center)
#>     Offered organs  : 21 (from all the centers)
#> 
#>     Region  : Veneto (Italy)
#>     Centers : Padova (#1)
#>     Acceptance rate : 0.9 (at least one center)
#>     Offered organs  : 10 (from all the centers)
get_centers(nitp)
#> $lombardia
#>     Center           : Pavia (Lombardia --- Italy)
#>     Acceptance rate  : 0.8
#>     Offered organs   : 11
#> 
#>     Center           : Bergamo (Lombardia --- Italy)
#>     Acceptance rate  : 0.5
#>     Offered organs   : 7
#> 
#>     Center           : Milano (Lombardia --- Italy)
#>     Acceptance rate  : <inherit from the region rate>
#>     Offered organs   : 3
#> 
#> 
#> $veneto
#>     Center           : Padova (Veneto --- Italy)
#>     Acceptance rate  : 0.9
#>     Offered organs   : 10

# add another region and setup a macroarea
torino   <- center('Torino', 'Piemonte', 7, 0.6)
piemonte <- region(set_centers(torino))

nord <- macroarea('Nord',
  macroregions = set_macroregions(piemonte, nitp)
)

nord
#>     Macroarea    : Nord (Italy)
#>     Macroregions : Piemonte; NITp (#2)
#>     Regions      : Piemonte; Lombardia, Veneto (#3)
#>     Centers      : Torino; Pavia, Bergamo, Milano; Padova (#5)
#>     Acceptance rate : 0.9968 (at least one center in some region)
#>     Offered organs  : 38 (from every centers of every region)
#>     Initail strip   : Piemonte --> Nitp
#>     Current strip   : Piemonte --> Nitp
#>     Time period     : 0
get_macroregions(nord)
#>     Region  : Piemonte (Italy)
#>     Centers : Torino (#1)
#>     Acceptance rate : 0.6 (at least one center)
#>     Offered organs  : 7 (from all the centers)
#> 
#>     Macroregion : NITp (Italy)
#>     Regions     : Lombardia; Veneto (#2)
#>     Centers     : Pavia, Bergamo, Milano; Padova (#4)
#>     Acceptance rate : 0.992 (at least one center in some region)
#>     Offered organs  : 31 (from every the centers of every region)
#>     Initail strip   : Lombardia --> Lombardia --> Veneto
#>     Current strip   : Lombardia --> Lombardia --> Veneto
#>     Time period     : 0
get_regions(nord)
#>     Region  : Piemonte (Italy)
#>     Centers : Torino (#1)
#>     Acceptance rate : 0.6 (at least one center)
#>     Offered organs  : 7 (from all the centers)
#> 
#>     Region  : Lombardia (Italy)
#>     Centers : Pavia, Bergamo, Milano (#3)
#>     Acceptance rate : 0.92 (at least one center)
#>     Offered organs  : 21 (from all the centers)
#> 
#>     Region  : Veneto (Italy)
#>     Centers : Padova (#1)
#>     Acceptance rate : 0.9 (at least one center)
#>     Offered organs  : 10 (from all the centers)
get_centers(nord)
#> $piemonte
#>     Center           : Torino (Piemonte --- Italy)
#>     Acceptance rate  : 0.6
#>     Offered organs   : 7
#> 
#> 
#> $lombardia
#>     Center           : Pavia (Lombardia --- Italy)
#>     Acceptance rate  : 0.8
#>     Offered organs   : 11
#> 
#>     Center           : Bergamo (Lombardia --- Italy)
#>     Acceptance rate  : 0.5
#>     Offered organs   : 7
#> 
#>     Center           : Milano (Lombardia --- Italy)
#>     Acceptance rate  : <inherit from the region rate>
#>     Offered organs   : 3
#> 
#> 
#> $veneto
#>     Center           : Padova (Veneto --- Italy)
#>     Acceptance rate  : 0.9
#>     Offered organs   : 10

roma   <- center('Roma', 'Lazio', 10, 0.9)
lazio  <- region(set_centers(roma))

sud <- macroarea('Sud',
  macroregions = set_macroregions(lazio)
)

italy <- state('Italy', set_macroareas(nord, sud))
italy
#>     State        : Italy
#>     Macroareas   : Nord; Sud (#2)
#>     Macroregions : Piemonte, NITp; Lazio (#3)
#>     Regions      : Piemonte; Lombardia, Veneto; Lazio (#4)
#>     Centers      : Torino; Pavia, Bergamo, Milano; Padova; Roma (#6)
#>     Acceptance rate : 0.99968 (at least one center in some region)
#>     Offered organs  : 48 (from every centers of every region)
#>     Initail strip   : Nord --> Sud
#>     Current strip   : Nord --> Sud
#>     Time period     : 0

get_macroareas(italy)
#>     Macroarea    : Nord (Italy)
#>     Macroregions : Piemonte; NITp (#2)
#>     Regions      : Piemonte; Lombardia, Veneto (#3)
#>     Centers      : Torino; Pavia, Bergamo, Milano; Padova (#5)
#>     Acceptance rate : 0.9968 (at least one center in some region)
#>     Offered organs  : 38 (from every centers of every region)
#>     Initail strip   : Piemonte --> Nitp
#>     Current strip   : Piemonte --> Nitp
#>     Time period     : 0
#> 
#>     Macroarea    : Sud (Italy)
#>     Macroregions : Lazio (#1)
#>     Regions      : Lazio (#1)
#>     Centers      : Roma (#1)
#>     Acceptance rate : 0.9 (at least one center in some region)
#>     Offered organs  : 10 (from every centers of every region)
#>     Initail strip   : Lazio
#>     Current strip   : Lazio
#>     Time period     : 0
get_macroregions(italy)
#>     Region  : Piemonte (Italy)
#>     Centers : Torino (#1)
#>     Acceptance rate : 0.6 (at least one center)
#>     Offered organs  : 7 (from all the centers)
#> 
#>     Macroregion : NITp (Italy)
#>     Regions     : Lombardia; Veneto (#2)
#>     Centers     : Pavia, Bergamo, Milano; Padova (#4)
#>     Acceptance rate : 0.992 (at least one center in some region)
#>     Offered organs  : 31 (from every the centers of every region)
#>     Initail strip   : Lombardia --> Lombardia --> Veneto
#>     Current strip   : Lombardia --> Lombardia --> Veneto
#>     Time period     : 0
#> 
#>     Region  : Lazio (Italy)
#>     Centers : Roma (#1)
#>     Acceptance rate : 0.9 (at least one center)
#>     Offered organs  : 10 (from all the centers)
get_regions(italy)
#>     Region  : Piemonte (Italy)
#>     Centers : Torino (#1)
#>     Acceptance rate : 0.6 (at least one center)
#>     Offered organs  : 7 (from all the centers)
#> 
#>     Region  : Lombardia (Italy)
#>     Centers : Pavia, Bergamo, Milano (#3)
#>     Acceptance rate : 0.92 (at least one center)
#>     Offered organs  : 21 (from all the centers)
#> 
#>     Region  : Veneto (Italy)
#>     Centers : Padova (#1)
#>     Acceptance rate : 0.9 (at least one center)
#>     Offered organs  : 10 (from all the centers)
#> 
#>     Region  : Lazio (Italy)
#>     Centers : Roma (#1)
#>     Acceptance rate : 0.9 (at least one center)
#>     Offered organs  : 10 (from all the centers)
get_centers(italy)
#> $piemonte
#>     Center           : Torino (Piemonte --- Italy)
#>     Acceptance rate  : 0.6
#>     Offered organs   : 7
#> 
#> 
#> $lombardia
#>     Center           : Pavia (Lombardia --- Italy)
#>     Acceptance rate  : 0.8
#>     Offered organs   : 11
#> 
#>     Center           : Bergamo (Lombardia --- Italy)
#>     Acceptance rate  : 0.5
#>     Offered organs   : 7
#> 
#>     Center           : Milano (Lombardia --- Italy)
#>     Acceptance rate  : <inherit from the region rate>
#>     Offered organs   : 3
#> 
#> 
#> $veneto
#>     Center           : Padova (Veneto --- Italy)
#>     Acceptance rate  : 0.9
#>     Offered organs   : 10
#> 
#> 
#> $lazio
#>     Center           : Roma (Lazio --- Italy)
#>     Acceptance rate  : 0.9
#>     Offered organs   : 10

# TODO
```

## Installation

You can install clumpr from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("UBESP-DCTV/clumpr")
```

## Bug reports

If you encounter a bug, please file a
[reprex](https://github.com/tidyverse/reprex) (minimal reproducible
example) to <https://github.com/UBESP-DCTV/clumpr/issues>

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## References

**Protocollo Nazionale per la Gestione delle Eccedenze di Tutti i
Programmi di Trapianto**
([pdf](http://www.policlinico.mi.it/AMM/nitp/area_operatore/linee_guida/03/ProtocolloNazionaleGestioneEccedenzeCNTO140804.pdf))
