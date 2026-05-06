install.packages("fred")

# método alternativo de instalación
library(devtools)
devtools::install_github("charlescoverdale/fred")



# Step 1: Create a FRED account
# Step 2: Request an API key
# Step 3: Save the key so R can find it
file.edit("~/.Renviron") # Guardo la Key en mi entorno golbal (de sistema) y reinicio la sesión de R para que lo tome
nchar(Sys.getenv("FRED_API_KEY")) # verificar que tenga longitud 32
# The best approach is to store your key in a file called .Renviron, which R reads automatically every time it starts. This means you only set it once and never have to think about it again.
library(fred)

install.packages("tibble")
library(tibble)
# crea listas
gdp <- fred_series("GDP") # Fetch a single series
macro <- fred_series(c("GDP", "UNRATE", "CPIAUCSL")) # Fetch multiple series in one call


tibble::as_tibble(macro) # muestra macro como si fuera un tibble
table(macro$series_id) # muestra macro como si fuera una tabla

gdp_growth <- fred_series("GDP", units = "pc1") # Growth rates and transformations
tail(gdp_growth, 4)

rates <- fred_series("DGS10", frequency = "m") # Aggregate daily data to a lower frequency
tail(rates, 4)

fred_info("UNRATE") # Look up what a series actually measures

# esto no anda, no entiendo por qué

install.packages("vignette")
devtools::install_github("")

library(vignette)
vignette("multi-series-workflows", "fred") # fetch, transform, widen, plot.


install.packages("imfapi")
library(imfapi)
