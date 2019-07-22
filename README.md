# Hydromorphology
Basic code to analyse temporal changes in channel morphology from hydrometric data.

In the _code_ folder you will find two R scripts:

**1. Import, prepare and output data**

This code imports US Geological Survey (USGS) historical transect (spot gauging) channel measurements (downloaded from the USGS National Water Information System, NWIS). It then filters the data, and creates new time series variables that represent channel conveyance capacity (m3/s), cross-sectional flow area (m2), cross-sectional mean streamflow velocity (m/s), channel width (m), and channel riverbed elevation (m), for further analysis. 

**2. Plot data**

This code will produce one PDF file for each gauging station, showing the raw/filtered data and the resulting morphology time series.

**References**

If you use this code, please cite one of the following papers:

* Slater, L. J., Singer, M. B., & Kirchner, J. W. (2015). Hydrologic versus geomorphic drivers of trends in flood hazard. Geophysical Research Letters, 42(2), 370-376.

* Slater, L. J. (2016). To what extent have changes in channel capacity contributed to flood hazard trends in England and Wales?. Earth Surface Processes and Landforms, 41(8), 1115-1128.