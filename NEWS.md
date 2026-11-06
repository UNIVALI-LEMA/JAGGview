# JAGGview 1.0.0

* Initial release.
* `*_data()` functions (e.g., `fits_data()`, `hindcast_data()`, `kobe_data()`) prepare structured data from JABBA fits and hindcasts.
* Static plots built with `ggplot2` for each analysis module (e.g., `fits_ggplot()`, `kobe_ggplot()`, `trajectories_ggplot()`).
* Extractor functions (`get_mase()`, `get_pars()`, `get_refpts()`, etc.) return results as data frames.
* `summary_table()` formats the output of the extractor functions.
* Interactive dashboard built with `shiny` and `bs4Dash`, launched by `create_report()`.