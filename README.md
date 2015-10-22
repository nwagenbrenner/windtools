windtools
=========

Tools for processing and visualizing wind data in R

Install with `devtools`:

`install_github('nwagenbrenner/windtools')`

`library(windtools)`

## Working with Observed Data:

Use `read_data.R`. The input file must be a csv file with the following columns: 

`identifier, lat, lon, datetime, obs_speed, obs_dir`

`datetime` must be in the format: `%Y-%b-%d %H:%M:%S`.

`obs_speed` must be in m/s.

`obs_dir` must be in degrees.

`read_data.R` returns a dataframe that can be used with other functions in the `windtools` package.

## Working with Observed and Predicted Data:

Use `wn_read_bias_data.R`. The input file must be a csv file with the following columns:

`identifier, lat, lon, datetime, obs_speed, pred_speed, bias_speed, obs_dir, pred_dir, bias_dir, wx_speed, wx_bias_speed, wx_dir, wx_bias_dir`

`datetime` must be in the format: `%Y-%b-%d %H:%M:%S`.

Speed variables must be in m/s.

Direction variables must be in degrees.

`wn_read_bias_data.R` returns a dataframe that can be used with other functions of the family `wn*` in the `windtools` package.
