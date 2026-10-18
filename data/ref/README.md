# Reference data

- `counties.geojson` is downloaded on the first run from Tailte Éireann
  (Counties - National Statutory Boundaries - 2019 - Generalised 20m, CC BY 4.0). It is not committed.
- `county_population.csv` is optional. Add it to enable "per 1,000 residents" on the map:

  ```csv
  county,population
  Carlow,...
  ```

  Use the CSO Census 2022 county populations (PxStat table FP001 or the Census 2022 Main Results),
  with Dublin, Cork, Galway, Limerick, Waterford and Tipperary as whole counties.
