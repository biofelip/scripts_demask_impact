# scripts_demask_impact

R scripts for the sensitivity analysis of offshore wind farm (OWF) impact in the German North Sea, developed for the DEMASK report.

For a given scenario (40, 50, 60 or 70 GW), the scripts select a set of candidate wind farms from a pool of mock farms, build their impact area (farm footprint plus buffer), and compare that area with other spatial inputs: marine mammal sensitivity maps, density surfaces, the German EEZ and the porpoise special area. Results are plotted per scenario and written to an Excel workbook.

## Workflow

1. Load the inputs: mock farm buffers, sensitivity maps (harbour porpoise, harbour seal, grey seal, aggregated marine mammal index), density surfaces (harbour porpoise, minke whale), the EEZ boundary and the porpoise special area.
2. For each scenario, select five farms from the pool of candidates that fall within that scenario, and extend each footprint with a buffer around its vertices (`greedy_search` and `create_buffer` in `utils.R`).
3. Intersect the resulting impact area with each spatial input and calculate the affected area, area per sensitivity category, and estimated number of individuals and percentage of population (`calculate_impact`).
4. Plot the impact area on top of each input, one panel per scenario (`plot_scenarios`).
5. Write the numerical results into the results workbook.

## Files

| File | Purpose |
|------|---------|
| `utils.R` | Core functions: `create_buffer`, `greedy_search`, `calculate_impact`, `plot_scenarios` |
| `Bauphase.R` | Construction phase: scenario selection, plots and results |
| `BetriebsPhase.r` | Operation phase, including already operating farms |
| `BetriebsPhase_nobuffer.r` | Operation phase variant without buffers |
| `Routes.r`, `Routes_BetriebsPhase.R` | Service vessel routes per scenario, compared with the same inputs, split into coastal waters, EEZ and non-German waters |
| `plot marine sensitivity.r` | Quick plot of the aggregated marine sensitivity map |
| `NaMaRo project 1.R`, `NaMaRo project 3.R` | Earlier exploratory versions of the analysis |

## Requirements

R with `sf`, `dplyr`, `purrr`, `stringr`, `ggplot2`, `units`, `patchwork`, `glue`, `openxlsx`, `ggnewscale`, `svglite`, `rnaturalearth` and `pelaDSM`.
