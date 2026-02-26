# ASEAN UNGA Voting Analysis

Analysis of the 10 ASEAN member states' voting behavior in the United Nations General Assembly, using ideal point estimates derived from Bailey, Strezhnev & Voeten (2017).

## Research Questions

1. **US vs China alignment**: Are ASEAN countries converging toward or diverging from the US and China in their UNGA voting patterns?
2. **Intra-ASEAN cohesion**: Do ASEAN member states vote as a cohesive bloc, and has this changed over time?
3. **Issue-specific patterns**: Do alignment patterns differ across issue areas (human rights, nuclear, Middle East, etc.)?

## Data Source

Ideal point estimates from:
> Bailey, Michael A., Anton Strezhnev, and Erik Voeten. "Estimating dynamic state preferences from United Nations voting data." *Journal of Conflict Resolution* 61, no. 2 (2017): 430-456.

Source repository: [United-Nations-General-Assembly-Votes-and-Ideal-Points](https://github.com/evoeten/United-Nations-General-Assembly-Votes-and-Ideal-Points)

## ASEAN Countries Analyzed

| Country | ASEAN Since | UN Data From |
|---------|-------------|--------------|
| Thailand | 1967 (founder) | Session 2 (1947) |
| Philippines | 1967 (founder) | Session 1 (1946) |
| Indonesia | 1967 (founder) | Session 5 (1950) |
| Malaysia | 1967 (founder) | Session 12 (1957) |
| Singapore | 1967 (founder) | Session 20 (1965) |
| Brunei | 1984 | Session 39 (1984) |
| Vietnam | 1995 | Session 32 (1977) |
| Laos | 1997 | Session 11 (1956) |
| Myanmar | 1997 | Session 3 (1948) |
| Cambodia | 1999 | Session 11 (1956) |

Reference countries: **United States** (all sessions), **China/PRC** (from session 26/1971).

## Issue Areas

- **All Votes** (sessions 22-74)
- **Human Rights** (sessions 25-74)
- **Nuclear** (sessions 33-74)
- **Middle East** (sessions 29-74)
- **Important Votes** (sessions 38-73)
- **Non-Nuclear** (sessions 22-74)

## Repository Structure

```
├── data/           Filtered ASEAN + US + China ideal point data
├── scripts/
│   ├── helpers.R               Shared constants, palettes, utilities
│   ├── 01_extract_data.R       Data extraction from source repo
│   ├── 02_alignment_analysis.R Distance and cohesion metrics
│   └── 03_visualizations.R     All ggplot2 visualizations
├── figures/        Output PNG visualizations
└── README.md
```

## How to Reproduce

1. Ensure R >= 4.0 and packages: `tidyverse`, `ggrepel`, `patchwork`, `viridis`, `scales`, `RColorBrewer`
2. Place the source repo at the expected path (see `helpers.R`)
3. Run scripts in order:
   ```bash
   Rscript scripts/01_extract_data.R
   Rscript scripts/02_alignment_analysis.R
   Rscript scripts/03_visualizations.R
   ```

## Output Figures

| # | File | Description |
|---|------|-------------|
| 1 | `01_asean_idealpoints_overall.png` | All 10 ASEAN countries' ideal points over time |
| 2 | `02_asean_mean_vs_us_china.png` | ASEAN mean (with SD ribbon) vs US and China |
| 3 | `03_asean_mean_vs_refs_by_issue.png` | ASEAN mean vs US vs China, faceted by 6 issue areas |
| 4 | `04_asean_distance_from_us.png` | Each ASEAN country's distance from US |
| 5 | `05_asean_distance_from_china.png` | Each ASEAN country's distance from China |
| 6 | `06_asean_mean_distance_us_vs_china.png` | Mean ASEAN distance: US vs China |
| 7 | `07_asean_cohesion.png` | Intra-ASEAN cohesion (SD) with LOESS trend |
| 8 | `08_asean_cohesion_by_issue.png` | ASEAN cohesion across issue areas |
| 9 | `09_asean_heatmap.png` | Heatmap: country x year ideal points |
| 10 | `10_asean_closer_to_us_china.png` | Tile chart: closer to US or China? |
| 11 | `11_asean_correlation_heatmap.png` | Pairwise correlation of ASEAN countries |
| 12 | `12_asean_individual_by_issue.png` | Small multiples: each country x each issue |

## Methodology Notes

- **Ideal points** are estimated using a Bayesian IRT model with dynamic state preferences
- Scale: higher values = more aligned with Western/US positions; lower = more aligned with non-aligned/Eastern positions
- **Cohesion** is measured as the standard deviation of ASEAN ideal points per session (lower = more cohesive)
- **Distance** is the absolute difference between a country's ideal point and the reference (US or China)
