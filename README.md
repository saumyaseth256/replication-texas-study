# Replication: [Craig, Imberman & Perdue (2013)](https://doi.org/10.1016/j.jue.2012.07.002)

Replication of Craig, Imberman & Perdue (2013), "Does it pay to get an A? School resource allocations in response to accountability ratings," *Journal of Urban Economics*, 73(1), 30-42.

Craig, Imberman and Perdue (2013) examine whether school districts reallocate resources in response to accountability ratings. Their identification comes from Texas’s 2002–03 transition to a more difficult state exam, a year in which no ratings were issued. The harder exam lowered many schools’ predicted ratings without any real change in school quality. The authors estimate each school’s gap-year rating and regress the change in resources between 2002-03 and 2003-04 on predicted rating declines, controlling for prior ratings and student demographics. Schools predicted to fall to "Acceptable" received approximately $73 more per pupil, primarily in instructional spending, and experienced a modest reduction in the student-teacher ratio, compared to schools that remained at "Acceptable". Declines to other rating categories produced no significant effects, and the additional funding had largely vanished within three years.

## How to run

Open `replication-texas-study.Rproj` (or set the working directory to the project folder) and run:

```r
source("code/code.R")
```

The script creates a directory called `data/` and downloads and unzips the data from OneDrive. The outputs (as created by `code/code.R`) are stored in the directory `tables/`.

Packages used: `dplyr`, `tidyr`, `stringr`, `data.table`, `readr`, `tibble`, `purrr`.

## Code

| File | What it does |
|---|---|
| `code/code.R` | Builds the campus panels, estimates the 2002-03 ratings according to the 2004 rules, and produces Tables 1-10. Calls `compare_to_published.R` at the end. |
| `code/published_values.R` | Values from the original Craig et al. (2013) paper. |
| `code/compare_to_published.R` | Writes tables laid out like the paper. |

## Data

All files are raw downloads from the Texas Education Agency.

- `data/aeis campus info/`: AEIS campus files (reference, students, staff, TAKS results, completion), 1997-98 to 2007-08
- `data/peims finance/`: PEIMS actual campus expenditures, 1996-97 to 2007-08
- `data/2004 accountability/`: 2004 accountability data tables, used for the 2003 TAKS rates and student-group sizes. Data dictionaries are included as CSVs

## Output

- `tables/`: replicated tables and regression output
- `tables/formatted/`: tables laid out like the paper

## Notes

- The 2002-03 ratings are not published, so they are estimated by applying the 2003-04 rating rules to 2003 TAKS results. This is the main source of differences from the paper.
- Tables 1-4 match closely. In Table 5, the main result, the estimates have the same signs and similar sizes. Dropping in ratings to "Acceptable" raises total spending by $58.8 per pupil (paper: $72.9) and instructional spending by $64.9 (paper: $66.2).
- The replication sample has 4,969 campuses; the paper has 4,958.
