# COMP3020 Group 73: Is the gender gap in AI closing?

## How to add your part to the report

The report is split into one file per person, so we never edit the same file
and never get Git conflicts.

| File | Who | What goes in it |
|---|---|---|
| `report.Rmd` | Naomi | Main file. Loads the data. **Don't edit.** |
| `sections/01_data_collection.Rmd` | Naomi | 2.1 Data collection |
| `sections/02_rq1_rq2.Rmd` | Naomi | RQ1, RQ2 |
| `sections/03_rq3_rq4.Rmd` | Amber | RQ3, RQ4 |
| `sections/04_rq5.Rmd` | Tristan | RQ5 |
| `sections/05_overall_findings.Rmd` | Tristan | Overall findings and limitations |

### Steps

1. **Open the project.** In RStudio, open `COMP-3020---Group-Assignment.Rproj`
   (File > Open Project). The Git tab appears top right.
2. **Pull** (blue down arrow in the Git tab) to get the latest version.
3. **Open your section file** from the Files tab, in the `sections/` folder.
4. **Paste your code into the empty chunks** and write your text under each heading.
5. **Knit `report.Rmd`** (not your section file) to check everything works.
   Your section appears inside the full report.
6. **Commit and push:** Git tab > Commit > tick only your section file and any
   figures > write a message > Commit > Push.

### Rules for your code

- **Don't push** PDFs, `.RData` or `.Rhistory`.

### Data

- `data/people.csv`: one row per AI researcher (919 rows)
- `data/edges.csv`: links between their Wikipedia articles (2,204 rows)

