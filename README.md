# Did Derrick Rose Deserve the 2010-11 NBA MVP?

**[View the full report](https://ifeobateru.github.io/rose-mvp-2011/report.html)**

In 2011, Derrick Rose became the youngest MVP in NBA history, winning 113 of 121 first-place votes. But other players, especially LeBron James, had stronger numbers that season. I used R to test whether Rose's MVP was backed up by the stats.

## Key findings

- Rose ranked **4th in Win Shares** and **6th in scoring** among MVP vote-getters.
- I built regression models on 10 seasons of MVP voting (2006-2015) to learn what voters typically reward. Win Shares and team win percentage were significant predictors; age was not.
- The best model predicted Rose would get about **39%** of the vote share. He got **98%**.
- **Verdict:** by the numbers, LeBron James was the stronger MVP choice. Rose's MVP was driven by team success and his story, not his stats alone.

## Tools

R, tidyverse (dplyr, readr, ggplot2), ggrepel, R Markdown, linear regression

## Files

- `report.Rmd` / `report.html`: the full report
- `analysis.R`: the analysis script
- `data/`: MVP voting and team ratings data from [Basketball-Reference](https://www.basketball-reference.com)
