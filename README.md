# Hobbit’s Choice Restaurant Survey Analysis

I used R to explore how restaurant spending, age, location, and dining preferences were related to interest in visiting a proposed restaurant. The project uses the Hobbit’s Choice Restaurant teaching case from *Marketing Research* by Burns and Bush.

## What I analysed

- **Spending, age, and visit likelihood.** I used histograms, boxplots, and a scatter plot to explore the distributions and compare groups.
- **ZIP groups, decor, and media.** I compared counts and percentages for short-drive preference, patron status, decor and music ratings, and media responses.
- **Statistical comparisons.** I used Pearson correlations, Welch t-tests, ANOVA with Tukey comparisons, and an exploratory MANOVA. I also used bootstrapping to estimate intervals for mean spending, household income, and expected entrée price.
- **Regression models.** I modelled spending on the original and log scales, checked the residuals, and used logistic regression to explore probable patron status.

## Selected findings

- People who said they were very likely to visit reported spending about **$280 per month** on their own restaurant meals, compared with **$145** for the other respondents.
- **50%** of respondents in ZIP group 2 were very likely to visit, compared with **6%** in group 3.
- Probable patrons tended to give higher ratings to **elegant decor and a string quartet**. Other respondents tended to give higher ratings to **simple decor and jazz**.
- Higher expected entrée prices were associated with higher reported monthly restaurant spending. The simple regression estimated about **$7 more per month** for each **$1 higher** expected entrée price.

These findings describe the **312 complete responses** used in the main analysis. The original workbook has 400 responses. The complete-case sample covers two ZIP groups.

## Report and code

- [Read the report](Hobbit_Restaurant_Statistical_Analysis.pdf) for the charts, methods, and results.
- [Read the R script](Hobbit_Restaurant_Statistical_Analysis.R) for the full workflow.

## Data and how to run

The Excel workbook is distributed separately as teaching material. The report contains the results and charts. To reproduce the analysis, obtain the original numeric workbook, `Hobbit_values.xls`, through your course or the publisher’s materials.

Install the required packages once in R.

```r
install.packages(c("readxl", "ggplot2", "dplyr"))
```

Open the script in RStudio and click **Source**. If the workbook is outside the working directory, the script asks you to select it. From a terminal, run the script with the workbook path.

```sh
Rscript Hobbit_Restaurant_Analysis.R "/path/to/Hobbit_values.xls"
```

The script prints results and creates plots. In RStudio, use the **Plots** tab to see the charts. In a terminal, the default R graphics device typically saves them to `Rplots.pdf`.
