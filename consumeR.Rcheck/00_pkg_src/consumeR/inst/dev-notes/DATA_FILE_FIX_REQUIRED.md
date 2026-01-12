# ⚠️ ACTION REQUIRED: Generate consumer_survey.rda

## Issue Discovered

The `consumer_survey` dataset is documented in `R/data.R` and the code to create it exists in `R/consumer_survey_data.R`, but the actual `data/consumer_survey.rda` file is missing.

This causes the error:
```
Error: 'consumer_survey' is not an exported object from 'namespace:consumeR'
```

## Solution

Run this command in R/RStudio from the package root directory:

```r
source("data-raw/create_consumer_survey.R")
```

This will:
1. Create the `data/` directory
2. Generate `data/consumer_survey.rda` with the proper dataset
3. Make the dataset available to package users

## After Running

After running the script:

1. **Commit the data file**:
   ```bash
   git add data/consumer_survey.rda data-raw/
   git commit -m "Add consumer_survey dataset file"
   git push
   ```

2. **Verify it works**:
   ```r
   devtools::load_all()
   data(consumer_survey)
   head(consumer_survey)
   ```

3. **Optional cleanup**:
   You can then remove `R/consumer_survey_data.R` since the data will be in `data/consumer_survey.rda`:
   ```bash
   git rm R/consumer_survey_data.R
   git commit -m "Remove redundant data generation file"
   ```

## Why This Happened

The `R/consumer_survey_data.R` file creates the dataset when sourced, but R packages need datasets in the `data/` directory as `.rda` files for them to be accessible via `data()` and properly exported.

## Alternative: LazyData

If you prefer datasets to be automatically loaded (without needing `data(consumer_survey)`), you can also set `LazyData: true` in DESCRIPTION after creating the .rda file.
