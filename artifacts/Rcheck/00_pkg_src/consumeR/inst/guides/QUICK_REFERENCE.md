# consumeR Quick Reference Card

Quick reference for all consumeR package functions.

For complete workflow guidance, see `RESEARCH_WORKFLOW.md`.

---

## 📦 Package Functions

### `calculate_summary_stats(data, include_all=TRUE, round_digits=2)`
Calculate descriptive statistics with transparency

```r
stats <- calculate_summary_stats(spending)
# Returns: n, mean, median, sd, min, max, q25, q75, variance, range, iqr
```

### `test_group_differences(group1, group2, test_type="auto", alternative="two.sided", conf_level=0.95, paired=FALSE)`
Compare two groups statistically

```r
result <- test_group_differences(treatment, control)
cat(result$interpretation)  # Plain English results
```

### `create_analysis_report(data, variable=NULL, group_var=NULL, title="...", report_file=NULL)`
Generate comprehensive analysis report

```r
create_analysis_report(data=df, variable="spending",
                      group_var="condition", report_file="report.txt")
```

---

## 📊 Example Dataset

```r
consumer_survey  # Cloud 9 themed customer data

# 100 customers with fun names (Amy Sosa, Jim Halpert, Dwight Schrute, etc.)
# Variables: customer_id, customer_name, flyer_group, spending,
#            satisfaction, loyalty_score
```

---

## 🔧 Common Tasks

### Install Package Locally
```r
devtools::install()
library(consumeR)
```

### Run Tests
```r
devtools::test()
```

### Check Package
```r
devtools::check()
```

### Build Documentation
```r
devtools::document()
```

### Build Package
```r
devtools::build()
```

---

## 📚 Documentation Files

| File | Purpose |
|------|---------|
| **START_HERE.md** | 👈 Start with this one |
| **QUICKSTART.md** | 5-minute intro |
| **EXAMPLES_GUIDE.md** | Fun Superstore/Office examples |
| **GITHUB_SETUP.md** | Upload to GitHub |
| **DEVELOPMENT.md** | Developer guide |
| **README.md** | Package overview |

---

## 🎯 Quick Examples

### Example 1: Basic Stats
```r
library(consumeR)
spending <- c(45.2, 67.8, 23.4, 89.1, 34.5)
calculate_summary_stats(spending)
```

### Example 2: Group Comparison
```r
consumer_survey
flyer <- consumer_survey$spending[consumer_survey$flyer_group == "Got Flyer"]
no_flyer <- consumer_survey$spending[consumer_survey$flyer_group == "No Flyer"]
test_group_differences(flyer, no_flyer)
```

### Example 3: Full Report
```r
create_analysis_report(
  data = consumer_survey,
  variable = "spending",
  group_var = "flyer_group",
  title = "Flyer Campaign Analysis"
)
```

---

## 🌐 URLs (Already Updated!)

All URLs have been updated to use `phdemotions`:

✅ **DESCRIPTION** - GitHub URLs set
✅ **_pkgdown.yml** - Website URL set
✅ **README.md** - Installation and badges set
✅ **CITATION** - GitHub URL set

---

## ✅ Package Status

- **Version**: 0.1.0
- **Status**: CRAN-ready
- **Functions**: 3 main functions
- **Tests**: 40+ unit tests
- **Documentation**: Complete
- **Examples**: Cloud 9/Office themed
- **Git**: Ready to push
- **GitHub Pages**: Configured

---

## 🎬 Next Steps

1. **Read**: START_HERE.md
2. **Try**: Examples in EXAMPLES_GUIDE.md
3. **Upload**: Follow GITHUB_SETUP.md
4. **Share**: Your GitHub Pages site!

---

## 💡 Key Features

✅ Transparent code (extensive comments)
✅ Plain English results
✅ Automatic best practices
✅ Fun themed examples
✅ CRAN-ready structure
✅ Auto-updating documentation site

---

## 🆘 Get Help

```r
?consumeR                         # Package help
?calculate_summary_stats          # Function help
vignette("getting-started")       # Tutorial
```

---

**Website**: https://phdemotions.github.io/consumeR (after GitHub setup)
**Install**: `devtools::install_github("phdemotions/consumeR")`
