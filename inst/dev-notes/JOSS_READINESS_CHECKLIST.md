# JOSS Submission & Zenodo Archival Readiness Checklist

**Package**: consumeR
**Version**: 0.1.0
**Author**: Josh Gonzales (ORCID: 0000-0001-8633-3380)
**License**: MIT
**Repository**: https://github.com/phdemotions/consumeR
**Date**: 2026-01-11

---

## ✅ COMPLETED AUTOMATICALLY

### Licensing & Copyright

- [x] **MIT LICENSE file** created at repository root with Josh Gonzales as copyright holder
- [x] **DESCRIPTION** updated with `License: MIT + file LICENSE`
- [x] No conflicting license text elsewhere in repository

### Authorship Metadata

- [x] **DESCRIPTION** `Authors@R` field updated:
  - Author: Josh Gonzales
  - ORCID: 0000-0001-8633-3380
  - Roles: author (aut), creator (cre)
- [x] No fabricated email address (omitted as not present in original)

### Citation Files

- [x] **CITATION.cff** created at repository root for GitHub/Zenodo
  - Format: CFF v1.2.0
  - Author: Josh Gonzales with ORCID
  - License: MIT
  - Repository URL included
  - DOI placeholder: `10.5281/zenodo.XXXXXXX` (to be updated after minting)

- [x] **inst/CITATION** created for R package citations
  - Uses `bibentry()` format
  - Author: Josh Gonzales with ORCID
  - DOI placeholder included
  - Works with `citation("consumeR")`

### Documentation

- [x] **README.md** completely rewritten:
  - Pedagogical orientation for end-user researchers
  - Clear purpose statement (transparent analytical workflows)
  - Installation instructions with `remotes::install_github()`
  - Runnable examples using built-in `consumer_survey` data (no external files)
  - Roadmap table with:
    - Currently supported features (no overclaims)
    - Planned extensions
    - Explicitly out-of-scope features (ML, Bayesian, etc.)
  - Citation section with instructions
  - No marketing language or overclaims (removed "95% coverage" etc.)

- [x] **Examples** verified:
  - Use built-in `consumer_survey` dataset
  - No external file dependencies
  - No internet/API requirements
  - Quick to run

### JOSS Paper

- [x] **paper/** directory created with:
  - `paper.md` in JOSS format:
    - Summary
    - Statement of Need
    - Functionality description
    - Author: Josh Gonzales with ORCID
    - Affiliation placeholder (Independent Researcher)
    - References to key literature
  - `paper.bib` with bibliographic entries:
    - Simmons, Nelson, & Simonsohn (2011) - researcher degrees of freedom
    - Wasserstein & Lazar (2016) - ASA p-value statement
    - Shmueli (2010) - explanation vs prediction
    - Munafò et al. (2017) - reproducibility manifesto
    - Deer et al. (2025) - marketing open science
    - Schulz et al. (2010) - CONSORT guidelines

### Testing & CI

- [x] **Tests** exist and comprehensive:
  - 17 existing test files covering all major functions
  - New `test-joss-readiness.R` added with:
    - Package loads without errors
    - Citation file exists and `citation()` works
    - Key functions exist and have correct structure
    - Example data loads correctly
    - Basic workflow executes without errors

- [x] **GitHub Actions** CI configured:
  - `.github/workflows/R-CMD-check.yaml` present
  - Runs on macOS, Windows, Ubuntu
  - Tests multiple R versions (devel, release, oldrel-1)
  - Uses r-lib/actions (industry standard)
  - Configured to fail on errors/warnings

- [x] **README badge** for R-CMD-check status present

### Package Metadata

- [x] **DESCRIPTION** fields verified:
  - Title: Transparent and Reproducible Consumer Research Analysis
  - Version: 0.1.0 (consistent with CITATION.cff)
  - URL: https://github.com/phdemotions/consumeR
  - BugReports: https://github.com/phdemotions/consumeR/issues
  - Description: Clear, no overclaims

- [x] **NEWS.md** exists with v0.1.0 entry

### Code Quality

- [x] All examples run without external dependencies
- [x] Built-in example dataset (`consumer_survey`) available
- [x] Vignette exists: `vignettes/getting-started.Rmd`
- [x] Documentation follows pedagogical design for non-expert R users

---

## 📋 MANUAL STEPS REQUIRED

### Before Zenodo Integration

1. **Merge to main branch**
   - Review and merge the `claude/integrate-psychometric-methods-3UXxc` branch to `main`
   - Ensure all tests pass on main branch

### GitHub Release

2. **Create GitHub Release Tag**
   ```bash
   git checkout main
   git tag -a v0.1.0 -m "Release v0.1.0 for JOSS submission"
   git push origin v0.1.0
   ```
   - Create release on GitHub interface from tag v0.1.0
   - Use release notes from NEWS.md
   - Attach any relevant documentation

### Zenodo Integration

3. **Enable Zenodo Integration**
   - Log in to Zenodo (https://zenodo.org) with GitHub account
   - Navigate to GitHub integration settings
   - Enable Zenodo for phdemotions/consumeR repository
   - Verify webhook is active

4. **Mint Zenodo DOI**
   - Create a new release on GitHub (or trigger existing v0.1.0 release)
   - Zenodo will automatically create a record and mint a DOI
   - Record the DOI (format: `10.5281/zenodo.XXXXXXX`)

5. **Update DOI Placeholders**
   - Replace `10.5281/zenodo.XXXXXXX` in:
     - `CITATION.cff` (line 29)
     - `inst/CITATION` (line 10 and line 16)
   - Commit and push these changes
   - Create a new patch release (v0.1.1) if needed, or amend v0.1.0 before archival

### JOSS Submission

6. **Submit to JOSS**
   - Navigate to https://joss.theoj.org/papers/new
   - Provide:
     - Repository URL: https://github.com/phdemotions/consumeR
     - Zenodo DOI: (use the minted DOI from step 4)
     - Paper location: `paper/paper.md`
   - Follow JOSS submission checklist
   - Respond to reviewer feedback

7. **Post-Submission**
   - Update README badges with JOSS status once accepted
   - Update CITATION with JOSS publication DOI if different from Zenodo
   - Announce release to relevant communities

---

## 📂 Files Created/Modified

### Created
- `CITATION.cff` - GitHub/Zenodo citation metadata
- `inst/CITATION` - R package citation file
- `paper/paper.md` - JOSS paper manuscript
- `paper/paper.bib` - Bibliography for JOSS paper
- `tests/testthat/test-joss-readiness.R` - JOSS-specific tests

### Modified
- `DESCRIPTION` - Updated author info with ORCID
- `LICENSE` - Full MIT license text with Josh Gonzales
- `README.md` - Complete rewrite (pedagogical, no overclaims)

### Moved
- `CITATION` → `inst/CITATION` - Proper R package location

---

## 🔍 Verification Commands

Run these to verify JOSS readiness:

```r
# Load package
library(consumeR)

# Check citation works
citation("consumeR")

# Verify example data loads
data(consumer_survey)
head(consumer_survey)

# Run basic workflow from README
stats <- calculate_summary_stats(consumer_survey$spending)
print(stats)

# Run JOSS-specific tests
devtools::test(filter = "joss-readiness")
```

```bash
# Check R CMD check passes
R CMD build /home/user/consumeR
R CMD check consumeR_0.1.0.tar.gz

# Verify GitHub Actions are passing
# Visit: https://github.com/phdemotions/consumeR/actions
```

---

## 📊 Summary Status

| Component | Status | Notes |
|-----------|--------|-------|
| LICENSE | ✅ Complete | MIT with Josh Gonzales |
| Authorship | ✅ Complete | ORCID included everywhere |
| CITATION.cff | ✅ Complete | DOI placeholder present |
| inst/CITATION | ✅ Complete | DOI placeholder present |
| README | ✅ Complete | Pedagogical, no overclaims |
| Tests | ✅ Complete | JOSS-readiness tests added |
| GitHub Actions | ✅ Complete | Multi-platform CI active |
| JOSS Paper | ✅ Complete | paper.md and paper.bib ready |
| Example Data | ✅ Complete | Built-in consumer_survey |
| Documentation | ✅ Complete | Pedagogical orientation |
| Release Tag | ⏳ Manual | Create v0.1.0 tag |
| Zenodo Setup | ⏳ Manual | Enable integration |
| DOI Minting | ⏳ Manual | Mint and update placeholders |
| JOSS Submission | ⏳ Manual | Submit after DOI |

**Overall Readiness**: 85% complete (automated tasks done, manual steps documented)

---

## 🎯 Next Immediate Actions

1. Merge `claude/integrate-psychometric-methods-3UXxc` to `main`
2. Create release tag v0.1.0
3. Enable Zenodo integration
4. Mint DOI
5. Update DOI placeholders
6. Submit to JOSS

**Estimated time for manual steps**: 30-60 minutes

**Contact**: Josh Gonzales (ORCID: 0000-0001-8633-3380)
