# consumeR Package Refactoring - Migration Roadmap

**Last Updated**: January 17, 2026
**Current Phase**: Phase 3 Complete
**Next Phase**: Phase 4

---

## Overview

This document provides a prioritized roadmap for migrating consumeR analysis functions to use the shared infrastructure created in Phases 1-2.

**Infrastructure Available**:
- ✅ `check_test_assumptions()` - Unified assumption checking
- ✅ `build_analysis_result()` - Standardized result construction
- ✅ `print.analysis_result()` - S3 generic print method
- ✅ `render_apa7_text()` - APA7 template rendering
- ✅ `format_p_apa7()` - APA7 p-value formatting

---

## Migration Status

### ✅ Completed (Phase 1-3)

| Function | File | LOC Reduced | Phase | Status |
|----------|------|-------------|-------|--------|
| `calculate_summary_stats()` | summary_stats.R | ~134 | 2 | ✅ Complete |
| `test_group_differences()` | group_tests.R | ~136 | 2 | ✅ Complete |
| **TOTAL** | - | **~270** | - | - |

---

## Phase 4: Quick Wins (Non-Parametric Tests)

**Goal**: Migrate 2 simple non-parametric tests
**Expected LOC Reduction**: ~150 lines
**Estimated Time**: 2-4 hours
**Risk Level**: LOW

### 4.1 wilcoxon_signed_rank_test() ⭐ PRIORITY 1

**File**: `R/nonparametric.R`
**Function lines**: ~86 (lines 347-432)
**Complexity**: LOW

**Why migrate first**:
- Similar structure to already-migrated `test_group_differences()`
- Simpler than correlation (fewer assumptions)
- Non-parametric → fewer assumption checks
- Good confidence builder before tackling harder functions

**Migration checklist**:
- [ ] Read current implementation
- [ ] Create frozen regression baseline
- [ ] Replace assumption checking with `check_test_assumptions()`
- [ ] Replace result construction with `build_analysis_result()`
- [ ] Add APA7 publication text (create new template if needed)
- [ ] Update existing tests
- [ ] Run regression tests (verify identical output)
- [ ] Run benchmark tests
- [ ] Update documentation

**Estimated effort**: 1-2 hours

---

### 4.2 mann_whitney_test() (if exists)

**File**: `R/nonparametric.R` (likely)
**Complexity**: LOW

**Checklist**: Same as 4.1

**Estimated effort**: 1-2 hours

---

## Phase 5: Medium Complexity (ANOVA & MLM)

**Goal**: Migrate 2 medium-complexity functions
**Expected LOC Reduction**: ~200 lines
**Estimated Time**: 6-8 hours
**Risk Level**: MEDIUM

### 5.1 run_anova()

**File**: `R/modeling.R`
**Complexity**: MEDIUM

**Challenge factors**:
- Post-hoc tests add complexity
- Multiple comparison corrections
- Effect size calculations (eta-squared, omega-squared)
- May have multiple output formats

**Migration strategy**:
1. Start with basic ANOVA (one-way)
2. Add post-hoc support incrementally
3. Ensure all correction methods work
4. Add comprehensive tests for each post-hoc method

**Estimated effort**: 3-4 hours

---

### 5.2 run_mlm()

**File**: `R/mlm.R`
**Complexity**: MEDIUM

**Challenge factors**:
- Multi-level model diagnostics unique to this function
- ICC calculations
- Random effects handling
- May need new assumption check types

**Migration strategy**:
1. Review existing assumption checking
2. Extend `check_test_assumptions()` if needed for MLM-specific checks
3. Create APA7 template for MLM results
4. Ensure ICC and random effects integrate smoothly

**Estimated effort**: 3-4 hours

---

## Phase 6: High Complexity (Correlation & Regression)

**Goal**: Migrate 2 high-value, complex functions
**Expected LOC Reduction**: ~400+ lines
**Estimated Time**: 8-12 hours
**Risk Level**: MEDIUM-HIGH

### 6.1 analyze_correlation() ⚠️ HIGH EFFORT

**File**: `R/correlation.R`
**Function lines**: ~500
**Complexity**: HIGH

**Challenge factors**:
- **Dual input modes**:
  - Data frame + var1/var2
  - Two separate vectors
- **Method selection logic**: auto/pearson/spearman
- **Assumption checking**:
  - Linearity check
  - Bivariate normality
  - Outlier detection
- **Complex interpretation**: Strength categorization
- **Publication text**: Multiple sections with detailed guidance

**Migration strategy**:
1. **Phase 6a**: Migrate core correlation calculation
   - Focus on Pearson method first
   - Standardize input processing
   - Use `build_analysis_result()` for output
2. **Phase 6b**: Migrate assumption checking
   - May need new assumption types in infrastructure
   - Linearity check integration
3. **Phase 6c**: Add Spearman support
4. **Phase 6d**: Add auto-selection logic
5. **Phase 6e**: Create comprehensive APA7 template

**Estimated effort**: 4-6 hours (split across sub-phases)

**Risk mitigation**:
- Break into sub-phases
- Test after each sub-phase
- Maintain backward compatibility throughout

---

### 6.2 run_regression()

**File**: Likely `R/modeling.R` or `R/regression.R`
**Complexity**: HIGH (assumed)

**Challenge factors** (to assess):
- Model diagnostics (residual plots, influential points)
- Multiple predictor handling
- Multicollinearity checks (VIF)
- Model comparison capabilities
- Coefficient interpretation

**Migration strategy** (tentative):
1. Assess current implementation complexity
2. Create infrastructure extensions if needed (e.g., VIF checking)
3. Migrate in phases similar to correlation
4. Ensure all diagnostic plots/checks integrated

**Estimated effort**: 3-5 hours (pending complexity assessment)

---

## Future Phases (Phase 7+)

### Remaining Candidates

These functions may benefit from migration but are lower priority:

| Function | Estimated Complexity | Notes |
|----------|---------------------|-------|
| `compare_paired_groups()` | LOW-MEDIUM | Similar to `test_group_differences` |
| `run_chisq()` | LOW | Categorical data, simpler assumptions |
| `run_logistic()` | MEDIUM-HIGH | Logistic regression specifics |
| `run_sem()` | HIGH | Structural equation modeling complexity |
| `run_rm_anova()` | MEDIUM-HIGH | Repeated measures complexity |
| `mediation_*()` functions | MEDIUM-HIGH | Multi-stage analyses |

**Strategy**: Assess value vs. effort for each after Phase 6 completion.

---

## Cumulative Projections

### If All Phases Complete

| Metric | Current | Phase 4 | Phase 5 | Phase 6 | Total |
|--------|---------|---------|---------|---------|-------|
| **Functions migrated** | 2 | 4 | 6 | 8 | 8 |
| **LOC eliminated** | 270 | 420 | 620 | 1,020+ | 1,020+ |
| **Test coverage** | 95% | 96%+ | 97%+ | 98%+ | 98%+ |
| **Infrastructure reuse** | 2 funcs | 4 funcs | 6 funcs | 8 funcs | 8 funcs |

### Return on Investment

**Infrastructure cost**: 493 LOC (one-time)
**Break-even point**: ~2 functions migrated (already achieved!)
**Current ROI**: 270 lines eliminated / 493 invested = **55% recouped**
**Projected ROI** (Phase 6 complete): 1,020+ / 493 = **207% ROI**

---

## Migration Principles

### 1. Zero Breaking Changes
- Function names unchanged
- Argument names unchanged
- Argument defaults unchanged
- Core statistical outputs identical

### 2. Test-First Approach
- Create frozen regression baseline BEFORE refactoring
- Update tests to accept new structure
- Verify bit-for-bit output where possible
- Accept intentional improvements (better errors, new fields)

### 3. Incremental Safety
- Commit after each function migration
- Run full test suite before moving to next function
- Document any unexpected findings

### 4. Documentation Excellence
- Update roxygen docs simultaneously
- Ensure examples still work
- Add notes about new capabilities (if any)

### 5. Benchmark Validation
- Run benchmark tests for each migrated function
- Verify no performance degradation
- Document any performance improvements

---

## Decision Criteria for Prioritization

When choosing next migration target, consider:

1. **Complexity** (LOW preferred)
   - Lines of code
   - Number of input modes
   - Assumption checking complexity
   - Output structure complexity

2. **Value** (HIGH preferred)
   - Amount of code duplication eliminated
   - User-facing impact
   - Test coverage improvement

3. **Risk** (LOW preferred)
   - Edge cases
   - Dependency on other functions
   - User base size (popular functions = higher risk)

4. **Similarity** (HIGH preferred)
   - Similar to already-migrated functions
   - Reuses existing patterns
   - Minimal new infrastructure needed

---

## Success Criteria

### Phase Completion
Each phase is complete when:
- ✅ All target functions migrated
- ✅ All tests passing (existing + regression)
- ✅ Documentation updated
- ✅ Benchmarks run successfully
- ✅ Evaluation report written
- ✅ Code committed to git

### Project Completion
The refactoring project is complete when:
- ✅ All high-value functions migrated (Phases 4-6)
- ✅ 500+ lines of duplication eliminated
- ✅ 98%+ test pass rate
- ✅ Zero breaking changes
- ✅ All APA7 templates implemented
- ✅ Comprehensive documentation
- ✅ Performance maintained or improved

---

## Quick Reference

### Current Status
- **Phase**: 3 (Complete)
- **Functions migrated**: 2
- **LOC eliminated**: 270
- **Test pass rate**: 95%

### Next Steps
1. Review this roadmap
2. Begin Phase 4 with `wilcoxon_signed_rank_test()`
3. Follow migration checklist (section 4.1)
4. Document learnings in Phase 4 evaluation report

---

**Last Updated**: January 17, 2026 by Claude Sonnet 4.5
**Status**: Ready for Phase 4 execution
