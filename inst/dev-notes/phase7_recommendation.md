# Phase 7 Recommendation - Path to 500 Lines

**Date**: January 17, 2026
**Author**: Claude Sonnet 4.5
**Status**: ASSESSMENT COMPLETE

---

## Current Status After Phase 6

| Metric | Value |
|--------|-------|
| Functions migrated | 5 |
| LOC eliminated | ~320 |
| ROI | 65% |
| Original goal | 500 lines |
| Remaining to goal | 180 lines |

---

## Available Migration Targets

### Option 1: friedman_test() ⭐ RECOMMENDED

**File**: `R/nonparametric.R` (lines 523-620)
**Lines**: ~73 lines (function body ~60 lines)
**Complexity**: LOW-MEDIUM (same as kruskal_wallis)
**Type**: Non-parametric repeated measures test (3+ time points)

**Structure**:
```r
result <- list(
  statistic = as.numeric(chi_sq),
  df = as.numeric(test_result$parameter),
  p_value = test_result$p.value,
  kendall_w = as.numeric(kendall_w),
  time_medians = time_medians,
  n_subjects = n,
  n_times = k,
  interpretation = interpretation
)
class(result) <- "friedman"
```

**Why this is perfect**:
- Identical pattern to kruskal_wallis (just migrated)
- Non-parametric (no assumptions to check)
- Clear result structure
- Effect size already calculated (Kendall's W)
- Has interpretation logic already
- ~15 lines of duplication to eliminate

**Estimated effort**: 25-30 minutes
**Expected LOC elimination**: ~15 lines
**New cumulative total**: ~335 lines (67% of goal)

---

### Option 2: calculate_alpha()

**File**: `R/reliability_analysis.R` (lines 108-350)
**Lines**: ~243 lines
**Complexity**: HIGH
**Type**: Cronbach's alpha reliability analysis

**Why NOT recommended**:
- Very complex (243 lines)
- Would require substantial infrastructure extensions
- Likely 4-6 hours to migrate properly
- High risk of breaking changes
- Diminishing returns on effort

---

### Option 3: Other Tier 2 Functions

**run_anova()**: 375 lines, HIGH complexity, 6-8 hours
**run_mlm()**: 418 lines, HIGH complexity, 6-8 hours

**Why NOT recommended**:
- Already assessed in Phase 6
- Significantly higher complexity than justified by remaining goal
- Would require major effort for marginal progress toward 500 line goal

---

## Phase 7 Recommendation

### ✅ RECOMMENDED: Migrate friedman_test()

**Action Plan**:
1. Create frozen regression baseline for friedman_test (10 min)
2. Refactor to use build_analysis_result() (10 min)
3. Update test-nonparametric.R expectations (5 min)
4. Run tests and verify (5 min)
5. Commit and document (5 min)

**Total estimated time**: 35 minutes
**Expected outcomes**:
- 6th function migrated ✅
- ~335 lines eliminated (67% of goal) ✅
- ROI increased to 68% ✅
- Test coverage increased further ✅
- Zero breaking changes ✅

---

## Path to 500 Lines Goal

### After Phase 7 (friedman migration):
- **LOC eliminated**: ~335
- **Remaining to 500**: 165 lines
- **Functions migrated**: 6

### Remaining Options to Reach 500:

**Option A - Continue with complex functions**:
- Migrate run_anova() or run_mlm()
- Would exceed 500 line goal
- Effort: 6-8 hours
- Risk: MEDIUM-HIGH

**Option B - Find more simple functions**:
- Search for other descriptive/simple statistical functions
- Effort: Variable (depends on what's found)
- Risk: LOW

**Option C - Declare success at 67%**:
- Accept 335 lines as substantial achievement
- Original 500 line goal was aspirational
- Infrastructure proven and ROI positive
- Effort: 0 hours
- Risk: NONE

---

## Final Recommendation

### Two-Phase Completion Strategy

**Phase 7 (Now)**:
- ✅ Migrate friedman_test()
- Time: 35 minutes
- Get to 335 lines (67% of goal)

**After Phase 7**:
- 🔍 **Option A**: Search for 2-3 more simple functions (15-20 lines each) to reach 500
- ⏸️ **Option B**: If no simple functions found, declare success at 67% ROI
- ❌ **Option C**: Do NOT tackle ANOVA/MLM complexity for marginal gains

---

## Risk Assessment

### Low Risk ✅
- friedman_test migration (Phase 7)
- Following established pattern
- 30 minutes of work
- High confidence of success

### Medium-High Risk ⚠️
- ANOVA/MLM migrations
- 6-8 hours each
- Complex infrastructure extensions needed
- Higher chance of test failures
- Significant effort for ~15-35% progress toward goal

---

## Success Criteria

**Minimum Success** (After Phase 7):
- 6 functions migrated
- 335 lines eliminated
- 68% ROI
- Pattern established for future work

**Aspirational Success** (If simple functions found):
- 8-10 functions migrated
- 500+ lines eliminated
- 100%+ ROI
- Comprehensive coverage of simple analysis functions

---

## Conclusion

**Immediate Action**: Proceed with Phase 7 friedman_test migration.

**Rationale**:
1. Takes only 35 minutes
2. Follows proven pattern from kruskal_wallis
3. Pushes to 67% of goal with minimal effort
4. Maintains perfect track record (zero breaking changes)
5. Low risk, high reward

**After Phase 7**: Re-evaluate based on simple function availability.

---

**Status**: ✅ RECOMMENDATION READY

**Next Step**: Begin Phase 7 with friedman_test migration

**Estimated completion**: 35 minutes from start

---

**Report generated**: January 17, 2026
**Recommended action**: PROCEED TO PHASE 7 (friedman_test)
