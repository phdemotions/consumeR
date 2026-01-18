# Phases 9-11: Migration Plan for Tibble-Returning Functions

**Date**: January 17, 2026
**Author**: Claude Sonnet 4.5
**Status**: PLANNING

---

## Executive Summary

Plan to migrate 3 functions that currently return tibbles to use the standardized `build_analysis_result()` infrastructure while maintaining backward compatibility through S3 class methods.

**Target Functions**:
1. **fisher_exact_test()** - Phase 9
2. **mcnemar_test()** - Phase 10
3. **score_composite()** - Phase 11 (different pattern - utility function)

**Challenge**: These functions return tibbles, not structured lists with classes. Migration requires careful approach to maintain compatibility.

---

## Current State Analysis

### Pattern Comparison

**Successfully Migrated (Phases 2-8)**:
```r
# Return pattern
result <- build_analysis_result(...)
class(result) <- c("specific_result", "old_class", "analysis_result", "list")
return(result)
```

**Functions Needing Migration**:
```r
# Current pattern
tibble::tibble(
  field1 = ...,
  field2 = ...,
  interpretation = ...
)
```

### Key Differences

| Aspect | Migrated Functions | Tibble Functions |
|--------|-------------------|------------------|
| Return type | `list` | `tibble` |
| S3 class | Custom classes | Just `tbl_df`, `tbl`, `data.frame` |
| Print method | Custom S3 methods | tibble default |
| Field access | `result$field` | `result$field` (same) |
| Subsetting | List behavior | Data frame behavior |

---

## Migration Strategy

### Approach: Structured List with tibble Compatibility

**Key Decision**: Return structured list (like other migrations) but maintain tibble-like behavior where needed.

**Benefits**:
- ✅ Consistent with established pattern
- ✅ Works with `build_analysis_result()`
- ✅ Can add custom S3 classes
- ✅ Field access remains `result$field`
- ✅ Can preserve interpretation text

**Trade-off**:
- ⚠️ No longer a "real" tibble (but fields still accessible same way)
- ⚠️ Need to ensure existing code doesn't depend on tibble-specific methods

---

## Phase 9: fisher_exact_test()

### Current Structure

**File**: `R/categorical_tests.R` (lines 286-379)
**Return**: Tibble with 7 fields
**Complexity**: MEDIUM (96 lines)

**Current return**:
```r
tibble::tibble(
  p_value = p_value,
  odds_ratio = or,
  or_ci_lower = or_ci_lower,
  or_ci_upper = or_ci_upper,
  alternative = alternative,
  n = sum(obs_table),
  interpretation = interpretation
)
```

### Migration Plan

**Step 1: Create frozen regression baseline**
```r
test_that("fisher_exact_test produces identical output after refactoring (FROZEN BASELINE)", {
  set.seed(12345)
  df <- data.frame(
    x = rep(c("A", "B"), each = 20),
    y = c(rep(c("Yes", "No"), times = c(12, 8)),
          rep(c("Yes", "No"), times = c(5, 15)))
  )
  result <- suppressMessages(fisher_exact_test(df, x = "x", y = "y"))

  # Frozen values
  expect_equal(result$p_value, [actual_value], tolerance = 1e-6)
  expect_equal(result$n, 40)
  # ... all fields
})
```

**Step 2: Refactor to use build_analysis_result()**
```r
# Replace tibble return with:
result <- build_analysis_result(
  test_type = "fisher_exact",
  test_name = "Fisher's Exact Test",
  core_stats = list(p_value = p_value, n = sum(obs_table)),
  specific_stats = list(
    odds_ratio = or,
    or_ci_lower = or_ci_lower,
    or_ci_upper = or_ci_upper,
    alternative = alternative,
    observed = obs_table  # Add this for completeness
  ),
  assumptions = NULL,
  interpretation = interpretation,
  publication_text = NULL  # Could add later
)
class(result) <- c("fisher_exact_result", "fisher_exact", "analysis_result", "list")
result
```

**Step 3: Update tests**
- Update test-categorical_tests.R expectations
- Check for new standardized fields
- Verify all fisher-specific fields present

**Step 4: Check backward compatibility**
- Verify `result$p_value` still works
- Verify `result$odds_ratio` still works
- Check if any code depends on tibble-specific behavior

**Estimated time**: 45-60 minutes

---

## Phase 10: mcnemar_test()

### Current Structure

**File**: `R/categorical_tests.R` (lines 417-506)
**Return**: Tibble with 8 fields
**Complexity**: MEDIUM (90 lines)

**Current return**:
```r
tibble::tibble(
  statistic = as.numeric(chisq_stat),
  df = 1,
  p_value = p_value,
  n_pairs = sum(obs_table),
  n_changed = n_changed,
  pct_changed = pct_changed,
  odds_ratio_change = or_change,
  interpretation = interpretation
)
```

### Migration Plan

**Step 1: Create frozen regression baseline**
```r
test_that("mcnemar_test produces identical output after refactoring (FROZEN BASELINE)", {
  set.seed(54321)
  df <- data.frame(
    before = sample(c("Yes", "No"), 50, replace = TRUE),
    after = sample(c("Yes", "No"), 50, replace = TRUE)
  )
  result <- suppressMessages(mcnemar_test(df, "before", "after"))

  # Frozen values
  expect_equal(result$statistic, [actual_value], tolerance = 1e-6)
  expect_equal(result$n_pairs, 50)
  # ... all fields
})
```

**Step 2: Refactor to use build_analysis_result()**
```r
# Replace tibble return with:
result <- build_analysis_result(
  test_type = "mcnemar",
  test_name = "McNemar's Test",
  core_stats = list(p_value = p_value, n = sum(obs_table)),
  specific_stats = list(
    statistic = as.numeric(chisq_stat),
    df = 1,
    n_pairs = sum(obs_table),
    n_changed = n_changed,
    pct_changed = pct_changed,
    odds_ratio_change = or_change,
    observed = obs_table  # Add for completeness
  ),
  assumptions = NULL,
  interpretation = interpretation,
  publication_text = NULL
)
class(result) <- c("mcnemar_result", "mcnemar", "analysis_result", "list")
result
```

**Step 3: Update tests**
- Update test-categorical_tests.R expectations
- Add standardized field checks
- Verify mcnemar-specific fields present

**Estimated time**: 45-60 minutes

---

## Phase 11: score_composite()

### Current Structure

**File**: `R/composite_scoring.R` (lines 289-?)
**Return**: Structured list (already!) with S3 class
**Complexity**: MEDIUM-HIGH

**Current return** (already good!):
```r
result <- structure(
  list(
    value = composite_values,
    meta = list(
      name = name,
      items = items,
      reverse_items = reverse_items,
      ...
    ),
    ...
  ),
  class = c("composite_score", "list")
)
```

### Migration Assessment

**Good news**: `score_composite()` already returns a structured list with S3 class!

**Does it need migration?**
- ⚠️ Not a statistical test (it's a utility function)
- ⚠️ No p-value or hypothesis testing
- ⚠️ Different purpose than analysis functions

**Recommendation**:
- ✅ Keep as-is (already follows good pattern)
- ✅ Just fix error message tests if needed
- ❌ Don't force into `build_analysis_result()` (wrong tool for the job)

**Action for Phase 11**: Skip migration, just update error message tests

---

## Backward Compatibility Strategy

### Potential Breaking Changes

**What could break**:
1. Code checking `inherits(result, "tbl_df")`
2. Code using tibble-specific methods like `pull()`, `slice()`
3. Code that subsets like a data frame: `result[1, ]`

**Mitigation**:
1. Search codebase for tibble-specific usage
2. Add compatibility methods if needed
3. Document change in NEWS

### Testing Strategy

**For each function**:
1. ✅ Create frozen regression baseline
2. ✅ Verify all field values identical
3. ✅ Check existing tests still pass
4. ✅ Test field access: `result$field_name`
5. ⚠️ Check for tibble-specific usage in tests
6. ⚠️ Search package for usage of these functions

---

## Pre-Migration Checklist

Before starting Phase 9:

### 1. Search for Tibble Dependencies
```r
# Search for tibble-specific usage
grep -r "pull(" tests/
grep -r "as_tibble" R/
grep -r "tbl_df" tests/
```

### 2. Find All Usages
```r
# Find where fisher_exact_test is used
grep -r "fisher_exact_test" tests/
grep -r "fisher_exact_test" R/

# Find where mcnemar_test is used
grep -r "mcnemar_test" tests/
grep -r "mcnemar_test" R/
```

### 3. Review Existing Tests
- Read all fisher_exact tests
- Read all mcnemar tests
- Identify tests that check return type

---

## Expected Outcomes

### After Phase 9 (fisher_exact)

**Functions migrated**: 8 total
**LOC eliminated**: ~368 lines (+15 from fisher)
**ROI**: 75% (368/493)
**Test status**: All passing, zero breaking changes

### After Phase 10 (mcnemar)

**Functions migrated**: 9 total
**LOC eliminated**: ~383 lines (+15 from mcnemar)
**ROI**: 78% (383/493)
**Test status**: All passing, zero breaking changes

### After Phase 11 (score_composite review)

**Functions migrated**: 9 total (score_composite skipped)
**LOC eliminated**: ~383 lines (unchanged)
**ROI**: 78%
**Status**: Error message tests fixed

---

## Risks and Mitigation

### Risk 1: Tibble-Specific Code Breaks

**Risk Level**: MEDIUM
**Impact**: Some existing code might expect tibble

**Mitigation**:
- Search codebase thoroughly before migration
- Add tibble-compatible methods if needed
- Test extensively

### Risk 2: Print Output Changes

**Risk Level**: LOW
**Impact**: Console output might look different

**Mitigation**:
- Test print methods
- Ensure interpretation text still shows
- Document changes if significant

### Risk 3: Performance Degradation

**Risk Level**: VERY LOW
**Impact**: List access might be different from tibble

**Mitigation**:
- Benchmark if concerned
- List access is typically faster than data frame

---

## Success Criteria

### Phase 9 Success
- ✅ fisher_exact_test returns structured list
- ✅ All field values identical to frozen baseline
- ✅ All tests passing
- ✅ Zero breaking changes in existing code
- ✅ New standardized classes added

### Phase 10 Success
- ✅ mcnemar_test returns structured list
- ✅ All field values identical to frozen baseline
- ✅ All tests passing
- ✅ Zero breaking changes
- ✅ New standardized classes added

### Phase 11 Success
- ✅ score_composite assessed (no migration needed)
- ✅ Error message tests updated
- ✅ All tests passing

---

## Alternative Approach (If Issues Found)

### If Tibble Dependency is Critical

**Fallback Option**: Hybrid approach

```r
# Return list that "looks like" a tibble
result <- build_analysis_result(...)
class(result) <- c("fisher_exact_result", "fisher_exact", "analysis_result",
                   "tbl_df", "tbl", "data.frame", "list")

# Add tibble-compatible attributes
attr(result, "row.names") <- 1L
attr(result, "class") <- c("fisher_exact_result", "tbl_df", "tbl", "data.frame")
```

**When to use**: Only if we find critical tibble dependencies

---

## Timeline Estimate

### Phase 9: fisher_exact_test
- Pre-migration search: 15 min
- Create baseline: 10 min
- Refactor: 20 min
- Update tests: 10 min
- Verify: 10 min
- **Total**: 60-75 minutes

### Phase 10: mcnemar_test
- (Similar to Phase 9)
- **Total**: 60-75 minutes

### Phase 11: score_composite
- Assessment: 10 min
- Fix error tests: 15 min
- **Total**: 25 minutes

### Overall
**Total estimated time**: 2.5-3 hours for all three phases

---

## Recommendation

### ✅ PROCEED with Phased Approach

**Rationale**:
1. Consistent pattern across all test functions
2. Likely low risk (field access unchanged)
3. Maintains established migration momentum
4. Achieves ~78% ROI

**Order**:
1. Phase 9: fisher_exact_test (easier, builds confidence)
2. Phase 10: mcnemar_test (similar to fisher)
3. Phase 11: score_composite (just review/fix)

**Next action**: Begin pre-migration checklist (search for dependencies)

---

**Status**: ✅ PLAN COMPLETE - READY FOR EXECUTION

**Estimated completion**: 2.5-3 hours
**Expected final ROI**: 78%
**Expected functions migrated**: 9 (8 tests + review of 1 utility)

---

**Plan created**: January 17, 2026
**Ready to begin**: Phase 9 pre-migration checks
