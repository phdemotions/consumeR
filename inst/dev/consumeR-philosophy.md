# consumeR Package Philosophy

**Version:** 0.1.0
**Last Updated:** 2026-01-17
**Audience:** Package maintainers and contributors

────────────────────────────────────────────────────────────────────────────────

## Core Mission

**consumeR exists to make consumer research transparent, reproducible, and accessible to absolute beginners.**

Every design decision must serve this mission. We are not building a general-purpose statistics package. We are building a teaching tool that happens to produce publication-ready research.

────────────────────────────────────────────────────────────────────────────────

## The Three Pillars

### 1. **BEGINNER-FIRST DESIGN**

**Our users are PhD students who:**
- Are learning statistics for the first time
- May not know R idioms or programming concepts
- Need to publish in top journals (JCP, JCR, JMR)
- Are overwhelmed by choices and technical jargon

**What this means in practice:**
- Every function explains what it does, what assumptions it makes, and what to do next
- Silence is a bug. If we drop NAs, coerce types, or make assumptions, we tell the user explicitly
- Error messages are educational: "You got X error. Here's why. Here's what to do."
- No function returns just a number. Every function returns structured output with interpretation
- We over-communicate. Always.

**Anti-pattern:** "Assume the user knows what a Shapiro-Wilk test is."
**Our pattern:** "Testing normality using Shapiro-Wilk test (checks if data follows a bell curve)..."

────────────────────────────────────────────────────────────────────────────────

### 2. **PUBLICATION-READY BY DEFAULT**

**Our users must publish in:**
- Journal of Consumer Psychology (JCP)
- Journal of Consumer Research (JCR)
- Journal of Marketing Research (JMR)
- Other top-tier, peer-reviewed journals

**What this means in practice:**
- Every analysis function returns APA7-formatted text ready to paste into a paper
- Every output includes reproducibility metadata (R version, package version, seed, alpha level)
- We follow the most conservative, widely-accepted practices by default
- No shortcuts that reviewers will flag

**The APA7 Reporting Contract (mandatory for all analysis functions):**
```r
result <- list(
  results = <tidy data frame>,           # Numeric results
  apa_text = <APA7 narrative>,          # "t(98) = 2.45, p = .016, d = 0.49"
  methods_text = <plain language>,       # "We conducted an independent samples t-test..."
  interpretation_text = <beginner help>, # "This means Group A scored significantly higher..."
  reproducibility = <metadata>           # Version, seed, alpha, CI level
)
```

**Anti-pattern:** Return just a p-value.
**Our pattern:** Return everything needed to write the Methods and Results sections.

────────────────────────────────────────────────────────────────────────────────

### 3. **FORWARD-ONLY DEVELOPMENT**

**Technical debt is poison for a research package.**

**What this means in practice:**
- No regression to old code. If existing code conflicts with best practice, refactor forward.
- No resurrection of deprecated patterns. Don't patch legacy helpers; build new ones.
- Bottom-up scaffolding: L1 helpers → L2 engines → L3 user-facing. Never scaffold downward.
- Every new feature must be CRAN + JOSS ready from day one.

**Anti-pattern:** "Let's add a quick fix to make this work, we'll clean it up later."
**Our pattern:** "This requires L1 helpers. Let me implement them properly first."

────────────────────────────────────────────────────────────────────────────────

## Design Principles

### **Transparency Over Convenience**
- We show the user what we're doing, even if it's verbose
- We log every transformation, exclusion, and assumption
- We never silently coerce or drop data

### **Education Over Efficiency**
- We teach statistics through our messages and documentation
- We explain the "why" not just the "what"
- We guide the user to the next step

### **Clarity Over Cleverness**
- Explicit is better than implicit (use `stats::lm`, not `lm`)
- Simple is better than sophisticated
- Readable is better than compact

### **Reproducibility Over Flexibility**
- Deterministic outputs (set seeds for randomness)
- No side effects (no file writes unless explicitly requested)
- Complete metadata for every analysis

### **Safety Over Speed**
- Validate inputs aggressively with educational errors
- Fail fast with clear guidance
- Never let the user publish wrong results

────────────────────────────────────────────────────────────────────────────────

## Architectural Rules

### **Layered Architecture (L1 → L2 → L3)**

**L1 - Pure Helpers:**
- No external dependencies (just base R, stats, rlang)
- Single responsibility (one small, testable task)
- No user-facing messages
- Examples: `standardize_z()`, `extract_value_labels()`

**L2 - Internal Engines:**
- Compose L1 helpers into analysis logic
- Handle core computation and validation
- Can use `cli` for structured output
- Examples: `t_test_engine()`, `composite_scoring_engine()`

**L3 - User-Facing Functions:**
- Wrap L2 engines with beginner-friendly interface
- Return APA7 reporting contract objects
- Extensive documentation with examples
- Examples: `test_group_differences()`, `create_composite()`

**Rule:** Never skip layers. If L3 needs something L1 doesn't provide, build L1 first.

────────────────────────────────────────────────────────────────────────────────

### **Testing Strategy**

**Layer-Specific Tests:**
- `test-L1-<topic>.R` - Test L1 helpers in isolation
- `test-L2-<topic>.R` - Test L2 engines (can use L1)
- `test-L3-<topic>.R` - Test L3 user-facing functions (full integration)

**Development Workflow:**
- During L1/L2 development: `testthat::test_file("tests/testthat/test-L1-*.R")`
- Before merge: `devtools::test()` (all tests must pass)
- Before release: `devtools::check()` with 0 errors, 0 warnings, 0 notes

**CI Rule:** Skipped tests in `test-L3-*` files are NOT allowed on main branch.

────────────────────────────────────────────────────────────────────────────────

## Code Style (Non-Negotiable)

### **Namespace Discipline**
```r
# GOOD
stats::lm(y ~ x, data = df)
dplyr::mutate(df, new_col = old_col * 2)

# BAD
lm(y ~ x, data = df)         # Implicit namespace
mutate(df, new_col = old_col * 2)
```

### **Messaging**
```r
# GOOD
cli::cli_alert_success("Loaded {n} observations")
rlang::abort(c("Input validation failed",
               "x" = "You provided {class(x)}",
               "i" = "Expected a data frame"))

# BAD
message("Loaded ", n, " observations")  # Hard to format
stop("Bad input")                       # Not educational
```

### **Validation**
```r
# GOOD - Use centralized helper
assert_beginner_safe(x, type = "numeric", min_length = 1)

# BAD - Hand-rolled validation
if (!is.numeric(x)) stop("x must be numeric")
```

────────────────────────────────────────────────────────────────────────────────

## The Beginner Lens

**Before committing any code, ask:**

1. **Can a beginner understand what this function does?**
   - If not: Add more documentation, examples, and explanation

2. **Will this help them learn statistics?**
   - If not: Add interpretation text and educational messages

3. **Will this pass peer review at JCP/JCR/JMR?**
   - If not: Use more conservative defaults and add citations

4. **Will this work 5 years from now?**
   - If not: Add more reproducibility metadata

5. **Will this confuse reviewers?**
   - If yes: Add more transparency and documentation

────────────────────────────────────────────────────────────────────────────────

## What We Are NOT

- ❌ A general-purpose statistics package (use `stats`, `lme4`, `lavaan` for that)
- ❌ A plotting package (we provide basic plots, use `ggplot2` for customization)
- ❌ A data wrangling package (use `dplyr`, `tidyr` for complex manipulation)
- ❌ A replacement for statistical education (we guide, but users still need to learn)

────────────────────────────────────────────────────────────────────────────────

## What We ARE

- ✅ A teaching tool that produces publication-ready research
- ✅ A transparency engine for peer review
- ✅ A safety net for statistics beginners
- ✅ A bridge between "I have data" and "I have a published paper"

────────────────────────────────────────────────────────────────────────────────

## Success Metrics

**We succeed when:**
1. A beginner can run their first analysis without reading the manual
2. A reviewer can reproduce results exactly from the Methods section
3. A user learns statistics concepts while using the package
4. A paper using consumeR passes peer review without statistical revisions

**We fail when:**
1. A user publishes incorrect results
2. A reviewer can't reproduce our output
3. A beginner is confused or intimidated
4. Technical debt forces us to maintain legacy code

────────────────────────────────────────────────────────────────────────────────

## Reference This Philosophy

**In the main prompt (for Claude Code sessions):**
> Before contributing, read `inst/dev/consumeR-philosophy.md` for core principles.

**In CONTRIBUTING.md (for human contributors):**
> Our package philosophy is documented in `inst/dev/consumeR-philosophy.md`. Please read it before making significant changes.

**In code reviews:**
> "Does this align with our beginner-first philosophy? See inst/dev/consumeR-philosophy.md"

────────────────────────────────────────────────────────────────────────────────

## Living Document

This philosophy evolves as we learn from users, reviewers, and contributors.

**To propose changes:** Open an issue with the "philosophy" label.

**Last major revision:** 2026-01-17 (Initial version)

────────────────────────────────────────────────────────────────────────────────
