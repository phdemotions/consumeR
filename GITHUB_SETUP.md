# GitHub Setup Instructions

This guide will help you upload the consumeR package to GitHub and set up an automatic documentation website.

---

## Part 1: Create GitHub Repository (5 minutes)

### Step 1: Create Repository on GitHub.com

1. Go to https://github.com
2. Click the **"+"** icon (top right) → **"New repository"**
3. Fill in the details:
   - **Repository name**: `consumeR`
   - **Description**: `Transparent and reproducible consumer research analysis for R`
   - **Visibility**: Choose **Public** (required for free GitHub Pages)
   - **DO NOT** initialize with README, .gitignore, or license (we already have these)
4. Click **"Create repository"**

### Step 2: Link Local Repository to GitHub

GitHub will show you commands. Use these in Terminal:

```bash
cd "/Users/josh/My Drive/R/consumeR"

# Add GitHub as remote (replace YOUR_USERNAME with your GitHub username)
git remote add origin https://github.com/YOUR_USERNAME/consumeR.git

# Push to GitHub
git branch -M main
git push -u origin main
```

**Example** (if your username is "jsmith"):
```bash
git remote add origin https://github.com/jsmith/consumeR.git
git branch -M main
git push -u origin main
```

### Step 3: Verify Upload

Go to your repository URL: `https://github.com/YOUR_USERNAME/consumeR`

You should see all 31 files uploaded!

---

## Part 2: Enable GitHub Pages (2 minutes)

### Step 1: Enable GitHub Actions

1. Go to your repository on GitHub
2. Click **"Settings"** tab
3. Click **"Actions"** → **"General"** (left sidebar)
4. Under **"Workflow permissions"**, select:
   - ✅ **"Read and write permissions"**
5. Click **"Save"**

### Step 2: Enable GitHub Pages

1. Still in Settings, click **"Pages"** (left sidebar)
2. Under **"Source"**, select:
   - Source: **Deploy from a branch**
   - Branch: **gh-pages** (it will be created automatically)
   - Folder: **/ (root)**
3. Click **"Save"**

### Step 3: Wait for Website Build

1. Go to **"Actions"** tab in your repository
2. You should see a workflow running called **"pkgdown"**
3. Wait 2-5 minutes for it to complete (green checkmark)
4. Once complete, go back to **Settings → Pages**
5. You'll see: **"Your site is live at https://YOUR_USERNAME.github.io/consumeR"**

---

## Part 3: Your Documentation Website

### What Gets Built Automatically

Your GitHub Pages site includes:

1. **Home Page** - README.md content
2. **Function Reference** - Auto-generated from roxygen2 docs
3. **Getting Started** - Your vignette
4. **Examples Guide** - EXAMPLES_GUIDE.md
5. **Quick Start** - QUICKSTART.md
6. **Development Guide** - DEVELOPMENT.md
7. **Package Overview** - PACKAGE_OVERVIEW.md

### Website URL

```
https://YOUR_USERNAME.github.io/consumeR
```

### Navigation Menu

The site has this menu structure:

- **Home** - Package overview
- **Get Started** - Tutorial vignette
- **Functions** - Complete function reference
- **Examples** - Fun Superstore/Office themed examples
- **Guides** - Development and usage guides
- **News** - Version history

---

## Part 4: Updating Your Package

### After Making Changes

```bash
cd "/Users/josh/My Drive/R/consumeR"

# Stage your changes
git add -A

# Commit with a descriptive message
git commit -m "Your description of changes"

# Push to GitHub
git push
```

### Automatic Updates

Every time you push to GitHub:
1. ✅ Package is automatically tested on Windows, macOS, and Linux
2. ✅ Documentation website is automatically rebuilt
3. ✅ Changes appear on your GitHub Pages site within 2-5 minutes

---

## Part 5: Customize Your Site

### Update URLs in Package Files

Replace `yourusername` with your actual GitHub username in these files:

1. **DESCRIPTION** file:
   ```
   URL: https://github.com/YOUR_USERNAME/consumeR
   BugReports: https://github.com/YOUR_USERNAME/consumeR/issues
   ```

2. **_pkgdown.yml** file:
   ```
   url: https://YOUR_USERNAME.github.io/consumeR
   ```

3. **README.md** file:
   - Update badge URLs
   - Update installation instructions

### Commit These Changes

```bash
git add -A
git commit -m "Update URLs with actual GitHub username"
git push
```

---

## Part 6: Add Badges to README (Optional)

Add these badges to the top of your README.md:

```markdown
<!-- badges: start -->
[![R-CMD-check](https://github.com/YOUR_USERNAME/consumeR/workflows/R-CMD-check/badge.svg)](https://github.com/YOUR_USERNAME/consumeR/actions)
[![pkgdown](https://github.com/YOUR_USERNAME/consumeR/workflows/pkgdown/badge.svg)](https://YOUR_USERNAME.github.io/consumeR)
<!-- badges: end -->
```

These show build status and link to your documentation!

---

## Part 7: Sharing Your Package

### Installation Instructions for Users

Once on GitHub, users can install with:

```r
# Install from GitHub
devtools::install_github("YOUR_USERNAME/consumeR")

# Or using remotes
remotes::install_github("YOUR_USERNAME/consumeR")
```

### Documentation Link

Share your documentation site:
```
https://YOUR_USERNAME.github.io/consumeR
```

---

## Troubleshooting

### Problem: Actions workflow failed

**Solution**:
1. Go to Actions tab
2. Click the failed workflow
3. Read error message
4. Usually it's a missing dependency - check DESCRIPTION file

### Problem: GitHub Pages not showing up

**Solution**:
1. Ensure repository is **Public**
2. Check Settings → Pages is enabled
3. Wait 5-10 minutes after first push
4. Check Actions tab to see if pkgdown workflow succeeded

### Problem: Website looks broken

**Solution**:
1. Check _pkgdown.yml syntax
2. Ensure all .md files referenced exist
3. Rebuild locally first: `pkgdown::build_site()`

### Problem: Can't push to GitHub

**Solution**:
```bash
# Check remote is set
git remote -v

# If not set, add it
git remote add origin https://github.com/YOUR_USERNAME/consumeR.git

# Try push again
git push -u origin main
```

---

## Local Website Preview (Optional)

Want to preview the website before pushing to GitHub?

```r
# Install pkgdown if you haven't
install.packages("pkgdown")

# Build site locally
pkgdown::build_site()

# Opens in browser - preview at docs/index.html
```

---

## Complete Example Workflow

Here's the complete process from scratch:

### 1. Create GitHub repo (on github.com)
```
Repository name: consumeR
Description: Transparent consumer research analysis
Public: Yes
Initialize: No (already have files)
```

### 2. Link and push (in Terminal)
```bash
cd "/Users/josh/My Drive/R/consumeR"
git remote add origin https://github.com/jsmith/consumeR.git
git branch -M main
git push -u origin main
```

### 3. Enable Actions & Pages (on github.com)
```
Settings → Actions → General → Read and write permissions → Save
Settings → Pages → Deploy from branch: gh-pages → Save
```

### 4. Wait for build (2-5 minutes)
```
Actions tab → Watch pkgdown workflow complete
```

### 5. Visit your site!
```
https://jsmith.github.io/consumeR
```

### 6. Update URLs and push
```bash
# Edit DESCRIPTION, _pkgdown.yml, README.md
# Replace "yourusername" with "jsmith"

git add -A
git commit -m "Update URLs with GitHub username"
git push
```

---

## What Your Site Will Look Like

### Home Page
- Package description
- Installation instructions
- Quick examples
- Links to detailed guides

### Function Reference
- `calculate_summary_stats()` - Full documentation
- `test_group_differences()` - Full documentation
- `create_analysis_report()` - Full documentation
- `consumer_survey` - Dataset documentation

### Articles/Vignettes
- Getting Started guide
- Examples with Cloud 9/Office themes
- Quick start guide
- Development guide

### Navigation
- Beautiful Bootstrap 5 theme (Flatly)
- Search functionality
- Mobile-friendly
- Automatic table of contents

---

## Advanced: Custom Domain (Optional)

Want to use your own domain (e.g., `consumeR.yourdomain.com`)?

1. Add a file named `CNAME` to your repo:
   ```
   consumeR.yourdomain.com
   ```

2. Configure DNS with your domain provider:
   - Add CNAME record pointing to `YOUR_USERNAME.github.io`

3. Enable in Settings → Pages → Custom domain

---

## Summary Checklist

- [ ] Create GitHub repository (public)
- [ ] Link local repo and push
- [ ] Enable Actions with write permissions
- [ ] Enable GitHub Pages (gh-pages branch)
- [ ] Wait for pkgdown workflow to complete
- [ ] Update URLs in DESCRIPTION, _pkgdown.yml, README.md
- [ ] Add badges to README (optional)
- [ ] Share your documentation site!

---

## Your Package is Now:

✅ **On GitHub** - Version controlled and backed up
✅ **Tested Automatically** - CI/CD on Windows, macOS, Linux
✅ **Documented Online** - Beautiful website with all docs
✅ **Easy to Install** - One command: `devtools::install_github()`
✅ **Professional** - Looks like a real CRAN package!

---

## Need Help?

- GitHub Docs: https://docs.github.com/en/pages
- pkgdown Docs: https://pkgdown.r-lib.org/
- R Packages book: https://r-pkgs.org/

---

**Next Step**: Follow Part 1 to create your GitHub repository and upload the package!
