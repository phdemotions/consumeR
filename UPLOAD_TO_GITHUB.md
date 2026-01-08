# Upload consumeR to GitHub - Simple Instructions

## Step 1: Create GitHub Repository (2 minutes)

1. Go to https://github.com/phdemotions
2. Click the green **"New"** button (or **"+"** → **"New repository"**)
3. Fill in:
   - **Repository name**: `consumeR`
   - **Description**: `Transparent and reproducible consumer research analysis for R`
   - **Public** repository (required for free GitHub Pages)
   - **DO NOT** check "Initialize with README" (we already have one)
4. Click **"Create repository"**

## Step 2: Upload Your Code (1 minute)

Copy and paste these commands into Terminal:

```bash
cd "/Users/josh/My Drive/R/consumeR"
git remote add origin https://github.com/phdemotions/consumeR.git
git push -u origin main
```

That's it! Your code is now on GitHub at:
**https://github.com/phdemotions/consumeR**

## Step 3: Enable GitHub Pages (2 minutes)

1. Go to your repository: https://github.com/phdemotions/consumeR
2. Click **"Settings"** tab
3. Click **"Actions"** → **"General"** in left sidebar
4. Under "Workflow permissions", select:
   - ✅ **"Read and write permissions"**
   - Click **"Save"**
5. Click **"Pages"** in left sidebar
6. Under "Build and deployment":
   - Source: **"Deploy from a branch"**
   - Branch: **"gh-pages"** (select from dropdown)
   - Click **"Save"**

## Step 4: Wait for Your Website (5 minutes)

1. Click **"Actions"** tab
2. Watch the **"pkgdown"** workflow run (takes ~5 minutes)
3. When it shows a green checkmark ✅, your site is ready!
4. Visit: **https://phdemotions.github.io/consumeR**

## Your Documentation Website Will Include:

✅ **Home Page** - Package overview and quick start
✅ **Get Started** - Complete tutorial vignette
✅ **Functions** - Full documentation for all 3 functions
✅ **Examples** - Fun Cloud 9/Office themed examples
✅ **Guides** - Development guide, quick start, and more

## Sharing Your Package

Tell people to install with:

```r
devtools::install_github("phdemotions/consumeR")
```

Share your documentation site:
```
https://phdemotions.github.io/consumeR
```

## Making Future Updates

Whenever you make changes:

```bash
cd "/Users/josh/My Drive/R/consumeR"
git add -A
git commit -m "Description of your changes"
git push
```

Your website will automatically rebuild in ~5 minutes!

## Troubleshooting

**Q: GitHub Pages not showing up?**
- Make sure repository is Public
- Check that gh-pages branch was created (look in branches dropdown)
- Wait 5-10 minutes after first push
- Check Actions tab to see if workflow succeeded

**Q: Workflow failed?**
- Click on the failed workflow in Actions tab
- Read the error message
- Usually it's a typo in _pkgdown.yml or missing file

**Q: Can't push to GitHub?**
- Make sure you created the repository with name exactly: `consumeR`
- Check you're in the right directory: `pwd` should show `.../R/consumeR`
- Try: `git remote -v` to see if origin is set correctly

## That's It!

You now have:
- ✅ Package on GitHub with version control
- ✅ Automatic testing on every push
- ✅ Beautiful documentation website
- ✅ Easy installation for users

**Next**: Visit https://phdemotions.github.io/consumeR to see your live documentation site!
