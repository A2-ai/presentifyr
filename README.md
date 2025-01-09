# pptx

<!-- badges: start -->

<!-- badges: end -->

The goal of pptx is to automate creation of PowerPoints for outputs for internal reviews or report reference purposes.

## Installation

To install the development version of pptx:

1.  In your general project directory, `gh repo clone A2-ai/pptx`

2a. For projects with renv snapshots of 2024-07-20 or later: Within the project you want to use the app (run these separately):

``` r
install.packages("devtools") # if not already installed
```

``` r
devtools::load_all("../pptx") # for directory organization where your project directory is Projects/<project>; otherwise, devtools::load_all("path/to/pptx/repo")
```

```r
app()
```

2b. For projects with renv snapshots prior to 2024-07-20: Within the pptx project:

``` r
renv::restore() 
```

```r
devtools::load_all() 
```

``` r
app_with_dir("path/to/client/repo") # for typical set-up, path will be "../<client repo>"
```

## Example

<https://github.com/user-attachments/assets/cda8ed10-f472-4395-be56-83f61ef1c5a4>

## Notes

-   Requires the project to have a remote repo set up.
-   Only works for image files (png, pdf, etc.). PDFs (and the warning below) will only show up in Microsoft PowerPoint. WPS Office (and potentially others) will just show a blank page for pdfs.
-   PDFs will only print out the first page even if it's a multi-page pdf. Powerpoint will also pop up with the warning below if your pptx contains a pdf file, which can just be clicked through with "Repair":

```         
PowerPoint found a problem with content in report.pptx.
PowerPoint can attempt to repair the presentation.

If you trust the source of this presentation, click Repair.
```

-   The app prints out images from the local repo, rather than pulling from github source, and creates the links based on their position in the local repo. Make sure to commit and push items if they are meant to be shared with others.
-   The links created also take into account which branch the user is on. Ensure the branch is correct prior to creation of pptx.

## Updates

### August 27, 2024

-   Added `app_with_dir()` function to allow use of pptx with differing package versions in repos/older repos. See Troubleshooting section for how-to.

### August 21, 2024

-   Added options to choose from default/blank or A2-Ai template pptx and customizable height/width dimensions for image.

-   To mass format images within the pptx (this example uses a 5x5 img):

-   Using default templates:

    1.  Select all slides you want to apply changes to
    2.  Select a design that has a picture label ![image](https://github.com/user-attachments/assets/20f67bb1-dd23-427a-ab15-42dc53a92d1d) ![image](https://github.com/user-attachments/assets/8d800fd5-d861-4d4e-8955-0571e102e201)

<br />

-   Creating your own template/style:

<https://github.com/user-attachments/assets/e39c8677-e842-4f34-85fe-fe78c6a46759>

## Troubleshooting

-   `Warning: Error in : could not find file 'deliv/a2_ai_temp.pptx'`
    -   Solution: Please pull the latest version of the app.
-   My pptx slides are blank/my pdfs don't show in my pptx
    -   Solution: Please use Microsoft Powerpoint to open the pptx. I'm not sure if other apps can show pdfs as images this way.
-   Errors related to namespaces or functions similar to below:
    -   `In loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : namespace ‘bslib’ 0.4.2 is already loaded, but >= 0.7.0 is required`
    -   `Could not find function page_sidebar`
    -   This is due to package version discrepancies, where the client project has older versions of the packages already. Assuming the aim is to keep these versions and not upgrade, follow below:
    -   Solution: Please pull the latest version of the app, and instead of navigating to a client project like before, stay in the pptx project and load the pptx package using `devtools::load_all()`. Then point to the client project with `app_with_dir("path/to/client/repo")` relative to the pptx directory.

``` r
renv::restore()
install.packages("devtools") # if not already installed
devtools::load_all() # make sure getwd() is pptx prior if not already
```

``` r
app_with_dir("path/to/client/repo") # for typical set-up, path will be "../<client repo>"
```

## TODO:

-   Add option to grab the image from pdf rather than inserting the pdf itself as an image.
-   Add option to print out all the pages from a pdf.
