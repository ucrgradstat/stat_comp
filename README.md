# Statistical Computing Website Repository

This Repository contains all the files to construct the website: [ucrgradstat.github.io/stat_comp/](https://ucrgradstat.github.io/stat_comp/)

## Building the Website

The website is a collection of R Markdown files. The website is built using the R package [distill](https://rstudio.github.io/distill/). Make changes to R Markdown files to update the website. Then use the code below to build the website:

```
rmarkdown::render_site(encoding = 'UTF-8')
```

You may also build the website from the build tab. Make sure to build the website before pushing to GitHub.

## Website Documents

Other than the README.md file and tutorials, all the pages with this website were constructed with R Markdown Files.  The three components the R Markdown files needed in the yaml header were the "title", "description", and "site". The "description" line is needed to make the document searchable. The "site" line must follow what is below:

```
site: distill:distill_website
```

## Configuring `_site.yml`

The `_site.yml` file contains all the information needed to build the website. Make sure to keep the following lines:

```
base_url: https://ucrgradstat.github.io/stat_comp
output_dir: "docs"
```
These lines ensure the website is build popularly. Additionally, make sure to keep the `.nojekyll` in the repository.


## Tutorials

The tutorials can be any format. However, to host it on the website, an html file is needed. If students submit an R Markdown file, compile it and use the html file.


