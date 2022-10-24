# Survey_EDA Package

This is a repo for the Survey_EDA package. This package is designed to automate large parts of the exploratory data analysis and generate summary tables, diagnostic statistics and graphics in a quick and simple manner. This package has the ability to detect and analyse survey data including numeric, count, natural language and categorical data. For further details on package capability please see the summary Vignette (available in the doc folder).

# Installation 

To simply install the package functionality alone run the following command in your console:

```
remotes::install_github("RobTomkies/Survey_EDA/SurveyEdaPackage")
```
To install the package and compile vignettes to have available on your PC run:


```
remotes::install_github("RobTomkies/Survey_EDA/SurveyEdaPackage",
                         build_vignettes = TRUE)

```
To install the package, compile the vignette and also make unit testing available run:

```
remotes::install_github("RobTomkies/Survey_EDA/SurveyEdaPackage",
                         INSTALL_opts="--install-tests",
                         type='source',
                         build_vignettes = TRUE)

```

# Contact

For further questions please contact Rob at rob.tomkies\@outlook.com
