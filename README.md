# Bottom Trawl Survey
Data cleaning and exploratory analysis of bottom trawl survey data.

If you have any questions, shoot me an email or create an issue.

Please note that accessing `FishBase` and `SeaLifeBase` might take a few seconds to a minute or more. 


### Update: 2020-12-29  
I changed the YAML heading in all the .Rmd files to outputs `github_documents` from of `HTML` and added `always_allow_HTML: true`. This allows you to see the outputs in the repo (open the .md files). No code in the .Rmd files has changed and I kept the `HTML` files for legacy and in case you would to open them in your browser instead of dealing with github. The table formatting got messed up in the .md files, but they are still readable.

Except for EDA 4 - the maps make raw files too large to render in GitHub. :( You'll have to download the `HTML` file and open it in your browser. 


`BTS_catch-data_WoRMS.R` is stand alone code. It is similar to EDA 1 but it accesses the WoRMS database.