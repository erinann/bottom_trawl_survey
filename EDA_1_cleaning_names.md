EDA 1 - Cleaning species names via rfishbase
================
Erin LaBrecque
10/09/2020

This is Exploratory Data Analysis (EDA) file 1 of the NEFSC Bottom Trawl
Survey catch counts from 2008 through 2019.

I exposed all the R code in case you wanted to explore it. If you want
to explore and run the code in these HTML files, I can send you the
rMarkdown files (.Rmd) or you can copy and paste the code from here. You
will need to create the folders, `output_input` and `output_csv` in your
working directory to store the intermediate .rds and .csv files. You
will use `output_input` in this EDA and both `output_input` and
`output_csv` will be used in subsequent EDAs.

``` r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, 
                      fig.width = 7, fig.height = 6.5)
library(knitr)
library(tidyverse)
```

    ## Warning: package 'tibble' was built under R version 4.0.3

``` r
library(fs)
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.0.3

``` r
library(kableExtra)
```

    ## Warning: package 'kableExtra' was built under R version 4.0.3

``` r
library(rfishbase)
```

# Loading the data

To successfully run this code, all the bottom trawl survey .csv files
need to be in a folder called `data` that needs to be in your working
directory.

``` r
# These lines of code tell R to look in the "data" folder, read in each .csv file,
# and then bind all the row from each file together. All columns are kept.
dta <- dir_ls(path = "data") %>% 
  lapply(read_csv, col_types=cols(begindatetime=col_datetime("%d/%m/%Y-%H:%M:%S"))) %>% 
  bind_rows()

# filtering rows with missing metadata
bad_dta <- dta %>% 
  filter(is.na(begindatetime))

# creating a metadata list to use later
meta_data <- c("cruiseid", "cruise", "station", "beginlon", "beginlat", "begindatetime", "stratum")
```

When reading in the data you will get warnings about parsing failures.
This is because the `col_types` arguments above is looking for a
**“%d/%m/%Y-%H:%M:%S”** datetime format for `begindatetime`, but there
are several rows in a few of the .csv files that do not have data in the
`begindatetime` column.

<br/>  
Example warning

| row | col           | expected                    | actual | file                                     |
| --- | ------------- | --------------------------- | ------ | ---------------------------------------- |
| 227 | begindatetime | date like %d/%m/%Y-%H:%M:%S | \-     | ‘data/HB201301\_FSCS\_Catch\_output.csv’ |
| 373 | begindatetime | date like %d/%m/%Y-%H:%M:%S | \-     | ‘data/HB201301\_FSCS\_Catch\_output.csv’ |

Warning: 2 parsing failures.

<br/>

## Missing data from parse warnings

These 13 Cruise ID/Station/Stratum combinations do not have a
`begindatetime`, `latitude`, or `longitude`. Species were collected at
these stations.

``` r
bad_dta %>% 
  select(cruiseid, cruise, station, stratum) %>% 
  kable(col.names = c("Cruise ID", "Cruise", "Station", "Stratum"),
               escape = FALSE) %>% 
   kable_styling(bootstrap_options = c("striped"), full_width = TRUE) 
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

Cruise ID

</th>

<th style="text-align:right;">

Cruise

</th>

<th style="text-align:right;">

Station

</th>

<th style="text-align:right;">

Stratum

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

HB201201

</td>

<td style="text-align:right;">

201202

</td>

<td style="text-align:right;">

220

</td>

<td style="text-align:right;">

1250

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201201

</td>

<td style="text-align:right;">

201202

</td>

<td style="text-align:right;">

389

</td>

<td style="text-align:right;">

1351

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201201

</td>

<td style="text-align:right;">

201202

</td>

<td style="text-align:right;">

341

</td>

<td style="text-align:right;">

1260

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201201

</td>

<td style="text-align:right;">

201202

</td>

<td style="text-align:right;">

378

</td>

<td style="text-align:right;">

1390

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201301

</td>

<td style="text-align:right;">

201302

</td>

<td style="text-align:right;">

403

</td>

<td style="text-align:right;">

1260

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201301

</td>

<td style="text-align:right;">

201302

</td>

<td style="text-align:right;">

49

</td>

<td style="text-align:right;">

8500

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201401

</td>

<td style="text-align:right;">

201402

</td>

<td style="text-align:right;">

198

</td>

<td style="text-align:right;">

1190

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201401

</td>

<td style="text-align:right;">

201402

</td>

<td style="text-align:right;">

137

</td>

<td style="text-align:right;">

3050

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201501

</td>

<td style="text-align:right;">

201502

</td>

<td style="text-align:right;">

315

</td>

<td style="text-align:right;">

1210

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201501

</td>

<td style="text-align:right;">

201502

</td>

<td style="text-align:right;">

101

</td>

<td style="text-align:right;">

1720

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201601

</td>

<td style="text-align:right;">

201602

</td>

<td style="text-align:right;">

79

</td>

<td style="text-align:right;">

8510

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201802

</td>

<td style="text-align:right;">

201802

</td>

<td style="text-align:right;">

200

</td>

<td style="text-align:right;">

1220

</td>

</tr>

<tr>

<td style="text-align:left;">

HB201802

</td>

<td style="text-align:right;">

201802

</td>

<td style="text-align:right;">

195

</td>

<td style="text-align:right;">

1160

</td>

</tr>

</tbody>

</table>

``` r
# removing variable 
rm(bad_dta)
```

<br/>

## Reorganizing the data frame

``` r
# making a list of taxa names from the column names
taxa_ls <- names(dta[8:ncol(dta)]) %>% 
  sort()

# pivoting the table from wide to long,
# putting taxa values in sentence case, adding year and month variables
dta_long <- dta %>% 
  pivot_longer(all_of(taxa_ls),
               names_to = "taxa", 
               values_to = "count",
               values_drop_na = TRUE) %>% 
  mutate(taxa  = str_to_sentence(taxa),
         year =  year(begindatetime),
         month = month(begindatetime, abbr = FALSE, label = TRUE))

# removing 0 counts to work with a smaller data set
dta_ed <- dta_long %>% 
  filter(count > 0)

# dta_long is almost 70 MB. Removing it.
rm(dta_long)
```

<br/>

# Query FishBase and SeaLifeBase

Using the r package, `rfishbase`, we can access information in
[FishBase](www.fishbase.org) without having to go through each species
one-by-one. `rfishbase` also has an experimental feature to query
[SeaLifeBase](www.sealifebase.ca). In the next few sections I query both
databases using `species()` to determine if a given species and
associated life history data can be retrieved programmatically.

<br/>

First, separate the data into the different taxonomic groups. Life
history information can only be retrieved for species.

``` r
# converting taxa list to sentence case
taxa_sentence_case <- str_to_sentence(taxa_ls)

# parsing out the higher taxonomy levels
poda <-  str_subset(taxa_sentence_case, pattern = "poda$")

formes <- str_subset(taxa_sentence_case, pattern = "formes$") 

fam <- str_subset(taxa_sentence_case, pattern = "idae$") 

unknws <- str_subset(taxa_sentence_case, pattern = "^Unknown")

genus <- str_subset(taxa_sentence_case, pattern = " sp$")

ptrn <- c("Trash species in catch", "Crustacea shrimp", 
          "Selachimorpha", "Stelleroidea")
outliers <- map(ptrn, str_subset, string = taxa_sentence_case) %>% 
  unlist()
  
species <- taxa_sentence_case[!taxa_sentence_case %in% 
                                c(poda, formes, fam, genus, unknws, outliers)]
```

  - unknws = 9
  - outliers = 4  
  - poda = 3  
  - formes = 3  
  - fam = 48  
  - genus = 11  
  - species = 338

<br/>

``` r
kable(list(outliers, poda, formes, unknws), 
      col.names = NULL,
      caption = "Unknown and taxa above Family")  %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE) 
```

<table class="kable_wrapper table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Unknown and taxa above Family

</caption>

<tbody>

<tr>

<td>

<table>

<tbody>

<tr>

<td style="text-align:left;">

Trash species in catch

</td>

</tr>

<tr>

<td style="text-align:left;">

Crustacea shrimp

</td>

</tr>

<tr>

<td style="text-align:left;">

Selachimorpha

</td>

</tr>

<tr>

<td style="text-align:left;">

Stelleroidea

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<tbody>

<tr>

<td style="text-align:left;">

Cephalopoda

</td>

</tr>

<tr>

<td style="text-align:left;">

Octopoda

</td>

</tr>

<tr>

<td style="text-align:left;">

Stomatopoda

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<tbody>

<tr>

<td style="text-align:left;">

Anguilliformes

</td>

</tr>

<tr>

<td style="text-align:left;">

Lophiiformes

</td>

</tr>

<tr>

<td style="text-align:left;">

Pleuronectiformes

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<tbody>

<tr>

<td style="text-align:left;">

Unknown 01

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 02

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 03

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 04

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 05

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 06

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 07

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 08

</td>

</tr>

<tr>

<td style="text-align:left;">

Unknown 11

</td>

</tr>

</tbody>

</table>

</td>

</tr>

</tbody>

</table>

<br/>

``` r
fam %>% 
  as_tibble() %>% 
  split(1:3) %>% 
  kable(col.names = "Family",
        caption = "Families") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", full_width = TRUE)) 
```

<table class="kable_wrapper table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Families

</caption>

<tbody>

<tr>

<td>

<table>

<thead>

<tr>

<th style="text-align:left;">

Family

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Alepocephaliidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Blenniidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Cancridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Clupeidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Cyclopteridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Gobiidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Lutjanidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Moridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Ogcocephalidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Paralepidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Portunidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Scombridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Serranidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Stomiidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Synodontidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Triglidae

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<thead>

<tr>

<th style="text-align:left;">

Family

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Apogonidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Bothidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Caproidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Congridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Gadidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Holocentridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Macrouridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Muraenidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Ophichthidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Pleuronectidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Scaridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Scorpaenidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Squalidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Stromateidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Tetraodontidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Uranoscopidae

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<thead>

<tr>

<th style="text-align:left;">

Family

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Balistidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Calappidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Carcharhinidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Cottidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Galatheidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Labridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Majidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Myctophidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Ophidiidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Pomacentridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Sciaenidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Sepiolidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Sternoptychidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Syngnathidae

</td>

</tr>

<tr>

<td style="text-align:left;">

Trichiuridae

</td>

</tr>

<tr>

<td style="text-align:left;">

Zoarcidae

</td>

</tr>

</tbody>

</table>

</td>

</tr>

</tbody>

</table>

<br/>

``` r
genus %>% 
  as_tibble() %>% 
  split(1:2) %>%  
  kable(col.names = "Genus",
        caption = "Genera") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", full_width = TRUE)) 
```

<table class="kable_wrapper table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Genera

</caption>

<tbody>

<tr>

<td>

<table>

<thead>

<tr>

<th style="text-align:left;">

Genus

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Artediellus sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Citharichthys sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Fistularia sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Paralichthys sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Sphyraena sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Urophycis sp

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<thead>

<tr>

<th style="text-align:left;">

Genus

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Chlorophthalmus sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Etropus sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Ovalipes sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Penaeus sp

</td>

</tr>

<tr>

<td style="text-align:left;">

Symphurus sp

</td>

</tr>

</tbody>

</table>

</td>

</tr>

</tbody>

</table>

<br/>

Now query the databases for species. This will take a minute or two if
the databases have not been cashed.

``` r
# querying fishbase to see which taxa are fish
sp_fish <- species(species, fields = c("SpecCode", "Species", "FBname"), 
                   server = "fishbase") %>% 
  rename(BTS_sp = Species, Com_name = FBname)

# querying sealifebase to see which taxa are something other than fish
sp_sealife <- species(species, fields = c("SpecCode", "Species", "FBname"), 
                      server = "sealifebase") %>% 
  rename(BTS_sp = Species, Com_name = FBname)
```

<br/>

This section checks that all species listed have been identified in one
of the two databases.

``` r
# turning the species list into a data frame
sp_full <- tibble(species) %>% 
  rename(BTS_sp = species)

# combining the query results with the original list of species and renaming columns
# FB = fishbase, SLB = sealifebase
sp_checks <- bind_cols(sp_full, sp_fish, sp_sealife) %>% 
  select(-starts_with("Com_name")) %>% 
  rename(BTS_sp = BTS_sp...1,
         FB_SpecCode = SpecCode...2,
         FB_species = BTS_sp...3,
         SLB_SpecCode = SpecCode...5,
         SLB_species = BTS_sp...6)

# determining what entries in the species list were not in either database
not_in_either_db <- sp_checks %>% 
  filter(is.na(FB_species) & is.na(SLB_species)) %>% 
  select(BTS_sp)
```

<br/>

45 species in the bottom trawl data set were not found in either
database using the function `species()`.

``` r
not_in_either_db %>% 
  as_tibble() %>% 
  split(1:3) %>%
  kable(caption = "Species in the BTS data not found in either database") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", full_width = TRUE)) %>%
  column_spec(1, italic = TRUE) %>% 
  column_spec(2, italic = TRUE) %>% 
  column_spec(3, italic = TRUE)
```

<table class="kable_wrapper table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Species in the BTS data not found in either database

</caption>

<tbody>

<tr>

<td style="font-style: italic;">

<table>

<thead>

<tr>

<th style="text-align:left;">

BTS\_sp

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Acipenser oxyrhynchus

</td>

</tr>

<tr>

<td style="text-align:left;">

Antennarius radiosus

</td>

</tr>

<tr>

<td style="text-align:left;">

Chaetodon aya

</td>

</tr>

<tr>

<td style="text-align:left;">

Dasyatis say

</td>

</tr>

<tr>

<td style="text-align:left;">

Etrumeus teres

</td>

</tr>

<tr>

<td style="text-align:left;">

Geryon fenneri

</td>

</tr>

<tr>

<td style="text-align:left;">

Hemipteronotus novacula

</td>

</tr>

<tr>

<td style="text-align:left;">

Lepidochelys kempi

</td>

</tr>

<tr>

<td style="text-align:left;">

Lumpenus lumpretaeformis

</td>

</tr>

<tr>

<td style="text-align:left;">

Macrorhamphosus scolopax

</td>

</tr>

<tr>

<td style="text-align:left;">

Nezumia bairdi

</td>

</tr>

<tr>

<td style="text-align:left;">

Pandalus propinquus

</td>

</tr>

<tr>

<td style="text-align:left;">

Poecilopsetta beani

</td>

</tr>

<tr>

<td style="text-align:left;">

Stomias boa

</td>

</tr>

<tr>

<td style="text-align:left;">

Urophycis chesteri

</td>

</tr>

</tbody>

</table>

</td>

<td style="font-style: italic;">

<table>

<thead>

<tr>

<th style="text-align:left;">

BTS\_sp

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Aluterus heudeloti

</td>

</tr>

<tr>

<td style="text-align:left;">

Arctozenus rissoi

</td>

</tr>

<tr>

<td style="text-align:left;">

Dasyatis americana

</td>

</tr>

<tr>

<td style="text-align:left;">

Equetus acuminatus

</td>

</tr>

<tr>

<td style="text-align:left;">

Foetorepus agassizi

</td>

</tr>

<tr>

<td style="text-align:left;">

Geryon quinquedens

</td>

</tr>

<tr>

<td style="text-align:left;">

Lactophrys polygonia

</td>

</tr>

<tr>

<td style="text-align:left;">

Loligo pealeii

</td>

</tr>

<tr>

<td style="text-align:left;">

Lumpenus maculatus

</td>

</tr>

<tr>

<td style="text-align:left;">

Macrozoarces americanus

</td>

</tr>

<tr>

<td style="text-align:left;">

Ophidion holbrooki

</td>

</tr>

<tr>

<td style="text-align:left;">

Paralichthys oblongus

</td>

</tr>

<tr>

<td style="text-align:left;">

Scorpaena agassizi

</td>

</tr>

<tr>

<td style="text-align:left;">

Synagrops spinosus

</td>

</tr>

<tr>

<td style="text-align:left;">

Urophycis earlli

</td>

</tr>

</tbody>

</table>

</td>

<td style="font-style: italic;">

<table>

<thead>

<tr>

<th style="text-align:left;">

BTS\_sp

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Ancylopsetta quadrocellata

</td>

</tr>

<tr>

<td style="text-align:left;">

Asterias vulgaris

</td>

</tr>

<tr>

<td style="text-align:left;">

Dasyatis centroura

</td>

</tr>

<tr>

<td style="text-align:left;">

Equetus umbrosus

</td>

</tr>

<tr>

<td style="text-align:left;">

Gephyroberyx darwini

</td>

</tr>

<tr>

<td style="text-align:left;">

Hemanthias aureorubens

</td>

</tr>

<tr>

<td style="text-align:left;">

Lactophrys quadricornis

</td>

</tr>

<tr>

<td style="text-align:left;">

Loligo pleii

</td>

</tr>

<tr>

<td style="text-align:left;">

Lycenchelys verrilli

</td>

</tr>

<tr>

<td style="text-align:left;">

Monacanthus hispidus

</td>

</tr>

<tr>

<td style="text-align:left;">

Pagrus sedecim

</td>

</tr>

<tr>

<td style="text-align:left;">

Peprilus alepidotus

</td>

</tr>

<tr>

<td style="text-align:left;">

Spirontocaris liljeborgii

</td>

</tr>

<tr>

<td style="text-align:left;">

Torpedo nobiliana

</td>

</tr>

<tr>

<td style="text-align:left;">

Zenopsis conchifera

</td>

</tr>

</tbody>

</table>

</td>

</tr>

</tbody>

</table>

<br/>

### Checking for synonoms

The species listed above might be in the databases under different
names. Thankfully `rfishbase` has a function that looks up synonyms.

``` r
# checking for synonyms in fishbase, removing NAs, removing any duplicates
sp_fish_sy <- synonyms(pull(not_in_either_db), server = "fishbase") %>% 
  select(BTS_sp = synonym, Status, SpecCode, TSN, DB_sp = Species) %>% 
  filter(!is.na(TSN)) %>% 
  distinct(DB_sp, .keep_all = TRUE)

# checking for synonyms in sealifebase and removing NAs, removing any duplicates
sp_sealife_sy <- synonyms(pull(not_in_either_db), server = "sealifebase") %>% 
  select(BTS_sp = synonym, Status, SpecCode, DB_sp = Species) %>% 
  filter(!is.na(Status)) %>% 
  distinct(DB_sp, .keep_all = TRUE)

# checking to see if any species were missed
if (nrow(sp_fish_sy) + nrow(sp_sealife_sy) == nrow(not_in_either_db)){
  print("All species acounted for!")
} else {
  paste(nrow(not_in_either_db) - (nrow(sp_fish_sy) + nrow(sp_sealife_sy)), "species not accounted for.")
}
```

    ## [1] "2 species not accounted for."

<br/>

### Finding the species that did not have synonyms

``` r
no_syn_sp <- bind_rows(sp_fish_sy, sp_sealife_sy) %>% 
  select(BTS_sp)

not_in_either_db %>% 
  filter(!BTS_sp %in% no_syn_sp$BTS_sp) %>% 
  kable(col.name = "BTS name",
        caption = "Species in the BTS data not found as synonyms the databases") %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(1, italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Species in the BTS data not found as synonyms the databases

</caption>

<thead>

<tr>

<th style="text-align:left;">

BTS name

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-style: italic;">

Aluterus heudeloti

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis earlli

</td>

</tr>

</tbody>

</table>

<br/>

Looks like a of couple misspellings (or a character limit was set in the
bottom trawl survey database), not synonym issues. This section fixes
the misspelling but keeps the original spelling in `BTS_sp` column to
reference back to the original data.

``` r
sp_spell_fix <- species(c("Urophycis earllii", "Aluterus heudelotii"), 
                  fields = c("SpecCode", "Species", "FBname"), server = "fishbase") %>% 
  rename(DB_sp = Species, Com_name = FBname) %>% 
  mutate(BTS_sp = case_when(DB_sp == "Urophycis earllii" ~ "Urophycis earlli",
                            DB_sp == "Aluterus heudelotii" ~ "Aluterus heudeloti"))


#combining data frames
sp_syn <- bind_rows(sp_fish_sy, sp_sealife_sy)

# printing out table
sp_syn %>%
  select(BTS_sp, Status, DB_sp) %>%
  kable(caption = "Synonyms found in FishBase and SeaLifeBase",
  col.names = c("BTS name", "Status", "Database name")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", full_width = TRUE)) %>%
  column_spec(1, italic = TRUE) %>%
  column_spec(3, italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Synonyms found in FishBase and SeaLifeBase

</caption>

<thead>

<tr>

<th style="text-align:left;">

BTS name

</th>

<th style="text-align:left;">

Status

</th>

<th style="text-align:left;">

Database name

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-style: italic;">

Acipenser oxyrhynchus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Acipenser oxyrinchus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ancylopsetta quadrocellata

</td>

<td style="text-align:left;">

ambiguous synonym

</td>

<td style="text-align:left;font-style: italic;">

Ancylopsetta ommata

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Antennarius radiosus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Fowlerichthys radiosus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Arctozenus rissoi

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Arctozenus risso

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chaetodon aya

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Prognathodes aya

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dasyatis americana

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Hypanus americanus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dasyatis centroura

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Bathytoshia centroura

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dasyatis say

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Hypanus say

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Equetus acuminatus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Pareques acuminatus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Equetus umbrosus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Pareques umbrosus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Etrumeus teres

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Etrumeus sadina

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Foetorepus agassizi

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Foetorepus agassizii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gephyroberyx darwini

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Gephyroberyx darwinii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hemanthias aureorubens

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Baldwinella aureorubens

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hemipteronotus novacula

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Xyrichtys novacula

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lactophrys polygonia

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Acanthostracion polygonius

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lactophrys quadricornis

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Acanthostracion quadricornis

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lumpenus lumpretaeformis

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Lumpenus lampretaeformis

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lumpenus maculatus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Leptoclinus maculatus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lycenchelys verrilli

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Lycenchelys verrillii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Macrorhamphosus scolopax

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Macroramphosus scolopax

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Macrozoarces americanus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Zoarces americanus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Monacanthus hispidus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Stephanolepis hispidus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Nezumia bairdi

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Nezumia bairdii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ophidion holbrooki

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Ophidion holbrookii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pagrus sedecim

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Pagrus pagrus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Paralichthys oblongus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Hippoglossina oblonga

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Peprilus alepidotus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Peprilus paru

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Poecilopsetta beani

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Poecilopsetta beanii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scorpaena agassizi

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Scorpaena agassizii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Stomias boa

</td>

<td style="text-align:left;">

accepted name

</td>

<td style="text-align:left;font-style: italic;">

Stomias boa boa

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Synagrops spinosus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Parascombrops spinosus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Torpedo nobiliana

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Tetronarce nobiliana

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis chesteri

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Phycis chesteri

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Zenopsis conchifera

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Zenopsis conchifer

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Asterias vulgaris

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Asterias rubens

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Geryon fenneri

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Chaceon fenneri

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Geryon quinquedens

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Chaceon quinquedens

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lepidochelys kempi

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Lepidochelys kempii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Loligo pealeii

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Doryteuthis pealeii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Loligo pleii

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Doryteuthis pleii

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pandalus propinquus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Atlantopandalus propinqvus

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Spirontocaris liljeborgii

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Spirontocaris lilljeborgii

</td>

</tr>

</tbody>

</table>

<br/>

This section queries FishBase and SeaLifeBase to get the species codes
and common names for the synonyms.

``` r
# getting common names and species codes for FishBase then joining to the synonym data frame
# to attach the original names
sp_fish_sy_spcodes <- species(sp_fish_sy$DB_sp,  
                              fields = c("SpecCode", "Species", "FBname"), 
                              server = "fishbase") %>% 
  rename(DB_sp = Species, Com_name = FBname) %>% 
  left_join(sp_fish_sy, by = "SpecCode")%>% 
  select(SpecCode, BTS_sp, Status, Com_name, DB_sp = DB_sp.x, TSN)

# same for sealife base
sp_sealife_sy_spcodes <- species(sp_sealife_sy$DB_sp,  
                              fields = c("SpecCode", "Species", "FBname"), 
                              server = "sealifebase") %>% 
  rename(DB_sp = Species, Com_name = FBname) %>% 
  left_join(sp_sealife_sy, by = "SpecCode")%>% 
  select(SpecCode, BTS_sp, Status, Com_name, DB_sp = DB_sp.x)
```

### Combing data frames

``` r
# removing the NAs (not fish) from the original fish query and adding in the new spellings
sp_fish_almost_all <- sp_fish %>% 
  filter(!is.na(BTS_sp)) %>% 
  bind_rows(sp_spell_fix) %>% 
  mutate(DB_sp = ifelse(is.na(DB_sp), BTS_sp, DB_sp))

sp_fish_all <- sp_fish_almost_all %>% 
  bind_rows(sp_fish_sy_spcodes)

# same for sealifebase but do not need to add in the misspellings
sp_sealife_all <- sp_sealife %>% 
  filter(!is.na(BTS_sp)) %>% 
  mutate(DB_sp = BTS_sp) %>% 
  bind_rows(sp_sealife_sy_spcodes)
```

Species names have been checked, but the common names have not. This
section checks the common names for all the species.

``` r
sp_sealife_all %>% 
  filter(is.na(Com_name)) %>% 
  select(-SpecCode) %>% 
  kable(col.names = c("BTS name", "Common name", "Database name", "Status"),
        caption = "Missing common name") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", full_width = TRUE)) %>% 
  column_spec(c(1,3), italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Missing common name

</caption>

<thead>

<tr>

<th style="text-align:left;">

BTS name

</th>

<th style="text-align:left;">

Common name

</th>

<th style="text-align:left;">

Database name

</th>

<th style="text-align:left;">

Status

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-style: italic;">

Pandalus propinquus

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;font-style: italic;">

Atlantopandalus propinqvus

</td>

<td style="text-align:left;">

synonym

</td>

</tr>

</tbody>

</table>

``` r
sp_fish_all %>% 
  filter(is.na(Com_name)) %>% 
  select(-SpecCode, -TSN) %>% 
  kable(col.names = c("BTS name", "Common name", "Database name", "Status"),
        caption = "Missing common name") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", full_width = TRUE)) %>% 
  column_spec(c(1,3), italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Missing common name

</caption>

<thead>

<tr>

<th style="text-align:left;">

BTS name

</th>

<th style="text-align:left;">

Common name

</th>

<th style="text-align:left;">

Database name

</th>

<th style="text-align:left;">

Status

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-style: italic;">

Polymetme thaeocoryla

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;font-style: italic;">

Polymetme thaeocoryla

</td>

<td style="text-align:left;">

NA

</td>

</tr>

</tbody>

</table>

<br/>

Adding the common names to the data.

``` r
sp_sealife_all <- sp_sealife_all %>% 
  mutate(Com_name = case_when(BTS_sp == "Pandalus propinquus" ~ "Shrimp",
                              TRUE ~ Com_name))

sp_fish_all <- sp_fish_all %>% 
  mutate(Com_name = case_when(BTS_sp == "Polymetme thaeocoryla" ~ "Lightfishes",
                              TRUE ~ Com_name))
```

<br/>

# Wrap up EDA 1

This file is getting long. I’ll save the outputs here an start a new
file to look up habitats and gas bladder information. Un-comment (remove
\#) to write out the data files.

``` r
write_rds(dta, "output_input/dta.rds")
write_rds(dta_ed, "output_input/dta_ed.rds")
write_rds(sp_sealife_all, "output_input/sp_sealife.rds")
sp_fish_all %>% 
  select(-TSN) %>% 
  write_rds("output_input/sp_fish.rds")
```
