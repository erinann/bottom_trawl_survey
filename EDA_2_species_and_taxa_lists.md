EDA 2 - Species list and data counts
================
Erin LaBrecque
10/14/2020

This is file 2 of an exploratory data analysis (EDA) of the NEFSC Bottom
Trawl Survey catch counts from 2008 through 2019.

You must run the code in EDA 1 once to create and save the intermediate
files used in EDA 2.

``` r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, 
                      fig.width = 7, fig.height = 6.5)
library(knitr)
library(tidyverse)
```

    ## Warning: package 'tibble' was built under R version 4.0.3

``` r
library(kableExtra)
```

    ## Warning: package 'kableExtra' was built under R version 4.0.3

``` r
library(rfishbase)
```

# Loading the data

These are the outputs from the first EDA.

``` r
dta <- read_rds("output_input/dta.rds")
dta_ed <- read_rds("output_input/dta_ed.rds")
sp_fish <- read_rds("output_input/sp_fish.rds")
sp_sealife <- read_rds("output_input/sp_sealife.rds")

# loading the fishbase and sealife base data sets
# this will take a few moments but it will speed up analysis later in the file
fish_taxa <- load_taxa(server = "fishbase")
sealife_taxa <- load_taxa(server = "sealifebase")
```

# Taxonomy

I have a sneaky suspicion that having the full taxonomic hierarchy for
each species will be useful at some point. Because all the data munging
so far has been in FishBase, it is easiest to get taxonomic information
from there. Note that ITIS taxonomy is slightly different than FishBase
- e.g. ITIS ranks Chondrichthyes as a Class and Elasmobranchii as a
Subclass, while FishBase ranks Chondrichthyes as a Superclass and
Elasmobranchii as a Class.

This query is only for species. Instances of Genus, Family, Order, and
Class are in the next section.

``` r
# adding fish taxa hierarchy to the fish species information, and adding 
# group (fish/not-fish) identifier (for graphing later)
sp_fish <- sp_fish %>% 
  left_join(fish_taxa, by = "SpecCode") %>% 
  mutate(type = "fish")

# adding non-fish taxa hierarchy to the non-fish species information
# group (fish/not-fish) identifier (for graphing later)
sp_sealife <- sp_sealife %>% 
  left_join(sealife_taxa, by = "SpecCode") %>% 
  mutate(type = "not_fish")

# printing out a few rows of data to inspect it
tail(sp_fish, 4) %>% 
  kable(caption = "Three rows of the fish species data") %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(c(2, 4, 6), italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Three rows of the fish species data

</caption>

<thead>

<tr>

<th style="text-align:right;">

SpecCode

</th>

<th style="text-align:left;">

BTS\_sp

</th>

<th style="text-align:left;">

Com\_name

</th>

<th style="text-align:left;">

DB\_sp

</th>

<th style="text-align:left;">

Status

</th>

<th style="text-align:left;">

Species

</th>

<th style="text-align:left;">

Genus

</th>

<th style="text-align:left;">

Subfamily

</th>

<th style="text-align:left;">

Family

</th>

<th style="text-align:left;">

Order

</th>

<th style="text-align:left;">

Class

</th>

<th style="text-align:left;">

SuperClass

</th>

<th style="text-align:left;">

type

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

14336

</td>

<td style="text-align:left;font-style: italic;">

Synagrops spinosus

</td>

<td style="text-align:left;">

Keelcheek bass

</td>

<td style="text-align:left;font-style: italic;">

Parascombrops spinosus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Parascombrops spinosus

</td>

<td style="text-align:left;">

Parascombrops

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Acropomatidae

</td>

<td style="text-align:left;">

Perciformes

</td>

<td style="text-align:left;">

Actinopterygii

</td>

<td style="text-align:left;">

Osteichthyes

</td>

<td style="text-align:left;">

fish

</td>

</tr>

<tr>

<td style="text-align:right;">

2553

</td>

<td style="text-align:left;font-style: italic;">

Torpedo nobiliana

</td>

<td style="text-align:left;">

Electric ray

</td>

<td style="text-align:left;font-style: italic;">

Tetronarce nobiliana

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Tetronarce nobiliana

</td>

<td style="text-align:left;">

Tetronarce

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Torpedinidae

</td>

<td style="text-align:left;">

Torpediniformes

</td>

<td style="text-align:left;">

Elasmobranchii

</td>

<td style="text-align:left;">

Chondrichthyes

</td>

<td style="text-align:left;">

fish

</td>

</tr>

<tr>

<td style="text-align:right;">

1880

</td>

<td style="text-align:left;font-style: italic;">

Urophycis chesteri

</td>

<td style="text-align:left;">

Longfin hake

</td>

<td style="text-align:left;font-style: italic;">

Phycis chesteri

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Phycis chesteri

</td>

<td style="text-align:left;">

Phycis

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Phycidae

</td>

<td style="text-align:left;">

Gadiformes

</td>

<td style="text-align:left;">

Actinopterygii

</td>

<td style="text-align:left;">

Osteichthyes

</td>

<td style="text-align:left;">

fish

</td>

</tr>

<tr>

<td style="text-align:right;">

336

</td>

<td style="text-align:left;font-style: italic;">

Zenopsis conchifera

</td>

<td style="text-align:left;">

Silvery John dory

</td>

<td style="text-align:left;font-style: italic;">

Zenopsis conchifer

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Zenopsis conchifer

</td>

<td style="text-align:left;">

Zenopsis

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Zeidae

</td>

<td style="text-align:left;">

Zeiformes

</td>

<td style="text-align:left;">

Actinopterygii

</td>

<td style="text-align:left;">

Osteichthyes

</td>

<td style="text-align:left;">

fish

</td>

</tr>

</tbody>

</table>

``` r
# printing out a few rows of data to inspect it
tail(sp_sealife, 4) %>% 
  kable(caption = "Three rows of the sealife species data") %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(c(2, 4, 6), italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<caption>

Three rows of the sealife species data

</caption>

<thead>

<tr>

<th style="text-align:right;">

SpecCode

</th>

<th style="text-align:left;">

BTS\_sp

</th>

<th style="text-align:left;">

Com\_name

</th>

<th style="text-align:left;">

DB\_sp

</th>

<th style="text-align:left;">

Status

</th>

<th style="text-align:left;">

Species

</th>

<th style="text-align:left;">

Genus

</th>

<th style="text-align:left;">

Subfamily

</th>

<th style="text-align:left;">

Family

</th>

<th style="text-align:left;">

Order

</th>

<th style="text-align:left;">

Class

</th>

<th style="text-align:left;">

Phylum

</th>

<th style="text-align:left;">

Kingdom

</th>

<th style="text-align:left;">

type

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

57477

</td>

<td style="text-align:left;font-style: italic;">

Loligo pealeii

</td>

<td style="text-align:left;">

longfin inshore squid

</td>

<td style="text-align:left;font-style: italic;">

Doryteuthis pealeii

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Doryteuthis pealeii

</td>

<td style="text-align:left;">

Doryteuthis

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Loliginidae

</td>

<td style="text-align:left;">

Teuthida

</td>

<td style="text-align:left;">

Cephalopoda

</td>

<td style="text-align:left;">

Mollusca

</td>

<td style="text-align:left;">

Animalia

</td>

<td style="text-align:left;">

not\_fish

</td>

</tr>

<tr>

<td style="text-align:right;">

57478

</td>

<td style="text-align:left;font-style: italic;">

Loligo pleii

</td>

<td style="text-align:left;">

slender inshore squid

</td>

<td style="text-align:left;font-style: italic;">

Doryteuthis pleii

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Doryteuthis pleii

</td>

<td style="text-align:left;">

Doryteuthis

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Loliginidae

</td>

<td style="text-align:left;">

Teuthida

</td>

<td style="text-align:left;">

Cephalopoda

</td>

<td style="text-align:left;">

Mollusca

</td>

<td style="text-align:left;">

Animalia

</td>

<td style="text-align:left;">

not\_fish

</td>

</tr>

<tr>

<td style="text-align:right;">

19454

</td>

<td style="text-align:left;font-style: italic;">

Pandalus propinquus

</td>

<td style="text-align:left;">

Shrimp

</td>

<td style="text-align:left;font-style: italic;">

Atlantopandalus propinqvus

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Atlantopandalus propinqvus

</td>

<td style="text-align:left;">

Atlantopandalus

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Pandalidae

</td>

<td style="text-align:left;">

Decapoda

</td>

<td style="text-align:left;">

Malacostraca

</td>

<td style="text-align:left;">

Arthropoda

</td>

<td style="text-align:left;">

Animalia

</td>

<td style="text-align:left;">

not\_fish

</td>

</tr>

<tr>

<td style="text-align:right;">

21005

</td>

<td style="text-align:left;font-style: italic;">

Spirontocaris liljeborgii

</td>

<td style="text-align:left;">

friendly blade shrimp

</td>

<td style="text-align:left;font-style: italic;">

Spirontocaris lilljeborgii

</td>

<td style="text-align:left;">

synonym

</td>

<td style="text-align:left;font-style: italic;">

Spirontocaris lilljeborgii

</td>

<td style="text-align:left;">

Spirontocaris

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

Hippolytidae

</td>

<td style="text-align:left;">

Decapoda

</td>

<td style="text-align:left;">

Malacostraca

</td>

<td style="text-align:left;">

Arthropoda

</td>

<td style="text-align:left;">

Animalia

</td>

<td style="text-align:left;">

not\_fish

</td>

</tr>

</tbody>

</table>

<br/>

A lot of the data is classified down to species, but as seen in EDA 1,
some critters are only classified to Genus, Family, Order, and in a
couple cases, Class. The next few sections pull out higher taxa
instances and fill in the appropriate taxonomic information.

``` r
taxa_ls <- names(dta[8:ncol(dta)]) %>% 
  sort()

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

<br/>

This sections makes a list of all the observations of organisms that
were not classified to species and saves the list as a .csv file.

``` r
# all non-species entries. Unknowns are not included.
non_sp_taxa <- tibble(non_sp = c(formes, poda, fam, genus, outliers)) %>% 
  mutate(level = case_when(str_detect(non_sp, pattern = "Cephalopoda") ~ "Class",
                           str_detect(non_sp, pattern = "formes$") |
                             str_detect(non_sp, pattern = "Octopoda") |
                             str_detect(non_sp, pattern = "Stomatopoda") ~ "Order",
                           str_detect(non_sp, pattern = "idae$") ~ "Family",
                           str_detect(non_sp, pattern = " sp$") ~ "Genus",
                           TRUE ~ " ")
         )

# saving as a csv
write_csv(non_sp_taxa, "output_csv/non_sp_taxa.csv")
```

I went though the non-species taxa .csv file by hand to fixed spelling
mistakes and add the appropriate additional information. Unfortunately
this could not be done programmatically with `rfishbase` or with any of
the other packages I looked into.

``` r
# reading in the data edited for spelling mistakes and synonyms
non_sp_taxa <- read_csv("output_csv/non_sp_taxa_edit.csv") 
```

## Joining the taxa data to the trawl data

All the work so far has been to assess the species in the data sets
without looking at the counts of organisms from the trawls. Here we add
the taxonomic trawl count data for species.

``` r
# making a single data frame to add to the count data
both_fish_sealife <- bind_rows(sp_fish, sp_sealife)

# combining the count data with the species data and
# removing higher level taxa data
dta_sp_only <- dta_ed %>% 
  left_join(both_fish_sealife, by = c("taxa"= "BTS_sp")) %>% # using the BTS names here
  rename(BTS_sp = taxa) %>% 
  filter(!is.na(Species)) %>% 
  mutate(level = "Species")

# combining the count data with the higher level taxa data and
# removing the species level data
dta_nonsp_only <- dta_ed %>% 
  left_join(non_sp_taxa, by = c("taxa" = "BTS_sp")) %>% 
  rename(BTS_sp = taxa) %>% 
  filter(!is.na(Com_name))
```

## Quesiton:

How many of each species were caught in the trawls over all the data
sets?

Printing species, common names and counts.

``` r
dta_sp_only %>% 
  count(BTS_sp, Com_name, type) %>% 
  arrange(type) %>% 
  kable(col.names = c("BTS name", "Common name", "Category", "Count")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(1, italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

BTS name

</th>

<th style="text-align:left;">

Common name

</th>

<th style="text-align:left;">

Category

</th>

<th style="text-align:right;">

Count

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-style: italic;">

Acipenser oxyrhynchus

</td>

<td style="text-align:left;">

Atlantic sturgeon

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

18

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Alopias superciliosus

</td>

<td style="text-align:left;">

Bigeye thresher

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Alosa aestivalis

</td>

<td style="text-align:left;">

Blueback shad

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1221

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Alosa mediocris

</td>

<td style="text-align:left;">

Hickory shad

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Alosa pseudoharengus

</td>

<td style="text-align:left;">

Alewife

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1841

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Alosa sapidissima

</td>

<td style="text-align:left;">

American shad

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

788

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Aluterus heudeloti

</td>

<td style="text-align:left;">

Dotterel filefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Aluterus monoceros

</td>

<td style="text-align:left;">

Unicorn leatherjacket filefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Aluterus scriptus

</td>

<td style="text-align:left;">

Scribbled leatherjacket filefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Amblyraja radiata

</td>

<td style="text-align:left;">

Starry ray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

556

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ammodytes dubius

</td>

<td style="text-align:left;">

Northern sand lance

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

250

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Anarhichas lupus

</td>

<td style="text-align:left;">

Atlantic wolffish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

88

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Anchoa hepsetus

</td>

<td style="text-align:left;">

Broad-striped anchovy

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

17

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Anchoa mitchilli

</td>

<td style="text-align:left;">

Bay anchovy

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

63

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ancylopsetta dilecta

</td>

<td style="text-align:left;">

Three-eye flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ancylopsetta quadrocellata

</td>

<td style="text-align:left;">

Gulf of Mexico ocellated flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Anguilla rostrata

</td>

<td style="text-align:left;">

American eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Antennarius radiosus

</td>

<td style="text-align:left;">

Singlespot frogfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Anthias nicholsi

</td>

<td style="text-align:left;">

Yellowfin bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

11

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Antigonia capros

</td>

<td style="text-align:left;">

Deepbody boarfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

105

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Antigonia combatia

</td>

<td style="text-align:left;">

Shortspine boarfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Antimora rostrata

</td>

<td style="text-align:left;">

Blue antimora

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Apogon pseudomaculatus

</td>

<td style="text-align:left;">

Twospot cardinalfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Archosargus probatocephalus

</td>

<td style="text-align:left;">

Sheepshead

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Arctozenus rissoi

</td>

<td style="text-align:left;">

Spotted barracudina

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Argentina silus

</td>

<td style="text-align:left;">

Greater argentine

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

68

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Argentina striata

</td>

<td style="text-align:left;">

Striated argentine

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

72

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Argyropelecus aculeatus

</td>

<td style="text-align:left;">

Lovely hatchetfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ariomma bondi

</td>

<td style="text-align:left;">

Silver-rag driftfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

18

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ariomma melanum

</td>

<td style="text-align:left;">

Brown driftfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ariosoma balearicum

</td>

<td style="text-align:left;">

Bandtooth conger

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Aspidophoroides monopterygius

</td>

<td style="text-align:left;">

Alligatorfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

300

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Astroscopus guttatus

</td>

<td style="text-align:left;">

Northern stargazer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Astroscopus y-graecum

</td>

<td style="text-align:left;">

Southern stargazer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Aulostomus maculatus

</td>

<td style="text-align:left;">

Trumpetfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bairdiella chrysoura

</td>

<td style="text-align:left;">

Silver perch

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

15

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Balistes capriscus

</td>

<td style="text-align:left;">

Grey triggerfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bellator brachychir

</td>

<td style="text-align:left;">

Shortfin searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bellator egretta

</td>

<td style="text-align:left;">

Streamer searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bembrops gobioides

</td>

<td style="text-align:left;">

Goby flathead

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Benthodesmus simonyi

</td>

<td style="text-align:left;">

Simony’s frostfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bothus ocellatus

</td>

<td style="text-align:left;">

Eyed flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bothus robinsi

</td>

<td style="text-align:left;">

Twospot flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Brama brama

</td>

<td style="text-align:left;">

Atlantic pomfret

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Brevoortia tyrannus

</td>

<td style="text-align:left;">

Atlantic menhaden

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

53

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Brosme brosme

</td>

<td style="text-align:left;">

Cusk

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

90

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Calamus leucosteus

</td>

<td style="text-align:left;">

Whitebone porgy

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Canthigaster rostrata

</td>

<td style="text-align:left;">

Caribbean sharpnose-puffer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Carcharhinus acronotus

</td>

<td style="text-align:left;">

Blacknose shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Carcharhinus brevipinna

</td>

<td style="text-align:left;">

Spinner shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Carcharhinus falciformis

</td>

<td style="text-align:left;">

Silky shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Carcharhinus obscurus

</td>

<td style="text-align:left;">

Dusky shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Carcharhinus plumbeus

</td>

<td style="text-align:left;">

Sandbar shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

16

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Carcharias taurus

</td>

<td style="text-align:left;">

Sand tiger shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

11

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Caulolatilus chrysops

</td>

<td style="text-align:left;">

Atlantic goldeneye tilefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Caulolatilus microps

</td>

<td style="text-align:left;">

Grey tilefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

17

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Centropristis ocyurus

</td>

<td style="text-align:left;">

Bank sea bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Centropristis philadelphica

</td>

<td style="text-align:left;">

Rock sea bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Centropristis striata

</td>

<td style="text-align:left;">

Black seabass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

594

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cetorhinus maximus

</td>

<td style="text-align:left;">

Basking shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chaetodipterus faber

</td>

<td style="text-align:left;">

Atlantic spadefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chaetodon aya

</td>

<td style="text-align:left;">

Bank butterflyfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chaetodon ocellatus

</td>

<td style="text-align:left;">

Spotfin butterflyfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chaetodon sedentarius

</td>

<td style="text-align:left;">

Reef butterflyfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chaetodon striatus

</td>

<td style="text-align:left;">

Banded butterflyfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chauliodus danae

</td>

<td style="text-align:left;">

Dana viperfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chauliodus sloani

</td>

<td style="text-align:left;">

Sloane’s viperfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chaunax stigmaeus

</td>

<td style="text-align:left;">

Redeye gaper

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chlorophthalmus agassizi

</td>

<td style="text-align:left;">

Shortnose greeneye

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

280

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Citharichthys arctifrons

</td>

<td style="text-align:left;">

Gulf Stream flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1287

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Citharichthys cornutus

</td>

<td style="text-align:left;">

Horned whiff

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

16

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Citharichthys gymnorhinus

</td>

<td style="text-align:left;">

Anglefin whiff

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Citharichthys macrops

</td>

<td style="text-align:left;">

Spotted whiff

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Clupea harengus

</td>

<td style="text-align:left;">

Atlantic herring

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2327

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Conger oceanicus

</td>

<td style="text-align:left;">

American conger

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

44

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cookeolus japonicus

</td>

<td style="text-align:left;">

Longfinned bullseye

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cryptacanthodes maculatus

</td>

<td style="text-align:left;">

Wrymouth

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

195

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cyclopsetta fimbriata

</td>

<td style="text-align:left;">

Spotfin flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cyclopterus lumpus

</td>

<td style="text-align:left;">

Lumpfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

74

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cynoscion nothus

</td>

<td style="text-align:left;">

Silver seatrout

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cynoscion regalis

</td>

<td style="text-align:left;">

Squeteague

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

92

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cyttopsis rosea

</td>

<td style="text-align:left;">

Rosy dory

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

11

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dactylopterus volitans

</td>

<td style="text-align:left;">

Flying gurnard

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dasyatis americana

</td>

<td style="text-align:left;">

Southern stingray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

14

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dasyatis centroura

</td>

<td style="text-align:left;">

Roughtail stingray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

57

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dasyatis say

</td>

<td style="text-align:left;">

Bluntnose stingray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

26

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Decapterus macarellus

</td>

<td style="text-align:left;">

Mackerel scad

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Decapterus punctatus

</td>

<td style="text-align:left;">

Round scad

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

11

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dibranchus atlanticus

</td>

<td style="text-align:left;">

Atlantic batfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Diodon holocanthus

</td>

<td style="text-align:left;">

Longspined porcupinefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Diplectrum formosum

</td>

<td style="text-align:left;">

Sand perch

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dipturus laevis

</td>

<td style="text-align:left;">

Barndoor skate

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1003

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Echeneis naucrates

</td>

<td style="text-align:left;">

Live sharksucker

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Echiophis intertinctus

</td>

<td style="text-align:left;">

Spotted spoon-nose eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Enchelyopus cimbrius

</td>

<td style="text-align:left;">

Fourbeard rockling

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

644

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Epigonus pandionis

</td>

<td style="text-align:left;">

Bigeye

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Equetus acuminatus

</td>

<td style="text-align:left;">

High-hat

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Equetus umbrosus

</td>

<td style="text-align:left;">

Cubbyu

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Etmopterus gracilispinis

</td>

<td style="text-align:left;">

Broadbanded lanternshark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Etropus microstomus

</td>

<td style="text-align:left;">

Smallmouth flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

164

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Etropus rimosus

</td>

<td style="text-align:left;">

Gray flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Etrumeus teres

</td>

<td style="text-align:left;">

Red-eye round herring

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

17

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Eumicrotremus spinosus

</td>

<td style="text-align:left;">

Atlantic spiny lumpsucker

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Fistularia petimba

</td>

<td style="text-align:left;">

Red cornetfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

14

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Fistularia tabacaria

</td>

<td style="text-align:left;">

Cornetfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Foetorepus agassizi

</td>

<td style="text-align:left;">

Spotfin dragonet

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

37

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gadella imberbis

</td>

<td style="text-align:left;">

Beardless codling

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gadus morhua

</td>

<td style="text-align:left;">

Atlantic cod

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1113

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Galeocerdo cuvier

</td>

<td style="text-align:left;">

Tiger shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gasterosteus aculeatus

</td>

<td style="text-align:left;">

Three-spined stickleback

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gastropsetta frontalis

</td>

<td style="text-align:left;">

Shrimp flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gephyroberyx darwini

</td>

<td style="text-align:left;">

Darwin’s slimehead

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Glyptocephalus cynoglossus

</td>

<td style="text-align:left;">

Witch flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1365

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gymnothorax saxicola

</td>

<td style="text-align:left;">

Honeycomb moray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gymnura altavela

</td>

<td style="text-align:left;">

Spiny butterfly ray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

32

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Gymnura micrura

</td>

<td style="text-align:left;">

Smooth butterfly ray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

14

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Haemulon aurolineatum

</td>

<td style="text-align:left;">

Tomtate grunt

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Halieutichthys aculeatus

</td>

<td style="text-align:left;">

Pancake batfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

18

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Helicolenus dactylopterus

</td>

<td style="text-align:left;">

Blackbelly rosefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

558

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hemanthias aureorubens

</td>

<td style="text-align:left;">

Streamer bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

19

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hemipteronotus novacula

</td>

<td style="text-align:left;">

Pearly razorfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hemitripterus americanus

</td>

<td style="text-align:left;">

Sea raven

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

921

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hippoglossoides platessoides

</td>

<td style="text-align:left;">

American plaice

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1246

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hippoglossus hippoglossus

</td>

<td style="text-align:left;">

Atlantic halibut

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

136

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Holocentrus adscensionis

</td>

<td style="text-align:left;">

Squirrelfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hoplostethus mediterraneus

</td>

<td style="text-align:left;">

Mediterranean slimehead

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hoplostethus occidentalis

</td>

<td style="text-align:left;">

Atlantic Roughy

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Kathetostoma albigutta

</td>

<td style="text-align:left;">

Lancer stargazer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lactophrys polygonia

</td>

<td style="text-align:left;">

Honeycomb cowfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lactophrys quadricornis

</td>

<td style="text-align:left;">

Scrawled cowfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Laemonema barbatulum

</td>

<td style="text-align:left;">

Shortbeard codling

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lagocephalus laevigatus

</td>

<td style="text-align:left;">

Smooth puffer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lagodon rhomboides

</td>

<td style="text-align:left;">

Pinfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

34

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Larimus fasciatus

</td>

<td style="text-align:left;">

Banded drum

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

16

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Leiostomus xanthurus

</td>

<td style="text-align:left;">

Spot croaker

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

49

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lepophidium jeannae

</td>

<td style="text-align:left;">

Mottled cusk-eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lepophidium profundorum

</td>

<td style="text-align:left;">

Blackrim cusk-eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

461

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Leucoraja erinacea

</td>

<td style="text-align:left;">

Little skate

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2391

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Leucoraja garmani

</td>

<td style="text-align:left;">

Rosette skate

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

370

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Leucoraja ocellata

</td>

<td style="text-align:left;">

Winter skate

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2028

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Limanda ferruginea

</td>

<td style="text-align:left;">

Yellowtail flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1161

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Liparis atlanticus

</td>

<td style="text-align:left;">

Atlantic seasnail

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

32

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lophius americanus

</td>

<td style="text-align:left;">

American angler

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1887

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lophius gastrophysus

</td>

<td style="text-align:left;">

Blackfin goosefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

27

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lopholatilus chamaeleonticeps

</td>

<td style="text-align:left;">

Great northern tilefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

35

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lumpenus lumpretaeformis

</td>

<td style="text-align:left;">

Snakeblenny

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

44

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lumpenus maculatus

</td>

<td style="text-align:left;">

Daubed shanny

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lutjanus campechanus

</td>

<td style="text-align:left;">

Northern red snapper

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lycenchelys verrilli

</td>

<td style="text-align:left;">

Wolf eelpout

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

30

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Macrorhamphosus scolopax

</td>

<td style="text-align:left;">

Longspine snipefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

82

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Macrozoarces americanus

</td>

<td style="text-align:left;">

Ocean pout

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1373

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Malacocephalus occidentalis

</td>

<td style="text-align:left;">

Western softhead grenadier

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Malacoraja senta

</td>

<td style="text-align:left;">

Smooth skate

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

748

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Maurolicus weitzmani

</td>

<td style="text-align:left;">

Atlantic pearlside

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

141

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Melanogrammus aeglefinus

</td>

<td style="text-align:left;">

Haddock

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1471

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Melanostigma atlanticum

</td>

<td style="text-align:left;">

Atlantic soft pout

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

52

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Menidia menidia

</td>

<td style="text-align:left;">

Atlantic silverside

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

138

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Menticirrhus americanus

</td>

<td style="text-align:left;">

Southern kingcroaker

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

49

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Menticirrhus saxatilis

</td>

<td style="text-align:left;">

Northern kingfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

53

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Merluccius albidus

</td>

<td style="text-align:left;">

Offshore silver hake

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

219

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Merluccius bilinearis

</td>

<td style="text-align:left;">

Silver hake

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3517

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Micropogonias undulatus

</td>

<td style="text-align:left;">

Atlantic croaker

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

57

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Mola mola

</td>

<td style="text-align:left;">

Ocean sunfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Monacanthus ciliatus

</td>

<td style="text-align:left;">

Fringed filefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Monacanthus hispidus

</td>

<td style="text-align:left;">

Planehead filefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Monolene sessilicauda

</td>

<td style="text-align:left;">

Deepwater flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

56

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Morone saxatilis

</td>

<td style="text-align:left;">

Striped bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

102

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Mullus auratus

</td>

<td style="text-align:left;">

Red goatfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Mustelus canis

</td>

<td style="text-align:left;">

Dusky smooth-hound

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

356

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Myliobatis freminvillei

</td>

<td style="text-align:left;">

Bullnose eagle ray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

15

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Myliobatis goodei

</td>

<td style="text-align:left;">

Southern eagle ray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Myoxocephalus aenaeus

</td>

<td style="text-align:left;">

Grubby

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

75

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Myoxocephalus octodecemspinosus

</td>

<td style="text-align:left;">

Longhorn sculpin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1418

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Myoxocephalus scorpius

</td>

<td style="text-align:left;">

Shorthorn sculpin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Myripristis jacobus

</td>

<td style="text-align:left;">

Blackbar soldierfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Myxine glutinosa

</td>

<td style="text-align:left;">

Atlantic hagfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

213

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Nemichthys scolopaceus

</td>

<td style="text-align:left;">

Slender snipe eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

32

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Nezumia bairdi

</td>

<td style="text-align:left;">

Marlin-spike grenadier

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ogcocephalus corniger

</td>

<td style="text-align:left;">

Longnose batfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ogcocephalus nasutus

</td>

<td style="text-align:left;">

Shortnose batfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ogcocephalus parvus

</td>

<td style="text-align:left;">

Roughback batfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ophichthus puncticeps

</td>

<td style="text-align:left;">

Palespotted eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ophidion grayi

</td>

<td style="text-align:left;">

Blotched cusk-eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ophidion holbrooki

</td>

<td style="text-align:left;">

Band cusk-eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ophidion marginatum

</td>

<td style="text-align:left;">

Striped cusk-eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

24

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ophidion selenops

</td>

<td style="text-align:left;">

Mooneye cusk-eel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Opisthonema oglinum

</td>

<td style="text-align:left;">

Atlantic thread herring

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Orthopristis chrysoptera

</td>

<td style="text-align:left;">

Pigfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

21

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Osmerus mordax

</td>

<td style="text-align:left;">

Rainbow smelt

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

11

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pagrus sedecim

</td>

<td style="text-align:left;">

Red porgy

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Paralichthys albigutta

</td>

<td style="text-align:left;">

Gulf flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Paralichthys dentatus

</td>

<td style="text-align:left;">

Summer flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1399

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Paralichthys oblongus

</td>

<td style="text-align:left;">

American fourspot flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1723

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Paralichthys squamilentus

</td>

<td style="text-align:left;">

Broad flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Parasudis truculenta

</td>

<td style="text-align:left;">

Longnose greeneye

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

45

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Peprilus alepidotus

</td>

<td style="text-align:left;">

American harvestfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Peprilus triacanthus

</td>

<td style="text-align:left;">

Atlantic butterfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1354

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Peristedion gracile

</td>

<td style="text-align:left;">

Slender searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Peristedion miniatum

</td>

<td style="text-align:left;">

Armored searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

82

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Petromyzon marinus

</td>

<td style="text-align:left;">

Sea lamprey

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

37

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pholis gunnellus

</td>

<td style="text-align:left;">

Rock gunnel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Physiculus fulvus

</td>

<td style="text-align:left;">

Hakeling

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Poecilopsetta beani

</td>

<td style="text-align:left;">

Deepwater dab

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pogonias cromis

</td>

<td style="text-align:left;">

Black drum

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pollachius virens

</td>

<td style="text-align:left;">

Saithe

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

461

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Polymetme thaeocoryla

</td>

<td style="text-align:left;">

Lightfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Polymixia lowei

</td>

<td style="text-align:left;">

Beardfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

162

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Polymixia nobilis

</td>

<td style="text-align:left;">

Stout beardfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pomatomus saltatrix

</td>

<td style="text-align:left;">

Bluefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

87

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pontinus longispinis

</td>

<td style="text-align:left;">

Longspine scorpionfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Porichthys plectrodon

</td>

<td style="text-align:left;">

Atlantic midshipman

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Priacanthus arenatus

</td>

<td style="text-align:left;">

Atlantic bigeye

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus alatus

</td>

<td style="text-align:left;">

Spiny searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

39

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus carolinus

</td>

<td style="text-align:left;">

Northern searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1288

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus evolans

</td>

<td style="text-align:left;">

Striped searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

229

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus ophryas

</td>

<td style="text-align:left;">

Bandtail searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus rubio

</td>

<td style="text-align:left;">

Blackwing searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus scitulus

</td>

<td style="text-align:left;">

Leopard searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus stearnsi

</td>

<td style="text-align:left;">

Shortwing searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Prionotus tribulus

</td>

<td style="text-align:left;">

Bighead searobin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pristigenys alta

</td>

<td style="text-align:left;">

Short bigeye

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pristipomoides aquilonaris

</td>

<td style="text-align:left;">

Wenchman

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Promethichthys prometheus

</td>

<td style="text-align:left;">

Roudi escolar

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pseudopleuronectes americanus

</td>

<td style="text-align:left;">

Winter flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1307

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Rachycentron canadum

</td>

<td style="text-align:left;">

Cobia

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Raja eglanteria

</td>

<td style="text-align:left;">

Clearnose skate

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

543

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Reinhardtius hippoglossoides

</td>

<td style="text-align:left;">

Greenland halibut

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

26

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Rhinoptera bonasus

</td>

<td style="text-align:left;">

Cownose ray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Rhizoprionodon terraenovae

</td>

<td style="text-align:left;">

Atlantic sharpnose shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

21

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Rhomboplites aurorubens

</td>

<td style="text-align:left;">

Vermilion snapper

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Salmo salar

</td>

<td style="text-align:left;">

Atlantic salmon

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sardinella aurita

</td>

<td style="text-align:left;">

Round sardinella

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Saurida normani

</td>

<td style="text-align:left;">

Shortjaw lizardfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sciaenops ocellatus

</td>

<td style="text-align:left;">

Red drum

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scomber japonicus

</td>

<td style="text-align:left;">

Chub mackerel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scomber scombrus

</td>

<td style="text-align:left;">

Atlantic mackerel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1373

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scomberesox saurus

</td>

<td style="text-align:left;">

Atlantic saury

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scomberomorus maculatus

</td>

<td style="text-align:left;">

Atlantic Spanish mackerel

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scophthalmus aquosus

</td>

<td style="text-align:left;">

Windowpane flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1481

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scorpaena agassizi

</td>

<td style="text-align:left;">

Longfin scorpionfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scorpaena brasiliensis

</td>

<td style="text-align:left;">

Barbfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scyliorhinus retifer

</td>

<td style="text-align:left;">

Chain catshark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

541

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sebastes fasciatus

</td>

<td style="text-align:left;">

Acadian redfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

919

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Selene setapinnis

</td>

<td style="text-align:left;">

Atlantic moonfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Seriola dumerili

</td>

<td style="text-align:left;">

Greater amberjack

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Seriola fasciata

</td>

<td style="text-align:left;">

Lesser amberjack

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Seriola rivoliana

</td>

<td style="text-align:left;">

Longfin yellowtail

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Seriola zonata

</td>

<td style="text-align:left;">

Banded rudderfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Serranus atrobranchus

</td>

<td style="text-align:left;">

Blackear bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Serranus notospilus

</td>

<td style="text-align:left;">

Saddle bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Serranus phoebe

</td>

<td style="text-align:left;">

Tattler

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sphoeroides dorsalis

</td>

<td style="text-align:left;">

Marbled puffer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sphoeroides maculatus

</td>

<td style="text-align:left;">

Northern puffer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

36

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sphoeroides nephelus

</td>

<td style="text-align:left;">

Southern puffer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sphoeroides pachygaster

</td>

<td style="text-align:left;">

Blunthead puffer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sphoeroides spengleri

</td>

<td style="text-align:left;">

Bandtail puffer

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sphyrna lewini

</td>

<td style="text-align:left;">

Scalloped hammerhead

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sphyrna zygaena

</td>

<td style="text-align:left;">

Smooth hammerhead

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Squalus acanthias

</td>

<td style="text-align:left;">

Picked dogfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2419

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Squatina dumeril

</td>

<td style="text-align:left;">

Atlantic angel shark

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

131

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Stellifer lanceolatus

</td>

<td style="text-align:left;">

American stardrum

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Stenotomus chrysops

</td>

<td style="text-align:left;">

Scup

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

434

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Stomias boa

</td>

<td style="text-align:left;">

Boa dragonfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Syacium papillosum

</td>

<td style="text-align:left;">

Dusky flounder

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

16

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Symphurus plagiusa

</td>

<td style="text-align:left;">

Blackcheek tonguefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Synagrops bellus

</td>

<td style="text-align:left;">

Blackmouth bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Synagrops spinosus

</td>

<td style="text-align:left;">

Keelcheek bass

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

17

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Syngnathus fuscus

</td>

<td style="text-align:left;">

Northern pipefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

23

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Synodus foetens

</td>

<td style="text-align:left;">

Inshore lizardfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

45

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Synodus intermedius

</td>

<td style="text-align:left;">

Sand diver

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Synodus poeyi

</td>

<td style="text-align:left;">

Offshore lizardfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Tautoga onitis

</td>

<td style="text-align:left;">

Tautog

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Tautogolabrus adspersus

</td>

<td style="text-align:left;">

Cunner

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

188

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Thunnus alalunga

</td>

<td style="text-align:left;">

Albacore

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Torpedo nobiliana

</td>

<td style="text-align:left;">

Electric ray

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

60

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Trachinocephalus myops

</td>

<td style="text-align:left;">

Snakefish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

29

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Trachurus lathami

</td>

<td style="text-align:left;">

Rough scad

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

39

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Trichiurus lepturus

</td>

<td style="text-align:left;">

Largehead hairtail

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

47

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Triglops murrayi

</td>

<td style="text-align:left;">

Moustache sculpin

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

98

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Trinectes maculatus

</td>

<td style="text-align:left;">

Hogchoker

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ulvaria subbifurcata

</td>

<td style="text-align:left;">

Radiated shanny

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

15

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Upeneus parvus

</td>

<td style="text-align:left;">

Dwarf goatfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis chesteri

</td>

<td style="text-align:left;">

Longfin hake

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

89

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis chuss

</td>

<td style="text-align:left;">

Red hake

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2566

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis earlli

</td>

<td style="text-align:left;">

Carolina hake

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis floridana

</td>

<td style="text-align:left;">

Southern codling

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis regia

</td>

<td style="text-align:left;">

Spotted codling

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1841

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Urophycis tenuis

</td>

<td style="text-align:left;">

White hake

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1062

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Xenodermichthys copei

</td>

<td style="text-align:left;">

Bluntsnout smooth-head

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Xenolepidichthys dalgleishi

</td>

<td style="text-align:left;">

Spotted tinselfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Zenopsis conchifera

</td>

<td style="text-align:left;">

Silvery John dory

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

117

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Abralia veranyi

</td>

<td style="text-align:left;">

eye-flash squid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Acanthocarpus alexandri

</td>

<td style="text-align:left;">

gladiator box crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

28

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Arctica islandica

</td>

<td style="text-align:left;">

ocean quahog

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

26

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Argopecten gibbus

</td>

<td style="text-align:left;">

Calico scallop

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Argopecten irradians

</td>

<td style="text-align:left;">

Atlantic bay scallop

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Asterias vulgaris

</td>

<td style="text-align:left;">

common starfish

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bathynectes longispina

</td>

<td style="text-align:left;">

bathyal swimming crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

30

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Bathypolypus arcticus

</td>

<td style="text-align:left;">

North Atlantic octopus

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

806

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Buccinum undatum

</td>

<td style="text-align:left;">

waved whelk

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Callinectes sapidus

</td>

<td style="text-align:left;">

blue crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

14

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cancer borealis

</td>

<td style="text-align:left;">

Jonah crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

967

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Cancer irroratus

</td>

<td style="text-align:left;">

Atlantic rock crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1436

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Caretta caretta

</td>

<td style="text-align:left;">

loggerhead turtle

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chionoecetes opilio

</td>

<td style="text-align:left;">

snow crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

99

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Chlamys islandica

</td>

<td style="text-align:left;">

Iceland scallop

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Crangon septemspinosa

</td>

<td style="text-align:left;">

sevenspine bay shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

39

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Dichelopandalus leptocerus

</td>

<td style="text-align:left;">

bristled longbeak

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

690

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Geryon fenneri

</td>

<td style="text-align:left;">

golden deepsea crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Geryon quinquedens

</td>

<td style="text-align:left;">

red crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

92

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Halichoerus grypus

</td>

<td style="text-align:left;">

grey seal

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Hepatus epheliticus

</td>

<td style="text-align:left;">

calico box crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Homarus americanus

</td>

<td style="text-align:left;">

American lobster

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1594

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Illex illecebrosus

</td>

<td style="text-align:left;">

northern shortfin squid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

848

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lebbeus groenlandicus

</td>

<td style="text-align:left;">

spiny lebbeid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lebbeus polaris

</td>

<td style="text-align:left;">

polar lebbeid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

62

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lepidochelys kempi

</td>

<td style="text-align:left;">

Kemp’s ridley turtle

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Limulus polyphemus

</td>

<td style="text-align:left;">

horseshoe crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

340

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lithodes maja

</td>

<td style="text-align:left;">

Norway king crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

65

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Loligo pealeii

</td>

<td style="text-align:left;">

longfin inshore squid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

2048

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Loligo pleii

</td>

<td style="text-align:left;">

slender inshore squid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Lolliguncula brevis

</td>

<td style="text-align:left;">

Western Atlantic brief squid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Octopus vulgaris

</td>

<td style="text-align:left;">

common octopus

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ovalipes ocellatus

</td>

<td style="text-align:left;">

ocellate lady crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

132

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Ovalipes stephensoni

</td>

<td style="text-align:left;">

coarsehand lady crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

141

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pandalus borealis

</td>

<td style="text-align:left;">

northern shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

192

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pandalus montagui

</td>

<td style="text-align:left;">

Aesop shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

104

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pandalus propinquus

</td>

<td style="text-align:left;">

Shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

154

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pasiphaea multidentata

</td>

<td style="text-align:left;">

pink glass shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

254

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Placopecten magellanicus

</td>

<td style="text-align:left;">

deep sea scallop

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1342

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pleoticus robustus

</td>

<td style="text-align:left;">

royal red shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Pontophilus norvegicus

</td>

<td style="text-align:left;">

Norwegian shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

206

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Portunus spinimanus

</td>

<td style="text-align:left;">

blotched swimming crab

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Scyllarides nodifer

</td>

<td style="text-align:left;">

ridged slipper lobster

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Sicyonia brevirostris

</td>

<td style="text-align:left;">

brown rock shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

33

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Spirontocaris liljeborgii

</td>

<td style="text-align:left;">

friendly blade shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

36

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Spisula solidissima

</td>

<td style="text-align:left;">

Atlantic surfclam

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

34

</td>

</tr>

<tr>

<td style="text-align:left;font-style: italic;">

Stoloteuthis leucoptera

</td>

<td style="text-align:left;">

butterfly bob tail

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

68

</td>

</tr>

</tbody>

</table>

<br/>

I need to come up with a ecologically relevant way to group species.
What would you suggest? The taxonomic information for each species is
also in the data and I can use Genus, Family, Order, and Class to
filter, group, and summarize the data.

## Question:

How many organisms that could not be classified to species were caugt in
the trawls over all the data sets?

Printing taxa level, common names, and counts.

``` r
dta_nonsp_only %>% 
  count(BTS_sp, level, Com_name, type) %>% 
  arrange(type) %>% 
  kable(col.names = c("BTS name", "Taxa level", "Common name", "Category", "Count")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

BTS name

</th>

<th style="text-align:left;">

Taxa level

</th>

<th style="text-align:left;">

Common name

</th>

<th style="text-align:left;">

Category

</th>

<th style="text-align:right;">

Count

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Alepocephaliidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Smelts

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Anguilliformes

</td>

<td style="text-align:left;">

Order

</td>

<td style="text-align:left;">

Eels

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

Apogonidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Cardinalfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Balistidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Triggerfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Blenniidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Combtooth blenny

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Bothidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Lefteye flounders

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

15

</td>

</tr>

<tr>

<td style="text-align:left;">

Caproidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Boarfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Carcharhinidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Requiem sharks

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Clupeidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Herrings, shads, sardines, menhadens

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Congridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Conger and garden eels

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

103

</td>

</tr>

<tr>

<td style="text-align:left;">

Cottidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Sculpins

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

Cyclopteridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Lumpfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

Gadidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Cods

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Gobiidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

True gobies

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Holocentridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Squirrelfish, soldierfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Labridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Wrasses

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Lophiiformes

</td>

<td style="text-align:left;">

Order

</td>

<td style="text-align:left;">

Anglerfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Lutjanidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Snappers

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Macrouridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Deep sea Gadiformes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

202

</td>

</tr>

<tr>

<td style="text-align:left;">

Moridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Codlings, hakelings, moras

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

Muraenidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Moray eels

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Myctophidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Lanternfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

343

</td>

</tr>

<tr>

<td style="text-align:left;">

Ogcocephalidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Deep sea anglerfish, batfishes, seabats

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

Ophichthidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Snake eels

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

21

</td>

</tr>

<tr>

<td style="text-align:left;">

Ophidiidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Cusk-eels

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

Paralepidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Barracudina

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

52

</td>

</tr>

<tr>

<td style="text-align:left;">

Pleuronectidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Righteye flounders

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Pleuronectiformes

</td>

<td style="text-align:left;">

Order

</td>

<td style="text-align:left;">

Flatfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Pomacentridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Damselfishes, clownfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Scaridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Parrotfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Sciaenidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Drums, croakers

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Scombridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Mackerel, tuna, bonito

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

Scorpaenidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Scorpionfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

124

</td>

</tr>

<tr>

<td style="text-align:left;">

Selachimorpha

</td>

<td style="text-align:left;">

Class

</td>

<td style="text-align:left;">

Sharks, rays, and skates

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Serranidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Basses, groupers

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Squalidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

dogfish sharks, spiny dogfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Sternoptychidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

deepsea hatchetfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

22

</td>

</tr>

<tr>

<td style="text-align:left;">

Stomiidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Barbeled dragonfishes

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

Stromateidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Butterfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Syngnathidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

seahorses, pipefish, seadragons

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

71

</td>

</tr>

<tr>

<td style="text-align:left;">

Synodontidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Lizardfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

16

</td>

</tr>

<tr>

<td style="text-align:left;">

Tetraodontidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Pufferfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Trichiuridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Cutlassfish

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

Triglidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Sea robins, gurnard

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

Uranoscopidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Stargazers

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Zoarcidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Eelpouts

</td>

<td style="text-align:left;">

fish

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Calappidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Crabs

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

7

</td>

</tr>

<tr>

<td style="text-align:left;">

Cancridae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Crabs

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Cephalopoda

</td>

<td style="text-align:left;">

Class

</td>

<td style="text-align:left;">

Squid, octopus, cuttlefish, nautilus

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

15

</td>

</tr>

<tr>

<td style="text-align:left;">

Crustacea shrimp

</td>

<td style="text-align:left;">

Order

</td>

<td style="text-align:left;">

Shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

1013

</td>

</tr>

<tr>

<td style="text-align:left;">

Galatheidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Squat lobsters

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

277

</td>

</tr>

<tr>

<td style="text-align:left;">

Majidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Crabs

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

215

</td>

</tr>

<tr>

<td style="text-align:left;">

Octopoda

</td>

<td style="text-align:left;">

Order

</td>

<td style="text-align:left;">

Octopus

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

6

</td>

</tr>

<tr>

<td style="text-align:left;">

Portunidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Swimming crabs

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

65

</td>

</tr>

<tr>

<td style="text-align:left;">

Sepiolidae

</td>

<td style="text-align:left;">

Family

</td>

<td style="text-align:left;">

Bobtail squid

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

861

</td>

</tr>

<tr>

<td style="text-align:left;">

Stelleroidea

</td>

<td style="text-align:left;">

Class

</td>

<td style="text-align:left;">

Sea stars

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

Stomatopoda

</td>

<td style="text-align:left;">

Order

</td>

<td style="text-align:left;">

Mantis shrimp

</td>

<td style="text-align:left;">

not\_fish

</td>

<td style="text-align:right;">

62

</td>

</tr>

</tbody>

</table>

<br/>

All of these data also have `cruiseid`, `datetime` (except the 13
missing mentioned in EDA 1), and `stratum` information associated with
them. The data are now in a format to parse, aggregate, and summarize in
multiple ways (e.g. spatially, temporally, by taxa level).

# Wrap up EDA 2

In the next EDA, I will incorporate swim bladder and habitat information
for species.

Saving the intermediate files for the next EDA. Note that `sp_fish` and
`sp_sealife` have changed since the first EDA and they are being saved
again.

``` r
write_rds(dta_nonsp_only, "output_input/dta_and_non_species.rds")
write_rds(dta_sp_only, "output_input/dta_and_species.rds")
write_rds(sp_sealife, "output_input/sp_sealife.rds")
write_rds(sp_fish, "output_input/sp_fish.rds")
```
