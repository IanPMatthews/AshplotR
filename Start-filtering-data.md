AshlplotR
================
Ian Matthews & Joshua Pike
2022-12-13

# Introduction

This script can be used to create bi-plots and Harker plots of all
tephra major elemental data. It includes interactive exploration of the
data and some discussion on identifying useful elements for
discrimination. Guidance for saving plots as .svg or .pdf files is also
provided. All the code used to produce this document is downloadable as
an RMarkdown file. To run this you will require r and RStudio. We are
happy for you to use, share and improve our code, but would ask that you
cite this as a poin tof origin for the plots.

## load packages

We first load in packages required to generate the plots and manipulate
the data. You may need to install these packages in r or update them to
work correctly.

``` r
library(ggplot2)
library(factoextra)
library(readr)
library(compositions)
library(gganimate)
library(cowplot)
library(tidyverse)
library(gt)
library(plotly)
library(svglite)
library(png)
library(here)
library(ggdensity)
```

## Load your data

Any data placed within the the data file of the r project can be read
using the code outlined below. As this is a Markdown script the ‘here’
package is used to ensure that project links do not break when moving
scripts around. As long as you start a new R project and drop this
script into it (using the structure outlined on the home page) then it
will work. Data files should be in .csv format and contain the group of
the tephra as column 1 under the title “id” (see example files for
reference). Loaded here are some data for core ODP-980 in the North
Atlantic and a terrestrial site from the UK called MT.

``` r
chem <- here::here("Data","Borrobol_type.csv") %>% read.csv()
```

## Check your data has loaded correctly

The following script produces a summary table of mean values and number
of analyses in your data.

``` r
chem_sum <- chem %>% group_by(id) %>% dplyr::summarise(                                            
                                                       count=n(),
                                                       SiO2.= mean(SiO2),
                                                       dev_1 = sd(SiO2),
                                                       TiO2. = mean(TiO2),
                                                       dev_2 = sd(TiO2),
                                                       Al2O3. = mean(Al2O3),
                                                       dev_3 = sd(Al2O3),
                                                       FeOt = mean(FeO),
                                                       dev_4 = sd(FeO),
                                                       MnO.= mean(MnO),
                                                       dev_5 = sd(MnO),
                                                       MgO.= mean(MgO),
                                                       dev_6 = sd(MgO),
                                                       CaO.= mean(CaO),
                                                       dev_7 = sd(CaO),
                                                       Na2O. = mean(Na2O),
                                                       dev_8 = sd(Na2O),
                                                       K2O. = mean(K2O),
                                                       dev_9 = sd(K2O),
                                                       P2O5. = mean(P2O5),
                                                       dev_10 = sd(P2O5),
                                                       Total.=mean(Total),
                                                       dev_11 = sd(Total))
chem_sum <- chem_sum %>% mutate(across(where(is.numeric), ~ round(., digits = 2)))
chem_sum %>% rename(layer = id, n = count , SiO2 = SiO2., sd1 = dev_1, TiO2 = TiO2. , sd2 = dev_2)
```

    ## # A tibble: 12 x 24
    ##    layer          n  SiO2   sd1  TiO2   sd2 Al2O3. dev_3  FeOt dev_4  MnO. dev_5
    ##    <chr>      <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 Borrobol-~     2  76.4  0.39  0.07  0.01   13.0  0.42  0.95  0.01  0.05  0.01
    ##  2 Borrobol-~    12  76.4  0.41  0.12  0.01   12.7  0.34  1.56  0.12  0.04  0.01
    ##  3 Borrobol-~     8  76.4  0.59  0.12  0.01   12.6  0.57  1.47  0.15  0.04  0.01
    ##  4 Borrobol-~    35  75.9  0.31  0.15  0.01   12.9  0.2   1.58  0.13  0.04  0.01
    ##  5 Borrobol ~   124  76.4  0.36  0.13  0.01   12.7  0.21  1.55  0.11  0.04  0.01
    ##  6 Borrobol ~    13  76.5  0.25  0.13  0.01   12.8  0.23  1.53  0.1   0.04  0.01
    ##  7 Borrobol ~    21  76.5  0.54  0.12  0.02   12.9  0.48  1.36  0.16  0.04  0.01
    ##  8 CRUM1 597~    28  76.7  0.27  0.13  0.01   12.6  0.21  1.53  0.11  0.04  0.01
    ##  9 Fosen Tep~    10  76.9  0.49  0.12  0.01   12.3  0.43  1.54  0.2   0.05  0   
    ## 10 Hasseldal~   104  77.8  0.5   0.09  0.02   12.2  0.32  1.14  0.14  0.04  0.02
    ## 11 Hovsdalur~    23  77.9  0.53  0.12  0.02   12.3  0.12  1.16  0.12  0.04  0.02
    ## 12 Penifiler~   171  76.8  0.49  0.12  0.02   12.8  0.31  1.36  0.2   0.04  0.02
    ## # ... with 12 more variables: MgO. <dbl>, dev_6 <dbl>, CaO. <dbl>, dev_7 <dbl>,
    ## #   Na2O. <dbl>, dev_8 <dbl>, K2O. <dbl>, dev_9 <dbl>, P2O5. <dbl>,
    ## #   dev_10 <dbl>, Total. <dbl>, dev_11 <dbl>

``` r
gt(chem_sum)
```

<div id="jpxbvpmunc" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jpxbvpmunc .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jpxbvpmunc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jpxbvpmunc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jpxbvpmunc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jpxbvpmunc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jpxbvpmunc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jpxbvpmunc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jpxbvpmunc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jpxbvpmunc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jpxbvpmunc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jpxbvpmunc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jpxbvpmunc .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#jpxbvpmunc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jpxbvpmunc .gt_from_md > :first-child {
  margin-top: 0;
}

#jpxbvpmunc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jpxbvpmunc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jpxbvpmunc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jpxbvpmunc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jpxbvpmunc .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jpxbvpmunc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jpxbvpmunc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jpxbvpmunc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jpxbvpmunc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jpxbvpmunc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jpxbvpmunc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jpxbvpmunc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jpxbvpmunc .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jpxbvpmunc .gt_left {
  text-align: left;
}

#jpxbvpmunc .gt_center {
  text-align: center;
}

#jpxbvpmunc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jpxbvpmunc .gt_font_normal {
  font-weight: normal;
}

#jpxbvpmunc .gt_font_bold {
  font-weight: bold;
}

#jpxbvpmunc .gt_font_italic {
  font-style: italic;
}

#jpxbvpmunc .gt_super {
  font-size: 65%;
}

#jpxbvpmunc .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">SiO2.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">TiO2.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Al2O3.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">FeOt</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">MnO.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_5</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">MgO.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_6</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">CaO.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_7</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Na2O.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_8</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">K2O.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_9</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">P2O5.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_10</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Total.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">dev_11</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Borrobol-type Tephra GI-1a</td>
<td class="gt_row gt_right">2</td>
<td class="gt_row gt_right">76.37</td>
<td class="gt_row gt_right">0.39</td>
<td class="gt_row gt_right">0.07</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">13.04</td>
<td class="gt_row gt_right">0.42</td>
<td class="gt_row gt_right">0.95</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.05</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">0.69</td>
<td class="gt_row gt_right">0.18</td>
<td class="gt_row gt_right">4.01</td>
<td class="gt_row gt_right">0.03</td>
<td class="gt_row gt_right">4.77</td>
<td class="gt_row gt_right">0.21</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Borrobol-type Tephra GI-1e_pt1</td>
<td class="gt_row gt_right">12</td>
<td class="gt_row gt_right">76.38</td>
<td class="gt_row gt_right">0.41</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">12.72</td>
<td class="gt_row gt_right">0.34</td>
<td class="gt_row gt_right">1.56</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.08</td>
<td class="gt_row gt_right">0.03</td>
<td class="gt_row gt_right">0.81</td>
<td class="gt_row gt_right">0.07</td>
<td class="gt_row gt_right">4.13</td>
<td class="gt_row gt_right">0.15</td>
<td class="gt_row gt_right">3.92</td>
<td class="gt_row gt_right">0.11</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Borrobol-type Tephra GI-1e_pt2</td>
<td class="gt_row gt_right">8</td>
<td class="gt_row gt_right">76.42</td>
<td class="gt_row gt_right">0.59</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">12.64</td>
<td class="gt_row gt_right">0.57</td>
<td class="gt_row gt_right">1.47</td>
<td class="gt_row gt_right">0.15</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.07</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.84</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">4.16</td>
<td class="gt_row gt_right">0.20</td>
<td class="gt_row gt_right">3.99</td>
<td class="gt_row gt_right">0.07</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Borrobol-type:GS-2.1-RHY Tephra</td>
<td class="gt_row gt_right">35</td>
<td class="gt_row gt_right">75.87</td>
<td class="gt_row gt_right">0.31</td>
<td class="gt_row gt_right">0.15</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">12.91</td>
<td class="gt_row gt_right">0.20</td>
<td class="gt_row gt_right">1.58</td>
<td class="gt_row gt_right">0.13</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.11</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.96</td>
<td class="gt_row gt_right">0.05</td>
<td class="gt_row gt_right">4.27</td>
<td class="gt_row gt_right">0.15</td>
<td class="gt_row gt_right">3.94</td>
<td class="gt_row gt_right">0.10</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Borrobol Tephra</td>
<td class="gt_row gt_right">124</td>
<td class="gt_row gt_right">76.43</td>
<td class="gt_row gt_right">0.36</td>
<td class="gt_row gt_right">0.13</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">12.74</td>
<td class="gt_row gt_right">0.21</td>
<td class="gt_row gt_right">1.55</td>
<td class="gt_row gt_right">0.11</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.08</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.78</td>
<td class="gt_row gt_right">0.05</td>
<td class="gt_row gt_right">4.26</td>
<td class="gt_row gt_right">0.21</td>
<td class="gt_row gt_right">3.94</td>
<td class="gt_row gt_right">0.10</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Borrobol Tephra-Sweden</td>
<td class="gt_row gt_right">13</td>
<td class="gt_row gt_right">76.50</td>
<td class="gt_row gt_right">0.25</td>
<td class="gt_row gt_right">0.13</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">12.76</td>
<td class="gt_row gt_right">0.23</td>
<td class="gt_row gt_right">1.53</td>
<td class="gt_row gt_right">0.10</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.07</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.77</td>
<td class="gt_row gt_right">0.05</td>
<td class="gt_row gt_right">4.24</td>
<td class="gt_row gt_right">0.32</td>
<td class="gt_row gt_right">3.96</td>
<td class="gt_row gt_right">0.13</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Borrobol Tephra - ASHIK</td>
<td class="gt_row gt_right">21</td>
<td class="gt_row gt_right">76.46</td>
<td class="gt_row gt_right">0.54</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">12.92</td>
<td class="gt_row gt_right">0.48</td>
<td class="gt_row gt_right">1.36</td>
<td class="gt_row gt_right">0.16</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.06</td>
<td class="gt_row gt_right">0.03</td>
<td class="gt_row gt_right">0.70</td>
<td class="gt_row gt_right">0.10</td>
<td class="gt_row gt_right">4.21</td>
<td class="gt_row gt_right">0.51</td>
<td class="gt_row gt_right">3.95</td>
<td class="gt_row gt_right">0.10</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">CRUM1 597 Tephra</td>
<td class="gt_row gt_right">28</td>
<td class="gt_row gt_right">76.67</td>
<td class="gt_row gt_right">0.27</td>
<td class="gt_row gt_right">0.13</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">12.59</td>
<td class="gt_row gt_right">0.21</td>
<td class="gt_row gt_right">1.53</td>
<td class="gt_row gt_right">0.11</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.07</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.77</td>
<td class="gt_row gt_right">0.06</td>
<td class="gt_row gt_right">4.30</td>
<td class="gt_row gt_right">0.14</td>
<td class="gt_row gt_right">3.91</td>
<td class="gt_row gt_right">0.09</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Fosen Tephra</td>
<td class="gt_row gt_right">10</td>
<td class="gt_row gt_right">76.88</td>
<td class="gt_row gt_right">0.49</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">12.31</td>
<td class="gt_row gt_right">0.43</td>
<td class="gt_row gt_right">1.54</td>
<td class="gt_row gt_right">0.20</td>
<td class="gt_row gt_right">0.05</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">0.07</td>
<td class="gt_row gt_right">0.03</td>
<td class="gt_row gt_right">0.70</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">4.30</td>
<td class="gt_row gt_right">0.14</td>
<td class="gt_row gt_right">4.02</td>
<td class="gt_row gt_right">0.30</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Hasseldalen Tephra</td>
<td class="gt_row gt_right">104</td>
<td class="gt_row gt_right">77.75</td>
<td class="gt_row gt_right">0.50</td>
<td class="gt_row gt_right">0.09</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">12.15</td>
<td class="gt_row gt_right">0.32</td>
<td class="gt_row gt_right">1.14</td>
<td class="gt_row gt_right">0.14</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.03</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.51</td>
<td class="gt_row gt_right">0.09</td>
<td class="gt_row gt_right">4.03</td>
<td class="gt_row gt_right">0.27</td>
<td class="gt_row gt_right">4.22</td>
<td class="gt_row gt_right">0.24</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Hovsdalur Tephra</td>
<td class="gt_row gt_right">23</td>
<td class="gt_row gt_right">77.88</td>
<td class="gt_row gt_right">0.53</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">12.27</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">1.16</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.05</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.49</td>
<td class="gt_row gt_right">0.06</td>
<td class="gt_row gt_right">3.61</td>
<td class="gt_row gt_right">0.24</td>
<td class="gt_row gt_right">4.38</td>
<td class="gt_row gt_right">0.60</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.00</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
    <tr><td class="gt_row gt_left">Penifiler Tephra</td>
<td class="gt_row gt_right">171</td>
<td class="gt_row gt_right">76.78</td>
<td class="gt_row gt_right">0.49</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">12.77</td>
<td class="gt_row gt_right">0.31</td>
<td class="gt_row gt_right">1.36</td>
<td class="gt_row gt_right">0.20</td>
<td class="gt_row gt_right">0.04</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">0.05</td>
<td class="gt_row gt_right">0.03</td>
<td class="gt_row gt_right">0.69</td>
<td class="gt_row gt_right">0.10</td>
<td class="gt_row gt_right">4.10</td>
<td class="gt_row gt_right">0.40</td>
<td class="gt_row gt_right">4.02</td>
<td class="gt_row gt_right">0.12</td>
<td class="gt_row gt_right">0.01</td>
<td class="gt_row gt_right">0.02</td>
<td class="gt_row gt_right">100</td>
<td class="gt_row gt_right">0</td></tr>
  </tbody>
  
  
</table>
</div>

## Make TAS calculations

To calculate for the TAS plot the data require normalisation of the
data. The rest of the script uses unnormalised data for plotting, but
the code below can be used to normalise other data also by replacing the
element to be normalise.

``` r
TAS_x <- (100/ chem [["Total"]]) * chem [["SiO2"]]
TAS_y <- (100/ chem [["Total"]]) * (chem [["K2O"]] + chem [["Na2O"]])

chem <- cbind(chem, TAS_x, TAS_y)
```

# Filtering mixed datasets

It is possible your datasheet contains multiple types of tephra which
would benefit from filtering before plotting. I suggest this is carried
out by using silica values as a divisor. Rhyolites \>69% silica is a
case for this. However, it might be worth plotting everything first
before then selecting a filter to ensure vital data is not being
excluded.

``` r
chem <- filter(chem, SiO2 >68)
```
