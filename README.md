# Vitamin C Concentration in Tomatoes 🍅 and Salads 🥬: Dataset and R Code
_Nicolas Biot, Rio Dallemagne, Emeline Dierge, Nicolas Dendoncker and Guillaume Lobet (2026)_

## Overview

This repository contains the dataset and R code used for analyzing the concentration of Vitamin C (total Vitamin C and Ascorbic Acid) in tomatoes and salads. The samples were collected from different supply chain types (supermarkets and local farms) and production systems (organic and conventional) in the Coeur de Condroz region of Belgium in September 2026.

## Dataset

The _Data_set.csv_ file includes the following information:
- Vegetable type: Tomatoes or salads.
- Supply chain type: Supermarket or local farm.
- Production system: Organic or conventional.
- Vitamin C concentration: Total Vitamin C and Ascorbic Acid content (with or without dtt), quantified using high-performance liquid chromatography (HPLC).

## Data Collection

A total of 78 tomato and 66 salad composite samples were collected over two weeks from:
- 5 local organic farms.
- 6 supermarkets.

## R Scripts

This repository includes the following R scripts:

- _Paper_vitC_2026.R_: Main analysis script used to process the data, perform statistical analyses, and generate results.
- _my_boxplot_theme.R_: Custom theme for creating publication-ready boxplots.
- _clean_anova_table.R_: Script to generate clean and formatted statistical tables from ANOVA results.

## How to Use

- Clone this repository to your local machine.
- Ensure all files (_Data_set.csv, Paper_vitC_2026.R, my_boxplot_theme.R, and clean_anova_table.R_) are in the same working directory.
- Run the _Paper_vitC_2026.R_ script to reproduce the analyses.

