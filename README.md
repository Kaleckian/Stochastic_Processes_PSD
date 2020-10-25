# Stochastic Processes for the PSD
This project is authored by Aline Silva Lima and Guilherme Hideo Assaoka Hossaka. One of its first relevant products will be an undergraduate thesis to be presented at the end of November 2020.

It aims to study the PSD (price for settlement of differences or "preço de liquidação de diferenças" - PLD) on its usual weekly grain and the recent hourly version to be fully implemented in January 2021 (Hourly PSD or "PLD horário").

The datasets of PSD, Hourly PSD (PSDh), and other relevant data (holidays, load step classification, Daylight Saving Time schedules, etc.) comes directly from the Electrical Energy Chamber of Trading (CCEE) on its website (links in "CCCE_links_de_preço.txt" file).

## Requirements
Project runs in R programming language (version 4.0.2, 2020-06-22) under R Studio IDE (version 1.3.1073). R Studio IDE is stricly necessary due rstudioapi library.

Other relevant libraries are called (or installed if missing) at '000_xls_to_rds.R' script.

## Manual Adjustments
Adjustments on the raw load step datasets at the "Other_Data" folder had to be made:

* Addition of 30 minutes on the specific starting and ending hours of the Daylight Saving Time;
* Renaming of columns to enable direct bind_rows() of the different yearly load step datasets.

## Current State on October 25th 2020
* Period of analysis runs from April 18th 2018 to October 21st 2020;
* Datasets of the weekly and hourly PSD are put on "tidy format", joined by load step classification, the 'dataframe_PSD' object in '000_xls_to_rds.R' script.
* Some plots for exploratory analysis visualization can be saved in .jpeg and .pdf format with '001_plot_to_jpeg.R' and '001_plot_to_pdf.R' respectively;
* The final datasets joins the weekly PSD publication on the Friday before the actual operational week ("semana operativa") from Saturday to the next Friday to the actual hourly PSDh.

## Final Remarks
In development. Next steps:
* Proper documentation;
* Proper usage of the English language;
* Implementing the manual adjustments on the actual code;
* Estimating probability distributions for the differences between the weekly PSD, issued on Friday, and its hourly counter-part (issued on D-1 basis);
* Pricing of contigency claims whose "underlying asset" is the difference beetween the weekly PSD and PSDh;
* Modeling those stochastic processes as SDEs.