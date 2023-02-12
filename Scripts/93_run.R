library(tidyverse)
library(gagglr)
library(glue)
library(here)
library(quarto)

get_metadata(type = "PSNU_IM")

vct_cntry <- pepfar_country_list %>%
  pull(country)

vct_cntry[1:2]

#output files
reports <- tibble(
  output_file = glue(here("markdown","{metadata$curr_pd}_{vct_cntry}_cop-support-viz_oha-siei.pptx")),
  params = map(vct_cntry, ~list(curr_pd = metadata$curr_pd, cntry = ., agency = "PEPFAR"))
)

#create reports
reports %>%
  pwalk(quarto_render,
        input = here("Scripts","reports.Qmd"))
