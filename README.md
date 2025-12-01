# Knight Lab: Local News Lens

Welcome! We are a group of journalism students at Northwestern University, and we completed this project as part of the [Knight Lab](https://knightlab.northwestern.edu/) at the Medill School of Journalism.

The authors of this repo are Jillian Moore, Eunice Lee, Sophia Zhang, Melissa Dai and Keya Chaudhuri. This is a prototype completed for [Block Club Chicago](https://blockclubchicago.org/) under the supervision of Professor Jill Blackman.

This repo is now maintained by the Knight Lab. We encourage adaptation and further development of this prototype for journalistic publications. For questions, please call (847) 467-4971.

### What's in the repo

#### Sub-folders/directories

-   [data/](./data/): contains all data for this project; note that api_scrape.rds can be obtained by running `1_data_clean.R` and ai_check_data.R can be obtained by running `3_ai_check.R`. We did not commit these files to the repo as they are over 100 MB.
-   [ai_check_data/](./ai_check_data/): contains the randomly assigned and completed CSV files for analysis of AI and human alignment on topic tagging. Please run `4_ai_check.R` to see data visualizations and printed output of our results.
-   [modules/](./modules/): contains each tab of the Shiny dashboard coded in a module. Please run `1_data_clean` to load the appropriate data into your environment, and then run `app.R` to quickly load and view the dashboard.
-   [www/](./www/): contains images rendered in our Shiny dashboard.
-   [reference/](./reference/): contains R scripts which we abandoned as we created additional iterations of our project. However, we believe these scripts could be a helpful starting point for replication of this prototype for publications with different structures.
-   [rsconnect/](./rsconnect/): contains generated file to make our dashboard URL-accessible. 

#### R scripts

-   `1_api_scrape`: contains a function to scrape the REST API of Block Club Chicago.
-   `2_data_clean.R`: contains detailed code to clean, merge and save out the API scrape, census, and geo data.
-   `3_ai_tags.R`: contains code to run initial large dataset through open LLM key; automation for dataset updates can be done with free LLMs.
-   `4_ai_check.R`: contains analysis of the [ai_check_data/](./ai_check_data/) experiment, where we each independently selected topics for randomly assigned articles to compare human and AI judgement.
-   `5_local_hosting.R`: contains code to deploy dashboard to URL through shinyapps.io.
-   `app.R`: contains code to run the Shiny dashboard.

#### Final Artifact

-   `final_artifact.qmd`: QMD file for creating final artifact
-   `final_artifact.html`: rendered html version of final artifact
