
{
  library(tidyverse)
  library(skimr)
  library(countrycode)
  library(modelStudio)
  library(DALEX)
  library(tidymodels)
  library(xgboost)
}


# LOAD AND READ DATASET ---------------------------------------------------


cholera_reported_cases <- read.csv("C:/Users/PC-005/Downloads/Reported-Cases-Till-2016.csv")
cholera_reported_cases%>%
  glimpse()%>%
  skim()


cholera_death_cases <- read.csv("C:/Users/PC-005/Downloads/Number-of-Deaths-Till-2016.csv")
cholera_death_cases%>%
  glimpse()%>%
  skim()

# CONVERT CHARACTER COLUMN WITH REPORTED/DEATH CASES TO NUMERIC USING FIX FUNCTION --------


fix(cholera_reported_cases)

restructure_cholera_reportedcases <- fix(cholera_reported_cases)
restructure_cholera_reportedcases%>%
  glimpse()%>%
  skim()


fix(cholera_death_cases)

restructure_cholera_deathcases <- fix(cholera_death_cases)
restructure_cholera_deathcases%>%
  glimpse()%>%
  skim()





cholera_combine_dataset <- merge(restructure_cholera_reportedcases,restructure_cholera_deathcases)
cholera_combine_dataset%>%
  drop_na()%>%
  glimpse()%>%
  skim()



# CREATE AN XGBOOST MODEL -------------------------------------------------

cholera_xgboost_model <- boost_tree(learn_rate = 0.3) %>%
  set_mode("regression") %>%
  set_engine("xgboost") %>%
  fit(Number.of.reported.deaths.from.cholera ~ ., data = cholera_combine_dataset )



cholera_xgboost_model



# CHOLERA EXPLAINER -------------------------------------------------------

cholera_explainer <- DALEX::explain(
  model = cholera_xgboost_model,
  data  = cholera_combine_dataset ,
  y     = cholera_combine_dataset$Number.of.reported.deaths.from.cholera,
  label = "XGBoost Model for Cholera"
)





# RUN THE MODELSTUDIO FOR  CHOLERA DATASET --------------------------------

modelStudio::modelStudio(cholera_explainer)


