% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/risk-diet-exercise.R
\name{risk_diet_exercise}
\alias{risk_diet_exercise}
\title{Access to Diet & Exercise Screening Data}
\usage{
risk_diet_exercise(diet_exercise, race, sex)
}
\arguments{
\item{diet_exercise}{One of the following values:
\itemize{
\item \code{"bmi is healthy, ages 20+"}
\item \code{"bmi is obese, ages 20+"}
\item \code{"bmi is obese, high school survey"}
\item \code{"bmi is overweight, high school survey"}
\item \code{"consumed 1 or more fruits per day"}
\item \code{"consumed 1 or more vegetables per day"}
\item \code{"no leisure time physical activity"}.
}}

\item{race}{One of the following values:
\itemize{
\item \code{"All Races (includes Hispanic)"}
\item \code{"White (non-Hispanic)"}
\item \code{"Black (non-Hispanic)"}
\item \code{"American Indian / Alaska Native (non-Hispanic)"}
\item \code{"Asian / Pacific Islander (non-Hispanic)"}
\item \code{"Hispanic (Any Race)"}.
}}

\item{sex}{One of the following values:
\itemize{
\item \code{"both sexes"}
\item \code{"male"}
\item \code{"female"}.
}}
}
\value{
A data frame with the following columns:
Area Type, Area Code, Percent,
Lower 95\% CI, Upper 95\% CI, Number of Respondents.
}
\description{
This function returns a data frame about diet and exercise risk
from State Cancer Profiles.
}
\examples{
\dontrun{
risk_diet_exercise(
  diet_exercise = "bmi is healthy, ages 20+",
  race = "all races (includes hispanic)",
  sex = "both sexes"
)
risk_diet_exercise(
  diet_exercise = "bmi is obese, high school survey",
  race = "all races (includes hispanic)",
  sex = "males"
)
}
}
\seealso{
Other risks: 
\code{\link{risk_alcohol}()},
\code{\link{risk_colorectal_screening}()},
\code{\link{risk_smoking}()},
\code{\link{risk_vaccines}()},
\code{\link{risk_women_health}()}
}
\concept{risks}
