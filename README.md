# oneFootball Coding assignment


In this assignment we've worked on user churn from a subset of data from onefootball.

The project is organized as:

* data
    + `user_activity.csv`
    + `user_classified.csv`
* analysis
    + `analysis.R`
    + `assignment.Rmd`
    + `assignment.html` (html output)
* `Dockerfile`
* `README.md`
* `run-notebook.sh`

The assignment is explained in `assignment.Rmd` and the code used to classified users is in `analysis.R`. The oneFootball data used is in `user_activity.csv` and the classification is in `user_classified.csv`. The later contains two columns:

* `user_id`
* `churn_may_estimate` <- TRUE for predicted churn users, FALSE for predicted returning users


To reproduce the results using Docker first build the image:

```bash
docker build -t onefootball .
```

Then run it:

```bash
docker run -e PASSWORD=onefoot -p 8787:8787 onefootball
```

You can now connect to http://localhost:8787/ where you'll be greeted by the Rstudio login page. Use `rstudio` as the username and `onefoot` as the password.


