# Ryan Hübert and Ryan Copus
# Political Appointments and Outcomes in Federal District Courts

from datetime import datetime
import pandas as pd
import h2o
from h2o.estimators.random_forest import H2ORandomForestEstimator
from h2o.estimators.glm import H2OGeneralizedLinearEstimator
import time

## Define working directory
## Note: you should paste the full path to your local working directory
root = ""

## Root directory must have a trailing slash
if root[-1] != "/":
    root = root + "/"

################################################################################
## Section 3.1: Balance test for main district court analysis
################################################################################

## Define filepaths to save prediction data generated below
path_roc = root + "Analysis/usdc_balance_roc.csv"
path_perf = root + "Analysis/usdc_balance_performance.csv"
path_pred = root + "Analysis/usdc_balance_predictions.csv"

## Import data and drop observations not in main analysis
df = pd.read_csv(root + "Analysis/USDC-DATASET-CLEANED.csv", dtype = object)
df = df.loc[df["main_subset"] == "1", :]

## Define sets of pretreatment predictors
pvars1 = ["block", "nature_of_suit", "SECTION", "ORIGIN", "JURIS", "jury_demand",
          "PROSE", "COUNTY"]
pvars2 = ["pla_count", "pla_count_prose", "pla_count_anonymous", "pla_count_IND",
          "pla_count_BUS", "pla_count_GOV", "pla_count_LAW", "pla_count_OTH",
          "pla_count_repeat", "pla_acount", "pla_acount_repeat", "def_count",
          "def_count_prose", "def_count_anonymous", "def_count_IND",
          "def_count_BUS", "def_count_GOV", "def_count_LAW", "def_count_OTH",
          "def_count_repeat", "def_acount", "def_acount_repeat", "oth_count",
          "oth_count_prose", "oth_count_anonymous", "oth_count_IND",
          "oth_count_BUS", "oth_count_GOV", "oth_count_LAW", "oth_count_OTH",
          "oth_count_repeat", "oth_acount", "oth_acount_repeat"]

## Drop some unused variables to free up memory
df = df.loc[:, ["file", "jrepublican"] + [x for x in pvars1 + pvars2]]

## For randomization check, we want to estimate two models, a benchmark model
## and a full pretreatment model. See paper for details.
models = ["bench", "full"]

## Create dataframes to store ROC curve data, performance data and predictions
roc = pd.DataFrame({"model": [], "algorithm": [], "fpr": [], "tpr": []})
perfs = pd.DataFrame({"model": [], "algorithm": [], "mse_tf_cv": [],
                      "auc_tf_cv": [], "mse_vf": [], "auc_vf": [], "obs_tf": [], "obs_vf": []})
preds = pd.DataFrame({"model": [], "file": [], "outcome": [],
                      "my_rf": [], "my_lasso": [], "my_ols": [], "my_ensemble": []})

try:
    ## Initialize the cluster and convert data into h2o format
    h2o.init(max_mem_size="32G")
    hf = h2o.H2OFrame(df)
    hf["jrepublican"] = hf["jrepublican"].asfactor()

    for model in models:

        ## Create empty dataframes to collect cross-validation holdout
        ## predicions and out of sample predictions
        pr0 = pd.DataFrame(df.loc[:, ["file", "jrepublican"]])
        pr1 = pd.DataFrame(df.loc[:, ["file", "jrepublican"]])
        pr0 = pr0.reset_index().loc[:, ["file", "jrepublican"]]
        pr1 = pr1.reset_index().loc[:, ["file", "jrepublican"]]

        tf = hf
        vf = hf

        if model == "bench":
            predictors = ["block"]
            predictors = [x for x in predictors if x in hf.columns]
            toPrint = " Estimating benchmark model for USDC randomization check. "
        else:
            predictors = pvars1 + pvars2
            predictors = [x for x in predictors if x in hf.columns]
            toPrint = " Estimating full model for USDC randomization check. "

        # Print some useful information
        print("\n\n" +
              "".join(["#"] * len(toPrint)) +
              "\n" +
              toPrint + "\n " + str(datetime.now())[0:16] + "\n" +
              "".join(["#"] * len(toPrint)) +
              "\n\n")

        print("\nEstimating the base learners ...\n")

        ## Train and cross validate a random forest
        my_rf = H2ORandomForestEstimator(model_id = "_".join(["jrepublican", str(model), "rf"]),
                                         nfolds=10,
                                         fold_assignment="Modulo",
                                         keep_cross_validation_predictions=True,
                                         ntrees=1000,
                                         seed=2020)
        my_rf.train(x = predictors, y = "jrepublican", training_frame = tf)
        pr0["my_rf"] = h2o.as_list(my_rf.cross_validation_holdout_predictions())["p1"]
        pr1["my_rf"] = h2o.as_list(my_rf.predict(vf))["p1"]

        ## Train and cross validate a LASSO regression
        my_lasso = H2OGeneralizedLinearEstimator(model_id = "_".join(["jrepublican", str(model), "lasso"]),
                                                 nfolds=10,
                                                 fold_assignment="Modulo",
                                                 keep_cross_validation_predictions=True,
                                                 family="binomial",  # for logistic regression
                                                 alpha=1,  # for LASSO
                                                 nlambdas=100,
                                                 seed=2020)
        my_lasso.train(x = predictors, y = "jrepublican", training_frame = tf)
        pr0["my_lasso"] = h2o.as_list(my_lasso.cross_validation_holdout_predictions())["p1"]
        pr1["my_lasso"] = h2o.as_list(my_lasso.predict(vf))["p1"]

        ## Train and cross validate an OLS regression
        my_ols = H2OGeneralizedLinearEstimator(family="binomial",
                                               lambda_=0,
                                               standardize=False,
                                               nfolds=10,
                                               keep_cross_validation_predictions=True,
                                               seed=2020)
        my_ols.train(x = predictors, y = "jrepublican", training_frame = tf)
        pr0["my_ols"] = h2o.as_list(my_ols.cross_validation_holdout_predictions())["p1"]
        pr1["my_ols"] = h2o.as_list(my_ols.predict(vf))["p1"]

        print("\nEstimating the ensemble ...\n")

        ## Train and cross validate an ensemble of the base learners
        enspredicts = [x for x in pr0.columns if "my_" in x and "my_ensemble" not in x]

        hr0 = h2o.H2OFrame(pr0)
        hr1 = h2o.H2OFrame(pr1)

        hr0["jrepublican"] = hr0["jrepublican"].asfactor()
        hr1["jrepublican"] = hr1["jrepublican"].asfactor()

        my_ensemble = H2OGeneralizedLinearEstimator(family="binomial",
                                                    lambda_=0,
                                                    standardize=False,
                                                    non_negative=True,
                                                    nfolds=10,
                                                    keep_cross_validation_predictions=True,
                                                    seed=2020)
        my_ensemble.train(x = enspredicts, y = "jrepublican", training_frame = hr0)

        pr0["my_ensemble"] = h2o.as_list(my_ensemble.cross_validation_holdout_predictions())["p1"]
        pr1["my_ensemble"] = h2o.as_list(my_ensemble.predict(hr1))["p1"]

        ## Extract and save various performance statistics for base and ensemble
        for q in enspredicts + ["my_ensemble"]:
            if q in enspredicts:
                perf = eval(q).model_performance(test_data=vf)
            else:
                perf = eval(q).model_performance(test_data=hr1)

            row = {}
            row["model"] = model
            row["algorithm"] = q
            row["mse_tf_cv"] = round(eval(q).mse(xval=True), 6)
            row["auc_tf_cv"] = round(eval(q).auc(xval=True), 6)
            row["mse_vf"] = round(perf.mse(), 6)
            row["auc_vf"] = round(perf.auc(), 6)
            row["obs_tf"] = len(tf)
            row["obs_vf"] = len(vf)

            perfs = perfs.append(row, ignore_index=True)

            perf = eval(q).model_performance(xval=True)
            roc0 = pd.DataFrame({"fpr": perf.fprs, "tpr": perf.tprs})
            roc0["model"] = model
            roc0["algorithm"] = q
            roc0 = roc0.loc[:,[x for x in roc.columns]]

            roc = roc.append(roc0, ignore_index=True)

        ## Save the predictions
        pr0["model"] = model
        pr0["outcome"] = pr0["jrepublican"]
        pr0 = pr0.loc[:,[x for x in preds.columns]]
        preds = preds.append(pr0)

        ## Save the ROC data, performance statistics and predictions
        roc.to_csv(path_or_buf=path_roc, mode="w", index=False, header=True)
        perfs.to_csv(path_or_buf=path_perf, mode="w", index=False, header=True)
        preds.to_csv(path_or_buf=path_pred, mode="w", index=False, header=True)

        del tf, vf, hr0, hr1, pr0, pr1

    print("FINISHED RUNNING MODELS")

    h2o.cluster().shutdown()

except:
    print("ERROR RUNNING MODELS")
    raise

del df, hf, model, models

time.sleep(300)

################################################################################
## Section 4.1: Balance test and predictions for appeals court data
################################################################################

## This completes a similar process as above

## Define filepaths to save prediction data generated below
path_roc = root + "Analysis/usca_balance_roc.csv"
path_perf = root + "Analysis/usca_balance_performance.csv"
path_pred = root + "Analysis/usca_balance_predictions.csv"

df = pd.read_csv(root + "USDC-APPEALS-JOP.csv", dtype = "object")

vars_cat = ["panel_anon", "CIRDIST", "YEAR", "APPTYPE", "NOS", "JURIS", "USAPT",
            "USAPE", "DDIST", "DOFFICE", "dORIGIN", "dSECTION",
            "dJURY", "dCOUNTY","dPROSE", "dTRCLACT", "dPROCPROG", "dNOJ",
            "dJUDGMENT", "dDISP", "djudge_race", "djudge_gender",
            "djudge_party", "djudge_birth_year", "djudge_senior", "djudge_president"]
vars_int = ["pro_defendant", "maj_rep", "aple_count", "aplt_count", "aple_att_count",
            "aplt_att_count", "aplt_deft", "aplt_pltf", "apltprose", "apleprose",
            "bothprose", "ftnoa", "dCLASSACT", "dtrmfee", "dpltres", "ddefres"]

for i in vars_int:
    df[i] = df[i].astype(float).astype(int)
for i in vars_cat:
    df[i] = df[i].astype("category")

df["block"] = df["CIRDIST"].astype("object") + df["YEAR"].astype("object")
df["block"] = df["block"].astype("category")
pvars = vars_cat[3:] + vars_int[2:]

# Initialize the cluster and convert data into h2o format
h2o.init(max_mem_size="32G")

# Blank dataframes
roc = pd.DataFrame({"subset": [], "model": [], "algorithm": [],
                    "fpr": [], "tpr": []})
perfs = pd.DataFrame({"subset": [], "model": [], "algorithm": [],
                      "mse_tf_cv": [], "auc_tf_cv": [], "mse_vf": [],
                      "auc_vf": [], "obs_tf": [], "obs_vf": []})
preds = pd.DataFrame({"subset": [], "model": [], "case_id": [],
                      "outcome": [], "my_rf": [], "my_lasso": [],
                      "my_ols": [], "my_ensemble": []})

try:
    for outcome in ["pro_defendant", "maj_rep"]:

        if "maj_rep" != outcome:
            models = ["0", "1"]
        else:
            models = ["bench", "full"]

        hf = h2o.H2OFrame(df)
        hf[outcome] = hf[outcome].asfactor()
        hf["maj_rep"] = hf["maj_rep"].asfactor()

        for model in models:
            # model = "0"
            if model == "bench":
                y = "maj_rep"
                pr0 = pd.DataFrame(df.loc[:, ["case_id", y]])
                pr1 = pd.DataFrame(df.loc[:, ["case_id", y]])
                pr0 = pr0.reset_index().loc[:, ["case_id", y]]
                pr1 = pr1.reset_index().loc[:, ["case_id", y]]

                tf = hf
                vf = hf

                predictors = ["block"]
                predictors = [x for x in predictors if x in hf.columns]

                toPrint = " Working on randomization check for " + y + " (benchmark model) "

            elif model == "full":
                y = "maj_rep"
                pr0 = pd.DataFrame(df.loc[:, ["case_id", y]])
                pr1 = pd.DataFrame(df.loc[:, ["case_id", y]])
                pr0 = pr0.reset_index().loc[:,["case_id", y]]
                pr1 = pr1.reset_index().loc[:, ["case_id", y]]

                tf = hf
                vf = hf

                predictors = ["block"] + pvars
                predictors = [x for x in predictors if x in hf.columns]

                toPrint = " Working on randomization check for " + y + " (full pretreatment model) "

            else:
                y = outcome
                pr0 = pd.DataFrame(df.loc[df["maj_rep"] == int(model), ["case_id", y]])
                pr1 = pd.DataFrame(df.loc[df["maj_rep"] != int(model), ["case_id", y]])
                pr0 = pr0.reset_index().loc[:, ["case_id", y]]
                pr1 = pr1.reset_index().loc[:, ["case_id", y]]

                tf = hf[hf["maj_rep"] == model, :]
                vf = hf[hf["maj_rep"] != model, :]

                predictors = ["block"] + pvars
                predictors = [x for x in predictors if x in hf.columns]

                toPrint = " Working on " + y + " predictions " + "(" + "maj_rep" + " = " + model +  ")"

            # Print some useful information
            print("\n\n" +
                  "".join(["#"] * len(toPrint)) +
                  "\n" +
                  toPrint + "\n " + str(datetime.now())[0:16] + "\n" +
                  "".join(["#"] * len(toPrint)) +
                  "\n\n")

            print("\nEstimating the base learners ...\n")

            my_rf = H2ORandomForestEstimator(model_id = "_".join([y, str(model), "rf"]),
                                             nfolds=10,
                                             fold_assignment="Modulo",
                                             keep_cross_validation_predictions=True,
                                             ntrees=1000,
                                             seed=2020)
            my_rf.train(x=predictors, y=y, training_frame=tf)
            pr0["my_rf"] = h2o.as_list(my_rf.cross_validation_holdout_predictions())["p1"]
            pr1["my_rf"] = h2o.as_list(my_rf.predict(vf))["p1"]

            # Train & Cross-validate a LASSO Regression
            my_lasso = H2OGeneralizedLinearEstimator(model_id = "_".join([y, str(model), "lasso"]),
                                                     nfolds=10,
                                                     fold_assignment="Modulo",
                                                     keep_cross_validation_predictions=True,
                                                     family="binomial",  # for logistic regression
                                                     alpha=1,  # for LASSO
                                                     nlambdas=100,  # from SL.glmnet(?)
                                                     seed=2020)
            my_lasso.train(x = predictors, y = y, training_frame = tf)
            pr0["my_lasso"] = h2o.as_list(my_lasso.cross_validation_holdout_predictions())["p1"]
            pr1["my_lasso"] = h2o.as_list(my_lasso.predict(vf))["p1"]

            # Train & Cross-validate OLS Regression
            my_ols = H2OGeneralizedLinearEstimator(family="binomial",
                                                   lambda_=0,
                                                   standardize=False,
                                                   nfolds=10,
                                                   keep_cross_validation_predictions=True,
                                                   seed=2020)
            my_ols.train(x = predictors, y = y, training_frame = tf)
            pr0["my_ols"] = h2o.as_list(my_ols.cross_validation_holdout_predictions())["p1"]
            pr1["my_ols"] = h2o.as_list(my_ols.predict(vf))["p1"]

            print("\nEstimating the ensemble ...\n")

            # Do the ensemble
            enspredicts = [x for x in pr0.columns if "my_" in x and "my_ensemble" not in x]

            hr0 = h2o.H2OFrame(pr0)
            hr1 = h2o.H2OFrame(pr1)

            hr0[y] = hr0[y].asfactor()
            hr1[y] = hr1[y].asfactor()

            my_ensemble = H2OGeneralizedLinearEstimator(family="binomial",
                                                        lambda_=0,
                                                        standardize=False,
                                                        non_negative=True,
                                                        nfolds=10,
                                                        keep_cross_validation_predictions=True,
                                                        seed=2020)
            my_ensemble.train(enspredicts, y=y, training_frame=hr0)

            pr0["my_ensemble"] = h2o.as_list(my_ensemble.cross_validation_holdout_predictions())["p1"]
            pr1["my_ensemble"] = h2o.as_list(my_ensemble.predict(hr1))["p1"]

            # Look at the performance statistics
            pcols = [x for x in perfs.columns]

            for q in enspredicts + ["my_ensemble"]:
                if q in enspredicts:
                    perf = eval(q).model_performance(test_data=vf)
                else:
                    perf = eval(q).model_performance(test_data=hr1)

                row = {}
                row["subset"] = "maj_rep" + "-" + outcome
                row["model"] = model
                row["algorithm"] = q
                row["mse_tf_cv"] = round(eval(q).mse(xval=True), 6)
                row["auc_tf_cv"] = round(eval(q).auc(xval=True), 6)
                row["mse_vf"] = round(perf.mse(), 6)
                row["auc_vf"] = round(perf.auc(), 6)
                row["obs_tf"] = len(tf)
                row["obs_vf"] = len(vf)

                perfs = perfs.append(row, ignore_index=True)

                perf = eval(q).model_performance(xval=True)
                roc0 = pd.DataFrame({"fpr": perf.fprs, "tpr": perf.tprs})
                roc0["model"] = model
                roc0["algorithm"] = q
                roc0["subset"] = "maj_rep" + "-" + outcome
                roc0 = roc0.loc[:,[x for x in roc.columns]]

                roc = roc.append(roc0, ignore_index=True)

            # Save the predictions
            if model in ["0", "1"]:
                pr0 = pr0.append(pr1)
            pr0["model"] = model
            pr0["subset"] = "maj_rep" + "-" + outcome
            pr0["outcome"] = pr0[y]
            pr0 = pr0.loc[:,[x for x in preds.columns]]
            preds = preds.append(pr0)

            # Save the ROC data, performance statistics and predictions
            roc.to_csv(path_or_buf=path_roc, mode="w", index=False, header=True)
            perfs.to_csv(path_or_buf=path_perf, mode="w", index=False, header=True)
            preds.to_csv(path_or_buf=path_pred, mode="w", index=False, header=True)

            del tf, vf, hr0, hr1, pr0, pr1

    print("FINISHED RUNNING MODELS")
    h2o.cluster().shutdown()

except:
    print("ERROR RUNNING MODELS")
    raise
