# Ryan Hübert and Ryan Copus
# Political Appointments and Outcomes in Federal District Courts
# January 2021

import numpy as np
import pandas as pd

## Define working directory
## Note: you should paste the full path to your local working directory
root = ""

## Root directory must have a trailing slash
if root[-1] != "/":
    root = root + "/"

################################################################################
## Define functions to calculate IPW, AIPW, and bootstrapped SEs
################################################################################

def IPW(datafr, out, trt, unadjusted = False, augmented = False, unit_wt = None):
    if any(x not in datafr.columns for x in ["propensity","pred0","pred1"]):
        return None
    if unit_wt is None:
        wt = [1] * len(datafr)
    else:
        wt = datafr[unit_wt]

    # Unadjusted difference in means estimator
    if unadjusted is True:
        col1 = (wt * datafr[trt] * datafr[out]).sum()
        col2 = (wt * (1 - datafr[trt]) * datafr[out]).sum()
        col1b = (wt * datafr[trt]).sum()
        col2b = (wt * (1 - datafr[trt])).sum()
        if col1b > 0 and col2b > 0:
            est = (1 / col1b) * col1 - (1 / col2b) * col2
        else:
            est = None

    # IPW Estimator with renormalized weights, p. 39 of Glynn and Quinn (2010)
    elif augmented is False:
        col1 = ((wt * datafr[trt] * datafr[out]) / datafr["propensity"]).sum()
        col2 = ((wt * (1 - datafr[trt]) * datafr[out]) / (1 - datafr["propensity"])).sum()
        col1b = ((wt * datafr[trt]) / datafr["propensity"]).sum()
        col2b = ((wt * (1 - datafr[trt])) / (1 - datafr["propensity"])).sum()
        if col1b > 0 and col2b > 0:
            est = (1 / col1b) * col1 - (1 / col2b) * col2
        else:
            est = None

    # AIPW Estimator, Equation 3 of Glynn and Quinn (2010)
    elif augmented is True:
        col1 = (datafr[trt] * datafr[out])/datafr["propensity"]
        col2 = ((1-datafr[trt]) * datafr[out])/(1-datafr["propensity"])
        col3 = (datafr[trt]-datafr["propensity"])/(datafr["propensity"]*(1-datafr["propensity"]))
        col4 = ((1-datafr["propensity"])*datafr["pred1"] + datafr["propensity"]*datafr["pred0"])
        est = (1/(sum(wt))) * (wt * (col1 - col2 - (col3 * col4))).sum()

    return est

def BootSEs(datafr, out, trt, clustervar, block = False, boots = 400, seed = 2020):
    # datafr = pd.DataFrame(tmp); out = y; trt = t; clustervar = "jid_anon"

    datafr["const"] = 1

    bf = pd.DataFrame(columns=["boot", "effect", "outcome", "treatment", "obs",
                               "obs_boot", "estimator", "effect_wt"])
    jwt = datafr.groupby(clustervar)["block"].count()
    jwt = jwt.reset_index()

    ## Set the seed
    rs = np.random.RandomState(np.random.MT19937(np.random.SeedSequence(seed)))

    print("     ... Doing the bootstrap ... ")
    N = len(datafr)
    fes = set(datafr["block"])
    for i in range(0, boots):
        print(i)
        datafr["bs"] = datafr[clustervar]
        bs = np.random.choice(jwt[clustervar], size=len(jwt), replace=True)
        bs = {x: len(bs[bs == x]) if x in bs else 0 for x in jwt[clustervar]}
        datafr["bs"] = datafr["bs"].map(bs)
        if block:
            est0 = []
            est1 = []
            est2 = []
            for b in fes:
                Nb = datafr.loc[datafr["block"] == b, "bs"].sum()
                est0.append((IPW(datafr.loc[datafr["block"] == b, :], out, trt,
                                 unadjusted=True, unit_wt="bs"), Nb))
                est1.append((IPW(datafr.loc[datafr["block"] == b, :], out, trt,
                                 augmented=False, unit_wt="bs"), Nb))
                est2.append((IPW(datafr.loc[datafr["block"] == b, :], out, trt,
                                 augmented=True, unit_wt="bs"), Nb))
            est0 = sum([(x[1] / datafr["bs"].sum()) * x[0] for x in est0 if not np.isnan(x[0])])
            est1 = sum([(x[1] / datafr["bs"].sum()) * x[0] for x in est1 if not np.isnan(x[0])])
            est2 = sum([(x[1] / datafr["bs"].sum()) * x[0] for x in est2 if not np.isnan(x[0])])
        else:
            est0 = IPW(datafr, out, trt, unadjusted=True, unit_wt="bs")
            est1 = IPW(datafr, out, trt, augmented=False, unit_wt="bs")
            est2 = IPW(datafr, out, trt, augmented=True, unit_wt="bs")

        row = [i, est0, out, trt, N, datafr["bs"].sum(), "naive", ""]
        bf = bf.append(pd.DataFrame([row], columns=bf.columns))
        row = [i, est1, out, trt, N, datafr["bs"].sum(), "ipw_rn", ""]
        bf = bf.append(pd.DataFrame([row], columns=bf.columns))
        row = [i, est2, out, trt, N, datafr["bs"].sum(), "aipw", ""]
        bf = bf.append(pd.DataFrame([row], columns=bf.columns))

    ## Reweight the bootstrap estimates as in Sherman and le Cessie (1997)
    bf["effect_wt"] = (bf["obs_boot"] / bf["obs"]) ** (1 / 2) * bf["effect"]

    return bf

################################################################################
## Estimate effects in Ninth Circuit
################################################################################

## Import appeals data

af = pd.read_csv(root + "USDC-APPEALS-JOP.csv", dtype = "object")
af = af.loc[:,["case_id", "pro_defendant", "maj_rep", "panel_anon", "CIRDIST", "YEAR"]]
af["block"] = af["CIRDIST"] + "_" + af["YEAR"]

## Import predictions
af1 = pd.read_csv(root + "Analysis/usca_balance_predictions.csv", dtype = "object")

## Merge appeals data and predictions
af = af.merge(af1.loc[af1["model"] == "full",["case_id","my_ensemble"]],
              how = "left", on = "case_id")
af = af.rename(columns = {"my_ensemble" : "propensity"})
af = af.merge(af1.loc[af1["model"] == "0",["case_id","my_ensemble"]],
              how = "left", on = "case_id")
af = af.rename(columns = {"my_ensemble" : "pred0"})
af = af.merge(af1.loc[af1["model"] == "1",["case_id","my_ensemble"]],
              how = "left", on = "case_id")
af = af.rename(columns = {"my_ensemble" : "pred1"})

## Clean up variables
for i in af.columns:
    if i in ["pro_defendant", "maj_rep"]:
        af[i] = af[i].astype(float).astype(int)
    if i in ["propensity","pred0","pred1"]:
        af[i] = af[i].astype(float)

## Check no NAs
c1 = len(af.loc[af["propensity"].isnull(),"case_id"])/len(af)
c2 = len(af.loc[af["pred1"].isnull(),"case_id"])/len(af)
c3 = len(af.loc[af["pred0"].isnull(),"case_id"])/len(af)

if c1 > 0 or c2 > 0 or c3 > 0:
    print("WARNING: MISSING VALUES")

af = af.rename(columns = {"case_id" : "file"}) # just to fix a bug
af["block_panel"] = af["block"]  + "_" + af["panel_anon"]

################################################################################
## Estimate effects in Ninth Circuit
################################################################################

## What is the mean of the outcome variable?
print("Mean of the outcome variable: " + str(round(af["pro_defendant"].mean(),2)))

## Bootstrap panel clustered standard errors
bf = BootSEs(af, "pro_defendant", "maj_rep", "panel_anon", block = False,
             boots = 1000, seed = 202007)

## Collect effects
cf = pd.DataFrame(columns=["effect", "se", "obs", "panels_treat",
                           "panels_control", "fe_count", "estimator"])

## Estimate unadjusted treatment effect
af1 = pd.concat([af.loc[:,["pro_defendant","maj_rep","block","panel_anon"]],
                 pd.get_dummies(af["block"])], axis=1)
af1 = af1.loc[:,af1.columns[3:]].mean().reset_index()
af1 = af1.rename(columns = {"index" : "block", 0 : "block_wt"})
af1["block_effect"] = pd.np.nan

for b in set(af["block"]):
    mt = af.loc[(af["block"] == b) & (af["maj_rep"] == 1), "pro_defendant"]
    mt = mt.mean()
    mc = af.loc[(af["block"] == b) & (af["maj_rep"] == 0), "pro_defendant"]
    mc = mc.mean()
    af1.loc[af1["block"] == b, "block_effect"] = mt - mc
est = round((af1["block_wt"] * af1["block_effect"]).sum(),4)
se = round(np.sqrt(bf.loc[bf["estimator"] == "naive","effect_wt"].var()),4)

toadd = [est, se, len(af),
         len(af.loc[af["maj_rep"]==1,:].groupby("panel_anon")["file"].count()),
         len(af.loc[af["maj_rep"]==0,:].groupby("panel_anon")["file"].count()),
         len(set(af["block"])),
         "unadjusted"]
toadd = {cf.columns[x] : [toadd[x]] for x in range(0,len(cf.columns))}

cf = cf.append(pd.DataFrame(toadd))

print("Unadjusted Treatment Effect: " + str(est))
print("Unadjusted Treatment Effect (Std. Error): " + str(se))

## Estimate IPW
est = round(IPW(af, "pro_defendant", "maj_rep", augmented=False),4)
se = round(np.sqrt(bf.loc[bf["estimator"] == "ipw_rn","effect_wt"].var()),4)

toadd = [est, se, len(af),
         len(af.loc[af["maj_rep"]==1,:].groupby("panel_anon")["file"].count()),
         len(af.loc[af["maj_rep"]==0,:].groupby("panel_anon")["file"].count()),
         len(set(af["block"])),
         "ipw"]
toadd = {cf.columns[x] : [toadd[x]] for x in range(0,len(cf.columns))}

cf = cf.append(pd.DataFrame(toadd))

print("IPW Treatment Effect: " + str(est))
print("IPW Treatment Effect (Std. Error): " + str(se))

## Estimate AIPW
est = round(IPW(af, "pro_defendant", "maj_rep", augmented=True),4)
se = round(np.sqrt(bf.loc[bf["estimator"] == "aipw","effect_wt"].var()),4)

toadd = [est, se, len(af),
         len(af.loc[af["maj_rep"]==1,:].groupby("panel_anon")["file"].count()),
         len(af.loc[af["maj_rep"]==0,:].groupby("panel_anon")["file"].count()),
         len(set(af["block"])),
         "aipw"]
toadd = {cf.columns[x] : [toadd[x]] for x in range(0,len(cf.columns))}

cf = cf.append(pd.DataFrame(toadd))

print("AIPW Treatment Effect: " + str(est))
print("AIPW Treatment Effect (Std. Error): " + str(se))

## Print additional data for the table.
print("Observations: " + str(len(af)))
print("Majority Republican Panels: " + str(len(af.loc[af["maj_rep"]==1,:].groupby("panel_anon")["file"].count())))
print("Majority Democratic Panels: " + str(len(af.loc[af["maj_rep"]==0,:].groupby("panel_anon")["file"].count())))
print("Division-Years: " + str(len(set(af["block"]))))

cf.to_csv(path_or_buf= root + "Analysis/effects_appeals.csv", mode="w", index=False, header=True)
