<!---
---
output:
  pdf_document: default
urlcolor: magenta
---
--->

# MARS APP ENGAGEMENT PIPELINE & ANALYSIS

## About this Repository

This repository contains a data pipeline that produces a core dataset that may be used for many types of subsequent data analyses relating to the Mobile Assistance for Regulating Smoking (MARS) Micro-Randomized Trial (MRT). In addition, it contains an additional pipeline that curates engagement variables on top of the core dataset.

## Protocol for the MARS MRT

Nahum-Shani, I., Potter, L. N., Lam, C. Y., Yap, J., Moreno, A., Stoffel, R., ... & Wetter, D. W. (2021). The mobile assistance for regulating smoking (MARS) micro-randomized trial design protocol. Contemporary clinical trials, 110, 106513.

## Data Pipeline

`pipeline-clean-data.R` is a script that documents the sequence in which each step is executed in the data pipeline.

## App Engagement Pipeline

`curate_app_usage_pipeline_gitrepo.R` is a script that curates and processes engagement indicators based on the app log data.

