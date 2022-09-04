# The prognostic value of machine learning techniques versus cox regression model for chordoma.

Data preprocessing is handled by R code: [DataPreprocessing.R](DataPreprocessing/DataPreprocessing.R)

Model construction, hyperparameters tuning and evaluation are handled with [pysurvival](https://github.com/square/pysurvival), [scikit-learn](https://github.com/scikit-learn/scikit-learn) and [lifelines](https://github.com/CamDavidsonPilon/lifelines) packages: [ModelDevelopment.ipynb](ModelDevelopment/ModelDevelopment.ipynb)

Web application based on [streamlit](https://github.com/streamlit/streamlit) package: [app.py](app.py)

The original data read in R code is not provided in this repository and needs to be extracted in the [SEER](https://seer.cancer.gov/) database according to inclusion criteria (AYA site recode 2020 Revision = 4.13 Chordoma)



[Online web application](https://share.streamlit.io/whuh-ml/chondrosarcoma/Predict/app.py)

[Paper link](https://pubmed.ncbi.nlm.nih.gov/)(To be updated)
