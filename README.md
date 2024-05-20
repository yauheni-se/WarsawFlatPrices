<div align="center">
  <img src="https://github.com/yauheni-se/WarsawFlatPrices/assets/84158821/5b8573d1-fea2-4fd1-96f0-8d6e2f0e54e7" alt="" width="100"/>
</div>

# ©️ Tags
- Models: Random Forests, ExtraTrees, Adaboost, gradient boosting, xgboost, LightGBM, catboost, NN, ANN, MLP, FFN, GWANN, SAR, SEM, SLX, SARAR, SDM, SDEM, GNS, GWR
- Areas: Machine Learning, Deep Learning, Spatial Econometrics, Spatial Statistics, Data Mining, Bayesian Inference, real estate, regression, prediction, genetic algorithm

# :bulb: About
The project evaluates the predictive accuracy of spatial models compared to Machine Learning & Deep Learning models, using the Warsaw real estate market as a case study.

# :open_file_folder: Content
- [research](https://github.com/yauheni-se/WarsawFlatPrices/blob/main/research.pdf) – main file containing the comprehensive research ✍️
- [data_preparation](https://github.com/yauheni-se/WarsawFlatPrices/tree/main/1.%20data_preparation) – Data Mining scripts and initial data processing ♻️
- [eda](https://github.com/yauheni-se/WarsawFlatPrices/tree/main/2.%20eda) – folder containing Exploratory Data Analysis steps where main insights were discovered 💭
- [modeling](https://github.com/yauheni-se/WarsawFlatPrices/tree/main/3.%20modelling) – final scripts where spatial, ML, and DL models were trained and compared 🏋️
- [data](https://github.com/yauheni-se/WarsawFlatPrices/tree/main/data) – all the data used within different scripts 🗃️
- [plots](https://github.com/yauheni-se/WarsawFlatPrices/tree/main/plots) – extensive gallery of plots 🖼️

# :test_tube: Methodology
- Dataset was collected by mining and combining [OpenStreetMap](https://www.openstreetmap.org/#map=11/52.2333/21.0615) & [otodom](https://www.otodom.pl/) data.
- Hyperparameter tuning was performed by applying the genetic algorithm.
- The comparative criteria for evaluating prediction quality included MSE, RMSE, MPE, MAPE, and sMAPE errors.
- Residual plots, coefficient interpretation, variable significance assessment, and computational time representation enhanced evaluation comprehensiveness.

# 🏆 Findings

<div align="center">

| Model             | MSE     | RMSE   | MPE     | MAPE   | sMAPE  | Time (min) |
|-------------------|---------|--------|---------|--------|--------|------------|
| adaboost          | 0.065   | 0.254  | -0.011  | 1.916  | 1.911  | 7.450      |
| bGNS              | 0.023   | 0.152  | 0.064   | 1.187  | 1.189  | 3.217      |
| bSAR              | 0.326   | 0.571  | 5.810   | 5.816  | 6.007  | 0.100      |
| bSAR (pure)       | 0.053   | 0.231  | 0.058   | 1.838  | 1.842  | 0.070      |
| bSARAR            | 0.026   | 0.160  | -0.013  | 1.284  | 1.284  | 2.357      |
| bSDEM             | 0.023   | 0.152  | -0.008  | 1.185  | 1.185  | 3.150      |
| bSDM              | 29.236  | 5.407  | -56.543 | 56.543 | 44.017 | 2.633      |
| bSEM              | 0.054   | 0.231  | -0.063  | 1.835  | 1.836  | 0.650      |
| bSLX              | 0.023   | 0.153  | -0.053  | 1.196  | 1.196  | 0.033      |
| catboost          | 0.030   | 0.174  | -0.034  | 1.139  | 1.132  | 0.917      |
| ExtraTrees        | 0.031   | 0.175  | -0.042  | 1.149  | 1.142  | 1.467      |
| GNS               | 0.022   | 0.148  | 0.023   | 1.149  | 1.150  | 3.200      |
| gradient boosting | 0.035   | 0.186  | -0.033  | 1.222  | 1.216  | 17.217     |
| GWANN             | 0.206   | 0.454  | -0.170  | 3.657  | 3.645  | 834.233    |
| GWR               | 0.016   | 0.126  | -0.018  | 1.032  | 1.032  | 2.698      |
| LGBM              | 0.030   | 0.174  | -0.023  | 1.148  | 1.141  | 0.100      |
| LM                | 0.026   | 0.161  | -0.028  | 1.266  | 1.266  | 0.004      |
| Mean              | 0.073   | 0.270  | -0.081  | 2.201  | 2.199  | 0.001      |
| Median            | 0.073   | 0.271  | 0.016   | 2.210  | 2.210  | 0.001      |
| MLP               | 168.945 | 12.998 | 38.787  | 87.367 | 78.930 | 2.433      |
| Random Forests    | 0.032   | 0.179  | -0.043  | 1.159  | 1.152  | 11.917     |
| SAR               | 0.187   | 0.433  | 0.049   | 3.962  | 3.963  | 1.733      |
| SAR (pure)        | 7.520   | 2.742  | 28.616  | 28.616 | 33.441 | 1.734      |
| SARAR             | 0.023   | 0.151  | -0.023  | 1.175  | 1.175  | 3.317      |
| SDEM              | 0.022   | 0.149  | -0.030  | 1.163  | 1.162  | 1.861      |
| SDM               | 0.022   | 0.147  | -0.024  | 1.143  | 1.143  | 2.033      |
| SEM               | 0.027   | 0.164  | 0.019   | 1.294  | 1.295  | 1.688      |
| SLX               | 0.101   | 0.318  | -0.096  | 2.694  | 2.693  | 1.291      |
| xgboost           | 0.032   | 0.179  | -0.033  | 1.157  | 1.150  | 1.133      |

</div>

- Concerning most selected metrics (except for computation time and MPE), Geographically Weighted Regression (GWR) proved the best performance for predicting apartment prices 💰


<div align="center">
  <img src="https://github.com/yauheni-se/WarsawFlatPrices/assets/84158821/934b7524-ec14-4cd1-b461-1f56b961424a" alt=""/>
</div>

- No clusters of residuals were found for the GWR model, except for the Powiśle area, which signifies decent coverage of spatial correlation of apartment prices in Warsaw 🗺️

<div align="center">
  <img src="https://github.com/yauheni-se/WarsawFlatPrices/assets/84158821/f9068732-3b6a-43ba-b2d6-f40917ced0d7" alt="" width="100"/>
</div>

- Conclusions from graphical analysis and error metrics recognize GWR model as the best predictor.
- Generally, spatial models tend to outperform both ensemble learning algorithms like LGBM and catboost, as well as neural networks and GWANN.
