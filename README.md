# Data-mining-on-COVID-19-Data  
# The Whole Project is divided into 3 parts

This project focuses on analyzing COVID-19 data provided by Google Cloud Platform, which includes information on the spread of the virus, demographics, and social distancing efforts in the United States. The goal is to understand trends, assess the effectiveness of social distancing, and predict future developments in various regions.
We are using **CRISP-DM** model to do our Data Analysis, starting with the Business Understanding as well as  Data Understanding

## Key Objectives:
- Analyze the trends in different states/counties of the US.
- Investigate whether social distancing is working.
- Identify regions performing well in controlling the virus.
- Predict future developments in regions based on the data from other regions.

## Business Understanding
This involves defining the problem and understanding the significance of the data for COVID-19. Here's a draft that you can build upon:

**COVID-19 Overview:**

COVID-19, caused by the SARS-CoV-2 virus, is a global pandemic that has affected millions of people worldwide. It is essential to understand how the virus spreads, the number of cases, hospitalizations, and deaths in various regions, and the impact of social distancing.
Social distancing: We are aware thats its a key method to slow the spread of the virus by reducing close contact between people. But we want to explore how effective this method was in terms of flattening the curve.

**Reason to Analyze COVID-19 Data**

Policymakers, healthcare institutions, and researchers are interested in data to understand the spread, manage healthcare resources, and devise strategies (like lockdowns) to reduce transmission rates. By analyzing trends in infection rates, hospitalizations, and social distancing measures, we can help predict future outbreaks, identify regions that have controlled the virus effectively, and inform decision-making.

**Key Decisions Informed by This Data:**
Healthcare resource allocation: Where to send more resources (hospitals, ventilators).
Policy decisions: Determining when to implement or ease restrictions based on the success of social distancing measures.
Public communication: Informing the public about infection trends and the importance of social distancing.

# Project Report 2: Clustering Analysis and Insights

This project report focuses on clustering analysis to identify patterns and relationships in data related to demographics, economics, and COVID-19 outcomes. Below is an overview of the contents:

## 1. Data Preparation
- **Objects Used for Clustering**: Selected states and counties are defined for analysis.
- **Features Used for Clustering**: Identification and explanation of the features considered in the clustering process.

## 2. Scale of Measurement of the Features
- **Measures for Similarity/Distance**: Explanation of distance metrics used for clustering (e.g., Euclidean distance).

## 3. Modeling
- **Cluster One**: Demographic and COVID-19 Impact.
- **Cluster Two**: Economic Vulnerability.
- **Cluster Three**: Socioeconomic Hardship and COVID-19 Outcomes.
- **Cluster Four**: Comprehensive Demographic and Economic Clustering.

## 4. Determining the Suitable Number of Clusters
- **Elbow Method**: Identification of optimal cluster count based on variance.
- **Silhouette Method**: Evaluation of cluster quality.
- **Dunn Index Analysis**: Assessment of compactness and separation of clusters.

## 5. Cluster Evaluation and Validation
- **Unsupervised Cluster Evaluation**: Internal validation of clustering models.
- **Supervised Evaluation of Clustering**: Comparison of clusters with labeled data.

## 6. Exceptional Work
- **Partitioning Around Medoids (PAM)**: Application of PAM clustering to uncover unique patterns.
- **Gaussian Mixture Model (GMM) Clustering**: Advanced probabilistic clustering approach for nuanced insights.

## 7. Conclusion and Recommendations
- **Summary of Findings**: Key observations from each cluster.
- **Public Health Recommendations**: Actionable suggestions based on clustering outcomes.

# Project Report 3 (final report): Data Classification and Modeling

This project report provides a comprehensive overview of the process of building classification models from raw data preparation to deployment. The workflow includes defining the problem, engineering predictive features, training various machine learning models, and evaluating their performance. The report is named as **Datamining_project3_Group_9.pdf** Below is a summary of its contents:

## 1. Data Preparation
- **Defining the Classes**: The classification problem and its objectives are defined.
- **Combining Files for Classification**: Data from multiple sources is merged for analysis.
- **Handling Missing Data**: Methods for identifying predictive features and addressing missing values are discussed.

## 2. Modeling
- **Data Preparation**: Steps for splitting the dataset into training and testing subsets.
- **Classification Models**: Implementation of various machine learning algorithms:
  - **Decision Tree Models**
  - **Random Forest Models**
  - **K-Nearest Neighbors (KNN) Models**
- **Model Comparison**: A detailed analysis of model performance and their comparative evaluation.

## 3. Evaluation
Assessment of model accuracy and robustness through various metrics and validation techniques.

## 4. Deployment
Description of how the best-performing models were prepared for real-world deployment.

## 5. Exceptional Work
Additional analyses and modeling techniques:
- **Naive Bayes Models**: Alternative approach for classification tasks.
- **Logistic Regression Models**: Evaluation and application of logistic regression techniques.
- **Comparative Evaluation**: Performance assessment of Naive Bayes and Logistic Regression models.

## 6. Appendix
Supplementary materials, data sources, and supporting documents.

This report not only highlights standard modeling practices but also includes innovative techniques for handling challenges like missing data and feature engineering. It concludes with a deployment strategy for practical application.
