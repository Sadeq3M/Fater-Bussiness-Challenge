# Fater-Business-Challenge

## Project Overview
This repository contains the analysis and predictive modeling for the Fater Business Challenge, focusing on estimating store potential revenue for diaper sales in Naples, Italy. The project follows the CRISP-DM methodology to systematically address the business problem, from data understanding to model deployment.

## Key Steps
1. **Data Understanding**: Analyzed datasets (`gravitation_NA`, `shapes_NA`, `socio_demo_NA`, and `store-NA`) with a focus on the `microcode` column.
2. **Data Preprocessing**: Handled missing values, removed duplicates, standardized variables, and addressed outliers using IQR.
3. **Exploratory Analysis**: Conducted correlation analysis and visualized data distributions to identify key patterns.
4. **Clustering**: Applied K-means clustering to segment stores into three groups based on sales potential (`potenziale`) and store size (`MQVEND`).
5. **Predictive Modeling**: Utilized Random Forest regression to predict store potential, with feature importance derived from the Gini coefficient.
6. **Evaluation**: Assessed model performance using cross-validation and RMSE metrics.

## Results
- Identified the most influential demographic features (e.g., population aged 45-54) for sales potential.
- Prioritized clusters for business strategy: Cluster 2 (high potential, smaller stores) > Cluster 1 > Cluster 3.
- Achieved high model accuracy (~94% variance explained) for predicting store potential.

## Repository Structure
- `/data`: Raw and processed datasets.
- `/notebooks`: Jupyter notebooks for analysis and modeling.
- `/results`: Visualizations and model outputs.
- `/docs`: Project report and supplementary materials.

## Tools Used
- **R**: For data preprocessing, statistical analysis, and visualization.
- **Python**: For clustering (K-means) and predictive modeling (Random Forest).
- **CRISP-DM**: Methodology framework for structured project execution.

## How to Use
1. Clone the repository.
2. Install required dependencies (listed in `requirements.txt`).
3. Run notebooks in sequence to reproduce the analysis.

For detailed insights, refer to the full report in the `/docs` folder.
