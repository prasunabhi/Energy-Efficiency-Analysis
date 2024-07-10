# Energy Efficiency - Machine Learning for Business

## Overview
This project aimed to analyze energy efficiency in building designs using machine learning techniques. By leveraging a dataset from the University of California – Irvine machine learning repository, the project provided valuable insights and recommendations for creating energy-efficient buildings, supporting sustainable architecture and environmental conservation.

## Key Features
* Analyzed energy efficiency in buildings by predicting Heating Load and Cooling Load.
* Applied various machine learning models including perceptron, SVM, neural networks, KNN, and Naïve Bayes.
* Achieved high prediction accuracies, notably 98.87% with a two-layer neural network and 80.95% with SVM.

## Methodology
* **Data Source:** EnergyEfficiency.xlsx dataset from the University of California – Irvine machine learning repository.
* **Data Processing:** Conducted multicollinearity analysis using Variance Inflation Factor (VIF) to ensure data robustness.
* **Modeling Techniques:** Utilized Python with libraries such as Pandas, NumPy, and scikit-learn.
    * Perceptron: Used for binary classification.
    * Support Vector Machines (SVM): Separated data into groups with a wide margin, ensuring good generalization.
    * Neural Networks: Implemented multiple layers and nodes for high prediction accuracy.
    * K-Nearest Neighbors (KNN): Classified data points based on proximity.
    * Naïve Bayes: Assumed conditional independence among features for classification.

## Key Findings
* **Multicollinearity:** Removing the X4 variable did not affect model accuracy due to collinearity.
* **Model Performance:** Neural networks achieved the highest accuracy, while SVM also performed well.
* **Surface Area:** Larger surface areas increase heating and cooling challenges.
* **Building Height:** Taller buildings require more energy for heating and cooling.
* **Glazing Area:** Increases heating and cooling loads, varying by region and window placement.
* **Relative Compactness:** Symmetrical shapes like cubes are easier to heat and cool.

## Recommendations for Builders
* **Glazing Area:** In colder regions, use south-facing windows to reduce heating loads. In warmer regions, minimize glazing areas to decrease cooling loads.
* **Natural Resources:** Utilize natural heating and cooling methods, such as earth-based systems and solar heating.
* **Lighting:** Switch to LED lights to decrease the heating load of buildings.
* **Building Design:** Focus on symmetrical shapes for easier heating and cooling.

## Conclusion
By applying advanced machine learning techniques, this project provided significant insights into energy-efficient building designs. The findings and recommendations can guide builders and architects in enhancing sustainability and reducing energy consumption, ultimately contributing to more environmentally friendly building practices.

## Reference
* Dataset: Energy Efficiency dataset from the University of California – Irvine machine learning repository. Available at: [Energy Efficiency dataset](https://archive.ics.uci.edu/ml/datasets/Energy+efficiency)
