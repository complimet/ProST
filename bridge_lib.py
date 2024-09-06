from sklearn.decomposition import PCA
from sklearn.preprocessing import MinMaxScaler
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import KNNImputer, SimpleImputer, IterativeImputer
from MulticoreTSNE import MulticoreTSNE as TSNE
import numpy as np
from scipy.stats import zscore, ttest_ind, mannwhitneyu
import warnings
import umap
from tsne_lib import compareClasses
import pandas as pd

VALID_IMPUTATION_METHODS = ["min1.5", "knn", "mice", "median", "none"]
VALID_NORMALIZATION_METHODS = ["none", "zscore", "minmax", "log2", "pareto"]
VALID_DIMENSIONAL_METHODS = ["PCA", "tSNE", "optSNE", "UMAP", "LDA"]


def getDimensionalReduction(
    df,
    class_column="Category",
    method="tSNE",
    statistic_type="mannu",
    perplexity=30,
    learning_rate=None,
    normalization_method="zscore",
    imputation_method="min1.5",
    logTransform=False,
    early_exaggeration=12
):
    if df[class_column].dtype is not object and df[class_column].dtype != 'string':
        df[class_column] = df[class_column].apply(lambda x: f"class_{x}")

    # Get List of Labels
    unique_classes = df[class_column].unique()
    sample_classes = df[class_column].values
    numeric_cols = df.select_dtypes(include=[np.number]).columns
    df[numeric_cols] = df[numeric_cols].astype(float)
    # (Optionally) Log-Transform data
    if logTransform:
        df[numeric_cols] = np.log2(df[numeric_cols].replace(0, value = 1e-5))
    # Perform Data Imputation
    if imputation_method not in VALID_IMPUTATION_METHODS:
        imputation_method = "min1.5"
        warnings.warn(
            f"{imputation_method} is not a valid imputation method. Using min1.5 by default."
        )
    match imputation_method:
        case "min1.5":
            # Set all missing values to 1.5
            print(f"Applying {imputation_method} imputation to numerical columns")
            df[numeric_cols]=df[numeric_cols].fillna(value=df[numeric_cols].min()/5)
        case "knn":
            # Predictive Imputation using KNN
            print(f"Applying {imputation_method} imputation to numerical columns")
            imputer = KNNImputer(n_neighbors=2, missing_values=np.nan)
            df[numeric_cols] = imputer.fit_transform(df[numeric_cols])
        case "mice":
            print(f"Applying {imputation_method} imputation to numerical columns")
            imputer = IterativeImputer(max_iter=10, missing_values=np.nan)
            df[numeric_cols] = imputer.fit_transform(df[numeric_cols])
        case "median":
            print(f"Applying {imputation_method} imputation to numerical columns")
            imputer = SimpleImputer(missing_values=np.nan, strategy="median")
            df[numeric_cols] = imputer.fit_transform(df[numeric_cols])
        case "none":
            print(f"Applying no imputation to numerical columns")

    # Apply Desired Normalization
    match normalization_method:
        case "zscore":
            print("Applying zscore normalization to numerical columns")
            df[numeric_cols] = df[numeric_cols].apply(zscore)
        case "none":
            print("Applying no normalization to numerical columns")
        case "minmax":
            df[numeric_cols] = MinMaxScaler().fit_transform(df[numeric_cols])
        case "log2":
            df[numeric_cols] = np.log2(df[numeric_cols].replace(0, value = 1e-5))
        case "pareto":
            df[numeric_cols] = df[numeric_cols].apply(
                lambda x: [(y - np.mean(x)) / np.sqrt(np.std(x)) for y in x], #
            )
    # Perform Dimensional Reduction
    match method:
        case "PCA":
            # Do PCA
            pca = PCA().fit(
                df[numeric_cols]
            )  # default n_components is min(n_samples, n_features)
            Y = pca.transform(df[numeric_cols])
            pcvars = pca.explained_variance_
            percent_explained = 100 * pcvars / sum(pcvars)
        case "tSNE" | "optSNE":
                adjusted_learning_rate = int(learning_rate if learning_rate is not None else df.shape[0] / early_exaggeration)
                print(f"Using {method} projection with a learning rate of {adjusted_learning_rate}")
                # Do t-SNE
                Y = TSNE(
                    perplexity=perplexity,
                    learning_rate= adjusted_learning_rate,
                    verbose=0,
                    n_jobs=8,
                    auto_iter=method == "optSNE",
                ).fit_transform(X=df[numeric_cols])

        case "UMAP":  # default n_components is 2
            Y = umap.UMAP().fit_transform(X=df[numeric_cols])
        case "LDA":  # default n_components is min(n_classes - 1, n_features)
            Y = LinearDiscriminantAnalysis(n_components=2).fit_transform(
                X=df[numeric_cols], y=sample_classes
            )
    # Perform Statistical Test
    if statistic_type == "ttest":
        pc1_table, pc2_table = compareClasses(
            unique_classes, Y, sample_classes, ttest_ind
        )
    else:
        if statistic_type != "mannu":
            warnings.warn(
                f"Statistical method: {statistic_type} unrecognized. MannWhitney-U used by default."
            )
        pc1_table, pc2_table = compareClasses(
            unique_classes, Y, sample_classes, mannwhitneyu
        )

    return Y, pc1_table, pc2_table, list(sample_classes), list(unique_classes), df
