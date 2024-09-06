from plot_lib import plotDimensionalReduction, plotComparison
from bridge_lib import getDimensionalReduction
from sklearn import datasets
import gzip
import pickle
import pandas as pd
import random
import numpy as np

testMethods = ['UMAP']
testSet = 'random'

class_column="target"

if testSet == 'random':
    cols=50
    n = 15000
    classes = 5
    n_per_class = n//classes
    means = [random.randint(-100,100) for x in range(cols)]
    std = [random.random() * 10 for x in range(cols)]
    offsets = [random.random()*2-1 for x in range(classes)]
    x = np.transpose(np.stack([np.random.normal(mu, sigma, n) for mu,sigma in zip(means,std)]))
    y=[]
    for row in range(n):
        x[row,:] = x[row,:] * offsets[row // n_per_class]
        y += [f'class_{row // n_per_class}']

    df = pd.DataFrame(x, columns =[f'column{i+1}'for i in range(cols)])
    df['target'] = y
    #print(df) # cols,n



# Load Dataset, Create DataFrame.
if testSet == 'wine':
    dataset = datasets.load_wine(as_frame=True)
    df = dataset["frame"]
    df[class_column] = df[class_column].map(lambda x: dataset["target_names"][x])
    x=np.array(df.drop(columns=[class_column]).values.tolist())

if testSet == 'mouse':
    with gzip.open("../../data/macosko_2015.pkl.gz", "rb") as f:
        data = pickle.load(f)
    x = data["pca_50"]
    y = data["CellType1"].astype(str)
    df = pd.DataFrame(x, columns =[f'PC{i+1}'for i in range(50)])


if testSet == 'large_prost':
    class_column="Class"
    df = pd.read_csv("../../data/large_data_prost.csv")

if testSet == 'mira_test':
    class_column="Group"
    df = pd.read_csv("../../data/test.csv")

# TSNE
if 'TSNE' in testMethods:

    Y, pc1_table, pc2_table, sample_classes, unique_classes, df = getDimensionalReduction(
        df,
        class_column=class_column,
        method="tSNE",
        statistic_type="mannu",
        normalization_method="zscore",
        imputation_method="min1.5",
        logTransform=False,
        tsne_perplexity=30,
        tsne_early_exaggeration_coefficient=12,
        tsne_affinity_metric='euclidean',
        tsne_use_multiscale=False,
        tsne_n_iter_early=250,
        tsne_n_iter=500,
    )

    plotDimensionalReduction(Y, sample_classes, unique_classes, filename='test_plots/tSNE_DR.png')
    plotComparison(pc1_table, pc2_table, unique_classes, filename='test_plots/tSNE_Stats.png')

# UMAP
if 'UMAP' in testMethods:

    Y, pc1_table, pc2_table, sample_classes, unique_classes, df = getDimensionalReduction(
        df,
        class_column=class_column,
        method="UMAP",
        statistic_type="mannu",
        normalization_method="zscore",
        imputation_method="min1.5",
        logTransform=False,
    )

    plotDimensionalReduction(Y, sample_classes, unique_classes, filename='test_plots/UMAP_DR.png')
    plotComparison(pc1_table, pc2_table, unique_classes, filename='test_plots/UMAP_Stats.png')

# LDA
if 'LDA' in testMethods:

    Y, pc1_table, pc2_table, sample_classes, unique_classes, df = getDimensionalReduction(
        df,
        class_column=class_column,
        method="LDA",
        statistic_type="mannu",
        learning_rate=None,
        normalization_method="zscore",
        imputation_method="min1.5",
        logTransform=False,
    )

    plotDimensionalReduction(Y, sample_classes, unique_classes, filename='test_plots/LDA_DR.png')
    plotComparison(pc1_table, pc2_table, unique_classes, filename='test_plots/LDA_Stats.png')