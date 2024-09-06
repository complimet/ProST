from matplotlib import pyplot as plt
import seaborn as sns
import pandas as pd


def plotComparison(pc1_table, pc2_table, unique_labels, filename = None, annot=False):
    pc1_table = pd.DataFrame(pc1_table, columns=unique_labels)
    pc1_table = pc1_table.rename(
        index={i: label for i, label in enumerate(unique_labels)}
    )
    pc2_table = pd.DataFrame(pc2_table, columns=unique_labels)
    pc2_table = pc2_table.rename(
        index={i: label for i, label in enumerate(unique_labels)}
    )
    plt.figure(figsize=(18, 8))
    ax1 = plt.subplot2grid((1, 2), (0, 0))
    ax2 = plt.subplot2grid((1, 2), (0, 1))
    axes = [ax1, ax2]
    sns.heatmap(ax=axes[0], data=pc1_table, annot=True)
    sns.heatmap(ax=axes[1], data=pc2_table, annot=True)
    if filename is not None:
        plt.savefig(filename)
    else:
        plt.show()


def plotDimensionalReduction(Y, labels, unique_labels, filename = None, percent_explained=None):
    plot_data = {
        "labels": labels,
        "pc1": Y[:, 0],
        "pc2": Y[:, 1],
    }
    fig = plt.figure(figsize=(8, 12))
    ax1 = plt.subplot2grid((3, 2), (0, 0), colspan=2, rowspan=2)
    ax2 = plt.subplot2grid((3, 2), (2, 0))
    ax3 = plt.subplot2grid((3, 2), (2, 1))

    axes = [ax1, ax2, ax3]

    sns.scatterplot(ax=axes[0], data=plot_data, x="pc1", y="pc2", hue="labels")
    axes[0].set(
        xlabel=(
            f"PC1 {percent_explained[0]:.2f}%"
            if percent_explained is not None
            else "t-SNE T1"
        ),
        ylabel=(
            f"PC2 {percent_explained[1]:.2f}%"
            if percent_explained is not None
            else "t-SNE T2"
        ),
    )
    sns.violinplot(ax=axes[1], x="labels", y="pc1", data=plot_data, color="0.8")
    sns.violinplot(ax=axes[2], x="labels", y="pc2", data=plot_data, color="0.8")
    if filename is not None:
        plt.savefig(filename)
    else:
        plt.show()