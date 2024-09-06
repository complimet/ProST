import numpy as np
from scipy.stats import mannwhitneyu

def compareClasses(
    unique_classes, sample_coeff_scores, sample_classes, statistical_test=mannwhitneyu
):
    num_classes = len(unique_classes)
    pc1_table = np.zeros((num_classes, num_classes))
    pc2_table = np.zeros((num_classes, num_classes))

    for i in range(num_classes):
        for j in range(num_classes):
            result = statistical_test(
                sample_coeff_scores[sample_classes == unique_classes[i], 0],
                sample_coeff_scores[sample_classes == unique_classes[j], 0],
            )
            pc1_table[i, j] = result[1]
            result = statistical_test(
                sample_coeff_scores[sample_classes == unique_classes[i], 1],
                sample_coeff_scores[sample_classes == unique_classes[j], 1],
            )
            pc2_table[i, j] = result[1]
    return pc1_table, pc2_table

