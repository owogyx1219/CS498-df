import pandas as pd
import numpy as np
from sklearn.cluster import KMeans
from collections import Counter

raw_data = np.loadtxt("docword.nips.txt", dtype='i', skiprows = 3)
# print raw_data.shape[0]

raw_vocab = pd.read_csv('vocab.nips.txt', header = None)
raw_vocab = raw_vocab.as_matrix()
# print(raw_vocab[0:10, 0])

numOfDocs = 1500
numOfWords = 12419
numOfTopics = 30

matrix = np.zeros((numOfDocs, numOfWords))

for i in range(raw_data.shape[0]):
	docIndex = raw_data[i, 0]-1
	wordIndex = raw_data[i, 1]-1
	matrix[docIndex, wordIndex] = raw_data[i, 2]

#print matrix
#print matrix.shape

W = np.zeros((numOfDocs, numOfTopics))
P_i_j = np.zeros(numOfTopics)

kmeans_model = KMeans(n_clusters =numOfTopics, random_state=0).fit(matrix)
kmeans_labels = kmeans_model.labels_

for i in range(numOfTopics):
	P_i_j[i] = Counter(kmeans_labels)[i] / numOfDocs

P_j_k = np.zeros((numOfTopics, numOfWords))
for i, topicLabel in enumerate(kmeans_labels):
	for j in range(numOfTopics):
		if(topicLabel == j):
			P_j_k[j, ] += matrix[i, ]

for j in range(numOfTopics):
	for k in range(numOfWords):
		if(P_j_k[j, k] == 0):
			P_j_k[j, k] = 1

#P_j_k = P_j_k / np.sum(P_j_k, axis = 1)[:, None]

# print P_j_k
# print P_j_k[0,]

for j in range(P_j_k.shape[0]):
	sum = 0
	for i in range(P_j_k.shape[1]):
		sum += P_j_k[j, i]
	P_j_k[j, ] = P_j_k[j, ] / sum

# print P_j_k[0,]
# print P_j_k





