import mnist
import pandas as pd
import numpy as np
import scipy.misc
from copy import deepcopy


train_images_origin = mnist.train_images()
train_images = train_images_origin[:20, :, :]


def bi_image(images):
    new_images = (images >= 128)
    new_images = new_images.astype(int)*2 - 1
    return new_images


bi_train_images = bi_image(train_images)


# create a noisy version by flipping some of the pixels
# read the NoiseCoordinates.csv
def modify_by_coordinates(images, filename, modify_type):
    coordinates = pd.read_csv(filename)
    i = 0
    results = deepcopy(images)
    while i < coordinates.shape[0]:
        image_num = i // 2
        for j in range(1, coordinates.shape[1]):
            row = coordinates.iloc[i, j]
            col = coordinates.iloc[i+1, j]
            if modify_type == 'flip':
                results[image_num, row, col] = (-1) * images[image_num, row, col]
            elif modify_type == 'update':
                index = image_num * 28 * 28 + j - 1
                results[index]= [image_num, row, col]
        i += 2
    return results


noise_images = modify_by_coordinates(bi_train_images,'SupplementaryAndSampleData/NoiseCoordinates.csv', 'flip')

#print(bi_train_images[1] == noise_images[1])
#print(np.sum(bi_train_images[1] != noise_images[1]))

"""noise_images_2 = modify_by_coordinates(bi_train_images,'SupplementaryAndSampleData/NoiseCoordinates.csv', 'flip')
im = scipy.misc.toimage(noise_images_2[0,:,:])
im.save('5.png')"""



def calculate_energy(image, q_matrix, hi_hj, hi_xj):
    log_q = 0
    log_p = 0
    c = 10 ** (-10)
    #print(q_matrix)
    #print('-----------------------------------------')
    for row in range(image.shape[0]):
        for col in range(image.shape[1]):
            positive = q_matrix[row][col] * np.log(q_matrix[row][col]+c)
            negative = (1 - q_matrix[row][col]) * np.log((1 - q_matrix[row][col]) + c)
            log_q += positive + negative
            E_qi_Hi = 2 * q_matrix[row][col] - 1
            if row - 1 >= 0:
                E_qj_Hj = 2 * q_matrix[row - 1][col] - 1
                log_p += hi_hj*E_qi_Hi * E_qj_Hj
            if row + 1 <= 27:
                E_qj_Hj = 2 * q_matrix[row + 1][col] - 1
                log_p += hi_hj * E_qi_Hi * E_qj_Hj
            if col-1 >= 0:
                E_qj_Hj = 2 * q_matrix[row][col - 1] - 1
                log_p += hi_hj * E_qi_Hi * E_qj_Hj
            if col+1 <= 27:
                E_qj_Hj = 2 * q_matrix[row][col + 1] - 1
                log_p += hi_hj * E_qi_Hi * E_qj_Hj
            log_p += hi_xj*E_qi_Hi*image[row][col]
    eq = log_q - log_p
    return eq


def boltzman_machine(images,filename, theta_ij, theta_ij2):
    hi_hj = theta_ij
    hi_xj = theta_ij2
    iteration_num = 10
    q_df = pd.read_csv(filename, header=None)
    q_matrix_array = np.empty([20,28,28])
    eq_array = np.empty([20,11])

    update_orders = modify_by_coordinates(np.empty([20*28*28, 3]), 'SupplementaryAndSampleData/UpdateOrderCoordinates.csv','update')

    # calculate initial eq
    for num in range(20):
        eq_array[num][0] = calculate_energy(images[num], q_df.as_matrix(), hi_hj, hi_xj)

    # initialize q_matrix for every image
    for image_n in range(20):
        q_matrix_array[image_n] = q_df.as_matrix()

    for iteration in range(iteration_num):
        #print('----------------------------------------------------------')
        #print(q_matrix_array[0])
        for pixel_num in range(update_orders.shape[0]):
            image_num = int(update_orders[pixel_num][0])
            row = int(update_orders[pixel_num][1])
            col = int(update_orders[pixel_num][2])
            #print(image_num,row,col)
            positive = 0
            if col-1 >= 0:
                positive += hi_hj * ((2 * q_matrix_array[image_num][row][col-1]) - 1)
            if col+1 <= 27:
                positive += hi_hj * ((2 * q_matrix_array[image_num][row][col + 1]) - 1)
            if row-1 >= 0:
                positive += hi_hj * ((2 * q_matrix_array[image_num][row - 1][col]) - 1)
            if row+1 <= 27:
                positive += hi_hj * ((2 * q_matrix_array[image_num][row + 1][col]) - 1)
            positive += hi_xj * images[image_num, row, col]
            q_matrix_array[image_num][row][col] = np.exp(positive) / (np.exp(-1*positive) + np.exp(positive))
            #print('-------------------')
            #print(q_matrix_array[image_num][row][col])

            if int((pixel_num + 1) % 784) == 0:
                eq_array[image_num][iteration+1] = calculate_energy(images[image_num], q_matrix_array[image_num], hi_hj, hi_xj)
                #print(eq_array[image_num][iteration+1])
    if(theta_ij == 0.8):
        np.savetxt('energy.csv', eq_array[10:12, 0:2], '%.8f', delimiter=',')
    return q_matrix_array

matrix_array = boltzman_machine(noise_images, 'SupplementaryAndSampleData/InitialParametersModel.csv', 0.8, 2)


matrix = np.zeros((28,28))
for i in range(10, 20):
    mat = matrix_array[i]
    mat[mat >= 0.5] = 1
    mat[mat < 0.5] = 0
    matrix = np.append(matrix, mat, axis=1)
    #print(matrix)

matrix = matrix[:, 28:]

np.savetxt("output.csv", matrix, '%.0f', delimiter=',')

im = scipy.misc.toimage(matrix)
im.save('1.png')


realMat = np.loadtxt('SupplementaryAndSampleData/SampleDenoised.csv', delimiter=",")

np.savetxt("compare.csv", matrix == realMat, '%.0f')

c_value = [5, 0.6, 0.4, 0.35, 0.3, 0.1]
test_matrix_array = []
for i in range(len(c_value)):
    test_matrix_array.append(boltzman_machine(noise_images, 'SupplementaryAndSampleData/InitialParametersModel.csv', c_value[i], 2))
    test_matrix_array[i][test_matrix_array[i] >= 0.5] = 1
    test_matrix_array[i][test_matrix_array[i] < 0.5] = 0


roc_tpr = []
roc_fpr = []
for i in range(len(c_value)):
    positive = 0
    negative = 0
    numOfBlack = 0
    numOfWhite = 0

    for imageIndex in range(10, 20):
        for row in range(28):
            for col in range(28):
                if(noise_images[imageIndex, row, col] == 1):
                    numOfBlack += 1
                    if(test_matrix_array[i][imageIndex][row][col] == 1):
                        positive += 1
                else:
                    numOfWhite += 1
                    if(test_matrix_array[i][imageIndex][row][col] == 1):
                        negative += 1
    print(positive / numOfBlack)
    print(negative / numOfWhite)
    roc_tpr.append(positive / numOfBlack)
    roc_fpr.append(negative / numOfWhite)

output = np.zeros((2, 6))
output[0, :] = roc_tpr
output[1, :] = roc_fpr
np.savetxt("Roc.csv", output, delimiter=',')


