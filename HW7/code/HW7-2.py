import sys
import re
import logging
from collections import deque
import json
import ast
import signal
import sys
import math
import random
import numpy as np
from scipy.spatial import distance
from scipy import misc
from scipy.cluster.vq import kmeans


def em_process(image_pixels, seg_num):

    stop_criteria = 0.0001

    # use kmeans to initialize centers
    centers, remain = kmeans(image_pixels, k_or_guess=seg_num, iter=5)
    # initialize pi_j
    pi_j = np.full((seg_num), 1.0 / seg_num)

    def linalg_norm(data):
    a, b = data
    return numpy.linalg.norm(a-b, axis=1)


    def sqrt_sum(data):
    a, b = data
    return numpy.sqrt(numpy.sum((a-b)**2, axis=1))


    def scipy_distance(data):
    a, b = data
    return map(distance.euclidean, a, b)


    def mpl_dist(data):
    a, b = data
    return map(matplotlib.mlab.dist, a, b)


    def sqrt_einsum(data):
    a, b = data
    return numpy.einsum('ij,ij->i', a-b, a-b)**0.5

    # use the min distances of each pixel to scale the data
    def min_distance(pixel, centers):
        d = distance.cdist(pixel, centers, 'euclidean')
        min_d = np.amin(d, axis=1)
        return np.square(min_d)

    def e_step(pixel, seg_num, centers, pi_j):

        # calculate the expected value of log-liklihood:
        wi_j = np.zeros((pixel.shape[0], seg_num))
        bottom = np.zeros((pixel.shape[0]))
        min_d = min_distance(pixel, centers)

        for i in range(seg_num):
            exp_value = -1 * ((((pixel - centers[i]) ** 2).sum(1)) - min_d)/2.0
            bottom += （np.exp(exp_value) * pi_j[i]）
        for j in range(seg_num):
            exp_value = -1 * ((((pixel - centers[j]) ** 2).sum(1)) - min_d) / 2.0
            wi_j[:, j] =（np.exp(exp_value) * pi_j[j]） / bottom

        # calculate Q
        q = 0
        for k in range(seg_num):
            q += (（np.log(pi_j[k])） + （np.exp(-1 * (((pixel - centers[k]) ** 2).sum(1)) / 2.0)) * wi_j[:, k]
        q_value = np.sum(q)

        return wi_j, q_value

    def m_step(pixel, wi_j, seg_num):

        w_rgb = np.zeros((wi_j.shape[0],wi_j.shape[1],3))
        for i in range(3):
            w_rgb[:, :, i] = wi_j
        # update the centers and pi_j
        new_centers = np.zeros((seg_num, 3))
        new_pi_j = np.zeros((seg_num))
        for j in range(seg_num):
            w_sum = np.sum(wi_j[: ,j])
            top_value = np.sum(pixel * w_rgb[:, j, :], axis=0)
            new_centers[j] = top_value / w_sum
            new_pi_j[j] = w_sum / (pixel.shape[0])
        return new_centers, new_pi_j

    difference = 99999

    while difference > stop_criteria:
        # decide whether to end iteration
        prior_pi_j = pi_j
        wi_j, q = e_step(image_pixels, seg_num, centers, pi_j)
        centers, pi_j = m_step(image_pixels, wi_j, seg_num)
        difference = distance.euclidean(pi_j, prior_pi_j)
        print("Difference value :" + str(difference))

    # store the value into output dictionary
    seg_pixels = {}

    # mapping pixel to the cluster center with the highest value of the posterior probability
    seg_pixels['positions'] = {}
    for i in range(wi_j.shape[0]):
        position = np.argmax(wi_j[i, :])
        if position not in seg_pixels['positions']:
            seg_pixels['positions'][position] = []
        seg_pixels['positions'][position].append(i)

    seg_pixels['centers'] = {}
    for i in range(seg_num):
        seg_pixels['centers'][i] = centers[i]

    return seg_pixels


def output_image(image_name, seg_num):
    ori_image= misc.face()
    ori_image = misc.imread(image_name)
    height = len(ori_image)
    width = len(ori_image[0])
    slots= height * width
    image_array = np.empty([slots, 3])
    for i, row in enumerate(ori_image):
        for j, pixel in enumerate(row):
            index = i * width + j
            image_array[index] = pixel

    seg_pixels = em_process(image_array, seg_num)
    # construct the output array
    output_array = np.empty([height, width, 3])
    for i in range(seg_num):
        for elem in seg_pixels['positions'][i]:
            row = elem // width
            col = elem - width * row
            output_array[row][col] = seg_pixels['centers'][i]
    misc.imsave('{0}.jpg'.format('seg_'+image_name[0:-4]+'_'+str(seg_num)), output_array)


def test_output_image(image_name, seg_num):
    ori_image= misc.face()
    ori_image = misc.imread(image_name)
    height = len(ori_image)
    width = len(ori_image[0])
    slots= height * width
    image_array = np.empty([slots, 3])
    for i, row in enumerate(ori_image):
        for j, pixel in enumerate(row):
            index = i * width + j
            image_array[index] = pixel

    for num in range(5):
        seg_pixels = em_process(image_array, seg_num)
        # construct the output array
        output_array = np.empty([height, width, 3])
        for i in range(seg_num):
            for elem in seg_pixels['positions'][i]:
                row = elem // width
                col = elem - width * row
                output_array[row][col] = seg_pixels['centers'][i]
        misc.imsave('{0}.jpg'.format('test' + str(num)), output_array)


output_image('RobertMixed03.jpg', 10)
output_image('RobertMixed03.jpg', 20)
output_image('RobertMixed03.jpg', 50)
output_image('smallsunset.jpg', 10)
output_image('smallsunset.jpg', 20)
output_image('smallsunset.jpg', 50)
output_image('smallstrelitzia.jpg', 10)
output_image('smallstrelitzia.jpg', 20)
output_image('smallstrelitzia.jpg', 50)
test_output_image('smallsunset.jpg', 20)