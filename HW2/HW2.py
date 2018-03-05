import pandas as pd
import numpy as np
import random
from sklearn import cross_validation
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt

###################Used adult income data from https://archive.ics.uci.edu/ml/datasets/Adult
###################and transformed them to .txt file for use in this program

ax = plt.subplot(111)
t1 = np.arange(0.0, 500.0, 0.1)



# read and combine the original data
original_train = pd.read_csv('adult.data.txt', header=None)
original_test = pd.read_csv('adult.test.txt', header=None, skiprows=1)
original_data = original_train.append(original_test)


# omit the examples with missing values
def pre_process(data):
    rows = [0, 2, 4, 10, 11, 12, 14]
    new_data = data.iloc[:, rows].dropna(axis=0, how='any')
    return new_data


filted_data = pre_process(original_data)

# split train,test and validation
train, remain = cross_validation.train_test_split(filted_data, test_size=0.2)
test, validation = cross_validation.train_test_split(remain, test_size=0.5)


# split the data into features and labels
train_x = train.iloc[:, 0:6]
train_y = train.iloc[:, 6]
test_x = test.iloc[:, 0:6]
test_y = test.iloc[:, 6]
validation_x = validation.iloc[:, 0:6]
validation_y = validation.iloc[:, 6]

# rescale the 6 numerical features
scaler = StandardScaler()
train_x = pd.DataFrame(scaler.fit_transform(train_x))
test_x = pd.DataFrame(scaler.fit_transform(test_x))
validation_x = pd.DataFrame(scaler.fit_transform(validation_x))

# change the index of labels
train_y.index = range(len(train_x))
test_y.index = range(len(test_x))
validation_y.index = range(len(validation_x))

# convert lables
def convert_lable(y):
    new_y = y.copy()
    for i in range(len(new_y)):
        if new_y.iloc[i] == ' >50K.' or new_y.iloc[i] == ' >50K':
            new_y.iloc[i] = 1
        elif new_y.iloc[i] == ' <=50K' or new_y.iloc[i] == ' <=50K.':
            new_y.iloc[i] = -1
        else:
            new_y.iloc[i] = 0
    return new_y


train_y = convert_lable(train_y)
test_y = convert_lable(test_y)
validation_y = convert_lable(validation_y)
#print(test_y)
def predict(x, a, b):
    x = x.values.tolist()
    a = a.tolist()
    dot_product = np.dot(a, x)
    return  dot_product + b


def accuracy(x, y, a, b):
    correct = 0
    wrong = 0
    for i in range(len(y)):
        pre = predict(x.iloc[i, ], a, b)
        if pre > 0:
            res = 1
        else:
            res = -1
        act = y.iloc[i]
        if res == act:
            correct += 1
        else:
            wrong += 1
    accuracy = correct / len(y)
    return accuracy

#list of validation accuracies
val_accuracies = []
test_accuracies = []
#coef_a =[]

# train an SVM
lambd_set = [0.001, 0.01, 0.1, 1]
epochs = 50
steps = 300
train_data = train_x
train_data[6] = train_y

for l in lambd_set:
    #val_accuracies = []
    #test_accuracies = []
    coef_a =[]

    print(l)
    a = np.array([0, 0, 0, 0, 0, 0])
    b = 0
    batch_size = 1
    accuracy_records = []
    current_step = 0
    #print(epochs)
    for epoch in range(epochs):
        #print(epoch)
        step_length = 1/(0.01 * epoch + 50)

        rows = random.sample(list(train_data.index), 50)
        training_data = train_data.ix[rows]
        evaluation_data = train_data.drop(rows)
        training_features = training_data.iloc[:, 0:6]
        training_lables = training_data.iloc[:, 6]
        evaluation_features = evaluation_data.iloc[:, 0:6]
        evaluation_labels = evaluation_data.iloc[:, 6]
        print(".......")
        #print(type(evaluation_features))
        for step in range(steps):
            print(step)
            if step % 30 == 0:
                accuracy_records.append(accuracy(evaluation_features, evaluation_labels, a, b))
                coef_a.append(np.linalg.norm(a))
                print(accuracy_records)
            train_batch = training_data.sample(n=1)
            train_batch_x = train_batch.iloc[:, 0:6]
            train_batch_y = train_batch.iloc[:, 6]
            #print(train_batch_x.values.tolist())
            predict_value = predict(train_batch_x.iloc[0, ], a, b)

            real_value = train_batch_y.iloc[0, ]

            if predict_value * real_value >= 1:
                a = a - step_length * (l * a)
                b = b - step_length * 0
            else:
                a = a - step_length * (l * a - real_value * np.array(train_batch_x))
                b = b - step_length * - real_value

            current_step += 1
    #print(accuracy_records)
    valeval = accuracy (validation_x, validation_y, a, b)
    #print(valeval)
    #print(val_accuracies)
    val_accuracies.append(valeval)
    #print(val_accuracies)
    #print('hhh')
    testeval = accuracy(test_x, test_y, a, b)
    #print(testeval)
    test_accuracies.append(testeval)
    #print(len(accuracy_records))
    #print(test_accuracies)
    

#    plt.plot(list(range(0,len(accuracy_records))), accuracy_records, label="lambda=%f"%(l,))
#    leg = plt.legend(loc='best', ncol=2, mode="expand", shadow=True, fancybox=True)
#    leg.get_frame().set_alpha(0.5)
    #plt.figure()
    #print(coef_a)
    plt.plot(list(range(0,len(coef_a))), coef_a, label="lambda=%f"%(l,))
    leg = plt.legend(loc='best', ncol=2, mode="expand", shadow=True, fancybox=True)
    leg.get_frame().set_alpha(0.5)

    print(val_accuracies)
    #print(lambd_set)
    print(test_accuracies)
    
    #val_accuracies = []
    #test_accuracies = []
plt.show()

#print(len(test_accuracies))
#print(len(val_accuracies))
maxConstantIndex = 0
for i in range(len(val_accuracies)):
    if(val_accuracies[i] >= val_accuracies[maxConstantIndex]):
        maxConstantIndex = i


max_constant = lambd_set[maxConstantIndex]
testAccuracy = test_accuracies[maxConstantIndex]

print(maxConstantIndex)
print(max_constant)
print(testAccuracy)



