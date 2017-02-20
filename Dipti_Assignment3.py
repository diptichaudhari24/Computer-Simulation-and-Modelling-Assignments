from __future__ import division
import heapq
import numpy as np
import math
import random
from scipy.stats import norm

# Priority Queue is implemented using the headq library
# of Python which gives basic functions

class PriorityQueue:

    def __init__(self):
        self._State_queue = []
        self._State_index = 0

    def push(self, item, priority):
        heapq.heappush(self._State_queue, (priority, self._State_index, item))
        #print("Future event generated with clock value " + str(priority))
        self._State_index += 1

    def pop(self):
        self._State_index -= 1
        return heapq.heappop(self._State_queue)[-1]

    def isEmpty(self):
        return (self._State_index == 0)


# Global Variables
current_time = 0
arrival_rate = 2.0/60 # 2 per hour or 2 per 60 mins
weather_status = 1 # 1 for good weather and 0 for bad
msg_count = 0

# Good weather parameters
good_weather_mean = 90
good_weather_standard_deviation = 10

# Bad weather parameters
bad_weather_mean = 60
bad_weather_standard_deviation = 20



#Control Variable

covariance_XM = 0
M_mean = 0

# Event types
msg_arrival = 0
msg_departure = 1
change_weather = 2

# Data Structure
free_Server = set()     # Pool of free Servers
fel = PriorityQueue()   # Future event list

# Tuple for event
class Event:
    def __init__(self):
        event_type = None
        time_stamp = 0
        server = None
        msg_id = None

# Check what is current weather and changes it and produce resulting future event
def sample_weather():
    global fel,weather_status
    if weather_status == 1:
        # Setting Good weather
        weather_change_time = np.random.normal(good_weather_mean, good_weather_standard_deviation)
        weather_change = Event()
        weather_change.event_type = change_weather
        weather_change.time_stamp = current_time + weather_change_time
        fel.push(weather_change,weather_change.time_stamp)
        weather_status = 0
    elif weather_status == 0:
        # Setting Bad weather
        weather_change_time = np.random.normal(bad_weather_mean, bad_weather_standard_deviation)
        weather_change = Event()
        weather_change.event_type = change_weather
        weather_change.time_stamp = current_time + weather_change_time
        fel.push(weather_change, weather_change.time_stamp)
        weather_status = 1


# Sample all the arriving messages and add to Future_Event_List
def initialize_arriving_msgs(redundant_mode = False, k = 0):
    global fel,msg_count
    time = current_time
    msg_id = 0

    while time < 6000:
        inter_arrival = -1 * math.log(1 - np.random.uniform(0,1))/ arrival_rate
        msg_arrival_event = Event()
        msg_arrival_event.event_type = msg_arrival
        msg_arrival_event.time_stamp = time + inter_arrival
        msg_arrival_event.msg_id = msg_id
        fel.push(msg_arrival_event,msg_arrival_event.time_stamp)
        msg_count += 1
        time += inter_arrival
        msg_id += 1
    # If redundant mode is on sample arrival of messages after 100th min
    if redundant_mode:
        while time < 12000:
            inter_arrival = -1 * math.log(1 - np.random.uniform(0, 1)) / arrival_rate
            msg_arrival_event = Event()
            msg_arrival_event.event_type = msg_arrival
            msg_arrival_event.time_stamp = time + inter_arrival
            msg_arrival_event.msg_id = msg_id
            fel.push(msg_arrival_event, msg_arrival_event.time_stamp)
            msg_count += 1
            # k Redundant messages generated
            for count in range(0,k):
                msg_arrival_event_r = Event()
                msg_arrival_event_r.event_type = msg_arrival
                msg_arrival_event_r.time_stamp = time + inter_arrival + (60 * count)
                msg_arrival_event_r.msg_id = msg_id
                fel.push(msg_arrival_event_r, msg_arrival_event_r.time_stamp)
                msg_count += 1
                del msg_arrival_event_r
            time += inter_arrival
            msg_id += 1


# Sampling good weather
# Adding all servers to pool of available servers
# Initializing arriving messsages with mode and k value
def initiatize_FEL(redundant_mode = False, k=0):
    global free_Server
    free_Server.add(1)
    free_Server.add(2)
    free_Server.add(3)
    sample_weather()
    initialize_arriving_msgs(redundant_mode,k)


# Discrete event management
def discrete_event_simulation(redundant_mode= False, k = 0):
    global fel,current_time
    processed_msgs_set = set()
    dropped_msg_set = set()

    while (not fel.isEmpty()) and current_time < 100*60:
        event = fel.pop()
        current_time = event.time_stamp
        if event.event_type == msg_arrival:
            if not(event.msg_id in processed_msgs_set):
                if len(free_Server) > 0:
                    ser = random.sample(free_Server, 1)
                    server = ser[0]
                    free_Server.remove(server)
                    U = np.random.uniform(0,1)
                    if weather_status == 1:
                        processing_time = U*60
                    else:
                        processing_time = (U ** (1. /3))*60
                    msg_departure_event = Event()
                    msg_departure_event.event_type = msg_departure
                    msg_departure_event.time_stamp = event.time_stamp + processing_time
                    msg_departure_event.server = server
                    msg_departure_event.msg_id = event.msg_id
                    fel.push(msg_departure_event,msg_departure_event.time_stamp)
                    processed_msgs_set.add(event.msg_id)
                else:
                    #  Msg dropped
                    dropped_msg_set.add(event.msg_id)
        elif event.event_type == msg_departure:

            server = event.server
            free_Server.add(server)
        elif event.event_type == change_weather:
            # Weather change
            sample_weather()

    drop_count = len(dropped_msg_set.difference(processed_msgs_set))
    counts_list = []
    counts_list.append(drop_count)
    counts_list.append(msg_count)
    return(counts_list)

# This Method executes ESTA algorithm
def esta(redundant_mode= False, k=0):
    global weather_status,fel,dropped_msg,current_time,msg_count

    weather_status = 1
    while not fel.isEmpty():
        fel.pop()
    current_time = 0
    msg_count = 0
    initiatize_FEL(redundant_mode,k)
    count = discrete_event_simulation(redundant_mode,k)
    return count


def imc(redundant_mode = False, k = 0):

    d = 0.9
    sum = 0
    total_msg = 0
    sum_of_square = 0
    print("\nRunning IMC:")
    for i in range(0,10000):
        f_list = esta(redundant_mode,k)
        f = f_list[0]
        sum = sum + f
        total_msg = total_msg + msg_count
        sum_of_square = sum_of_square + f * f

    n = 10000

    print("\nNumber of Samples: "+ str(n))

    sample_mean = sum / n
    print("Sample Mean: "+str(sample_mean))

    sample_variance = (sum_of_square - n * (sample_mean * sample_mean)) / (n - 1)
    print ("Sample Variance: " + str(sample_variance))

    standard_deviation = sample_variance**(1/2)
    print("Standard Deviation: " + str(standard_deviation))

    standard_error = standard_deviation / n**(1/2)
    print("Standard Error: "+str(standard_error))

    inverse_cdf = norm.ppf((1 + d) / 2)
    value = standard_error * inverse_cdf

    ci_left = sample_mean - value
    ci_right = sample_mean + value

    print( str(d) + "-confidence interval: [" + str(ci_left) + "," + str(ci_right) + "]\n")

    print("Probability of message being lost  " + str(sum) + "/" + str(total_msg) + ": " + str(sum / total_msg))

    array = []
    array.append(sample_variance)
    array.append(sum/total_msg)
    return(array)


def control_variable_preproccesing():
    global M_mean,covariance_XM
    X_array = []
    M_array = []
    Z_array = []
    for count in range(0,10000):
        variable_list = []
        variable_list = esta()
        X_array.append(variable_list[0])
        M_array.append(variable_list[1])

    covariance_XM = np.cov(np.stack((X_array, M_array)))[0][1]
    M_mean = np.mean(M_array)
    M_variance = np.var(M_array)
    constant = -1 * covariance_XM / M_variance
    print("\nFor new Variable M\nMean of M= "+str(M_mean))
    print("Cov(X,M)= " + str(covariance_XM))
    print("Var(M)=" + str(M_variance))
    print("c = - Cov(X,M) / Var(M)="+str(constant))
    for count in range(0, 10000):
        z = X_array[count] + constant *(M_array[count]-M_mean)
        Z_array.append(z)
    return Z_array


def control_variable_simulation():

    d = 0.9
    sum = 0
    total_msg = 0
    sum_of_square = 0
    Z_array = control_variable_preproccesing()

    print("\nRunning IMC using Control Variable:")
    #print(Z_array)

    for i in range(0, 10000):
        #print(str(i)+" "+str(Z_array[i]))
        f = Z_array[i]
        sum = sum + f
        total_msg = total_msg + msg_count
        sum_of_square = sum_of_square + f * f

    n = 10000
    print("Number of Samples: " + str(n))

    sample_mean = sum / n
    print("Sample Mean: " + str(sample_mean))

    sample_variance = (sum_of_square - n * (sample_mean * sample_mean)) / (n - 1)
    print ("Sample Variance: " + str(sample_variance))

    standard_deviation = sample_variance ** (1 / 2)
    print("Standard Deviation: " + str(standard_deviation))

    standard_error = standard_deviation / n ** (1 / 2)
    print("Standard Error: " + str(standard_error))

    inverse_cdf = norm.ppf((1 + d) / 2)
    value = standard_error * inverse_cdf

    ci_left = sample_mean - value
    ci_right = sample_mean + value

    print( str(d) + "-confidence interval: [" + str(ci_left) + "," + str(ci_right) + "]\n")
    print("Probability of message being lost  " + str(sum) + "/" +str(total_msg)+ ": "+ str(sum/total_msg))
    return(sample_variance)

# EXERCISE 3
# Variance reduction using Control Variable method


def variance_reduction():
    v= imc()
    v1 = v[0]
    v2= control_variable_simulation()
    print("\n Variance is reduced by: "+str((v1-v2)/v1*100))


# EXERCISE 4


def probability_reduction():
    array = imc()

    # Original probability
    p = array[1]
    p2 = 1
    k = 0

    # Run the loop for which Original probability is reduced to half
    while p2 >= p/2 :
        k += 1
        print("\nk = " + str(k))
        array = imc(True,k)
        p2 = array[1]

    print("\n\n K ="+str(k))

# Exercise 2
imc()

'''
/usr/bin/python2.7 /home/dipti/PycharmProjects/552/Dipti_Assignment3.py

Running IMC:

Number of Samples: 10000
Sample Mean: 21.1967
Sample Variance: 35.3303421442
Standard Deviation: 5.94393322172
Standard Error: 0.0594393322172
0.9-confidence interval: [21.0989309988,21.2944690012]

Probability of message being lost  211967/2009421: 0.105486605346


'''

# Exercise 3
variance_reduction()

'''
Running IMC:

Number of Samples: 10000
Sample Mean: 21.1683
Sample Variance: 34.0173768477
Standard Deviation: 5.8324417569
Standard Error: 0.058324417569
0.9-confidence interval: [21.0723648702,21.2642351298]

Probability of message being lost  211683/2011587: 0.105231839339

For new Variable M
Mean of M= 201.0739
Cov(X,M)= 61.6997511451
Var(M)=203.86163879
c = - Cov(X,M) / Var(M)=-0.302655033636

Running IMC using Control Variable:
Number of Samples: 10000
Sample Mean: 21.1897
Sample Variance: 16.8893973396
Standard Deviation: 4.10967119604
Standard Error: 0.0410967119604
0.9-confidence interval: [21.1221019243,21.2572980757]

Probability of message being lost  211897.0/2090000: 0.101386124402

 Variance is reduced by: 50.3506769049
'''

# Exercise 4
probability_reduction()

'''
Running IMC:

Number of Samples: 10000
Sample Mean: 21.2292
Sample Variance: 34.2736947295
Standard Deviation: 5.85437398271
Standard Error: 0.0585437398271
0.9-confidence interval: [21.1329041172,21.3254958828]

Probability of message being lost  212292/2011338: 0.10554765037

k = 1

Running IMC:

Number of Samples: 10000
Sample Mean: 21.1955
Sample Variance: 34.1816979198
Standard Deviation: 5.84651160264
Standard Error: 0.0584651160264
0.9-confidence interval: [21.0993334419,21.2916665581]

Probability of message being lost  211955/6012548: 0.0352521094218


 K =1

Process finished with exit code 0
'''

