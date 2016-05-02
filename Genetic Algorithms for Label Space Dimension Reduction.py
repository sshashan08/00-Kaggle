##|=========================================================================================================================================
## | Created by			: Team A04 (Dawn Schnettler, Shashank Sharma, T Sai Giridhar, Yi Qin)
## | Date Created		: 2016 04 21
## | Purpose			: Label Space Dimension reduction using Genetic Algorithms
## | Name				: 00 GA
## | Date Updated		: _
## | Version		    : v1.0
## | Previous Version	: _
##|=========================================================================================================================================





#--------------------------------------------------------------------------------------------------------------


###| Importing packages

import numpy as np
import pandas as pd
import random as rnd
from sklearn import linear_model



####| Importing the dataset

orig_train = pd.read_csv("C:/Users/shashank/Desktop/Capstone Final/Train_Shashank.csv")
print(orig_train.info())


orig_labels = orig_train.Response
orig_Id = orig_train.Id


orig_train = orig_train.drop(['Id'], axis=1)

print(orig_train.head())
print(orig_train.info())



Response = orig_train['Response']
train = orig_train.drop(['Response'],axis = 1)
print(train.info())

##| Testing Regression Model

clf = linear_model.LinearRegression()
clf.fit(train,Response)


# The mean square error
print("Residual sum of squares: %.2f"
      % np.mean((clf.predict(train) - Response) ** 2))


Response1 = []
for i in Response:
    Response1.append(i)


Resp = []
#--------------------------------------------------------------------------------------------------------------

###| Generating initial Population

##| Setting up range for generating initial population

##| Generating Population

def InitPop(A = 20):
    ##| Default: Generation 0 and Population Size 20
    Pop = []
    rnge = np.arange(-200, 200, 0.5)
    for i in range(0,A,1):
        tmp = rnd.sample(range(800),8)
        flag = []
        for j in tmp:
            flag.append(round(rnge[j],3))
        Pop.append(flag)
    
    return Pop


##| Selecting Elite Solutions (Min Residual Eror)

##| Selecting top 10 % (Elites)  - with  minimum residual error

def PopElites(Pop = [], B = 0.1):
    ##| Default: Population 0 and % Elites 10%
    if len(Pop) == 0:
        print("Empty Population")
        return None
    
    
    ResidualPop = []
    global Resp
    Elites = []
    
    for i in range(0,len(Pop),1):
        temp = []
        for j in range(0, len(Response1),1):
            t = np.asscalar(Response1[j]-1)
            temp.append(Pop[i][t-1])
        Resp.append(temp)
            

    ResidualPop = []
    for i in range(0,20,1):
        temp = Resp[:][i]
        clf = linear_model.LinearRegression()
        clf.fit(train,temp)
        ResidualPop.append(np.mean((clf.predict(train) - temp) ** 2))
        
    m = min(ResidualPop)
    
    L = int(len(ResidualPop)*B)
    ln = len(ResidualPop)
    for i in range(0,L,1):
        j = 0
        while (j < ln):
            if m == ResidualPop[j]:
                Elites.append(j)
                ResidualPop.pop(j)
                ln = len(ResidualPop)
            j += 1
        m = min(ResidualPop)        

    return Elites
    
    
   
     
    
##| Performing crossover and creating new generation of solutions for non elite population    
    
def crossover(Pop=[], Elites = []):
    
    Rem = []
    Crossed = []
    
    for i in range(0,len(Pop)):
        if i in Elites:
            continue
        else:
            Rem.append(i)
    
    for i in range(0,len(Rem)//2,1):
        selected = rnd.sample(range(0,len(Rem),1),2)
        thr = int(round(rnd.random()*7,0))
        C1 = Pop[Rem[selected[0]]]
        C2 = Pop[Rem[selected[1]]]
        C3 = C1[:thr:1] + C2[thr::1]
        C4 = C2[:thr:1] + C1[thr::1]
        Crossed.append(C3)
        Crossed.append(C4)
        
    return Crossed
        
##| Performing mutation on new generation of solutions
        
def mutation(Crossed = [], C= 0.1):
    
    ToMutate = []
    for i in range(0,len(Crossed),1):
        tmp = []
        for j in range(0,8,1):
            tmp.append(rnd.random())
        ToMutate.append(tmp)
        
    
    for i in range(0,len(Crossed),1):
        for j in range(0,8,1):
            if(ToMutate [i][j] <= 0.1):
                if (rnd.random() < 0.5):
                    Crossed[i][j] -= (Crossed[i][j]*0.1)
                else:
                    Crossed[i][j] += (Crossed[i][j]*0.1)
    
    return Crossed

##| Finalizing new generation

def NewGen(Pop = [], Elites = [], Crossed = []):
    El = []
    for i in Elites:
        El.append(Pop[i][:])
    
    return (El + Crossed)



        
        
        
        
        
def GenAlgo(A = 20, B = 0.1, C = 0.1, D = 10):
    
    Soln = []    
    Pop = InitPop(A)
    
    for i in range(0,D,1):
        Elites = PopElites(Pop, B)
        Crossed = crossover(Pop, Elites)
        Crossed = mutation(Crossed, C)
        Pop = NewGen(Pop, Elites, Crossed)

    for i in range(0,len(Pop),1):
        temp = []
        for j in range(0, len(Response1),1):
            t = np.asscalar(Response1[j]-1)
            temp.append(Pop[i][t-1])
        Resp.append(temp)
            

    ResidualPop = []
    for i in range(0,20,1):
        temp = Resp[:][i]
        clf = linear_model.LinearRegression()
        clf.fit(train,temp)
        ResidualPop.append(np.mean((clf.predict(train) - temp) ** 2))
        
    m = min(ResidualPop)
    flag = -1
    for i in range(0,len(ResidualPop),1):
        if m == ResidualPop[i]:
            flag = i
    
    Soln = Pop[flag][:]
    
    return Soln
       

######| Using GA for Generating the optimal solution       
       
GenAlgo(A=20, B=0.1, C=0.1, D = 10)  ##| Takes ~1 Hr to execute

##| Here A <- Size of initial population (default = 20)
##| B <- % of Elites in the population (default  = 10%)
##| C <- % of Mutation in the Crossovered Solution (default = 10%)
##| D <- Number of Evolved Generations (default = 10)
