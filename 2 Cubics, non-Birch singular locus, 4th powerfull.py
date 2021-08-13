import copy, time, random
#Elementary List Operations

def Number_Check(X):
    if type(X)==int or type(X)==float:
        return(1)
    else:
        return(0)

def List_Length_Checker(List_of_Lists):
    for i in range(1,len(List_of_Lists)):
        if len(List_of_Lists[i])!=len(List_of_Lists[0]):
            return("Error: List lengths not equal")
def CRF_Add(List_1,List_2): #This can be simplified by taking "Add" to equal List_1 in the first instance.
    Add=[None]*len(List_1)
    for i in range(0,len(List_1)):
        if List_1[i]!=None and List_2[i]!=None:
            Add[i]=List_1[i]+List_2[i]
        elif List_1[i]==None and List_2[i]!=None:
            Add[i]=List_2[i]
        elif List_1[i]!=None and List_2[i]==None:
            Add[i]=List_1[i]
    return(Add)

def Multi_Add(List_of_Lists):
    Multi_Add=[None]*len(List_of_Lists[0])
    for i in range(0,len(List_of_Lists)):
        Multi_Add=CRF_Add(Multi_Add, List_of_Lists[i])
    return(Multi_Add)

def CRF_Mult(List,scalar):
        Mult=[None]*len(List)
        for i in range(0,len(List)):
            if List[i]!=None:
                Mult[i]=scalar*List[i]
        return(Mult)

def List_Evaluate(List, Point): #Converts a list (which represents a parameter) into a number for a particular configuration of the main variables. For us, "Point" must have the following form: [phi,sig,phi_3].
    List_Copy=None_to_Zero(List[:])
    list_sum=List_Copy[0]
    for i in range(1, len(List)):
        list_sum=list_sum+Point[i-1]*List_Copy[i] #This "for loop" converts the coefficient representation of a parameter into its actual numerical value (for a particular point [phi,sig,phi_3]) by multiplying each element of the list by the value of its associated variable, and adding this to "list_sum".
    return(list_sum)

def CRF_to_NF(Parameter, Point): #This function converts a Parameter which is expressed as a max or a min (e.g. H) from its "coefficient representation form (CRF)" (a list of lists) into a list containing the actual numerical values of each part of the expression (Numerical form "NF") for a particular point [phi,sig,phi_3].
    Parameter_List=[]
    for i in range(0,len(Parameter)):
        Parameter_List.append(List_Evaluate(Parameter[i], Point)) #We use the List_Evaluate function to convert each part of the parameter from its coefficient representation form, into its numberical value, and then append this value to the list "Parameter_List".
    return(Parameter_List)

def CRF_Max(Parameter, Point):                    #This function determines which component of a parameter is maximised (e.g. for V, it will either be 1 or HP^2t), and then returns the Coefficient Representation Form (CRF) of this part.
    Parameter_NF= CRF_to_NF(Parameter, Point)     #We convert the parameter from CRF to numerical form (NF).
    max_position=0                                                 #max_position will be used to record the largest component of our parameter (for a given variable configuration). We start by setting this to 0. max_position will be used to point to a particular element of our parameter (NF) (represented as a list). As an example: If (in NF), we have H=[0.213,0.414], max_position=0 will be used to represent the 0th position of H, which is 0.213 in this case. 0.213 isn't represented by "1" even though it is the "first" element of the list because Python always starts counting from 0 (when it comes to list positions).
    for i in range(1,len(Parameter_NF)):
        if Parameter_NF[i]>Parameter_NF[max_position]:            #We compare the ith component of our Parameter to the largest previous component. In particular: The "if" statement within the "for loop" is designed to ensure that max_position points us to the largest component within list positions 0,1,...,i-1. We compare list position i to this component.
            max_position=i                                        #If this component is larger than the largest component within positions 0,1,...,i-1, we record this fact by setting max_position equal to i. By doing this, "max_position" will tell us the position of the largest component out the first i+1 components (0,1,...,i). We start the loop at i=1 because there is no point in comparing the 0-th component to itself.
    return(Parameter[max_position])                              #When the for loop finishes, max_position will tell us the position of the largest component of our parameter (for the given variable configuration). We then output the CRF form of this component.

def CRF_Min(Parameter, Point):                  #This function determines which component of a parameter is minimised. See the comments for CRF_Max.
    Parameter_NF= CRF_to_NF(Parameter, Point)
    min_position=0
    for i in range(1,len(Parameter_NF)):
        if Parameter_NF[i]<Parameter_NF[min_position]:
            min_position=i
    return(Parameter[min_position])

def None_to_Zero(List):
    for i in range(0,len(List)):
        if List[i]==None:
            List[i]=0
    return(List)

def None_to_Zero_List(List_of_Lists):
    for i in range(0, len(List_of_Lists)):
        List_of_Lists[i]=None_to_Zero(List_of_Lists[i])
    return(List_of_Lists)

def Zero_to_None(List):
    for i in range(0,len(List)):
        if List[i]==0:
            List[i]=None
    return(List)

def Zero_to_None_List(List_of_Lists):
    for i in range(0, len(List_of_Lists)):
        List_of_Lists[i]=Zero_to_None(List_of_Lists[i])
    return(List_of_Lists)
#Bound function building blocks

#    AVDC/Poisson

def H_Poisson(n,ep):
    return([ [float(10)/(n-2)+ep,None,None,None, None] , [float(2)/(n+2)+ep,float(6)/(n+2),None,None, None] ]) #This list of lists represents log_P(H). log_P(H) is a maximum of two functions of the form f_n(phi,sig,phi_3)= c_0+c_1phi+c_2sig+c_3phi_3. The elements of the lists are the respective c_i's.

#def H_CRF(Variables, n, ep): #Test functions needed from here
#    return(CRF_Max(H_Poisson(n, ep),Variables))

def V_bracket_old(H):
    V_1=CRF_Add([-float(3)/8,float(1)/2,None,None, None],CRF_Mult(H,0.5))
    return([V_1, [-1,1, None, None, None] , [None]*5])

def V_bracket(H):
    V_1=CRF_Add([None,1,float(1)/2,None, None],CRF_Mult(H,0.5))
    return([V_1, [-1,1, None, None, None] , [None]*5])

#def V_CRF(Variables, n, ep): #This is V in NF.
#    H=H_CRF(Variables, n, ep)
#    V=CRF_Max(V_bracket(H),Variables)
#    return(V)

def Tau_bracket(H): #Also used in AVDC/Weyl
    Tau_1=CRF_Add(H, [2, None, 1, None, None])
    return([Tau_1, [None]*5])

def Y_bracket(H,V,n):                                                                        #Note to self V<q^{1/2} for every q in our domain, so the q^1/2 V^{n-1} term dominates over V^n and q^1/2 dominates over V.
    Y_0=Multi_Add([CRF_Mult(H,n), CRF_Mult(V,n-1), [None, -float(n+1)/2, None, None, None]]) #This is technically not quite correct: This term represent H^n q^{-n/2} b_1^{-1} V^{n-1}(b_1q_3)^{1/2}. This is << H^n V^n-1 q^{-(n-1)/2} b_1^{-1}. But later on, b_1 is worst when it is of size q, so this expression is ultimately correct in the grander scheme of things.
    YY_0=CRF_Add(CRF_Mult(H,n), [None, -float(n+1)/2, None, float(n)/3-0.5 , float(n-1)/2])   #I bounded (b_1b_3q_4)^1/2 by q^1/2, so b_3^{n/6} becomes b_3^{(n-3)/6} and q_4^n/2 becomes q_4^{(n-1)/2}. I've put b_3 power to be n-1/6 since it should be (power of q_4)/3. See YY_0 comment concerning grand scheme correctness.
#    Y_n1=CRF_Add(H, [None,-1,None,None, None])                                              #This is technically not quite correct, but ultimately correct in the grand scheme. See YY_0 comment.
    return([Y_0, YY_0, [None]*5])


#    VDC/Weyl

H_Weyl=[[float(2)/5, float(1)/5, float(1)/5, None, None], [None, float(1)/6, None, None, None]]


def VDC_Weyl_bracket(H):
    A=CRF_Add([-2,-1,-1, None, None], H)
    B=CRF_Add([None, -1, None, None, None], CRF_Mult(H,2))
    return([A,B])


#    Weyl/Weyl

Weyl_min=[[None, -1, None, None, None],[-3,-1,-1, None, None]]

Weyl_2=[None, 2,2, None, None]


#Functions list


def AVDC_Poisson_Function_Builder(Pivot_List,n,ep):
    length=len(Pivot_List)
    for i in range(0,len(H_Poisson(n,ep))):
        H=H_Poisson(n,ep)[i]
        for j in range(0,len(V_bracket(H))):
            V=V_bracket(H)[j]
            for k in range(0,len(Tau_bracket(H))):
                Tau=Tau_bracket(H)[k]
                for l in range(0, len(Y_bracket(H,V,n))):
                    Y=Y_bracket(H,V,n)[l]
                    Function=Multi_Add([[n-5, 3, None, None, None], CRF_Mult(H, float(-2-n)/2), CRF_Mult(Tau, 2), CRF_Mult(Y, float(1)/2)])
                    Pivot_List.append([[Function], length])
                    length=length+1
    return(Pivot_List)

def PVDC_Poisson_Function_Builder(Pivot_List,n,ep):
    length=len(Pivot_List)
    for i in range(0,len(H_Poisson(n,ep))):
        H=H_Poisson(n,ep)[i]
        for j in range(0,len(V_bracket(H))):
            V=V_bracket(H)[j]
            for l in range(0, len(Y_bracket(H,V,n))):
                Y=Y_bracket(H,V,n)[l]
                Function=Multi_Add([[n, 3, 2, None, None], CRF_Mult(H, -float(n)/2), CRF_Mult(Y, float(1)/2)])
                Pivot_List.append([[Function], length])
                length=length+1
    return(Pivot_List)

def AVDC_Function_Builder(Pivot_List, n):
    length=len(Pivot_List)
    for i in range(0, len(H_Weyl)):
        H=H_Weyl[i]
        Tau=Tau_bracket(H)[i]        #H=(qP^2|t|)^1/5 if and only if HP^2|t|>=1. Likewise H=q^1/6 if and only if HP^2|t|<= 1
        Weyl=VDC_Weyl_bracket(H)[i] #NOTE: Need a version of this function which either depends on H or just outputs both possibilities
        Function=Multi_Add([[n-5, 3, None, -float(2)/3, -float(3)/4], CRF_Mult(H,float(n-2)/2), CRF_Mult(Tau, 2), CRF_Mult(Weyl, float(n-1)/8)]) #-2/3 is included in the first term due to that being saved in the q sum's cubefull part.
        Pivot_List.append([[Function], length])
        length=length+1
    return(Pivot_List) #NOTE to self: The final part of line 166 seems to be wrong at a glance... Correction in progress

def PVDC_Function_Builder(Pivot_List, n):
    length=len(Pivot_List)
    for i in range(0, len(H_Weyl)):
        H=H_Weyl[i]
        Weyl=VDC_Weyl_bracket(H)[i]
        Function=CRF_Add([n, 3, 2, -float(2)/3, -float(3)/4], CRF_Mult(Weyl,float(n-1)/8))
        Pivot_List.append([[Function], length])
        length=length+1
    return(Pivot_List)

def Weyl_Function_Builder(Pivot_List, n):
    length=len(Pivot_List)
    for i in range(0, 3): #There are 3 different values that the Weyl bracket can take: Weyl_2, Weyl_min[0], or Weyl_min[1]
        if i!=2:
            Weyl=Weyl_min[i]
            Function=CRF_Add([n, 3, 2, -float(2)/3, -float(3)/4], CRF_Mult(Weyl, float(n-1)/16))
            Pivot_List.append([[Function], length])
            length=length+1
        else:
            Weyl=Weyl_2
            Function=CRF_Add([n, 3, 2, -float(2)/3, -float(3)/4], CRF_Mult(Weyl, float(n-1)/16))
            Pivot_List.append([[Function], length])
            length=length+1
    return(Pivot_List)


#Plane Construction
#In this section, we aim to build a function which creates a list of every possible linear function our piecewise linear bound function can take (this will be represented in CRF). At the end of this list, we also add the planes which define the boundary of the domain.

def Domain_Conditions_List(Domain, l): #Domain will be in CRF
    length=l
    Domain_Conditions=[]
    for i in range(0,len(Domain)):
        Domain_Conditions.append([[[Domain[i][0],i+1]],length]) #The "i+1" is to keep track of which variable the condition is associated to. We use i+1 instead of i because the domain has 3 elements in CRF. The first element of the domain is associated to phi. However phi is associated to the second element of a list in CRF, so we need to add 1 so that this matches up. NOTE TO SELF: check whether this is actually necessary when the dust settles...
        length=length+1
        Domain_Conditions.append([[[Domain[i][1],i+1]],length])
        length=length+1
    return(Domain_Conditions) #Come back to this in a bit

def Function_List_Builder(Domain, n, ep):
    Temp=copy.deepcopy(Domain)
    Function_List=AVDC_Poisson_Function_Builder([],n,ep)
    Function_List=AVDC_Function_Builder(Function_List,n)
    Function_List=PVDC_Poisson_Function_Builder(Function_List,n,ep)
    Function_List=PVDC_Function_Builder(Function_List,n)
    Function_List=Weyl_Function_Builder(Function_List,n)
    Function_List.extend(Domain_Conditions_List(Temp,len(Function_List))) #Need to build a Domain conditions function!
    return(Function_List)

#Intersections

def Intersection_List(Prev_Int, List): #Need to be very careful with the input!!! Needs to be of form [ [ [[1,1]], 0 ] , [ [[2,2]], 1] , [ [[3,3]], 2] , [ [[4,4]], 3] ]. <-- Use this for test function? In my case, instead of [1,1] etc, I will have the functions list in CRF. "Intersection" will equal the functions list initially. Then apply this function recursively equal to the number of variables.
    New_Int=[]
    for i in range(0,len(Prev_Int)):
        for j in range(Prev_Int[i][1]+1,len(List)):
            Temp=Prev_Int[i][0][:]
            Temp.extend(List[j][0])
            New_Int.append([Temp,j])
    return(New_Int)

def Intersections_Builder(Function_List, var):
    Intersections=copy.deepcopy(Function_List) #may need copy/deepcopy? Since I only have to do this a few times and Function_List is small for a computer, this causes little-to-no time loss.
    for i in range(0,var):
        Temp=copy.deepcopy(Function_List)
        Intersections=Intersection_List(Intersections, Temp)
    return(Intersections)

#Conditions Finder

#NOTE: CHECK THIS COMMENT. Pivot has changed so may not be correct anymore. The setup for this function is as follows: It takes a parameter which is written as a max or a min (e.g. log_P(H) = max{10/(n-2)+ep, (2+6phi)/(n+2)+ep}) in CRF. It only works for parameters with two components (for more than two, a for loop is necessary). This function - for a particular variable - aims to determine what value that variable must equal in order for both components to equal each other, assuming all other variables are fixed. In the example of H, there is only one variable present in either component (phi), so we will let this be our free variable. The input for "Variables" is [1,None,0,0] (the final two entries can be anything in principle since they don't appear in H). The function then determines the value that phi must equal in order for 10/(n-2)+ep=(2+6phi)/(n+2)+ep to be true.

def Pivot_Assigner(X):
    To_Pivot=[]
    Conditions=[]
    for i in range(0,len(X)):
        if Number_Check(X[i][0])==1: #i.e. if Intersections[i][0][j][0] is an integer or a float. In this case, Intersections[i][0][j] is one of the functional values that our piecewise linear bound function can take. Note to self: Improve the explanation here...
            To_Pivot.append(X[i])
        else:
            Conditions.append(X[i])
    return([To_Pivot,Conditions])

def Pivot_Variable_Candidate(List1, List2):
    pivot_variable=None
    for i in range(1,len(List1)):
        if List1[i]!=None or List2[i]!=None:
            pivot_variable=i
    return(pivot_variable)

def Pivot_Data(List1,List2):
    pivot_variable=Pivot_Variable_Candidate(List1, List2)
    while pivot_variable!=None and List1[pivot_variable]==List2[pivot_variable]: #If the pivot variable of the 2 functions have the same coefficient, when we set these functions equal to each other, the pivot variable will vanish via subtraction. Here, we perform this subtraction by setting the pivot variable coefficients to None.
        List1[pivot_variable]=None
        List2[pivot_variable]=None
        pivot_variable=Pivot_Variable_Candidate(List1, List2) #We now find the next pivot variable candidate
    return([List1, List2, pivot_variable]) #This returns the (potentially) altered lists and the true pivot variable.

def Pivot(List_1, List_2, pivot_variable):
    Temp=None_to_Zero_List([List_1[:],List_2[:]]) #Convert None's into 0's so that we can perform arithmetic
    Plane=[None]*len(List_1)
    for i in range(0,len(List_1)):
        if type(Temp[0])!=list or type(Temp[1])!=list or Number_Check(pivot_variable)==0:
            print(Temp, pivot_variable)
        for j in range(0, len(List_1)):
            if Number_Check(Temp[0][j])==0 or Number_Check(Temp[1][j])==0:
                print(Temp, pivot_variable)
        denom=Temp[0][pivot_variable]-Temp[1][pivot_variable]
        if i!=pivot_variable:
            num=Temp[1][i]-Temp[0][i]
            Plane[i]=float(num)/denom
    Plane=Zero_to_None(Plane)
    return([Plane, pivot_variable])

def Intersect_Check(List_of_Lists):
    for i in range(0, len(List_of_Lists)): #If two of the functions coincide, or if we have something like [[1,2,3,4], [x, 1, None, 1], [y, 1, None, 1]], then the function returns 0. If two or more functions coincide, then setting them equal gives us a line/plane/hyperplane, instead of a point. We don't need to consider this since we can remove the functions which coincide and replace them with other functions which do not, giving us the critical points on this line/plane/hyperplane (this is covered by other elements of our intersection list). In the latter case, there is no pivot variable. This translates to the planes not intersecting each other simulataneously, and so there is no point to check.
        for j in range(i+1, len(List_of_Lists)):
            Temp=copy.deepcopy(List_of_Lists)
            if Temp[i]==Temp[j] or Pivot_Data(Temp[i],Temp[j])[2]==None:
                return(0)
    return(1)

def Functions_to_Conditions_Converter(List_of_Lists):
    if len(List_of_Lists)==0: #This can only happen if the collection of 4 functions are all taken from the boundary conditions, leading to an intersection of 4 distinct planes (impossible in R^3). NOTE to self: I don't know how to explain this nicely, think about this more...
        return(0)
    if len(List_of_Lists)==1: #if the length 1, then this function returns 0 (which tells Condition_Finder to ignore this data). This is how it should be since this for loop is intended to set 2 (or more) functions equal to each other and extract the condition(s) that this imposes on our variables.
        return([])
    Conditions=[]
    if Intersect_Check(List_of_Lists)==0:
        return(0)
    else:
        for i in range(1, len(List_of_Lists)):
            Temp=copy.deepcopy(List_of_Lists[0])
            Pivot_data=Pivot_Data(Temp,List_of_Lists[i])
            Conditions.append(Pivot(Pivot_data[0], Pivot_data[1], Pivot_data[2]))
        return(Conditions)


def Condition_List(Intersections):
    Conditions_List=[]
    for i in range(0, len(Intersections)):
        Temp_1=Pivot_Assigner(Intersections[i][0])
        To_Pivot=Temp_1[0]
        Conditions=Temp_1[1]
        Temp_2=Functions_to_Conditions_Converter(To_Pivot)
        if Temp_2!=0:
            Conditions.extend(Temp_2)
            Conditions_List.append(Conditions)
    return(Conditions_List)

def CL_Error_Check(Conditions_List, var):
    for i in range(0, len(Conditions_List)):
        if len(Conditions_List[i])!=var:
            print("Error Conditions_List not in correct form")
            print(Conditions_List[i], i)
            break
        for j in range(0,var):
            if len(Conditions_List[i][j])!=2 or Conditions_List[i][j][1]==None:
                print("Error Conditions_List not in correct form")
                print(Conditions_List[i], i)
                break

#Standardise Conditions

def Separate_Conditions(Conditions):
    S_Conditions=[[] for i in range(0, len(Conditions))]
    for i in range(0, len(Conditions)):
        if type(Conditions[i][1])==list or Conditions[i][1]==0 or Conditions[i][1]==None:
            print(Conditions)
        S_Conditions[Conditions[i][1]-1].append(Conditions[i][0]) #Conditions_List should be of form [ [[*,*,*,*], number], [[*,*,*,*], number], [[*,*,*,*], number] ]. Number determines which variable was pivoted. These elements represent pivot variable= c_1+c_2*var1+...+c_x*varx, where x="pivot variable"-1. This line of code separates out the conditions into separate lists for each pivot variable.
    return(S_Conditions)

def List_Collapse(List_of_Lists):
    New_List=[]
    for i in range(0,len(List_of_Lists)):
        New_List.append(List_of_Lists[i][0])
    return(New_List) 

def Standardise_Conditions(Conditions):
    variables=len(Conditions)
    intersection=1 #If the Conditions (which are determined by the intersection of 3 planes) do not give us a unique point, intersection will be set to 0. This will tell the main algorithm that this particular set of conditions should be discarded. Clearly if the 3 planes don't intersect, we will not be able to find a critical point to test. In the case that we get more than a unique point, that implies that at least 2 planes are identical, leading to a line/plane of points. The critical points will therefore be where this line (or plane) intersects with the domain, but there will be another set of conditions corresponding to this.
    S_Conditions=Separate_Conditions(Conditions) #Converts [ [[*,*,*,*], number], [[*,*,*,*], number], [[*,*,*,*], number] ] into [ [[*,*,*,*], ...], [[*,*,*,*], ...], [[*,*,*,*], ...] ], where the first list of lists contains all conditions which had the "number" 1 attached to them, etc.
    for i in range(variables-1,-1,-1): #ranging from var-1 to 0 (decreasing at each step)
        c_length=len(S_Conditions[i])
        if c_length>i+1 or c_length==0: #Variable i can depend on all variables before it (so variable i-1 etc). If c_length>i, then there are more than i conditions, but each of these conditions is in terms of only i variables. This implies that either there is a repeated condition (we can ignore this), or there are more equations than variables, leading to a contradiction.
            intersection=0
            return(0)
        if c_length>1:
            for j in range(c_length-1, 0, -1): #Due to how Python allocates memory when altering lists, it is more efficient to go backwards through the list instead of forwards.
                if Intersect_Check([S_Conditions[i][0],S_Conditions[i][j]])==0:
                    intersection=0
                    break
                else:
                    Temp=copy.deepcopy(S_Conditions[i][0])
                    Pivot_data=Pivot_Data(Temp,S_Conditions[i][j])
                    Temp_2=Pivot(Pivot_data[0], Pivot_data[1], Pivot_data[2]) #Let's say I have conditions which correspond to sigma=c_1+c_2*phi, sigma=d_1+d_2*phi. These equations can (and should) be solved simultaneously to get a unique phi and sigma. In this case S_Conditions[1]=[[c_1,c_2,None,None], [d_1,d_2,None,None]]. To solve the equations, we must use the Pivot function on these lists. After that, our conditions become phi=c, sigma=c_1+c_2*phi (Pivoting returns [[c, None, None, None],1]). We delete one of our sigma conditions (as this is equiv. to phi=c), and add [c,None,None,None] to the list of conditions corresponding to phi.
                    S_Conditions[Temp_2[1]-1].append(Temp_2[0]) #Python's lists start from position 0, so we use Temp[1]-1 for the first variable instead of Temp[1] (recall that Temp[1] gives us the position of the pivot variable).
                    del(S_Conditions[i][j])
    if intersection==0:
        return(0)
    else:
        return(List_Collapse(S_Conditions))

def Standardise_Conditions_List(Conditions_List):
    Standardised_Conditions_List=[]
    for i in range(0,len(Conditions_List)):
        Temp=Standardise_Conditions(Conditions_List[i])
        if Temp!=0:
            Standardised_Conditions_List.append(Temp)
    return(Standardised_Conditions_List)

def SC_Error_Check(S_Conditions, var):
    for i in range(0, len(S_Conditions)):
        if len(S_Conditions[i])!=var:
            print("Error S_Conditions not in correct form")
            print(S_Conditions[i], i)
            break
        for j in range(0,var):
            if type(S_Conditions[i][j])!=list and len(S_Conditions[i][j])!=var+1:
                print("Error S_Conditions not in correct form")
                print(S_Conditions[i][j], i,j)
                break
            for k in range(0,var+1):
                if S_Conditions[i][j][k]!=None and Number_Check(S_Conditions[i][j][k])==0:
                    print("Error S_Conditions not in correct form")
                    print(S_Conditions[i][j][k], i,j,k)
                    break


#Converting Conditions into Critical Points

def Conditions_to_Point(Standardised_Conditions):
    Point=[]
    variables=len(Standardised_Conditions)
    Temp=None_to_Zero_List(Standardised_Conditions)
    for i in range(0, variables):
        Point.append(Temp[i][0])
        for j in range(i+1, variables):
               Temp[j][0]=Temp[j][0]+Temp[j][i+1]*Point[i]
    return(Point)

def Conditions_to_Point_List(Standardised_Conditions_List):
    Points_List=[]
    length=len(Standardised_Conditions_List)
    for i in range(0, length):
        Points_List.append(Conditions_to_Point(Standardised_Conditions_List[i]))
    return(Points_List)

def CP_Error_Check(Points_List, var):
    for i in range(0,len(Points_List)):
        if len(Points_List[i])!=var:
            print("Error S_Conditions not in correct form")
            print(Points_List[i], i)
            break
        for j in range(0,var):
            if Points_List[i][j]!=Number_Check(Points_List[i][j])==0:
                print("Error S_Conditions not in correct form")
                print(Points_List[i], i)
                break

#Finding Critical Points in Domain

def Domain_Check(Point, Domain): #Point is of the form [phi,sigma,phi3]. This function checks whether this point lies within the domain or not. Domain is a triple list: It contains a list of end-points for the variables (phi, sigma, phi3 in our case), and each endpoint is in CRF. My domain is [[0, 3/2], [-4, -phi-0.75], [0, phi]] in NF.
    check=1
    for i in range(0, len(Point)):
        lower=List_Evaluate(Domain[i][0], Point)
        upper=List_Evaluate(Domain[i][1], Point) #This changes the upper domain limit of variable i into NF. For example if we have point [1,-3,0], and we wanted to look at the upper domain limit of sigma(=log_P|z|), this is -phi-0.75. The value for phi is Point[0] since this is the particular value of phi that we are considering.
        if Point[i]<lower or Point[i]>upper:
            check=0
            break
    return(check) #Domain_Check returns 0 if Point lies outside of the domain, and returns 1 if it lies within the domain.

def Points_in_Domain(Points_List, Domain):
    Valid_Points=[]
    for i in range(0, len(Points_List)):
        if Domain_Check(Points_List[i], Domain)==1:
            Valid_Points.append(Points_List[i])
    return(Valid_Points)

#Bound functions

def AVDC_Poisson_Bound(Point, n, ep):
    H=CRF_Max(H_Poisson(n,ep), Point)
    V=CRF_Max(V_bracket(H), Point)
    Tau=CRF_Max(Tau_bracket(H), Point)
    Y=CRF_Max(Y_bracket(H, V, n), Point) # This Y is actually not correct because we are now including (q_3+V)^n as a separate bracket.
    Bound=Multi_Add([[n-5,3, None, None, None],CRF_Mult(H,-1-float(n)/2),CRF_Mult(Tau,2),CRF_Mult(Y,float(1)/2)]) #Note for added bounf efficiency, can replace [n-5,3, None, None] with [n-5,3, None, -float(2)/3]. This will probably make the algorithm run slower though.
    return(Bound)

def AVDC_Weyl_Bound(Point, n):
    H=CRF_Max(H_Weyl, Point)
    Tau=CRF_Max(Tau_bracket(H), Point)
    Weyl=CRF_Min(VDC_Weyl_bracket(H), Point)
    Bound=Multi_Add([CRF_Mult(Tau,2), CRF_Mult(Weyl, float(n-1)/8), CRF_Mult(H,-1), [n-5, 3, None, -float(2)/3,-float(3)/4]])
    return(Bound)

def PVDC_Poisson_Bound(Point, n, ep):
    H=CRF_Max(H_Poisson(n,ep), Point)
    V=CRF_Max(V_bracket(H), Point)
    Y=CRF_Max(Y_bracket(H, V, n), Point) # This Y is actually not correct because we are now including (q_3+V)^n as a separate bracket.
    Bound=Multi_Add([[n, 3, 2, None, None], CRF_Mult(H, -float(n)/2), CRF_Mult(Y, float(1)/2)]) #Note for added bounf efficiency, can replace [n-5,3, None, None] with [n-5,3, None, -float(2)/3]. This will probably make the algorithm run slower though.
    return(Bound)

def PVDC_Weyl_Bound(Point, n):
    H=CRF_Max(H_Weyl, Point)
    Weyl=CRF_Min(VDC_Weyl_bracket(H), Point)
    Bound=CRF_Add([n, 3, 2, -float(2)/3, -float(3)/4], CRF_Mult(Weyl,float(n-1)/8))
    return(Bound)

def Weyl_Bound(Point, n):
    Bracket=CRF_Max([Weyl_2,CRF_Min(Weyl_min, Point)], Point)
    Bracket=CRF_Mult(Bracket, float(n-1)/16)
    Bracket=CRF_Add(Bracket, [n,3,2, -float(2)/3, -float(3)/4])
    return(Bracket)

def Dyadic_Bound(Point, n, ep):
    Temp=CRF_Min([AVDC_Poisson_Bound(Point, n, ep), AVDC_Weyl_Bound(Point, n), Weyl_Bound(Point, n), PVDC_Poisson_Bound(Point, n, ep), PVDC_Weyl_Bound(Point, n)], Point)
    return(List_Evaluate(Temp, Point))

def Critical_Points_Bound(Points, n, d, ep):
    max_value=0
    Second_max=[0]
    Bad_Points=[]
    for i in range(0,len(Points)):
        point_bound=Dyadic_Bound(Points[i], n, ep)
        if point_bound>max_value:
            max_value=point_bound
        if point_bound>=n-d:
            Bad_Points.append(Points[i])
        if point_bound>Second_max[0] and point_bound<max_value:
            Second_max=[point_bound, Points[i]]
    return([max_value, Bad_Points, Second_max])

#Final Function

def Minor_Arcs_Bound(Domain, n_lower, n_upper, d, ep, var):
    start=time.time()
    Minor_Arcs_Data=[]
    for n in range(n_lower, n_upper+1):
        Function_List=Function_List_Builder(Domain, n, ep)                               #This creates a list of all potential functions that our (piecewise linear) Dyadic bound function can take, and then appends the conditions associated to the Domain onto the end of this list.
        print("Function_List completed")
        Intersections=Intersections_Builder(Function_List, var)                          #This creates a list of all possible (var+1)-tuples which are made by taking any var+1 elements of Function_List.
        del(Function_List)                                                               #This List is no longer needed, so delete it to improve memory efficiency.
        print("Intersections completed")
        Conditions_List=Condition_List(Intersections)                             #Each (var+1)-tuple is associated to the intersection of "var" hyperplanes in R^var. In my case, var=3, and the dimension of the planes is 2. Intersections_Builds constructs the set of "var" restrictions (3 in my case) that this simultaneous intersection forces onto the variables. It does not check whether these conditions are contradictory (i.e. if there is no simultaneous intersection), or whether these conditions are not unique (i.e. if the planes coincide with each other).
        del(Intersections)                                                               #This List is no longer needed, so delete it to improve memory efficiency.
        print("Conditions_List completed")
        CL_Error_Check(Conditions_List, var)
        Standardised_Conditions_list=Standardise_Conditions_List(Conditions_List)         #Condition_Finder firstly determines whether or not the planes are distinct (not linear combinations of each other... maybe just distinct?), and whether they intersect similutaneously with each other. For collections of 3 distinct planes which intersect simultaneously, "Standardise_Conditions" converts each set of conditions to be of the form "variable1=[*,None,... None], variable2=[*,*, None,... None],... variable_var=[*,...*, None]". Since we have "var" restrictions, and "var" variables, this set of conditions will define a point.
        del(Conditions_List)                                                             #This List is no longer needed, so delete it to improve memory efficiency.
        print("Standardised_Conditions_List completed")
        SC_Error_Check(Standardised_Conditions_list, var)
        Points_List=Conditions_to_Point_List(Standardised_Conditions_list)               #This function converts sets of standardised conditions into points the lie in R^var (Lists of length var form [variable1,...,variable_var])
        del(Standardised_Conditions_list)                                              #This List is no longer needed, so delete it to improve memory efficiency
        print("Conditions_to_Point_List completed")
        CP_Error_Check(Points_List, var)
        Critical_Points=Points_in_Domain(Points_List, Domain)                            #This function checks whether each point lies in our domain and throws out the ones which do not lie in it.
        del(Points_List)                                                                #This List is no longer needed, so delete it to improve memory efficiency.
        print("Points_in_Domain completed")
        M_Arcs_Bound=Critical_Points_Bound(Critical_Points, n, d, ep)                      #This puts each point into our Dyadic bound. It then returns both the largest value it takes over these points and the list of points which have a value greater than or equal to n-d (d=6 in my case).
        del(Critical_Points)                                                      #This List is no longer needed, so delete it to improve memory efficiency.
        Minor_Arcs_Data.append([n, M_Arcs_Bound[0], "Bad points", M_Arcs_Bound[1], "Second Max", M_Arcs_Bound[2]]) #We then record this data.
        del(M_Arcs_Bound)                                                              #This List is no longer needed, so delete it to improve memory efficiency.
    end=time.time()
    print(end-start)
    return(Minor_Arcs_Data)


############ Above functions need test functions. Functions below may now be redundant due to the more general functions above. #############



#Test Functions

def Random_Point_Tester(k,n,ep):
    max_value=0
    Bad_Points=[]
    start=time.time()
    for i in range(0,k):
        x=random.uniform(float(1)/7-0.001, 1.5)
        y=random.uniform(-10, -0.75-x)
        z=random.uniform(0, x)
        temp=Dyadic_Bound([x,y,z], n, ep)
        if temp>max_value:
            max_value=temp
        if temp>32.99815:
            Bad_Points.append([x,y,z, "value", temp])
    end=time.time()
    return(max_value, Bad_Points, end-start)


def Combining_Sections_Tester():
        Function_List=Function_List_Builder(Domain, 39, ep)                               #This creates a list of all potential functions that our (piecewise linear) Dyadic bound function can take, and then appends the conditions associated to the Domain onto the end of this list.
        Intersections=Intersections_Builder(Function_List, var)                          #This creates a list of all possible (var+1)-tuples which are made by taking any var+1 elements of Function_List.
        del(Function_List)                                                               #This List is no longer needed, so delete it to improve memory efficiency.
        Conditions_List=Condition_List(Intersections)                             #Each (var+1)-tuple is associated to the intersection of "var" hyperplanes in R^var. In my case, var=3, and the dimension of the planes is 2. Intersections_Builds constructs the set of "var" restrictions (3 in my case) that this simultaneous intersection forces onto the variables. It does not check whether these conditions are contradictory (i.e. if there is no simultaneous intersection), or whether these conditions are not unique (i.e. if the planes coincide with each other).
        del(Intersections)                                                               #This List is no longer needed, so delete it to improve memory efficiency.
        CL_Error_Check(Conditions_List, var)
        S_Conditions_List=Standardise_Conditions_List(Conditions_List)
        return(S_Conditions_List)
#Data=Combining_Sections_Tester()
#Data_copy=copy.deepcopy(Data)

def CRF_Add_Test():
    if CRF_Add([1, 2, None, 1.38, None],[None, 2.25,3, None, None])!=[1,4.25,3, 1.38, None]:
        print("CRF_Add Test failed ")

def Multi_Add_Test():
    if Multi_Add([[1, 2, None, 1.38, None],[None, 2.25,3, None, None],[1,2,3, None, None]])!=[2,6.25,6, 1.38, None]:
        print("Multi_Add Test failed ")

def CRF_Mult_Test():
    if CRF_Mult([1, 2, None, 1.38, None],2)!=[2,4, None, 2.76, None]:
        print("CRF_Mult Test failed ")

def List_Evaluate_Test():
    if List_Evaluate([1.5,-2,3,1], Test_Point)!=-8.25:
        print("List_Evaluate Test failed")

def CRF_to_NF_Test():
    if CRF_to_NF(H_Poisson(39,ep),Test_Point)!=[0.2703702702702703, 0.2683926829268293]:
        print("CRF_to_NF test 1 failed")
    if CRF_to_NF([[1,2,3,4],[0,1,3,2]],Test_Point)!=[-2.75, -5.25]:
        print("CRF_to_NF test 2 failed")

def CRF_Max_Test():
    if CRF_Max(H_Poisson(39,ep),Test_Point)!=H_Poisson(39,ep)[0]:
        print("CRF_Max test 1 failed")
    if CRF_Max([[1,2,3,4],[0,1,3,2]],Test_Point)!=[1,2,3,4]:
        print("CRF_Max Test 2 failed")

def CRF_Min_Test():
    if CRF_Min(H_Poisson(39,ep),Test_Point)!=H_Poisson(39,ep)[1]:
        print("CRF_Min test 1 failed")
    if CRF_Min([[1,2,3,4],[0,1,3,2]],Test_Point)!=[0,1,3,2]:
        print("CRF_Min Test 2 failed")

def None_to_Zero_Test():
    if None_to_Zero([1, None, 1.5, None])!=[1,0,1.5,0]:
        print("None_to_Zero test failed")

def None_to_Zero_List_Test():
    if None_to_Zero_List([[1, None, 1.5, None],[1, None, 1.5, None],[1, None, 1.5, None]])!=[[1,0,1.5,0],[1,0,1.5,0],[1,0,1.5,0]]:
        print("None_to_Zero test failed")

def Zero_to_None_Test():
    if Zero_to_None([1, 0, 1.5, 0])!=[1, None, 1.5, None]:
        print("Zero_to_None test failed")

def Zero_to_None_List_Test():
    if Zero_to_None_List([[1,0,1.5,0],[1,0,1.5,0],[1,0,1.5,0]])!=[[1, None, 1.5, None],[1, None, 1.5, None],[1, None, 1.5, None]]:
        print("None_to_Zero test failed")

def Domain_Conditions_List_Test():
    if Domain_Conditions_List(Domain,0)!=[[[[[0.14185714285714285, None, None, None], 1]], 0], [[[[1.5, None, None, None], 1]], 1], [[[[-2.858142857142857, None, None, None], 2]], 2], [[[[-0.75, -1, None, None], 2]], 3], [[[[0, None, None, None], 3]], 4], [[[[None, 1, None, None], 3]], 5]]:
        print("Domain Conditions List test failed")

def Function_List_Builder_Test():
    if len(AVDC_Poisson_Function_Builder([],39,ep))!=48:
        print("AVDC_Poisson_Function_Builder incorrect length")
    if len(AVDC_Function_Builder([],39))!=2:
        print("AVDC_Weyl_Function_Builder incorrect length")
    if len(Weyl_Function_Builder([], 39))!=3:
        print("Weyl_Function_Builder incorrect length")
    if len(Function_List_Builder(Domain, 39, ep))!=59:
        print("Function_Builder incorrect length")

#NOTE: It would be good to test whether a random sample of the lists generated by Function_List_Builder are correct.

def Intersection_List_Test():
    List=Function_List_Builder(Domain, 39, ep)
    if len(Intersection_List(List,List))!=1711:
        print("Intersection List incorrect length")
    if Intersection_List([ [ [[1,1]], 0 ] , [ [[2,2]], 1] , [ [[3,3]], 2] ], [ [ [[1,1]], 0 ] , [ [[2,2]], 1] , [ [[3,3]], 2] ])!=[ [ [[1,1],[2,2]], 1], [ [[1,1],[3,3]], 2], [ [[2,2],[3,3]], 2]]:
        print("Intersection List  test failed")

def Intersections_Builder_Test():
    List=Function_List_Builder(Domain, 39, ep)
    if len(Intersections_Builder(List,3))!=455126:
        print("Intersections Builder incorrect length")
    if Intersections_Builder([ [ [[1,1]], 0 ] , [ [[2,2]], 1] , [ [[3,3]], 2] ], 2)!=[ [ [[1,1],[2,2],[3,3]], 2]]:
        print("Intersection List  test failed")

def Pivot_Assigner_Test():
    if Pivot_Assigner([[1,2,3,4], [1.5, 2.3, 7, float(1)/4], [[1,1,1,1],2], [0,0,0,0], [[1, float(1)/5, 2, 3.1],0] ])!=[ [ [1,2,3,4], [1.5, 2.3, 7, float(1)/4], [0,0,0,0] ], [ [[1,1,1,1],2], [[1, float(1)/5, 2, 3.1],0] ] ]:
        print("Pivot_Assigner_Test failed")

def Pivot_Variable_Candidate_Test():
    if Pivot_Variable_Candidate([1,None ,2.1, None],[2, 3, None, None])!=2:
        print("Pivot_Variable_Candidate_Test 1 failed")
    if Pivot_Variable_Candidate([1,None ,None, None],[2.3, None, None, None])!=None:
        print("Pivot_Variable_Candidate_Test 2 failed")

def Pivot_Data_Test():
    if Pivot_Data([1, 2, float(3)/2, 4],[2, 7.1 ,1.5 ,4])!=[[1,2, None, None],[2, 7.1, None, None],1]:
        print("Pivot_Data_Test failed")

def Pivot_Test():
    if Pivot([1,2,None, None],[2, 7.1, None, None], 1)!=[[-float(1)/5.1, None, None, None], 1]:
        print("Pivot_Test 1 failed")
    if Pivot([1,2,None,4], [0,None,2,3], 3)!=[[-1, -2, 2, None], 3]:
        print("Pivot_Test 2 failed")

def Functions_to_Conditions_Converter_Test():
    if Functions_to_Conditions_Converter([])!=0:
        print("Functions_to_Conditions_Converter_Test 'no list' failed")
    if Functions_to_Conditions_Converter([[1,2,3,4]])!=[]:
        print("Functions_to_Conditions_Converter_Test '1 list' failed")
    if Functions_to_Conditions_Converter([[1,2,3,5], [1.1, 2, 3, 4], [float(11)/10, 2 ,3 , 4]])!=0:
        print("Functions_to_Conditions_Converter_Test 'Lists equal' failed")
    if Functions_to_Conditions_Converter([[1,2,3,5], [1, 2, 3, 4], [float(11)/10, 2 ,3 , 4]])!=0:
        print("Functions_to_Conditions_Converter_Test 'No pivot variable' failed")
    if Functions_to_Conditions_Converter([[1,2,3,None], [1, 2.1, 3, 4], [float(11)/10, 2, 4, None]])!=[[[None, -0.025000000000000022, None, None], 3],[[-0.10000000000000009, None, None, None], 2]]: #There is a very small numerical error occuring here (should be -0.025 and -0.1), but the function is giving the correct output other than this.
        print("Functions_to_Conditions_Converter_Test 'Main' failed")

A=[ [1,2,3,4], [1,1,1,1], [2,1,1,1], [[3, None, None, None], 1] ]
B=[ [1,1,1,1], [1,2,3,4], [1,1,1,1], [[3, None, None, None], 1] ]

C=[ [1,2,3,4], [1,2,1,1], [2,1,1,1], [[3, None, None, None], 1] ]
C_answer=[ [[3, None, None, None], 1], [[None, None, -float(2)/3, None], 3], [[float(1)/3, -float(1)/3, -float(2)/3, None], 3]]

D=[ [1,2,3,4], [1,1,3,4], [2,1,1,1], [3, None, None, None] ]
D_answer=[ [[None, None, None, None], 1], [[float(1)/3, -float(1)/3, -float(2)/3, None], 3], [[0.5, -0.5, -0.75, None], 3]]

E=[ [1,2,3,4], [[3, None, None, None], 1], [[3, None, None, None], 2], [[3, None, None, None], 3] ]
E_answer=[ [[3, None, None, None], 1], [[3, None, None, None], 2], [[3, None, None, None], 3]]

def Condition_List_Test():
    if Condition_List([ [A,3], [B,4], [C,5], [D,6], [E,7] ])!=[C_answer, D_answer, E_answer]:
        print("Condition_Finder_Test failed")

def Separate_Conditions_Test():
    if Separate_Conditions(D_answer)!=[[[None, None, None, None]], [], [[float(1)/3, -float(1)/3, -float(2)/3, None], [0.5, -0.5, -0.75, None]]]:
        print("Separate_Conditions_Test failed")

def Standardise_Conditions_Test():
    TC_answer=copy.deepcopy(C_answer)
    TD_answer=copy.deepcopy(D_answer)
    TE_answer=copy.deepcopy(E_answer)
    if Standardise_Conditions(TC_answer)!=0:
        print("Standardise_Conditions_Test 1 failed")
    if Standardise_Conditions(TD_answer)!=[[None, None, None, None], [1.9999999999999993, -1.9999999999999993, None, None], [float(1)/3, -float(1)/3, -float(2)/3, None] ]: #2nd term should be [2,-2, None, None]. A tiny numerical error occurs due to dividing.
        print("Standardise_Conditions_Test 2 failed")
    if Standardise_Conditions(TE_answer)!=[[3, None, None, None], [3, None, None, None], [3, None, None, None]]:
        print("Standardise_Conditions_Test 3 failed")

def Standardise_Conditions_List_Test():
    TC_answer=copy.deepcopy(C_answer)
    TD_answer=copy.deepcopy(D_answer)
    TE_answer=copy.deepcopy(E_answer)
    if Standardise_Conditions_List([TC_answer, TD_answer, TE_answer])!=[ [[None, None, None, None], [1.9999999999999993, -1.9999999999999993, None, None], [float(1)/3, -float(1)/3, -float(2)/3, None] ], [[3, None, None, None], [3, None, None, None], [3, None, None, None]] ]:
        print("Standardise_Conditions_Test 1 failed")

def Conditions_to_Point_Test():
    TA=[[None, None, None, None], [2, -2, None, None], [float(1)/3, -float(1)/3, -float(2)/3, None]]
    if Conditions_to_Point(TA)!=[0, 2, -1]:
        print("Conditions_to_Point_Test failed")

def Conditions_to_Point_List_Test():
    TA=[[None, None, None, None], [2, -2, None, None], [float(1)/3, -float(1)/3, -float(2)/3, None]]
    TB=[[3, None, None, None], [3, None, None, None], [3, None, None, None]]
    if Conditions_to_Point_List([TA,TB])!=[[0, 2, -1],[3,3,3]]:
        print("Conditions_to_Point_List_Test failed")

def Domain_Check_Test():
    if Domain_Check([1,2,0.5], Domain)!=0:
        print("Domain_Check_Test 1 failed")
    if Domain_Check([1,-2,0.5], Domain)!=1:
        print("Domain_Check_Test 2 failed")

def Points_in_Domain_Test():
    if Points_in_Domain([[1,2,0.5], [1,-2,0.5]], Domain)!=[[1,-2,0.5]]:
        print("Points_in_Domain_Test failed")



def H_Poisson_Test():
    if H_Poisson(39,ep)!=[[0.2703702702702703, None, None, None], [0.04888048780487805, 0.14634146341463414, None, None]]:
        print("H_Poisson test 1 failed")

def V_bracket_Test():
    if V_bracket(CRF_Max(H_Poisson(39,ep),Test_Point))!=[ [-0.23981486486486486, 0.5, None, None], [-1, 1, None, None], [None, None, None, None] ]:
        print("V_bracket test 1 failed")
    if V_bracket(CRF_Max(H_Poisson(43,ep),Test_Point))!=[ [-0.35272777777777775, 0.5666666666666667, None, None], [-1, 1, None, None], [None, None, None, None] ]:
        print("V_bracket test 2 failed")

#H_1=CRF_Max(H_Poisson(39,ep),Test_Point)
#V_1=CRF_Max(V_bracket(H_1), Test_Point)

#def Tau_bracket_Test():
#    if Tau_bracket(H_1)!=[ [2.2703702702702704, None, 1, None], [None]*4 ]:
#        print("Tau_bracket test failed")

#def Y_bracket_Test():
#    if Y_bracket(H_1,V_1, 39)!=[ [1.4314756756756761, -1.0, None, None], [10.54444054054054, -20.0, None, 19.0], [0.2703702702702703, -1, None, None], [None, None, None, None] ]:
#        print("Y_bracket test 1 failed")

def VDC_Weyl_bracket_Test():
    H=CRF_Max(H_Weyl, Test_Point)
    if VDC_Weyl_bracket(H)!=[ [-float(8)/5, -float(4)/5, -float(4)/5, None], [float(4)/5, -float(3)/5, float(2)/5, None] ]:
        print("VDC_Weyl_bracket_Test failed")

def AVDC_Poisson_Bound_Test():
    if AVDC_Poisson_Bound(Test_Point, 39, ep)!=[32.99815, 3, 2, None]:
        print("AVDC_Poisson_Bound_Test failed")

def AVDC_Weyl_Bound_Test():
    if AVDC_Weyl_Bound(Test_Point, 39)!=[30.599999999999998, -0.7000000000000006, -1.7000000000000004, -0.6666666666666666]: #very small numerical errors present.
        print("AVDC_Weyl_Bound_Test failed")

def PVDC_Poisson_Bound_Test():
    if PVDC_Poisson_Bound(Test_Point, 39, ep)!=[33.72777972972973, 3, 2, None]:
        print("PVDC_Poisson_Bound_Test failed")

def PVDC_Weyl_Bound_Test():
    if PVDC_Weyl_Bound([1.5,-3,1], 39)!=[39, -0.25000000000000044, 2, -0.6666666666666666]: #very small numerical errors present.
        print("PVDC_Weyl_Bound_Test failed")

def Weyl_Bound_Test():
    if Weyl_Bound(Test_Point, 39)!=[39, 7.875, 6.875, -float(2)/3]:
        print("Weyl_Bound_Test failed")

def Dyadic_Bound_Test():
    if Dyadic_Bound(Test_Point, 39, ep)!=32.99815:
        print("Dyadic_Bound_Test failed")

def Critical_Points_Bound_Test():
    if Critical_Points_Bound([Test_Point], 39, 6, ep)[0:2]!=[32.99815, []]:
        print("Critical_Points_Bound_Test failed")




#CRF_Add_Test()
#Multi_Add_Test()
#CRF_Mult_Test()
#List_Evaluate_Test()
#CRF_to_NF_Test()
#CRF_Max_Test()
#CRF_Min_Test()
#None_to_Zero_Test()
#None_to_Zero_List_Test()
#Zero_to_None_Test()
#Zero_to_None_List_Test()
#Domain_Conditions_List_Test()
#Function_List_Builder_Test()
#Intersection_List_Test()
#Intersections_Builder_Test()
#Pivot_Assigner_Test()
#Pivot_Variable_Candidate_Test()
#Pivot_Data_Test()
#Pivot_Test()
#Functions_to_Conditions_Converter_Test()
#Condition_List_Test()                       #Formerly not commented
#Separate_Conditions_Test()
#Standardise_Conditions_Test()
#Standardise_Conditions_List_Test()
#Conditions_to_Point_Test()
#Conditions_to_Point_List_Test()
#Domain_Check_Test()
#Points_in_Domain_Test()
#H_Poisson_Test()                      #Formerly not commented
#V_bracket_Test()                      #Formerly not commented
#Tau_bracket_Test()                      #Formerly not commented
#Y_bracket_Test()                      #Formerly not commented
#VDC_Weyl_bracket_Test()                      #Formerly not commented
#AVDC_Poisson_Bound_Test()                     #Formerly not commented
#AVDC_Weyl_Bound_Test()                     #Formerly not commented
#PVDC_Poisson_Bound_Test()                     #Formerly not commented
#PVDC_Weyl_Bound_Test()                     #Formerly not commented
#Weyl_Bound_Test()                     #Formerly not commented
#Dyadic_Bound_Test()                     #Formerly not commented
#Critical_Points_Bound_Test()                     #Formerly not commented


#Implementation

ep=0.0001
var=4
delta=float(1)/7-0.001 #Delta is in the range (0,1/7), so take Delta=1/7-0.001
Test_Var1=[1,1.5,-2.25,0] #(constant, phi, sig, phi_3)
Test_Point=[1.5,-2.25,0]
Domain=[ [[delta, None, None, None, None],[1.5, None, None, None, None]], [[-10, None, None, None, None],[-0.75, -1, None, None, None]], [[0, None, None, None, None],[None, 1, None, None, None]], [[0, None, None, None, None],[None, 1, None, -1, None]] ] #(const,q,z,b_3,q_4)
Domain_2=[ [[0, None, None, None, None],[delta, None, None, None, None]], [[-3+delta, None, None, None, None],[-0.75, -1, None, None, None]], [[0, None, None, None, None],[None, 1, None, None, None]], [[0, None, None, None, None],[None, 1, None, -1, None]] ]
Domain_Spec=[ [[0.5, None, None, None, None],[1.5, None, None, None, None]], [[-10, None, None, None, None],[-0.75, -1, None, None, None]], [[0, None, None, None, None],[None, 1, None, None, None]], [[0, None, None, None, None],[None, 1, None, -1, None]] ]
Major_Arcs=[ [[0, None, None, None],[delta, None, None, None]], [[-10, None, None, None],[-3+delta, None, None, None]], [[0, None, None, None],[None, 1, None, None]] ]
n_lower=39
n_upper=39
d=6
n=39


#print(V_bracket(CRF_Max(H_Poisson(39,ep),Test_Point)))
print(Minor_Arcs_Bound(Domain, n_lower, n_upper, d, ep, var))

#Test_Point2=[1.5, -2.2514943968358594, 0.0]

#print(AVDC_Poisson_Bound(Test_Point, n, ep))
#print(AVDC_Weyl_Bound(Test_Point, n))
#print(Weyl_Bound(Test_Point, n))
#print(PVDC_Poisson_Bound(Test_Point, n, ep))
#print(PVDC_Weyl_Bound(Test_Point, n))

#print(List_Evaluate(AVDC_Poisson_Bound(Test_Point, n, ep),Test_Point))
#print(List_Evaluate(AVDC_Weyl_Bound(Test_Point, n),Test_Point))
#print(List_Evaluate(Weyl_Bound(Test_Point, n),Test_Point))
#print(List_Evaluate(PVDC_Poisson_Bound(Test_Point, n, ep),Test_Point))
#print(List_Evaluate(PVDC_Weyl_Bound(Test_Point, n),Test_Point))
#
#print(List_Evaluate(AVDC_Poisson_Bound(Test_Point2, n, ep),Test_Point2))
#print(List_Evaluate(AVDC_Weyl_Bound(Test_Point2, n),Test_Point2))
#print(List_Evaluate(Weyl_Bound(Test_Point2, n),Test_Point2))
#print(List_Evaluate(PVDC_Poisson_Bound(Test_Point2, n, ep),Test_Point2))
#print(List_Evaluate(PVDC_Weyl_Bound(Test_Point2, n),Test_Point2))

#Test_Point3=[0.7850512333965844, -2.7374293168880426, 0.5650794102801481]
#print(Dyadic_Bound(Test_Point3, n, ep))
#print(List_Evaluate(AVDC_Poisson_Bound(Test_Point3, n, ep),Test_Point3))
#print(List_Evaluate(AVDC_Weyl_Bound(Test_Point3, n),Test_Point3))
#print(List_Evaluate(Weyl_Bound(Test_Point3, n),Test_Point3))
#print(List_Evaluate(PVDC_Poisson_Bound(Test_Point3, n, ep),Test_Point3))
#print(List_Evaluate(PVDC_Weyl_Bound(Test_Point3, n),Test_Point3))

#print(Weyl_Bound(Test_Point3, n))
