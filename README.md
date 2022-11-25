# AutoAnnotatoR
An Automatic LC-MS/MS Data Analysis Workflow for Compound Annotation with AutoAnnotatoR

“R Step1” Instruction
1.	Data preparation
You need prepare a list of target ions (example list), including the information of retention time and m/z. Also 11 files about different CE optimization with .mgf format need to be uploaded.
2.	R running
Modify the data position and file name, define the acceptable deviation of retention time and m/z. then run the R package.
 
3.	Results
The optimal CE values were in the table of “optimal_HCD.xlsx”.

“R Step 2” Instruction
1.	Data preparation
You need prepare a list of target ions (example list), including the information of retention time, m/z and HCD. Also 11 files about different HCD optimization with .mgf format need to be uploaded.
2.	R running
Modify the data position and file name, define the acceptable deviation of retention time and m/z. then run the R package.
 
3.	Results
The data with optimal CE values were in the “new.mgf”.

“R Step3” Instruction
1.	Data preparation
You need prepare
List1: contains the information of diagnostic ions for each type.
 
List2: contains characteristic ions, neutral loss and corresponding scores.one sheet represents a type of compounds, and the sheet name is the name of type. Column 1 represents the characteristic ions, while column 3 represents the neutral loss. Column 2 and 4 represent the scores of corresponding ions.
 
example data.mgf: a mgf file contains the MS/MS data of all target ions.
2.	R running
Modify the data position and file name, define the acceptable deviation of m/z. then run the R package.
 
3.	Results
The results were in the table of “Score_result.xlsx”.

“R Step4” Instruction
1.	Data preparation
You need prepare
Compound_library_1: natural compound library, which includes the information of type, name, CAS, formula, m/z and Rt/min.
 
Compound_library_2: theorical compound library, which includes the information of type, name, formula, m/z.
 
example data.xlsx: the results of R Step3.
 
2.	R running
Modify the data position and file name, define the acceptable deviation of m/z and rt. then run the R package.
 
3.	Results
The results were in the table of “compound_result.xlsx”.

“R-formula” Instruction
1.	Data preparation
You need prepare a table with the format of “xlsx”, including the information of substituents (left) and skelecton (right), as well as their numbers of atoms: C H O N.
 
2.	R running
Modify the data position and file name, then run the R package.
 
3.	Results
As shown in the “Result out”, L1.xlsx contains all possible compounds after permutation and combination. L2.xlsx represents the result after removing repeated chemical formulas.


“R-Integration” Instruction
1.	Data preparation
You need prepare
example data.xlsx: multiple lists for the results of R.Step4.
 
2.	R running
Modify the data position, define the acceptable deviation of m/z and rt. then run the R package.
 
3.	Results
The results were in the table of “resultdata.xlsx”.
