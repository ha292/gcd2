# Submission for Project 2

## Result dataset
The result is in [result.csv](result.csv)

## Codebook
The codebook is described in [codebook.txt](codebook.txt)

## Script 
The scrip is in [run_analysis.R]

### How does the script work?

The script is organized a series of functions that are tied together by a "Main". The idea is similar to that of a pipleline.
The functions mirror the requirements.
Functions are
 * loadAndMerge() - Merges the training and the test sets to create one data set.
 * extractColumns() - Extracts only the measurements on the mean and standard deviation for each measurement. 
 * addSubject() - adds the subject column to the dataset
 * addActivities() - reads the activity labels and populates the activity column via merge()
 * createAverageDataFrame() - averages the variables
 * meltAndLabelVariables() - Metls the dataframe by subject and activity and creates the following columns from variable name   
  * sensor - which sensor is being measured (Gyroscope, Accelerometer)
  * component - Component of the measure from signal processiing (Body, Gravitiy)
  * domain - Domain of the measure -- (Time, Frequency)
  * var - name of the measure - (X, Y, Z, Jerk, JerkMag, Mag)
  * aggregation_type -- which aggregation was applied to the values  (mean, std)
 * writeDataSet() -- writes the result out		
