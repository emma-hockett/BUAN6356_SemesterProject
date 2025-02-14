
# Getting the Data frames set up correctly

# Reading in the data from a raw formatted GitHub link
Employee_Office_df <- read.csv("https://raw.githubusercontent.com/emma-hockett/BUAN6356_SemesterProject/refs/heads/main/Dataset/Employee_office_survey.csv")
HR_Employee_df <- read.csv("https://raw.githubusercontent.com/emma-hockett/BUAN6356_SemesterProject/refs/heads/main/Dataset/HR%20Employee%20data.csv")
Job_Position_df <- read.csv("https://raw.githubusercontent.com/emma-hockett/BUAN6356_SemesterProject/refs/heads/main/Dataset/Job_position_structure.csv")
Office_Codes_df <- read.csv("https://raw.githubusercontent.com/emma-hockett/BUAN6356_SemesterProject/refs/heads/main/Dataset/Office_codes.csv")


# Renaming the columns in the data frames to reflect the features
colnames(Employee_Office_df) <- c("Employee_ID", "Office_Code", "Rated_Year", "Rating")
colnames(HR_Employee_df) <- c("Employee_ID", "Joining_Year", "Age", "Business_Travel", "Daily_Rate", "Department", 
                              "Distance_From_Home", "Education","Employee_Count", "Employee_Number", "Environment_Satisfaction",
                              "Gender", "Hourly_Rate", "Job_Involvement", "Job_Satisfaction","Marital_Status", "Monthly_Income",
                              "Monthly_Rate", "Num_Companies_Worked", "Over_18", "Overtime", "Percent_Salary_Hike", "Performance_Rating",
                              "Relationship_Satisfaction", "Standard_Hours", "Stock_Option", "Total_Working_Years", "Training_Times_Last_Year",
                              "Work_Life_Balance", "Years_At_Company", "Years_In_Role", "Years_Since_Promotion", "Years_With_Manager",
                              "Attrition", "Leaving_Year", "Reason", "Relieving_Status", "Office_Code", "Job_Level")
colnames(Job_Position_df) <- c("Department", "Level", "Job_Role")
colnames(Office_Codes_df) <- c("Office_Code", "City", "Province", "Country")



# Cleaning up some of the Data

# Dropping the Employee Name which is constant across all employees
HR_Employee_df <- HR_Employee_df[, !names(HR_Employee_df) %in% c("Employee_Count", "Over_18", "Standard_Hours", "Employee_Number")]

# Changing some of the ordinal categorical data into ordinal numerical data
HR_Employee_df$Overtime <- ifelse(HR_Employee_df$Overtime == "Yes", 1, 0)
HR_Employee_df$Attrition <- ifelse(HR_Employee_df$Attrition == "Yes", 1, 0)
HR_Employee_df$Business_Travel <- ifelse(HR_Employee_df$Business_Travel == "Non-Travel", 0, ifelse(HR_Employee_df$Business_Travel == "Travel_Rarely", 1, 2))

# Changing the Job level from L7-L1 to just 7-1
HR_Employee_df$Job_Level <- as.numeric(gsub("L", "", HR_Employee_df$Job_Level))




# Checking the Data Quality

# Some of the Empty fields are stored as "" instead of NULL so converting those to NULL to get proper view of the data
HR_Employee_df[HR_Employee_df == ""] <- NA


#Checking the completeness of the data
colSums(is.na(HR_Employee_df))


# Checking for Data Uniqueness 
any(duplicated(HR_Employee_df))


#Checking the validity of various columns to make sure they fit make sense

# Making sure the ages make sense 
min(HR_Employee_df$Age)
max(HR_Employee_df$Age)

# Make sure the distance from home is reasonable amounts
min(HR_Employee_df$Distance_From_Home)
max(HR_Employee_df$Distance_From_Home)

# Checking the percentiles of certain numerical variables to ensure they are viable numbers
quantile(HR_Employee_df$Daily_Rate)
quantile(HR_Employee_df$Hourly_Rate)
quantile(HR_Employee_df$Monthly_Rate)
quantile(HR_Employee_df$Total_Working_Years)
quantile(HR_Employee_df$Years_At_Company)

# Checking logical requirements of the data 
HR_Employee_df[HR_Employee_df$Attrition == "1" & is.na(HR_Employee_df$Leaving_Year)& is.na(HR_Employee_df$Relieving_Status)& is.na(HR_Employee_df$Reason)]





