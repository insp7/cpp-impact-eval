import pandas as pd
import re

data = open("C:/Users/Aniket Konkar/Desktop/year_up_stats--imputed.txt", "r")

# Define the file path
file_path = 'C:/Users/Aniket Konkar/Desktop/sample.csv'

df = {}

pattern_min = r"Min:\s*(\d+\.?\d*)"
pattern_mean = r"Mean:\s*(\d+\.?\d*)"
pattern_max = r"Max:\s*(\d+\.?\d*)"
# pattern_sd = r"SD:\s*(\d+\.?\d*)"
pattern_missing = r"Missing:\s*(\d+)"
pattern_non_missing = r"Non-missing:\s*(\d+)"
pattern_ones = r"1:\s*(\d+)"
pattern_zeros = r"0:\s*(\d+)"

lines = ['Variable,Min,Mean,Max,0s,1s,Missing,Non-missing']

while True:
    # Get next line from file
    line = data.readline()
    
    if not line:
        break
    
    if("Non-missing:" not in line):
        variable = line.strip()
        stats = data.readline().strip()
        
        min = mean = max = zeros = ones = "NA"
        
        if not stats:
            break
        
        if(variable == "participant_id"):
            missing = re.search(pattern_missing, stats).group(1)
            non_missing = re.search(pattern_non_missing, stats).group(1)
        else:
            missing = re.search(pattern_missing, stats).group(1)
            non_missing = re.search(pattern_non_missing, stats).group(1)
            
            if("Min" in stats):
                min = re.search(pattern_min, stats).group(1)
                mean = re.search(pattern_mean, stats).group(1)
                max = re.search(pattern_max, stats).group(1)
            else:
                zeros = re.search(pattern_zeros, stats)
                if zeros:
                    zeros = zeros.group(1)
                    min = "0"
                    
                ones = re.search(pattern_ones, stats)
                if ones:
                    ones = ones.group(1)
                    max = "1"
                else:
                    max = "0"
                    
                if ones and zeros:
                    mean = int(ones) / (int(ones) + int(zeros))

        lines.append("{},{},{},{},{},{},{},{}".format(variable, min, mean, max, zeros, ones, missing, non_missing))

# print(lines)

# Open the file in write mode
with open(file_path, 'w') as file:
    # Write each line to the file
    for line in lines:
        file.write(line + '\n')  # Add a newline character after each line

print(f"Data has been written to {file_path}")

# convert to excel
df = pd.read_csv("C:/Users/Aniket Konkar/Desktop/sample.csv")
 
# then to_excel method converting the .csv file to .xlsx file.
 
df.to_excel("C:/Users/Aniket Konkar/Desktop/year_up_stats--imputed.xlsx", sheet_name="year_up_stats--imputed", index=False)