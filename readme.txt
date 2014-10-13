################################################################################################################
# The script returns the data frame of matched codes 
# The function pairs outputs from RQDA of 2 coders based on intersections of respective coded segments of text
# If there is an intersection of 2 same codes, the codes are matched
# If there is an intersection of 2 different codes, the codes are matched
# If there is an intersection of more than 2 codes: 
# (1) the same codes are matched at first
# (2) the different codes remain in the original order
# If there is not an intersection (the code does not have its counterpart), the code is matched with 0/NA values
################################################################################################################
