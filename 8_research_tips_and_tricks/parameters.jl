using DataFrames, JLD2

# Here we define the parameters we want to try (define the grid)
# We will try every possible combination of these parameters
depths = collect(2:2:20)
maxfeatures = collect(2:2:10)
numestimators = [20, 40, 80, 160]

"""
	This function is going to create a DataFrame,
	where each row corresponds to the parameters for one experiment
"""
function getparameters(depths, maxfeatures, numestimators)
	df = DataFrame()
	# TODO




	return df
end

# Let's get these parameters into the right format
params = getparameters(depths, maxfeatures, numestimators)
# and now let's save them to a JLD file


# Let's make a folder to save the results (if necessary)


