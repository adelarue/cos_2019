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
	df = DataFrame(experiment_id = [], max_depth = [],
	               max_features = [], n_estimators = [])
	experiment_id = 0
	# loop through all combinations
	for depth in depths, mf in maxfeatures, nest in numestimators
		experiment_id += 1
		# append a new row to the dataframe
		df = [df;
			  DataFrame(experiment_id = [experiment_id],
			            max_depth = [depth],
	           			max_features = [mf],
	           			n_estimators = [nest])]
	end
	return df
end

# Let's get these parameters into the right format
params = getparameters(depths, maxfeatures, numestimators)
# and now let's save them to a JLD file
@save "params.jld2" params

# Let's make a folder to save the results (if necessary)
try mkdir("results") catch e end
try mkdir("logs") catch e end

