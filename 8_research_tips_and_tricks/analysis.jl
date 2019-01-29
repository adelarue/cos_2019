using CSV, DataFrames
using ScikitLearn
using Random, Statistics
using JLD2

# Import the necessary modules and libraries from ScikitLearn
@sk_import ensemble:RandomForestClassifier

# Import our training function which trains a random forest
include("rf.jl")

# Load the data for training and validation
train = CSV.read("listings_clean_train.csv", header=false)
val = CSV.read("listings_clean_val.csv", header=false)

# Load the parameters dataframe
@load "params.jld2" params

# Get the experiment ID from the arguments that were passed to julia
experiment_id = ifelse(length(ARGS) > 0, parse(Int, ARGS[1]), 1)

# Set a random seed - always a good idea when using functions you didn't write
Random.seed!(1776)
for i = 1:nrow(params)
	# skip all experiment ids except for the one we care about
	if params[i, :experiment_id] != experiment_id
		continue
	end
	R2tr, R2va = trainrandomforest(train, val, params[i, :max_depth],
	                               params[i, :n_estimators], params[i, :max_features])
	@save "results/results-$experiment_id.jld2" R2tr R2va
end
