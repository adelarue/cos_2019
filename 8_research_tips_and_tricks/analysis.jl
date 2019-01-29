using CSV, DataFrames
using ScikitLearn
using Random, Statistics
using JLD2

# Import the necessary modules and libraries from ScikitLearn
@sk_import ensemble:RandomForestClassifier

"""
	Function that trains a random forest given the data and the parameters
	Returns training and validation R^2
"""
function trainrandomforest(train::DataFrame, val::DataFrame,
						   max_depth::Int, n_estimators::Int, max_features::Int)
	Xtr = convert(Matrix, train[2:end])
	Ytr = train[1]
	Xva = convert(Matrix, val[2:end])
	Yva = val[1]
	rf = RandomForestClassifier(max_depth=10, n_estimators=100, max_features=4)
	fit!(rf, Xtr, Ytr)
	predictTr = predict(rf, Xtr)
	predictVa = predict(rf, Xva)
	R2tr = 1 - sum((predictTr .- Ytr) .^ 2)/sum((Ytr .- mean(Ytr)) .^ 2)
	R2va = 1 - sum((predictVa .- Yva) .^ 2)/sum((Yva .- mean(Ytr)) .^ 2)
	return R2tr, R2va
end

train = CSV.read("listings_clean_train.csv", header=false)
val = CSV.read("listings_clean_val.csv", header=false)

@load "params.jld2" params

experiment_id = parse(Int, ARGS[1])

Random.seed!(1776)
for i = 1:nrow(params)
	if params[i, :experiment_id] != experiment_id
		continue
	end
	R2tr, R2va = trainrandomforest(train, val, params[i, :max_depth],
	                               params[i, :n_estimators], params[i, :max_features])
	@save "results-$experiment_id.jld2" R2tr R2va
end
