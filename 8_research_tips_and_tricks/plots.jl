using JLD2, Plots

@load "params.jld2" params

R2_train = []
R2_val = []

for i = 1:nrow(params)
	experiment_id = params[i, :experiment_id]
	@load "results/results-$experiment_id.jld2" R2tr R2va
	push!(R2_train, R2tr)
	push!(R2_val, R2va)
end

params[:R2_train] = R2_train
params[:R2_val] = R2_val

dataforplot = params[(params[:n_estimators] .== 160 .& params[:max_features] .== 2)]

plot(params[:max_depth], [params[:R2_train] params[:R2_val]])
