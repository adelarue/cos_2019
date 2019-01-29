# Session 8: Research Tips & Tricks

This session will cover topics that are useful in research, from obtaining results to presenting them.

## Before the session

1. Create a free Mendeley account at https://www.mendeley.com/?interaction_required=true and download Mendeley's desktop client from https://www.mendeley.com/download-desktop/.

2. Install a LaTeX distribution on your computer (platform-specific instructions can be found at https://www.latex-project.org/get/#tex-distributions)

3. Install Sublime Text 3 from https://www.sublimetext.com/3.
Install the Sublime Package Control following these instructions: https://packagecontrol.io/installation#st3
Then, restart Sublime Text 3 and follow these instructions to install the LaTeXTools package: https://latextools.readthedocs.io/en/latest/install/#installation

4. [Sloan/ORC only] If you are affiliated with Sloan or the ORC, you have access to the Engaging cluster. In order to login, you first need to request an account by emailing stshelp@mit.edu.

Once your account has been created, login to engaging.
> If you're using Mac/linux, type `ssh <user_name>@eosloan.mit.edu` into the terminal and enter your password.
> If you're using Windows, you will need to use Putty (as in session 1). Set the host name to `eosloan.mit.edu` and click "Open".

In this session, we will use julia, and we need to install a few packages. Once you've logged into Engaging, run the following commands (don't worry if you don't understand what we're doing here - we'll discuss during the session):
  1. `srun --pty --partition=sched_any_quicktest --cpus-per-task=1 --mem=2G bash` (Note: this may take a few minutes depending on how busy the cluster is)
  2. `module load engaging/julia/0.6.1`
  2. `module load sloan/python/modules/2.7` (very important to include this line)
  3. `julia`
(At this point, a julia session will open, so we're going to install the packages we need. This will take a few minutes.)
  4. Enter the following commands:
```julia
julia> using Pkg

julia> Pkg.add("ScikitLearn")

julia> exit()
```
  5. Reopen the julia session: `julia`
  6. Precompile the ScikitLearn package we just installed (may take a few mins) as follows:
```julia
julia> using ScikitLearn

julia> @sk_import ensemble:RandomForestClassifier
```
  7. Now install the other packages we need with the following commands:
```julia
julia> using Pkg

julia> Pkg.add("DataFrames")

julia> Pkg.add("CSV")

julia> Pkg.add("JLD2")

julia> using DataFrames, CSV, ScikitLearn, JLD2

julia> exit()
```
  8. Disconnect from engaging by entering `exit`, then `logout`.
