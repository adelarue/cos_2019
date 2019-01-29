# Session 8: Research Tips & Tricks

This session will cover topics that are useful in research, from obtaining results to presenting them.

## Before the session

1. Create a free Mendeley account at https://www.mendeley.com/?interaction_required=true and download Mendeley's desktop client from https://www.mendeley.com/download-desktop/.

2. Install a LaTeX distribution on your computer (platform-specific instructions can be found at https://www.latex-project.org/get/#tex-distributions)

3. [Sloan/ORC only] If you are affiliated with Sloan or the ORC, you have access to the Engaging cluster. In order to login, you first need to request an account by emailing stshelp@mit.edu.

Once your account has been created, login to engaging.
> If you're using Mac/linux, type `ssh <user_name>@eosloan.mit.edu` into the terminal and enter your password.
> If you're using Windows, you will need to use Putty (as in session 1). Set the host name to `eosloan.mit.edu` and click "Open".

<!---
In this session, we will use julia, and we need to install a few packages. Once you've logged into Engaging, run the following commands (don't worry if you don't understand what we're doing here - we'll discuss during the session):
  1. `module load engaging/julia/0.6.1`
  2. `srun --pty --partition=sched_any_quicktest --cpus-per-task=1 --mem=2G julia` (Note: this may take a few minutes depending on how busy the cluster is)
  3. (At this point, a julia session should be running, so we're going to install the packages we need. This will take a few minutes.)
  `Pkg.add("DecisionTree")`, `Pkg.add("ScikitLearn")`, and `Pkg.add("JLD")`
  7. `using DecisionTree, ScikitLearn, JLD` (make sure there are no errors loading them)
  8. `exit()`
  9. `logout`
-->
