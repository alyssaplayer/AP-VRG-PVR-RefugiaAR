testing

git fetch --all
git checkout master
git merge # at the end of this step, your local copy of `master` and the remote copy should be in sync. You can verify this by running `git log -1` - and all the copies of master should show up in the metadata
git checkout header
git merge # at the end of this step, your local copy of `master` and the remote copy should be in sync. IF you don't have any differences (ie incoming changes), you can skip this step.
git rebase master # This might rebase correctly or cause conflicts - if do, you will have to resolve them
git push --force-with-lease origin header
