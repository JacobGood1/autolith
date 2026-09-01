---
name: get-update
description: >
  Fetch Lambda Symbolics Autolith into this fork, compare upstream
  changes against our local work, summarize the delta, then rebase
  those updates onto our branch. Use when the user runs /get-update,
  asks to pull main into the fork, sync upstream, or see what
  lambda-symbolics changed versus our Autolith fork.
argument-hint: ""
user-invocable: true
---

# Get update from upstream Autolith

This skill is only for the JacobGood1 Autolith fork checkout. Do not
use it in other repos.

## Remotes

Required layout:

- `upstream` → `https://github.com/lambda-symbolics/autolith.git`
- `origin` → `https://github.com/JacobGood1/autolith.git`

If a remote is missing or points elsewhere, stop and report the actual
URLs. Do not add remotes unless the user asks.

Never `git push` to `upstream`. Never force-push `upstream`.

## Procedure

1. Confirm this is a git checkout of the fork (`origin` and `upstream`
   as above). Abort otherwise.
2. `git status --short` and `git branch --show-current`. If tracked
   files are dirty, stop and list them. Do not stash unless the user
   asks.
3. `git fetch upstream` and `git fetch origin`.
4. Collect the comparison (use `upstream/master` as main; if that
   branch is missing, use `upstream/HEAD` and say so):

   ```sh
   base=$(git merge-base HEAD upstream/master)
   git log --oneline --no-merges HEAD..upstream/master
   git log --oneline --no-merges upstream/master..HEAD
   git diff --stat "$base"...upstream/master
   git diff --name-only "$base"..HEAD
   git diff --name-only "$base"..upstream/master
   ```

5. Write a summary **before** integrating:

   - Incoming from main: commit list, grouped by theme when there are
     more than a handful.
   - Our unique work: commit list.
   - Overlap: files changed on both sides (intersection of the two
     name-only lists). Call out likely conflict areas.
   - Counts: incoming commits, our commits, overlapping files.

6. If there is nothing incoming, say so and stop. Do not rebase.
7. If there is incoming work, rebase our branch onto
   `upstream/master`:

   ```sh
   git rebase upstream/master
   ```

   On conflict: stop, leave the rebase in place, list conflicted
   files, and summarize what each side changed. Do not
   `--skip`, `--abort`, or invent resolutions unless the user asks.
8. After a clean rebase, push to **origin only**:

   ```sh
   git push --force-with-lease origin HEAD
   ```

   Skip the push if origin is not the JacobGood1 fork. If the push
   fails, report the error; do not retry with a naked `--force`.
9. After integration, give a short result: new `HEAD`, whether the
   push to the fork succeeded, remaining unique commits, and any
   follow-up (conflicts, check command, dirty tree).

## Summary style

Lead with the answer: what main added, what is ours, whether they
collide. Use short tables or bullets. Quote commit subjects, not
hashes alone. Do not dump raw git output when a distilled list will
do.
