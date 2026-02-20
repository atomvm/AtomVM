<!--
 Copyright 2026 Davide Bettio <davide@uninstall.it>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Git Workflow and History Style Guide

**Version**: 1.0

## Overview

This document defines how we write commit messages, structure git history, and shape pull requests (
PRs).

The goals are simple:

* Make history readable.
* Make reviews fast.
* Make `git bisect` reliable.
* Make maintenance (cherry-picks, forward ports, revert, backports) boring.

## Commit Messages

### The subject line

Rules:

* Max 50 characters.
* Capitalized.
* No trailing period.
* Imperative mood (think: a command).

Your subject line must complete the sentence:

"If applied, this commit will ..."

Good:

* Refactor subsystem X for readability
* Update getting started documentation
* Remove deprecated APIs
* Release version 1.0.0

Bad:

* Fixed bug with Y
* Changing behavior of X

Very bad:

* More fixes for broken stuff
* Sweet new API methods
* 42
* Fix the fix

### Standard verbs

Use a small, stable set of verbs. This keeps history scannable.

Allowed verbs:

* Add: Create a capability (feature, test, dependency).
* Remove: Remove a capability.
* Fix: Fix an issue (bug, typo, accident, misstatement).
* Change: Change behavior.
* Bump: Increase the version of something (often a dependency).
* Make: Change build/tooling/infra.
* Start: Begin doing something (often behind a flag).
* Stop: End doing something (often removing a flag).
* Refactor: Refactoring only. No behavior change.
* Reformat: Formatting only. No behavior change.
* Optimize: Performance refactor.
* Document: Documentation change.

Do not invent your own subject prefixes or tags. In particular, do not use:

* `jira://...`
* `[bug] ...`
* `(release) ...`
* `#12345 ...`

### Optional area prefix

If it helps navigation, you may prefix the subject with a short area name:

* `CI: Fix flaky test on Linux`
* `Doc: Remove broken link`
* `MainWindow: Change title`
* `main.c: Remove unused helper`
* `Context: Stop using deprecated field`

Rules for area prefixes:

* Keep them short and stable.
* Use Title Case (or a file name), followed by `: `.
* The action verb still applies (it is the first word after the prefix).

### The body

A body is not required for trivial commits. If you find yourself repeating the subject line, the
body is likely noise.

If you include a body:

* Leave a blank line between subject and body.
* Wrap paragraphs at 72 columns.
* Answer **why** first. The "why" is the part you will not recover from `git show`.
* Add "what" only when it helps understanding.
* Avoid narrating the implementation (the code already does that).
* If the diff is noisy (generated code, mechanical changes, large move/rename), describe what
matters.

Put issue tracker references at the bottom:

* `Fixes: #123`
* `See also: #456, #789`

### Commit message template

Use this shape:

```
Summarize the change in 50 characters or less

Explain why this change is needed, what problem it solves, and any
non-obvious consequences. Wrap at 72 columns.

Optional extra paragraphs after blank lines.

- Bullet lists are OK if they add clarity.

Fixes: #123
See also: #456
```

### Use the body for reasoning, not the diff

When the change is non-trivial, the body should save future readers time.

Example (generic):

```
Simplify error handling in the serializer

Remove the redundant error state tracking from the serializer stream.
The previous logic always raised an exception immediately, so keeping
a separate state flag provided no value and obscured the control flow.

This also removes dead code paths that were only reachable through
the unused state transitions.
```

## Generated Code and Tooling Output

When a generator is involved:

* The subject describes **what changed**.
* The exact command goes in the body.

Example:

```
Add database migration for album genres

run:
tool gen migration create_album_genre --unique
```

Critical rule:

* Never edit generated output before committing it.

Workflow:

1. Run the generator.
2. Commit the generator output as-is.
3. Make manual adjustments in a second commit.

This keeps `git show` honest: reviewers can separate "what the tool did" from "what you changed by
hand". It also makes it possible to re-run the generator later and re-apply manual edits with
confidence.

## Git History

Git history is not a dump of work-in-progress commits. It is the set of logical steps from A to B.

### Keep commits as logical steps

* Do not squash commits that represent different logical steps.
* Use fixup commits only when they carry near-zero information (missing `;`, typo, trivial
correction).
* Before merging, use interactive rebase to clean up and streamline the series.

A reviewer should be able to review a PR "by layers", commit-by-commit, from the bottom up. Each
commit should explain a coherent move and keep the diff reviewable.

### Each commit must build

At minimum:

* Every commit must compile/build.

Better:

* Tests are green for each commit.

If the only way to make progress is to introduce intermediate commits that do not build, do not.
Combine those steps into a larger commit that builds. This is the price we pay for reliable `git
bisect`.

### Prefer a predictable commit order

When possible, arrange commits in this order:

1. Reformat
2. Bug fix
3. Refactor
4. Feature / behavior change

This makes review easier. A reviewer can validate the reformat mechanically, then focus on bug
semantics, then refactor safety, then the feature itself.

## Pull Requests

* A PR should be focused on a single topic.
* The PR title must clearly say what it does.
* If the PR contains more than one commit, the PR description should include a short summary. Treat
it as the future merge commit body.
* PRs must be self-contained: code, tests, and documentation move together. Reverting the PR should
revert the feature completely, including docs and tests.

## Reformatting and Code Formatters

Formatting an existing codebase is a destructive operation. It has costs:

* `git blame` becomes less useful.
* Patches become harder to apply.
* Forward ports and backports become harder.
* Reverts become noisier.

Rules:

* Prefer gradual formatting when working in the same area anyway.
* If you do a large reformat, isolate it:

  * Formatting-only PR.
  * Automated changes only.
  * No functional changes mixed in.

## Diff Hygiene and General Style

* Whitespace errors are evil. Configure your editor to strip trailing whitespace.
* Keep diffs minimal:

  * No unrelated whitespace churn.
  * No drive-by changes outside the purpose of the commit.
  * Small diffs are easier to review, revert, and cherry-pick.
* Use correct US English.
* Markdown is fine in commit bodies and PR descriptions. Keep it simple.

## Appendix

This appendix is optional. Not every project needs release branches, a changelog, or an explicit
updating guide.

### A. Semantic Versioning

Use Semantic Versioning (SemVer).

### B. main and release branches

A pragmatic model for projects with releases:

* `main` is for development (the next release).
* Create maintenance branches per release series: `release-X.Y`.

Rules:

* Before tagging an alpha for a new series, branch the release line:

  * Example: create `release-0.9` for the `0.9.x` series.
* Do not tag on `main`.
* Tag releases on the corresponding `release-X.Y` branch.
* Compatible fixes and reasonable additions may go into the release branch.
* Keep disruptive changes on `main`.

### C. Forward port fixes (release branch -> main)

If you maintain a release branch, fix bugs on the release branch and then forward port into `main`.

Example:

```
git checkout release-0.9
# apply fix
git commit

git checkout main
git merge --no-ff release-0.9
```

A good forward port merge commit message looks like:

```
Forward port changes from release-0.9

Merge fixes and minor changes from release-0.9, including:
- Fix flaky test in the network suite (#1234)
- Correct bounds checking in packet parser (#1237)
- Clarify configuration docs (#1240)
```

This model requires discipline:

* Avoid cherry-picking between branches unless there is a strong reason.
* Avoid merging `main` into release branches.
* Keep release branches boring and targeted.

### D. Changelog

A changelog is a user-facing document, not a copy of git history.

["Keep a Changelog"](https://keepachangelog.com/) is a solid baseline format.

Include:

* New features, removed features, and behavior changes.
* Notable fixes.
* Performance improvements only when users will notice them (for example: 25% faster, not 2%).

Exclude:

* Refactors.
* Internal code motion.
* Reformatting.
* Build chores that are only interesting to maintainers reading `git log`.

#### Changelog updates and commit boundaries

If a single commit introduces a user-visible change, update the changelog in that same commit. This
makes `git blame` on the changelog useful.

If the change is the result of multiple commits (a PR as a whole), update the changelog in a final
commit at the end of the PR.

The same logic applies to issue tracker keywords (`Fixes:`, `Closes:`):

* If one commit is responsible, put `Fixes:` in that commit.
* If the PR as a whole is responsible, put it in the PR description / merge commit message.

#### Multi-branch changelog shape

If you maintain multiple branches, you can structure it like:

```
# Changelog
All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog,
and this project adheres to Semantic Versioning.

## Unreleased

### Added
- ...

## [1.2.3] - Unreleased

### Fixed
- ...

## [1.2.2] - 2025-06-23

### Added
- ...
```

### E. Updating guide

If upgrades require manual steps, keep an `UPDATING.md`.

Example:

```md
## v1.2.2 -> Unreleased
- `binary_to_integer` and `list_to_integer` no longer raise `overflow`.
  They now raise `badarg` when parsing integers larger than 256 bits.
  Update any relevant error handling.

## v1.2.0 -> v1.2.1
- ESP32: the partitioning schema for the "elixir" flavor changed.
  Update flashing offsets to use 0x250000 in your tooling.

```

### F. GPG signing

Some projects require GPG-signed commits to establish a clear chain of authorship.

Rules:

- If you sign commits with GPG, the signature must be created by a human.
- Do not let bots, CI, or automated tooling produce or apply signatures on your behalf.
  A machine can generate commits; it cannot take responsibility for them.

Practical workflow for automated commit series:

- Generate the commits locally.
- Before opening a PR, manually review the full series and then amend each commit
  so the final author attests to it and applies the signature.

One simple way is to rebase and amend every commit you are responsible for:

```sh
git rebase -i <base>
# mark all relevant commits as "edit"
git commit --amend --sign-off -S
git rebase --continue
# repeat until finished
```

### G. Sign-off (DCO style, optional)

Some projects require sign-off lines (Developer Certificate of Origin style).
A sign-off is not decoration. It is a statement of responsibility and intent.

Rules:

- Sign-off must be done by a human.
- Do not allow automated software to add sign-offs without human review.
  If a bot adds it, it is meaningless at best and misleading at worst.

When to use sign-off:

- When the project requires it (DCO).
- When contributing to projects that treat sign-off as a legal or policy signal.
- When you want a strong paper trail that the final author reviewed the change.

Manual approval workflow for automated commits:

1. Rebase interactively and stop on each commit you want to attest to.
2. Review the diff.
3. Amend the commit to add sign-off (and optionally GPG-sign it).

Example:

```sh
git rebase -i <base>
# mark all automated commits as "edit"
git commit --amend --sign-off -S
git rebase --continue
```

### H. More examples

#### Example: merge commit for a PR

```
Merge pull request #1234 from feature/bigint

Add 256-bit integer support

This change introduces support for integers up to 256 bits (sign plus
magnitude). It extends arithmetic and bitwise operations beyond the
previous 64-bit limit.

Highlights:
- New boxed integer representation with explicit sign handling
- Arithmetic ops updated: +, -, *, div, rem, abs, neg
- Bitwise ops updated: band, bor, bxor, bnot, bsl, bsr
- Conversion functions updated to handle larger integers
```

#### Example: focused commits within a feature

```
intn: Rename neg helper for clarity

The previous function name suggested an in-place mutation only, but it
also returned metadata used by callers. Rename it to match behavior and
remove the unused wrapper that relied on implicit array sizing.

Signed-off-by: Contributor Name <contributor@example.com>
```

```
intn: Use per-base maximum length table in to_string

Avoid allocating a worst-case buffer for every base. A small lookup
table provides tighter bounds and reduces transient allocations.

Signed-off-by: Contributor Name <contributor@example.com>
```
---

*Document Version*: 1.0
*Style Guide Name*: Git Workflow and History Style Guide
*Last Updated*: February 2026
