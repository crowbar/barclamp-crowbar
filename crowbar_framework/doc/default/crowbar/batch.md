# `crowbar-batch`

This is the documentation for the
[`crowbar batch`](../bin/crowbar_batch) subcommand.

## Description

As the name suggests, `crowbar batch` is intended to be run in "batch
mode", i.e. mostly unattended, and can be used to accurately capture
the configuration of an existing Crowbar environment, or drive Crowbar
to build a complete new environment from scratch.

`crowbar batch` has two modes of operation:

*   `crowbar batch export`

    Exports a YAML file which describes existing proposals and how
    their parameters deviate from the default proposal values for
    that barclamp.

*   `crowbar batch build`

    Imports a YAML file in the same format as above, and uses it to
    build new proposals if they don't yet exist, and then update the
    existing proposals so that their parameters match those given in
    the YAML file.

## YAML file format

Here is an example YAML file.  At the top-level, there is a
`proposals` array, each entry of which is a hash representing a
proposal:

```yaml
proposals:
- barclamp: provisioner
  # Proposal name defaults to 'default'.
  attributes:
    shell_prompt: USER@ALIAS:CWD SUFFIX
- barclamp: database
  # Default attributes are good enough, so we just need to assign
  # nodes to roles:
  deployment:
    elements:
      database-server:
        - @@controller1@@
- barclamp: rabbitmq
  deployment:
    elements:
      rabbitmq-server:
        - @@controller1@@
```

### Top-level proposal attributes

*   `barclamp` — name of the barclamp for this proposal (required)

*   `name` — name of this proposal (optional; defaults to `default`)

    In `build` mode, if the proposal doesn't already exist, it will be
    created.

*   `attributes` — an optional nested hash containing any attributes for
    this proposal which deviate from the defaults for the barclamp

    In `export` mode, any attributes set to the default values are
    excluded in order to keep the YAML as short and readable as
    possible.

    In `build` mode, these attributes are
    [*deep-merged*](https://docs.chef.io/attributes.html#about-deep-merge)
    with the current values for the proposal.  If the proposal didn't
    already exist, `batch build` will create it first, so the
    attributes are effectively merged with the default values for the
    barclamp's proposal.

*   `wipe_attributes` — an optional array of paths to nested attributes
    which should be removed from the proposal

    Each path is a period-delimited sequence of attributes; for
    example `pacemaker.stonith.sbd.nodes` would remove all SBD nodes
    from the proposal if it already exists.  If a path segment
    contains a period, it should be escaped with a backslash, e.g.
    `segment-one.segment\.two.segment_three`.

    This removal occurs *before* the deep merge described above, so
    for example a `batch build` with a YAML file which included
    `pacemaker.stonith.sbd.nodes` in `wipe_attributes` of a
    `pacemaker` barclamp proposal would ensure that at the end of the
    run, *only* SBD nodes listed in the `attributes` sibling hash
    would be used.  In contrast, without the `wipe_attributes` entry,
    the SBD nodes given would be appended to any SBD nodes already
    defined in the proposal.

*   `deployment` — a nested hash defining how and where this proposal
    should be deployed

    In `build` mode, this hash is deep-merged in the same way as the
    `attributes` hash, except that the array of `elements` for each
    Chef role is reset to the empty list before the deep merge.  This
    special exception may change in the future.

### Node alias substitutions

Any string anywhere in the YAML which is of the form `@@foo@@` and
where `foo` is a node alias will be substituted for the name of that
node.  For example if `controller1` is a Crowbar alias for node
`d52-54-02-77-77-02.mycloud.com`, then `@@controller1@@` will be
substituted for that hostname.  This allows YAML files to be reused
across environments.

## Options

In addition to the standard options available to every `crowbar`
subcommand (run `crowbar batch --help` for a full list), there are
some extra options specifically for `crowbar batch`:

*   `--include <barclamp[.proposal]>` — only include
    the barclamps / proposals given

    This option can be repeated multiple times.  The inclusion value
    can either be the name of a barclamp (e.g. `pacemaker`) or a
    specifically named proposal within the barclamp
    (e.g. `pacemaker.network_cluster`).

    If it is specified at all, then only the barclamps / proposals
    specified are included in the `build` or `export` operation, and
    all others are ignored.

*   `--exclude <barclamp[.proposal]>` — exclude the barclamps /
    proposals given

    This option can be repeated multiple times.  The exclusion value
    is the same format as for `--include`.  The barclamps / proposals
    specified are excluded from the `build` or `export` operation.

*   `--timeout <seconds>` — change the timeout for Crowbar API calls

    As Chef run_lists grow, some of the later OpenStack barclamp
    proposals (e.g. `nova`, `horizon`, `heat` can take over 5 or even
    10 minutes to apply), so you may need to increase this timeout
    to 900 seconds in some circumstances.
