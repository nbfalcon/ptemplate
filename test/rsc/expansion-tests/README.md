# About

This subfolder contains "expansion tests". These are executed by the ert-test
`ptemplate-expansion`, and work as follows:

Each directory in this folder shall contain two subdirectories, `template/` and
`result/`. Additional files (README.md) are allowed and ignored. `template/` is
expanded to a temporary directory and then compared against `result/`. If these
directories match exactly (all files and subfolders are the same, compared
recursively), then that expansion test succeeds, failing otherwise.
