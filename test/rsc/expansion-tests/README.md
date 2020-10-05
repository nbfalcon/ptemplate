# About

This subfolder contains "expansion tests". Each directory (non-directories are
ignored) shall contain the following:

- `template/`: expanded to a temporary directory
- `result/`: for the test to succeed, `template/` must expand to a directory
  recursively equal to this directory
  
Most of the time, the documentation of tests will be in their `.ptemplate.el`
files, since a `;;; Commentary: ` block is needed to suppress warnings.
