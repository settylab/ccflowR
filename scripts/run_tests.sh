#!/bin/bash
# Run tests for ccflowR package

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_DIR="$(dirname "$SCRIPT_DIR")"

# Micromamba paths
MAMBA_EXE="/usr/local/bin/micromamba"
MAMBA_ROOT="$HOME/micromamba"

echo "Running ccflowR tests..."
echo "Package directory: $PACKAGE_DIR"

# Run tests using micromamba
"$MAMBA_EXE" -r "$MAMBA_ROOT" run -n ccflowR_test Rscript -e "
  setwd('$PACKAGE_DIR')
  devtools::load_all()
  devtools::test()
"

echo "Tests completed!"
