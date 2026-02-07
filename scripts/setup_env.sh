#!/bin/bash
# Setup micromamba environment for ccflowR

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_DIR="$(dirname "$SCRIPT_DIR")"

# Micromamba paths
MAMBA_EXE="/usr/local/bin/micromamba"
MAMBA_ROOT="$HOME/micromamba"

echo "Creating ccflowR_test environment..."

# Create environment from yaml
"$MAMBA_EXE" -r "$MAMBA_ROOT" create -f "$PACKAGE_DIR/environment.yml" -y

echo "Environment created successfully!"
echo ""
echo "To activate: micromamba activate ccflowR_test"
echo "To run tests: $MAMBA_EXE -r $MAMBA_ROOT run -n ccflowR_test Rscript -e \"devtools::test('$PACKAGE_DIR')\""
