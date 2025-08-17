#!/bin/bash

# Helper script to download and update test snapshots from R-CMD-check workflow
# Usage: ./update-snapshots.sh [run_id]

set -e

# Use provided run ID or find latest successful R-CMD-check run
if [ -n "$1" ]; then
    RUN_ID="$1"
else
    echo "Finding latest R-CMD-check run on master..."
    RUN_ID=$(gh run list --workflow="R-CMD-check" --branch=master --status=completed --limit=10 --json databaseId,conclusion | jq -r '.[] | .databaseId' | head -1)
    
    if [ -z "$RUN_ID" ]; then
        echo "No successful R-CMD-check run found"
        exit 1
    fi
fi

echo "Using R-CMD-check run ID: $RUN_ID"

# Create temporary directory
TEMP_DIR=$(mktemp -d)
echo "Working in temporary directory: $TEMP_DIR"

# Function to download and process artifacts for a platform
process_platform() {
    local platform="$1"
    local artifact_pattern="$2"
    
    echo "Processing $platform platform..."
    
    gh run download $RUN_ID -p $artifact_pattern --dir "$TEMP_DIR/$platform"
    # List artifacts for this run
    # artifacts=$(gh run view $RUN_ID --json artifacts --jq '.artifacts[] | select(.name | test("'"$artifact_pattern"'")) | .name')
    
    #if [ -z "$artifacts" ]; then
    #    echo "No artifacts found matching pattern: $artifact_pattern"
    #    return 1
    #fi
    
    #echo "Found artifacts: $artifacts"
    
    # Download each matching artifact
    #for artifact in $artifacts; do
    #    echo "Downloading artifact: $artifact"
    #    gh run download $RUN_ID --name "$artifact" --dir "$TEMP_DIR/$platform"
        
    # Find and process snapshot files
    if [ -d "$TEMP_DIR/$platform" ]; then
        find "$TEMP_DIR/$platform" -path "*/tests/testthat/_snaps/*/validate-outputs/*" -name "*.new.*" -type f | while read -r new_file; do
            # Extract the relative path after _snaps/
            rel_path=${new_file#*/_snaps/}
            target_file="tests/testthat/_snaps/$rel_path"
            target_dir=$(dirname "$target_file")
                
            echo "Copying: $new_file -> $target_file"
                
            # Create target directory if it doesn't exist
            mkdir -p "$target_dir"
                
            # Copy the file
            cp "$new_file" "$target_file"
        done
    fi
    #done
}

# Process Windows platform
process_platform "windows" "Windows*testthat-snapshots"

# Process Linux release platform  
process_platform "linux" "Linux*rrelease*testthat-snapshots"

# Cleanup
echo "Cleaning up temporary directory: $TEMP_DIR"
rm -rf "$TEMP_DIR"

echo "Snapshot update complete!"
