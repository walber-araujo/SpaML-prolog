#!/bin/bash
# run_lint.sh - Run Prolog lint and fail if any error or warning is detected.

# Run the Prolog linter and capture both stdout and stderr.
OUTPUT=$(swipl -q -t halt -l lint.pl 2>&1)

# Print the output so it shows in the GitHub Actions log.
echo "$OUTPUT"

# Check for "ERROR:" lines in the output.
if echo "$OUTPUT" | grep -q "ERROR:"; then
    echo "Errors were detected in the lint output."
    exit 1
fi

# Check for "Warning:" lines in the output.
if echo "$OUTPUT" | grep -q "Warning:"; then
    echo "Warnings were detected in the lint output."
    exit 1
fi

# If no errors or warnings were found, exit successfully.
exit 0
