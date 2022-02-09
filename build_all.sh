#!/usr/bin/env bash

## Assembles a directory tree of PDFs into a single PDF

## Each sub-PDF of the final product is bookmarked for easy navigation and OCR
## is performed to produce text-searchable images

## Set some default argument values
ROOT=${1:-Lectures/}
OUTPUT=${2:-Merged.pdf}
shift 2 # Drop the first two arguments
OCROPTS=${@:---redo-ocr} # Get the rest of the arguments

echo -n "Scanning for source PDFs..."
# The `<(expr)` is an example of process substitution. The output of expr is
# treated as if it were a file which is redirected (using `<`) into
# readarray. The options `-print0`, `-z`, and `-d ''` result in the use of NULL
# separators, allowing for newlines in filenames
readarray -d '' PDFS < <(find "$ROOT" -name "*.pdf" -print0 | sort -z)
echo "done."

echo -n "Generating bookmark file..."
PAGE=1
# Loop through all PDFs
for pdf in "${PDFS[@]}"; do
    # Get the length of the current PDF (in pages)
    len=$(pdftk "$pdf" dump_data | awk '/NumberOfPages/ { print $2 }')
    # Break each PDF path into an array of its components
    IFS="/"
    patharr=(${pdf#"$ROOT"})
    # Declares a dictionary / map / associative array named `seen`
    declare -A seen
    # Loop through the indices of the path-component array
    for i in "${!patharr[@]}"; do
        # Look up the current path component
        node="${patharr[$i]}"
        # Strip numbering – `11) ...`  – from the front of filenames 
        node="${node#[0-9]*) }"
        # Drop `.pdf` from the end of filenames
        node="${node%.pdf}"
        # Get the full path of the current node (from the root)
        loc="${patharr[@]:0:$((i + 1))}"
        # If this full path hasn't been seen before, then add a bookmark
        if [[ -z "${seen[$loc]}" ]]; then
            echo "BookmarkBegin" >> bookmarks.txt
            echo "BookmarkTitle: $node" >> bookmarks.txt
            echo "BookmarkLevel: $((i + 1))" >> bookmarks.txt
            echo "BookmarkPageNumber: $PAGE" >> bookmarks.txt
            # Add this path to `seen` so its bookmark isn't duplicated
            seen["$loc"]=1
        fi
    done
    # After finishing up with one PDF, add it's length to the page-counter so
    # the next loop iteration knows where bookmarks should start
    ((PAGE += len))
done
echo "done."

echo -n "Merging all PDFs..."
pdftk "${PDFS[@]}" cat output TMP.pdf
echo "done."

echo -n "Adding bookmarks..."
pdftk TMP.pdf dump_data output meta.txt
# Remove all preexisting bookmarks from the merged file
sed -i '/Bookmark/d' meta.txt
# Add the new computed bookmarks
cat meta.txt bookmarks.txt > new_meta.txt
# Update the metadata of the merged PDF suppressing warnings
pdftk TMP.pdf update_info new_meta.txt output "$OUTPUT" 2> /dev/null
echo "done."

echo -n "Starting OCR..."
ocrmypdf "$OUTPUT" "OCR_$OUTPUT" "$OCROPTS"
echo "done."

echo -n "Cleaning up..."
rm meta.txt new_meta.txt bookmarks.txt TMP.pdf
echo "done."
