#!/bin/bash

# Map positional args
DAY=$1
PATH_USERNAME=$2
SOURCE_PATH=$3
DEST_PATH=$4

# Make destination
DEST_PATH_WITH_SUBPATH=${DEST_PATH}/day-$DAY/$PATH_USERNAME

# Print constructed destination path
echo "Source path: ${SOURCE_PATH}"
echo "Destination path: ${DEST_PATH_WITH_SUBPATH}"
echo ""
read -p "Press enter if ok..."

# Make path
mkdir -p  $DEST_PATH_WITH_SUBPATH

# Sync minimal files for the day
rsync -azvu \
    --include data/inputs/$DAY.txt \
    --include data/examples/$DAY.txt \
    --include bin/$DAY.rs \
    --include helpers.rs \
    --include lib.rs \
    --exclude target/\
    --exclude template/ \
    --exclude Cargo.lock \
    --exclude *.rs \
    --exclude *.txt \
    --exclude ".*" \
    --exclude "data/puzzles/" \
    $SOURCE_PATH/src \
    $SOURCE_PATH/data \
    $SOURCE_PATH/Cargo.toml \
    $DEST_PATH_WITH_SUBPATH/./

# Make README
printf "[Solution](src/bin/$DAY.rs) for day $DAY.\n\nRun examples with:\n\`\`\`\ncargo test --release --features minimal\n\`\`\`\n\nAnd solve inputs with:\n\`\`\`\ncargo run --bin ${DAY} --release --features minimal\n\`\`\`\n" \
    > $DEST_PATH_WITH_SUBPATH/README.md
