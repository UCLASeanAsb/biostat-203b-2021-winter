    #!/bin/sh
    # Select lines from the middle of a file.
    # Usage: bash middle.sh pride_and_prejudice.txt end_line num_lines
    head -n "$2" "$1" | tail -n "$3"
