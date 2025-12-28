#!/bin/bash

# ==============================================================
# Script: find_mpi_calls.sh
# Purpose:
#   Search for all calls to MPI collective functions in a given
#   source code directory and save the results to an output file.
#
# Usage:
#   ./find_mpi_calls.sh <source_directory> <output_file>
#   - <source_directory> : directory to search recursively
#   - <output_file>      : file to store search results
# ==============================================================

# Проверяем, что заданы два аргумента
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <source_directory> <output_file>"
    exit 1
fi

SOURCE_DIR="$1"
OUTPUT="$2"

# Очищаем файл перед запуском
> "$OUTPUT"

# Список MPI коллективных функций
MPI_FUNCTIONS=(
    MPI_Barrier
    MPI_Bcast
    MPI_Gather
    MPI_Gatherv
    MPI_Scatter
    MPI_Scatterv
    MPI_Allgather
    MPI_Allgatherv
    MPI_Alltoall
    MPI_Alltoallv
    MPI_Alltoallw
    MPI_Reduce
    MPI_Allreduce
    MPI_Reduce_scatter
    MPI_Reduce_scatter_block
    MPI_Scan
    MPI_Exscan
)

# Поиск каждой функции в указанной папке
for func in "${MPI_FUNCTIONS[@]}"; do
    echo "Searching for $func ..." >> "$OUTPUT"
    grep -rin "$func" "$SOURCE_DIR" >> "$OUTPUT"
    echo -e "\n" >> "$OUTPUT"
done

echo "Search complete. Results saved in $OUTPUT"

