Compilation Instructions
ocamlc heap.mli heap.ml io.mli io.ml compress.ml -o compress
ocamlc heap.mli heap.ml io.mli io.ml decompress.ml -o decompress

Usage Instructions
./compress infile outfile
./decompress infile outfile

Notes
- infile : path of input file
- outfile : path of output file
- required files : ensure that heap.mli heap.ml io.mli io.ml are all found in the root folder of the program.
- reserved filename : "tmp_203049238281.txt" is a reserved filename for the decompression process. Do not input or output into the file during decompression.
- testing files : marx.txt LKY.txt DNA.txt empty.txt sparse.txt binary.txt