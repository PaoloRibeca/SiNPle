To compile SiNPle, please install ocaml (http://www.ocaml.org/docs/install.html) and dune and run the following command:

  ./BUILD

This will compile the binary 'SiNPle' and 'SiNPle -h' would give information on all the options.
An example of generating SiNPle variant calls from the bamfile example.bam would be

  samtools mpileup -d 1000000 -a -A -B -Q 0 -x example.bam | SiNPle > variants.txt

