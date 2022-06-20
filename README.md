# `SiNPle`: Simplified Inference of Novel Polymorphisms from Large coveragE

`SiNPle` is a fast and sensitive variant caller based on a simplified Bayesian approach to compute the posterior probability that a variant is not generated by sequencing errors or PCR artefacts. It is particularly suited to low-frequency variant calling, as described in [Ferretti et al. (2019)](https://www.mdpi.com/2073-4425/10/8/561/htm). While calling heterozygous variants in samples with defined ploidy is relatively straightforward &mdash; for instance, the expected allele frequency is 50% or 100% in the diploid case &mdash; the underlying assumptions for low-frequency variant calling are very different and require a different statistical approach.
In the Bayesian model used by `SiNPle` we explicitly consider the limit in which ploidy tends to infinity, which is equivalent to a mixture with a very large number of molecules, and thus no artificial constraints are imposed. You can still use `SiNPle` if you know what the ploidy of your system is &mdash; you just need to post-filter the calls made by `SiNPle`.

## Comparison with other callers

Some variant callers, such as GATK, assume a ploidy of two. Others have hidden hardwired thresholds &mdash; for instance, a minimum number of covering reads needed to make a call &mdash; and hence miss many existing low-frequency variants. A detailed comparison with the low-frequency variant callers LoFreq and VarScan2 performed on CirSeq data can be found in [Ferretti et al., 2019](https://www.mdpi.com/2073-4425/10/8/561/htm); it shows that the approach adopted by `SiNPle` is faster and much more accurate.

## Citation

If you use `SiNPle`, please cite

## Table of contents

[1. Installation](#1-installation)<br>
&emsp; [1.1. Conda channel](#11-conda-channel)<br>
&emsp; [1.2. Pre-compiled binaries](#12-pre-compiled-binaries)<br>
&emsp; [1.3. Manual install](#13-manual-install)<br>
[2. How to run it](#2-how-to-run-it)<br>
[3. Interpreting the output](#3-interpreting-the-output)<br>
[4. Command line syntax](#4-command-line-syntax)<br>

## 1. Installation

> :warning: Note that the only operating system we support is Linux. :warning:
>
> OCaml is highly portable and you might be able to manually compile/install everything successfully on other platforms (for instance, Mac) but you will have to do it yourself. 

There are several possible ways of installing the software on your machine: through `conda`; by downloading pre-compiled binaries (Linux x86_64 only); or manually.

### 1.1. Conda channel

> :construction: Coming soon! :construction:

### 1.2. Pre-compiled binaries 

### 1.3. Manual install

Alternatively, you can install `SiNPle` manually by cloning and compiling its sources. You'll need an up-to-date distribution of the OCaml compiler and the [Dune package manager](https://github.com/ocaml/dune) for that. Both can be installed through [OPAM](https://opam.ocaml.org/), the official OCaml distribution system. Once you have a working OPAM distribution you'll also have a working OCaml compiler, and Dune can be installed with the command
```
$ opam install dune
```
if it is not already present. Make sure that you install OCaml version 4.12 or later.

You'll also need a copy of the sources for the [BiOCamLib library](https://github.com/PaoloRibeca/BiOCamLib). We'll assume that you have cloned the repository in the directory `../BiOCamLib` with respect to the `SiNPle` sources; you'll have to modify the file `BUILD` in the `SiNPle` directory if that is not the case.

Then go to the directory into which you have downloaded the latest `SiNPle` sources, and type
```
$ ./BUILD
```

That should generate the executable `SiNPle`. Copy it to some favourite location in your PATH, for instance `~/.local/bin`.

## 2. How to run it

This will compile the binary 'SiNPle' and 'SiNPle -h' would give information on all the options.
An example of generating SiNPle variant calls from the bamfile example.bam would be

```
samtools mpileup -d 1000000 -a -A -B -Q 0 -x example.bam | SiNPle > variants.txt
```
  
## 3. Interpreting the output

## 4. Command line syntax

```
This is the SiNPle variant calling program (version 0.8)
 (c) 2017-2019 Luca Ferretti, <luca.ferretti@gmail.com>
 (c) 2017-2019 Chandana Tennakoon, <drcyber@gmail.com>
 (c) 2017-2021 Paolo Ribeca, <paolo.ribeca@gmail.com>
```
Usage:
```
./SiNPle
```

Algorithmic parameters
| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-t`<br>`--theta` | _&lt;non\_negative\_float&gt;_ |  prior estimate of nucleotide diversity | <ins>default=<mark>_0\.001_</mark></ins> |
| `-T`<br>`--theta-indel` | _&lt;non\_negative\_float&gt;_ |  prior estimate of indel likelihood | <ins>default=<mark>_0\.0001_</mark></ins> |
| `-I`<br>`--quality-indel-short` | _&lt;non\_negative\_integer&gt;_ |  prior Phred\-scaled quality for indels of length 1 | <ins>default=<mark>_35_</mark></ins> |
| `-L`<br>`--quality-indel-long` | _&lt;non\_negative\_integer&gt;_ |  prior Phred\-scaled quality for indels of length &gt;1 | <ins>default=<mark>_45_</mark></ins> |
| `-p`<br>`--pcr-error-rate` | _&lt;non\_negative\_float&gt;_ |  prior estimate of error rate for PCR\-generated substitutions | <ins>default=<mark>_5e\-07_</mark></ins> |
| `-P`<br>`--pcr-error-rate-indel` | _&lt;non\_negative\_float&gt;_ |  prior estimate of error rate for PCR\-generated indels | <ins>default=<mark>_5e\-08_</mark></ins> |
| `--error-rate-substitution` | _&lt;non\_negative\_float&gt;_ |  prior estimate of error rate for sequencing\-generated substitutions | <ins>default=<mark>_0\.0001_</mark></ins> |
| `--error-rate-indel-short` | _&lt;non\_negative\_float&gt;_ |  prior estimate of error rate for sequencing\-generated indels of length 1 | <ins>default=<mark>_1e\-05_</mark></ins> |
| `--error-rate-indel-long` | _&lt;non\_negative\_float&gt;_ |  prior estimate of error rate for sequencing\-generated indels of length &gt;1 | <ins>default=<mark>_1e\-06_</mark></ins> |
| `-s`<br>`-S`<br>`--strandedness` | _forward&#124;reverse&#124;both_ |  strands to be taken into account for counts | <ins>default=<mark>_both_</mark></ins> |

Input/Output
| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-i`<br>`--input` | _&lt;input\_file&gt;_ |  name of input file \(in mpileup format\) | <ins>default=<mark>_&lt;stdin&gt;_</mark></ins> |
| `-o`<br>`--output` | _&lt;output\_file&gt;_ |  name of output file | <ins>default=<mark>_&lt;stdout&gt;_</mark></ins> |

Miscellaneous
| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-v`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |
