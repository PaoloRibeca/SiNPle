(*
    Caller.ml -- (c) 2017-2026 Luca Ferretti, Chandana Tennakoon, Paolo Ribeca

    The SiNPle command: calls the model on every position of its input, which is
    an mpileup, as samtools mpileup writes it, or, with --map, what gem3-mapper
    -F MAP wrote for the reads mapped to a reference, walked against that
    reference directly.

    This program was designed and developed by the author(s),
    with the assistance of the following AI tool(s):
      2026 Claude (Anthropic).
    The final logic and implementation were reviewed and verified in
    their entirety by the author(s).

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open BiOCamLib
open Better
open SiNPle

(* (1) Open a set of SiNPle files
   (2) Read next line from all files.
        Assume inputs are synchronised, i.e. they have been produced with mpileup option -a -a
   (3) Parse lines, compute union of genotypes and frequences
        Table: (stage,genotype)->(counts,frequency,p-value)
        Frquencies are w.r.t. the sum of genotypes for which the genotype has been deemed to be a variant
   (X) If in at least one condition p-value of variant <= thresold, then:
        output chromosome, position, variant, maximum/minimum/average global p-value (or all p-values?), list of frequences in all stages.
       Output one line per variant
   (Y) Global Shannon entropy?
   (Z) Convert to VCF?
*)

(*
  At the beginning (unless we are recalibrating p-values)
    theta=10^-3
    theta_indel=theta/10
    q_indel=25
    pcr_error_rate=0.5*10^-6 (min frequency=2*pcr_error_rate/theta~10^-3)
    q_eff=40
    q_eff_indel=q_indel
*)

module Defaults =
  struct
    let input_file = ""
    let output_file = ""
    let map_reference = ""
    let strata = 1
    let vcf = ""
    let vcf_min_posterior = 0.95
    let vcf_sample = "sample"
    let theta = 0.001
    let q_indel_short = 35
    let q_indel_long = 45
    let pcr_error_rate_substitution = 0.5e-6
    let error_rate_substitution = 1.e-4
    let error_rate_indel_short = 1.e-5
    let error_rate_indel_long = 1.e-6
    let strandedness = Strandedness.Both
  end

module Params =
  struct
    let input_file = ref Defaults.input_file
    let output_file = ref Defaults.output_file
    let map_reference = ref Defaults.map_reference
    let strata = ref Defaults.strata
    let vcf = ref Defaults.vcf
    let vcf_min_posterior = ref Defaults.vcf_min_posterior
    let vcf_sample = ref Defaults.vcf_sample
    let theta = ref Defaults.theta
    let theta_indel = ref (Defaults.theta /. 10.)
    let q_indel_short = ref Defaults.q_indel_short
    let q_indel_long = ref Defaults.q_indel_long
    let pcr_error_rate_substitution = ref Defaults.pcr_error_rate_substitution
    let pcr_error_rate_indel = ref (Defaults.pcr_error_rate_substitution /. 10.)
    let error_rate_substitution = ref Defaults.error_rate_substitution
    let error_rate_indel_short = ref Defaults.error_rate_indel_short
    let error_rate_indel_long = ref Defaults.error_rate_indel_long
    let strandedness = ref Defaults.strandedness
  end

let authors = [
  "2017-2019", "Luca Ferretti", "luca.ferretti@gmail.com";
  "2017-2019", "Chandana Tennakoon", "drcyber@gmail.com";
  "2017-2026", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  (* SiNPle.Info is the library's generated module, BiOCamLib.Info the other library's *)
  TA.set_header (SiNPle.Info.info, authors, [ BiOCamLib.Info.info ]);
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator "Algorithmic parameters";
    [ "-t"; "--theta" ],
      Some "<non_negative_float>",
      [ "prior estimate of nucleotide diversity" ],
      TA.Default (fun () -> string_of_float !Params.theta),
      (fun _ -> Params.theta := TA.get_parameter_float_non_neg ());
    [ "-T"; "--theta-indel" ],
      Some "<non_negative_float>",
      [ "prior estimate of indel likelihood" ],
      TA.Default (fun () -> string_of_float !Params.theta_indel),
      (fun _ -> Params.theta_indel := TA.get_parameter_float_non_neg ());
    [ "-I"; "--quality-indel-short" ],
      Some "<non_negative_integer>",
      [ "prior Phred-scaled quality for indels of length 1" ],
      TA.Default (fun () -> string_of_int !Params.q_indel_short),
      (fun _ -> Params.q_indel_short := TA.get_parameter_int_non_neg ());
    [ "-L"; "--quality-indel-long" ],
      Some "<non_negative_integer>",
      [ "prior Phred-scaled quality for indels of length >1" ],
      TA.Default (fun () -> string_of_int !Params.q_indel_long),
      (fun _ -> Params.q_indel_long := TA.get_parameter_int_non_neg ());
    [ "-p"; "--pcr-error-rate" ],
      Some "<non_negative_float>",
      [ "prior estimate of error rate for PCR-generated substitutions" ],
      TA.Default (fun () -> string_of_float !Params.pcr_error_rate_substitution),
      (fun _ -> Params.pcr_error_rate_substitution := TA.get_parameter_float_non_neg ());
    [ "-P"; "--pcr-error-rate-indel" ],
      Some "<non_negative_float>",
      [ "prior estimate of error rate for PCR-generated indels" ],
      TA.Default (fun () -> string_of_float !Params.pcr_error_rate_indel),
      (fun _ -> Params.pcr_error_rate_indel := TA.get_parameter_float_non_neg ());
    [ "--error-rate" ],
      Some "<non_negative_float>",
      [ "prior estimate of error rate for sequencing-generated substitutions" ],
      TA.Default (fun () -> string_of_float !Params.error_rate_substitution),
      (fun _ -> Params.error_rate_substitution := TA.get_parameter_float_non_neg ());
    [ "--error-rate-indel-short" ],
      Some "<non_negative_float>",
      [ "prior estimate of error rate for sequencing-generated indels of length 1" ],
      TA.Default (fun () -> string_of_float !Params.error_rate_indel_short),
      (fun _ -> Params.error_rate_indel_short := TA.get_parameter_float_non_neg ());
    [ "--error-rate-indel-long" ],
      Some "<non_negative_float>",
      [ "prior estimate of error rate for sequencing-generated indels of length >1" ],
      TA.Default (fun () -> string_of_float !Params.error_rate_indel_long),
      (fun _ -> Params.error_rate_indel_long := TA.get_parameter_float_non_neg ());
    [ "-s"; "-S"; "--strandedness" ],
      Some "forward|reverse|both",
      [ "strands to be taken into account for counts" ],
      TA.Default (fun () -> Strandedness.to_string !Params.strandedness),
      (fun _ -> Params.strandedness := Strandedness.of_string (TA.get_parameter ()));
    TA.make_separator "Input/Output";
    [ "-i"; "--input" ],
      Some "<input_file>",
      [ "name of input file (in mpileup format, or with --map in GEM MAP format)" ],
      TA.Default (fun () -> if !Params.input_file = "" then "<stdin>" else !Params.input_file),
      (fun _ -> Params.input_file := TA.get_parameter() );
    [ "-m"; "--map" ],
      Some "<reference_fasta_file>",
      [ "the input is what gem3-mapper -F MAP wrote for the reads mapped";
        "to the given reference rather than an mpileup: what the reads say";
        "about each position is read off it directly. The reads must have";
        "been mapped from FASTQ, as the model needs their qualities" ],
      TA.Default (fun () -> if !Params.map_reference = "" then "<none>" else !Params.map_reference),
      (fun _ -> Params.map_reference := TA.get_parameter ());
    [ "--strata" ],
      Some "<positive_integer>",
      [ "with --map, count only the placements in the first that many";
        "non-empty strata of each read, a stratum being its placements";
        "with the same number of errors: 1 keeps its best placements alone" ],
      TA.Default (fun () -> string_of_int !Params.strata),
      (fun _ -> Params.strata := TA.get_parameter_int_pos ());
    [ "-o"; "--output" ],
      Some "<output_file>",
      [ "name of output file" ],
      TA.Default (fun () -> if !Params.output_file = "" then "<stdout>" else !Params.output_file),
      (fun _ -> Params.output_file := TA.get_parameter() );
    [ "--vcf" ],
      Some "<vcf_file>",
      [ "also write the calls as VCF to the given file: one record per site";
        "where some read said something other than the reference base, every";
        "such genotype being an alternate allele, and FILTER PASS when one of";
        "them has a posterior at or above --vcf-minimum-posterior. The";
        "reference base must be known, which it is with --map and with an";
        "mpileup made with -f" ],
      TA.Default (fun () -> if !Params.vcf = "" then "<none>" else !Params.vcf),
      (fun _ -> Params.vcf := TA.get_parameter ());
    [ "--vcf-minimum-posterior" ],
      Some "<fraction>",
      [ "the posterior some alternate allele needs for its record to PASS" ],
      TA.Default (fun () -> string_of_float !Params.vcf_min_posterior),
      (fun _ -> Params.vcf_min_posterior := TA.get_parameter_float_fraction ());
    [ "--vcf-sample" ],
      Some "<name>",
      [ "the name of the VCF's one sample" ],
      TA.Default (fun () -> !Params.vcf_sample),
      (fun _ -> Params.vcf_sample := TA.get_parameter ());
    TA.make_separator "Miscellaneous";
    [ "-V"; "--version" ],
      None,
      [ "print version and exit" ],
      TA.Optional,
      (fun _ -> Printf.printf "%s\n%!" SiNPle.Info.info.version; exit 0);
    (* Hidden option to emit help in markdown format *)
    [ "--markdown" ], None, [], TA.Optional, (fun _ -> TA.markdown (); exit 0);
    [ "-h"; "--help" ],
      None,
      [ "print syntax and exit" ],
      TA.Optional,
      (fun _ -> TA.usage (); exit 0)
  ];
  let input =
    if !Params.input_file = "" then
      stdin
    else
      open_in !Params.input_file
  and output =
    if !Params.output_file = "" then
      stdout
    else
      open_out !Params.output_file in
  let parameters = {
    Genotype.theta = !Params.theta;
    theta_indel = !Params.theta_indel;
    q_indel_short = !Params.q_indel_short;
    q_indel_long = !Params.q_indel_long;
    pcr_error_rate_substitution = !Params.pcr_error_rate_substitution;
    pcr_error_rate_indel = !Params.pcr_error_rate_indel;
    error_rate_substitution = !Params.error_rate_substitution;
    error_rate_indel_short = !Params.error_rate_indel_short;
    error_rate_indel_long = !Params.error_rate_indel_long
  } in
  (* Decided once: it is a parameter of the run, not of a position *)
  let strand = Strandedness.to_strand !Params.strandedness in
  (* The reference the reads were mapped to, its sequences named as the mapper names them *)
  let reference =
    if !Params.map_reference = "" then
      None
    else begin
      let res = ref [] in
      Files.Reads.iter ~linter:Fun.id ~verbose:false
        (fun (_, _, { Files.Base.Read.tag; seq; _ }) ->
          List.accum res (List.hd (String.split_on_char ' ' tag), seq))
        (Files.Reads.FASTA !Params.map_reference);
      Some (List.rev !res |> Array.of_list)
    end in
  (* The VCF, when asked for, knows the reference and its contigs only when the input is the
      mapper's own *)
  let vcf =
    if !Params.vcf = "" then
      None
    else begin
      let oc = open_out !Params.vcf in
      VCF.header ?reference:(if reference = None then None else Some !Params.map_reference)
        ?contigs:(Option.map (Array.map (fun (name, seq) -> name, String.length seq)) reference)
        ~min_posterior:!Params.vcf_min_posterior ~source:("SiNPle " ^ SiNPle.Info.info.version)
        ~parameters ~sample:!Params.vcf_sample ()
        |> output_string oc;
      Some oc
    end in
  let call pileup =
    let genotype = Genotype.from_pileup pileup parameters in
    Printf.fprintf output "%s\n%!" (Genotype.to_sinple genotype);
    Option.iter
      (fun oc ->
        VCF.record ~min_posterior:!Params.vcf_min_posterior genotype
          |> Option.iter (Printf.fprintf oc "%s\n"))
      vcf in
  begin match reference with
  | None ->
    begin try
      while true do
        Pileup.from_mpileup_line ?strand (input_line input) |> call
      done
    with End_of_file ->
      ()
    end
  | Some reference ->
    Mpileup.Gem.iter ~qualities:true ~strata:!Params.strata ?strand
      ~path:(if !Params.input_file = "" then "-" else !Params.input_file) ~reference
      (fun summary -> Pileup.of_summary summary |> call)
      input
  end;
  Option.iter close_out vcf

