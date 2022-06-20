(*
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

module IntMap = Tools.IntMap
module QualitiesDistribution =
  struct
    include IntMap
    let get_cardinal qs =
      let res = ref 0 in
      IntMap.iter
        (fun _ times ->
          res := !res + times)
        qs;
      !res
    let get_tail ?(fraction = 0.5) qs =
      let card = get_cardinal qs in
      let to_discard = int_of_float (floor (float_of_int card *. (1. -. fraction)))
      and discarded = ref 0 and res = ref IntMap.empty in
      IntMap.iter
        (fun qual times ->
          if !discarded < to_discard then begin
            if !discarded + times >= to_discard then begin
              let not_ok = min times (to_discard - !discarded) in
              discarded := !discarded + not_ok;
              if times > not_ok then
                res := IntMap.singleton qual (times - not_ok)
            end else
              discarded := !discarded + times
          end else
            res := IntMap.add qual times !res)
        qs;
      !res
    let get_sum qs =
      let res = ref 0 in
      IntMap.iter
        (fun qual times ->
          res := !res + times * qual)
        qs;
      !res
    let get_mean qs =
      let acc = ref 0 and counts = ref 0 in
      IntMap.iter
        (fun qual times ->
          counts := !counts + times;
          acc := !acc + times * qual)
        qs;
      float_of_int !acc /. float_of_int !counts
    let get_variance qs =
      let card = get_cardinal qs in
      let mean = float_of_int (get_sum qs) /. float_of_int card in
      let res = ref 0 in
      IntMap.iter
        (fun qual times ->
          res := !res + times * qual * qual)
        qs;
      let f_red_card = float_of_int (card - 1) in
      float_of_int !res /. f_red_card -. mean *. mean *. (f_red_card +. 1.) /. f_red_card
    let merge qs_1 qs_2 =
      let res = ref qs_1 in
      IntMap.iter
        (fun qual times ->
          try
            res := IntMap.add qual (IntMap.find qual !res + times) !res
          with Not_found ->
            res := IntMap.add qual times !res)
        qs_2;
      !res
  end
type qualities_distribution_t = int QualitiesDistribution.t

module StringMap = Tools.StringMap

module Strand:
  sig
    type t =
      | Forward
      | Reverse
  end
= struct
    type t =
      | Forward
      | Reverse
  end

module Pileup:
  sig
    type t = {
      seq: string;
      pos: int;
      refr: string;
      (* Genotypes are ordered lexicographically here *)
      info: (int * qualities_distribution_t) StringMap.t
    }
    val from_mpileup_line: ?quality_offset:int -> string -> t
  end
= struct
    type t = {
      seq: string;
      pos: int;
      refr: string;
      info: (int * qualities_distribution_t) StringMap.t
    }
    let add_to stats what qual =
      try
        let num, quals = StringMap.find what stats in
        let quals =
          try
            let cntr = QualitiesDistribution.find qual quals in
            QualitiesDistribution.add qual (cntr + 1) quals
          with Not_found ->
            QualitiesDistribution.add qual 1 quals in
        StringMap.add what (num + 1, quals) stats
      with Not_found ->
        StringMap.add what (1, QualitiesDistribution.singleton qual 1) stats
    let parsed_lines = ref 1
    let parse_error s =
      Printf.eprintf "On line %d: %s\n%!" !parsed_lines s;
      exit 1
    let from_mpileup_line ?(quality_offset = 33) line =
      let line = Array.of_list (String.split_on_char '\t' line) in
      let len = Array.length line in
      if len < 6 then
        parse_error "Insufficient number of fields in input";
      let refr, pileup, quals = line.(2), line.(4), line.(5) in
      if String.length refr > 1 then
        "Invalid reference '" ^ refr ^ "'" |> parse_error;
      let refr_uc = String.uppercase_ascii refr and refr_lc = String.lowercase_ascii refr
      and len = String.length pileup and res = ref StringMap.empty in
      (* Here len might be > 0 even if the pileup is empty ("*") *)
      if line.(3) <> "0" && len > 0 then begin
        let quality_from_ascii c = Char.code c - quality_offset
        and i = ref 0 and qpos = ref 0 in
        while !i < len do
          let c = String.sub pileup !i 1 in
          begin match c with
          | "A" | "C" | "G" | "T" | "N" | "a" | "c" | "g" | "t" | "n" ->
            res := add_to !res c (quality_from_ascii quals.[!qpos]);
            incr qpos
          | "." ->
            res := add_to !res refr_uc (quality_from_ascii quals.[!qpos]);
            incr qpos
          | "," ->
            res := add_to !res refr_lc (quality_from_ascii quals.[!qpos]);
            incr qpos
          | "+" | "-" as dir -> (* Beginning of indel *)
            let how_many = ref "" in
            while begin
              incr i;
              let cc = String.sub pileup !i 1 in
              match cc with
              | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
                how_many := !how_many ^ cc;
                true
              | _ ->
                false
            end do
              ()
            done;
            let how_many = int_of_string !how_many in
            (* In this case the qualities will become known only at some later point *)
            res := add_to !res (dir ^ String.sub pileup !i how_many) 0;
            i := !i + how_many - 1
          | "*" | ">" | "<" ->
            (* Internal part of indel, soft clips.
               Due to some mysterious reason, they all come associated with a quality *)
            incr qpos
          | "^" -> (* Beginning of a read, followed by a quality *)
            incr i
          | "$" -> (* End of read *)
            ()
          | _ ->
            parse_error ("Unknown character '" ^  c ^ "' in pileup")
          end;
          incr i
        done;
        let qlen = String.length quals in
        if !qpos <> qlen then
          Printf.sprintf "Lengths of pileup and qualities are inconsistent (%d vs. %d)" !qpos qlen
            |> parse_error
      end;
      incr parsed_lines;
      { seq = line.(0); pos = int_of_string line.(1); refr = refr; info = !res }
  end

module Strandedness:
  sig
    type t =
      | Forward
      | Reverse
      | Both
    val of_string: string -> t
    val to_string: t -> string
  end
= struct
    type t =
      | Forward
      | Reverse
      | Both
    let of_string = function
      | "forward" -> Forward
      | "reverse" -> Reverse
      | "both" -> Both
      | s ->
        Printf.eprintf "Strandedness.of_string: Unknown initializer '%s'\n%!" s;
        exit 1
    let to_string = function
      | Forward -> "forward"
      | Reverse -> "reverse"
      | Both -> "both"
  end

module Genotype:
  sig
    type genobase_t = {
      symbol: string;
      counts: int;
      quals: qualities_distribution_t;
      p_value: float
    }
    type t = {
      seq: string;
      pos: int;
      info: genobase_t array
    }
    type parameters_t = {
      theta: float;
      theta_indel: float;
      (* A default quality/confidence level for indels
          (we cannot easily read it from the pileup) *)
      q_indel_short: int;
      q_indel_long: int;
      (* We do not believe variants below this [frequency] *)
      pcr_error_rate_substitution: float;
      pcr_error_rate_indel: float;
      (* Recalibrated parameters -- they are computed from the whole experiment -- q_eff would in fact be a substitution matrix *)
      error_rate_substitution: float;
      error_rate_indel_short: float;
      error_rate_indel_long: float
    }
    val from_pileup: Pileup.t -> Strandedness.t -> parameters_t -> t
    val to_sinple: t -> string
    val from_sinple: string -> t
    val recalibrate_p_values: t -> parameters_t -> t
  end
= struct
    type genobase_t = {
      symbol: string;
      counts: int;
      quals: qualities_distribution_t;
      p_value: float
    }
    type t = {
      seq: string;
      pos: int;
      info: genobase_t array
    }
    type parameters_t = {
      theta: float;
      theta_indel: float;
      q_indel_short: int;
      q_indel_long: int;
      pcr_error_rate_substitution: float;
      pcr_error_rate_indel: float;
      error_rate_substitution: float;
      error_rate_indel_short: float;
      error_rate_indel_long: float
    }
    (* Accessory functions *)
    let zero_if_div_by_zero a b =
      if b > 0 then
        float_of_int a /. float_of_int b
      else
        0.
    (* Approximation of the log of the factorial *)
    let log_stirling = function
      | 0 | 1 -> 0.
      | n ->
        let pi = 4. *. atan 1.
        and f_n = float_of_int n in
        let log_2_pi = log (2. *. pi)
        and log_n = log f_n in
        f_n *. (log_n -. 1.)
          +. (log_2_pi +. log_n) /. 2.
          +. 1. /. (12. *. f_n)
          -. 1. /. (360. *. f_n *. f_n *. f_n)
    (* Mapping from theta to actual probabilities *)
    let smooth_inverse_theta f_counts theta =
      (* Luca says that 0.5 approximates Euler-Mascheroni constant well *)
      let log_counts = log f_counts +. 0.577216 in
      (1. -. theta) *. log_counts /. theta
    let lucas ?(tail_fraction = 0.75) acc_acgtn_counts acc_quals gb parameters =
      match acc_acgtn_counts with
      | 0 -> 0.
      | 1 -> 1.
      | _ ->
        if gb.counts = acc_acgtn_counts then
          (* Case of only one genotype, computation pointless *)
          1.
        else begin
          let is_indel =
            match gb.symbol.[0] with
            | '+' | '-' -> true
            | _ -> false in
          let is_long_indel = is_indel && String.length gb.symbol > 2 in
          let rem_counts = acc_acgtn_counts - gb.counts in
          let f_acc_acgtn_counts = float_of_int acc_acgtn_counts
          and f_gb_counts = float_of_int gb.counts
          and f_rem_counts = float_of_int rem_counts
          and log10 = log 10. in
          if is_indel then
            (* Case of an indel *)
            exp begin
              -. log1p begin
                begin
                  begin
                    exp begin
                      +. log_stirling acc_acgtn_counts
                      -. log_stirling rem_counts -. log_stirling gb.counts
                      +. begin
                        let error_rate_indel =
                          if is_long_indel then
                            parameters.error_rate_indel_long
                          else
                            parameters.error_rate_indel_short in
                        log error_rate_indel *. f_gb_counts
                        -. error_rate_indel *. f_rem_counts
                      end
                    end
                    +. exp begin
                      -. log10 *. begin
                        float_of_int begin
                          if is_long_indel then
                            parameters.q_indel_long
                          else
                            parameters.q_indel_short
                        end *. f_gb_counts /. 10.
                      end
                    end
                  end
                  +. begin
                    2. *. parameters.pcr_error_rate_indel
                    *. f_acc_acgtn_counts /. (f_gb_counts ** 2.)
                  end
                end
                *. begin
                  ((f_rem_counts *. f_gb_counts) /. f_acc_acgtn_counts)
                  *. smooth_inverse_theta f_acc_acgtn_counts parameters.theta_indel
                end
              end
            end
          else begin
            (* Case of a SNP *)
            let mean_acc = QualitiesDistribution.get_mean acc_quals
            and mean_gb = QualitiesDistribution.get_mean (QualitiesDistribution.get_tail ~fraction:tail_fraction gb.quals)
            and var_acc = QualitiesDistribution.get_variance acc_quals in
            (* Variance cannot be zero *)
            let var_acc = max 1. var_acc in
            let soq_gb = QualitiesDistribution.get_sum gb.quals in
            let q_gb = (float_of_int soq_gb) /. 10.
            and pi = 4. *. atan 1. in
            exp begin
              -. log1p begin
                begin
                  begin
                    exp begin
                      +. log_stirling acc_acgtn_counts
                      -. log_stirling rem_counts -. log_stirling gb.counts
                      +. log parameters.error_rate_substitution *. f_gb_counts
                      -. parameters.error_rate_substitution *. f_rem_counts
                    end
                    +. exp begin
                      -. log10 *. q_gb
                    end
                  end /. begin
                    exp begin
                      -. begin
                        let what = mean_gb -. mean_acc in
                        if what < 0. then
                          what *. what
                        else
                          0.
                      end /. 2. /. var_acc *. f_gb_counts *. tail_fraction
                    end /. sqrt (2. *. pi *. var_acc *. f_gb_counts *. tail_fraction)
                  end
                  +. begin
                    2. *. parameters.pcr_error_rate_substitution
                    *. f_acc_acgtn_counts /. (f_gb_counts ** 2.)
                  end
                end
                *. begin
                  ((f_rem_counts *. f_gb_counts) /. f_acc_acgtn_counts)
                  *. smooth_inverse_theta f_acc_acgtn_counts parameters.theta
                end
              end
            end
          end
        end
    let from_pileup pileup strandedness parameters =
      let res = ref [] in
      (* We kill all unwanted pileup features according to cases *)
      begin match strandedness with
      | Strandedness.Forward ->
        (* Eliminate lowercase *)
        StringMap.iter
          (fun s (counts, quals) ->
            if String.lowercase_ascii s <> s then
              res := { symbol = String.uppercase_ascii s; counts = counts; quals = quals; p_value= 0. } :: !res)
          pileup.Pileup.info
      | Strandedness.Reverse ->
        (* Eliminate uppercase, and turn lowercase to uppercase *)
        StringMap.iter
          (fun s (counts, quals) ->
            if String.uppercase_ascii s <> s then
              res := { symbol = String.uppercase_ascii s; counts = counts; quals = quals; p_value= 0. } :: !res)
          pileup.Pileup.info
      | Strandedness.Both ->
        (* Sum lowercase to uppercase, eliminate lowercase *)
        let new_info = ref StringMap.empty in
        StringMap.iter
          (fun s (counts, quals) ->
            let upp = String.uppercase_ascii s in
            try
              let counts_upp, quals_upp = StringMap.find upp !new_info in
              new_info :=
                StringMap.add upp (counts_upp + counts, QualitiesDistribution.merge quals_upp quals) !new_info
            with Not_found ->
              new_info := StringMap.add upp (counts, quals) !new_info)
          pileup.Pileup.info;
        StringMap.iter
          (fun s (counts, quals) ->
            res := { symbol = String.uppercase_ascii s; counts = counts; quals = quals; p_value= 0. } :: !res)
          !new_info
      end;
      (* In order to be able to compute the p-value we need the cumulative statistics *)
      let acc_acgtn_counts = ref 0 and acc_quals = ref QualitiesDistribution.empty in
      List.iter
        (fun { symbol; counts; quals } ->
          begin match symbol.[0] with
          | 'A' | 'C' | 'G' | 'T' | 'N' ->
            acc_acgtn_counts := !acc_acgtn_counts + counts
          | '+' | '-' ->
            ()
          | _ ->
            assert false
          end;
          acc_quals := QualitiesDistribution.merge !acc_quals quals)
        !res;
      let acc_acgtn_counts = !acc_acgtn_counts and acc_quals = !acc_quals in
      let res =
        Array.of_list begin
          List.map
            (fun genobase ->
              { genobase with p_value = lucas acc_acgtn_counts acc_quals genobase parameters })
            !res
        end in
      (* We want most frequent bases first, and then genotypes sorted by p-value *)
      Array.sort
        (fun a b ->
          if a.counts < b.counts then
            1
          else if a.counts > b.counts then
            -1
          else if a.p_value < b.p_value then
            1
          else if a.p_value > b.p_value then
            -1
          else
            0)
        res;
      { seq = pileup.Pileup.seq; pos = pileup.Pileup.pos; info = res }
    let to_sinple { seq; pos; info } =
      let res = Buffer.create 512 in
      Buffer.add_string res (Printf.sprintf "%s\t%d" seq pos);
      Array.iter
        (fun { symbol; counts; quals; p_value } ->
          let soq = QualitiesDistribution.get_sum quals in
          Buffer.add_string res begin
            Printf.sprintf "\t%s\t%d\t%.3g\t%.3g"
              symbol counts (zero_if_div_by_zero soq counts) p_value
          end)
        info;
      Buffer.contents res
    let from_sinple file =

      { seq = "";
        pos = 0;
        info = [||] }

    let recalibrate_p_values obj x = obj

  end

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

let version = "0.8"

let header =
  Printf.sprintf begin
    "This is the SiNPle variant calling program (version %s)\n" ^^
    " (c) 2017-2019 Luca Ferretti, <luca.ferretti@gmail.com>\n" ^^
    " (c) 2017-2019 Chandana Tennakoon, <drcyber@gmail.com>\n" ^^
    " (c) 2017-2021 Paolo Ribeca, <paolo.ribeca@gmail.com>\n"
  end version

let () =
  let module TA = Tools.Argv in
  TA.set_header header;
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
    [ "--error-rate-substitution" ],
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
      [ "name of input file (in mpileup format)" ],
      TA.Default (fun () -> if !Params.input_file = "" then "<stdin>" else !Params.input_file),
      (fun _ -> Params.input_file := TA.get_parameter() );
    [ "-o"; "--output" ],
      Some "<output_file>",
      [ "name of output file" ],
      TA.Default (fun () -> if !Params.output_file = "" then "<stdout>" else !Params.output_file),
      (fun _ -> Params.output_file := TA.get_parameter() );
    TA.make_separator "Miscellaneous";
    [ "-v"; "--version" ],
      None,
      [ "print version and exit" ],
      TA.Optional,
      (fun _ -> Printf.printf "%s\n%!" version; exit 0);
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
  try
    while true do
      let pileup = Pileup.from_mpileup_line (input_line input) in
      let genotype = Genotype.from_pileup pileup !Params.strandedness parameters in
      Printf.fprintf output "%s\n%!" (Genotype.to_sinple genotype)
    done
  with End_of_file ->
    ()

