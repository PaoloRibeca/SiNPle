(* *)

module Argv:
  sig
    type class_t =
      | Mandatory (* Implies no default *)
      | Optional (* Implies no default *)
      | Default of (unit -> string) (* Implies optional - the function prints the default *)
    (* The specs from which usage is produced are a tuple with the following elements:
        (1) equivalent option names
        (2) optional explanation for the argument(s)
        (3) help text lines
        (4) mandatory/optional * default/no-default class
        (5) parsing action when option is encountered *)
    type spec_t = string list * string option * string list * class_t * (string -> unit)
    val usage: ?output:out_channel -> unit -> unit
    val get_parameter: unit -> string
    val get_int_parameter: unit -> int
    val get_float_parameter: unit -> float
    val get_pos_int_parameter: unit -> int
    val get_pos_float_parameter: unit -> float
    val get_non_neg_int_parameter: unit -> int
    val get_non_neg_float_parameter: unit -> float
    val parse: spec_t list -> unit
  end
= struct
    type class_t =
      | Mandatory
      | Optional
      | Default of (unit -> string)
    type spec_t = string list * string option * string list * class_t * (string -> unit)
    let argv = Sys.argv
    let i = ref 1
    let buf = ref ("Usage:\n " ^ argv.(0) ^ "\n")
    let usage ?(output = stdout) () = Printf.fprintf output "%s" !buf
    let error ?(output = stderr) f_n msg =
      usage ~output:output ();
      Printf.fprintf output "Argv.%s: %s\n%!" f_n msg;
      exit 1
    let template_get n what f =
      (fun () ->
        try
          f ()
        with _ ->
          error n ("Option '" ^ argv.(!i - 1) ^ "' needs a " ^ what ^ " parameter"))
    let template_filter f g =
      (fun () ->
        let res = f () in
        if res |> g then
          res
        else
          raise Not_found)
    let get_parameter =
      template_get "get_parameter" "" (fun () -> incr i; argv.(!i))
    let get_int_parameter =
      template_get "get_integer_parameter" "integer" (fun () -> get_parameter () |> int_of_string)
    let get_float_parameter =
      template_get "get_float_parameter" "float" (fun () -> get_parameter () |> float_of_string)
    let get_pos_int_parameter =
      template_get "get_pos_int_parameter" "positive integer"
      (template_filter get_int_parameter (fun x -> x > 0))
    let get_pos_float_parameter =
      template_get "get_pos_float_parameter" "positive float"
      (template_filter get_float_parameter (fun x -> x > 0.))
    let get_non_neg_int_parameter =
      template_get "get_non_neg_int_parameter" "non-negative integer"
      (template_filter get_int_parameter (fun x -> x >= 0))
    let get_non_neg_float_parameter =
      template_get "get_non_neg_float_parameter" "non-negative float"
      (template_filter get_float_parameter (fun x -> x >= 0.))
    let parse specs =
      let module StringMap = Map.Make (String) in
      let module StringSet = Set.Make (String) in
      let table = ref StringMap.empty and mandatory = ref StringSet.empty in
      List.iteri
        (fun i (opts, vl, help, clss, act) ->
          buf := !buf ^ "  ";
          if opts = [] && help = [] then
            error "parse" ("Malformed initializer for option #" ^ string_of_int i);
          List.iteri
            (fun i opt ->
              if i > 0 then
                buf := !buf ^ "|";
              buf := !buf ^ opt;
              if StringMap.mem opt !table then
                error "parse" ("Duplicate command line option '" ^ opt ^ "' in table");
              if clss = Mandatory then begin
                let repr = List.hd opts in
                mandatory := StringSet.add repr !mandatory;
                table :=
                  StringMap.add opt
                    (fun arg ->
                      mandatory := StringSet.remove repr !mandatory;
                      act arg)
                    !table
              end else
                table := StringMap.add opt act !table)
            opts;
          buf := !buf ^ begin
            match vl with
            | None -> ""
            | Some vl ->  " " ^ vl
          end ^ begin
            if opts <> [] then
              "\n"
            else
              ""
          end;
          List.iteri
            (fun i help -> buf := !buf ^ "\t" ^ help ^ "\n")
            help;
          match clss with
          | Mandatory ->
            buf := !buf ^ "\t(mandatory)\n"
          | Optional -> ()
          | Default def ->
            buf := !buf ^ "\t(default='" ^ def () ^ "')\n")
        specs;
      let len = Array.length argv in
      while !i < len do
        let arg = argv.(!i) in
        begin try
          StringMap.find arg !table
        with Not_found ->
          error "parse" ("Unknown option '" ^ argv.(!i) ^ "'")
        end arg;
        incr i
      done;
      if !mandatory <> StringSet.empty then
        StringSet.iter
          (fun opt ->
            error "parse" ("Option '" ^ opt ^ "' is mandatory"))
          !mandatory
  end

module IntMap: Map.S with type key = int
  = Map.Make(struct type t = int let compare = compare end)
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
    let get_sum qs =
      let res = ref 0 in
      IntMap.iter
        (fun qual times ->
          res := !res + times * qual)
        qs;
      !res
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

module StringMap: Map.S with type key = String.t
  = Map.Make(String)

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
    let empty = StringMap.empty
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
      if len < 6 then begin
        Printf.eprintf "Insufficient number of fields in input\n%!";
        exit 1
      end;
      let refr, pileup, quals = line.(2), line.(4), line.(5) in
      if String.length refr > 1 then
        parse_error ("Invalid reference '" ^ refr ^ "'");
	    let refr_uc = String.uppercase_ascii refr and refr_lc = String.lowercase_ascii refr
      and len = String.length pileup and res = ref empty in
      if len > 0 then begin
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
          parse_error (Printf.sprintf "Lengths of pileup and qualities are inconsistent (%d vs. %d)" !qpos qlen);
        incr parsed_lines
      end;
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
      q_indel: int; (* Result of a computation *)
      (* We do not believe variants below this [frequency] *)
      pcr_error: float;
      (* The following ones are computed from the whole experiment -- q_eff would in fact be a substitution matrix *)
      q_eff: int;
      q_indel_eff: int
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
      q_indel: int;
      pcr_error: float;
      q_eff: int;
      q_indel_eff: int
    }
    (* Accessory functions *)
    let zero_if_div_by_zero a b =
      if b > 0 then
        float_of_int a /. float_of_int b
      else
        0.
    (* Approximation of the log of the factorial *)
    let log_stirling n =
      let pi = 4. *. atan 1.
      and f_n = float_of_int n in
      let log_2_pi = log (2. *. pi)
      and log_n = log f_n in
      f_n *. (log_n -. 1.)
        +. (log_2_pi +. log_n) /. 2.
        +. 1. /. (12. *. f_n)
        -. 1. /. (360. *. f_n *. f_n *. f_n)
    (* In fact fst will contain the accumulated statistics *)
    let lucas fst snd parameters =
      if snd.counts > 0 then begin
        let is_indel =
          match snd.symbol.[0] with
          | '+' | '-' -> true
          | _ -> false in
        let c_f_snd = float_of_int snd.counts in
        let c_f_fst = float_of_int fst.counts -. c_f_snd in
        let var_fst = QualitiesDistribution.get_variance fst.quals
        and soq_fst = QualitiesDistribution.get_sum fst.quals
        and soq_snd = QualitiesDistribution.get_sum snd.quals in
        let q_fst = (float_of_int soq_fst) /. 10.
        and q_snd = (float_of_int soq_snd) /. 10.
        and pi = 4. *. atan 1. and log10 = log 10. in

Printf.eprintf "'%s': q_snd=%g\tall=%d\tc_f_fst=%g\tc_f_snd=%g\tfirst=%g\tsecond=%g\tthird=%g\tfour=%g\n%!" snd.symbol q_snd fst.counts c_f_fst c_f_snd begin
                +. log_stirling (fst.counts + snd.counts)
                -. log_stirling fst.counts -. log_stirling snd.counts
                -. log10 *. float_of_int begin
                  if is_indel then
                    parameters.q_indel_eff
                  else
                    parameters.q_eff
                end *. c_f_snd /. 10.
end begin
                -. log10 *. begin
                  if is_indel then
                    float_of_int parameters.q_indel_eff *. c_f_snd /. 10.
                  else
                    q_snd
                end
end begin
                if is_indel then
                  0.
                else
                  2. *. (parameters.pcr_error *. (c_f_fst +. c_f_snd) /. (c_f_snd ** 2.))
end begin
                exp begin
                  -. begin
                    let what = 10. *. (q_snd /. c_f_snd -. q_fst /. (c_f_fst +. c_f_snd)) in
                    what *. what
                  end /. 2. /. var_fst *. c_f_snd
                end
                /. sqrt (2. *. pi *. var_fst *. c_f_snd)
                *. ((c_f_fst +. c_f_snd) /. (c_f_fst *. c_f_snd))
                *. begin
                  if is_indel then
                    parameters.theta_indel
                  else
                    parameters.theta
                end
end;

        exp begin
          -. log1p begin
            begin
              exp begin
                +. log_stirling (fst.counts + snd.counts)
                -. log_stirling fst.counts -. log_stirling snd.counts
                -. log10 *. float_of_int begin
                  if is_indel then
                    parameters.q_indel_eff
                  else
                    parameters.q_eff
                end *. c_f_snd /. 10.
              end +.
              exp begin
                -. log10 *. begin
                  if is_indel then
                    float_of_int parameters.q_indel_eff *. c_f_snd /. 10.
                  else
                    q_snd
                end
              end +. begin
                if is_indel then
                  0.
                else
                  2. *. (parameters.pcr_error *. (c_f_fst +. c_f_snd) /. (c_f_snd ** 2.))
              end
            end
              /. begin
                exp begin
                  -. begin
                    let what = 10. *. q_snd /. c_f_snd -. q_fst /. (c_f_fst +. c_f_snd) in
                    what *. what
                  end /. 2. /. var_fst *. c_f_snd
                end
                /. sqrt (2. *. pi *. var_fst *. c_f_snd)
                *. ((c_f_fst +. c_f_snd) /. (c_f_fst *. c_f_snd))
                *. begin
                  if is_indel then
                    parameters.theta_indel
                  else
                    parameters.theta
                end
              end
          end
        end
      end else
        0.
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
      let cumul = ref { symbol = ""; counts = 0; quals = QualitiesDistribution.empty; p_value = 0. } in
      List.iter
        (fun { counts; quals } ->
          cumul :=
            { !cumul with
              counts = !cumul.counts + counts;
              quals = QualitiesDistribution.merge !cumul.quals quals })
        !res;
      let cumul = !cumul in
      (* *)
      let res =
        Array.of_list begin
          List.map
            (fun genobase ->
              { genobase with p_value = lucas cumul genobase parameters })
            !res
        end in
      (* We want most frequent bases first *)
      Array.sort
        (fun a b ->
          if a.counts < b.counts then
            1
          else if a.counts > b.counts then
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
    pcr_error=0.5*10^-6 (min frequency=2*pcr_error/theta~10^-3)
    q_eff=40
    q_indel_eff=q_indel
*)

module Defaults =
  struct
    let input_file = ""
    let output_file = ""
    let theta = 0.001
    let q_indel = 25
    let pcr_error = 0.5e-6
    let q_eff = 40
    let strandedness = Strandedness.Both
  end

module Params =
  struct
    let input_file = ref Defaults.input_file
    let output_file = ref Defaults.output_file
    let theta = ref Defaults.theta
    let theta_indel = ref (Defaults.theta /. 10.)
    let q_indel = ref Defaults.q_indel
    let pcr_error = ref Defaults.pcr_error
    let q_eff = ref Defaults.q_eff
    let q_indel_eff = ref Defaults.q_indel
    let strandedness = ref Defaults.strandedness
  end

let version = "0.3"

let _ =
  Printf.eprintf "This is the SiNPle SNP calling program (version %s)\n%!" version;
  Printf.eprintf " (c) 2017-2018 Luca Ferretti, <luca.ferretti@gmail.com>\n%!";
  Printf.eprintf " (c) 2017-2018 Chandana Tennakoon, <drcyber@gmail.com>\n%!";
  Printf.eprintf " (c) 2017-2018 Paolo Ribeca, <paolo.ribeca@gmail.com>\n%!";
  Argv.parse [
    [], None, [ "=== Algorithmic parameters ===" ], Argv.Optional, (fun _ -> ());
    [ "-t"; "--theta" ],
      Some "<non_negative_float>",
      [ "prior estimate of nucleotide diversity" ],
      Argv.Default (fun () -> string_of_float !Params.theta),
      (fun _ -> Params.theta := Argv.get_non_neg_float_parameter ());
    [ "-T"; "--theta-indel" ],
      Some "<non_negative_float>",
      [ "prior estimate of indel likelihood" ],
      Argv.Default (fun () -> string_of_float !Params.theta_indel),
      (fun _ -> Params.theta_indel := Argv.get_non_neg_float_parameter ());
    [ "-I"; "--indel-quality" ],
      Some "<non_negative_integer>",
      [ "...whatever that is..." ],
      Argv.Default (fun () -> string_of_int !Params.q_indel),
      (fun _ -> Params.q_indel := Argv.get_non_neg_int_parameter ());
    [ "-p"; "--pcr-error" ],
      Some "<non_negative_float>",
      [ "prior estimate of PCR error level" ],
      Argv.Default (fun () -> string_of_float !Params.pcr_error),
      (fun _ -> Params.pcr_error := Argv.get_non_neg_float_parameter ());
    [ "--effective-quality" ],
      Some "<non_negative_integer>",
      [ "...whatever that is..." ],
      Argv.Default (fun () -> string_of_int !Params.q_eff),
      (fun _ -> Params.q_eff := Argv.get_non_neg_int_parameter ());
    [ "--effective-indel-quality" ],
      Some "<non_negative_integer>",
      [ "...whatever that is..." ],
      Argv.Default (fun () -> string_of_int !Params.q_indel_eff),
      (fun _ -> Params.q_indel_eff := Argv.get_non_neg_int_parameter ());
    [ "-s"; "-S"; "--strandedness" ],
      Some "forward|reverse|both",
      [ "strands to be taken into account for counts" ],
      Argv.Default (fun () -> Strandedness.to_string !Params.strandedness),
      (fun _ -> Params.strandedness := Strandedness.of_string (Argv.get_parameter ()));
    [], None, [ "=== Input/Output ===" ], Argv.Optional, (fun _ -> ());
    [ "-i"; "--input" ],
      Some "<input_file>",
      [ "name of input file (in mpileup format)" ],
      Argv.Default (fun () -> if !Params.input_file = "" then "<stdin>" else !Params.input_file),
      (fun _ -> Params.input_file := Argv.get_parameter() );
    [ "-o"; "--output" ],
      Some "<output_file>",
      [ "name of output file" ],
      Argv.Default (fun () -> if !Params.output_file = "" then "<stdout>" else !Params.output_file),
      (fun _ -> Params.output_file := Argv.get_parameter() );
    [], None, [ "=== Miscellaneous ===" ], Argv.Optional, (fun _ -> ());
    [ "-h"; "--help" ],
      None,
      [ "print syntax and exit" ],
      Argv.Optional,
      (fun _ -> Argv.usage (); exit 1)
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
    q_indel = !Params.q_indel;
    pcr_error = !Params.pcr_error;
    q_eff = !Params.q_eff;
    q_indel_eff = !Params.q_indel_eff
  } in
  try
    while true do
      let pileup = Pileup.from_mpileup_line (input_line input) in
      let genotype = Genotype.from_pileup pileup !Params.strandedness parameters in
      Printf.fprintf output "%s\n%!" (Genotype.to_sinple genotype)



    done
  with End_of_file -> ()

