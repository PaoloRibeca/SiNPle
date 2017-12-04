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

let log_stirling n =
  let pi = 4. *. atan 1.
  and f_n = float_of_int n in
  let log_2_pi = log (2. *. pi)
  and log_n = log f_n in
  f_n *. (log_n -. 1.)
    +. (log_2_pi +. log_n) /. 2.
    +. 1. /. (12. *. f_n)
    -. 1. /. (360. *. f_n *. f_n *. f_n)

module IntMap: Map.S with type key = int
  = Map.Make(struct type t = int let compare = compare end)

module StringMap: Map.S with type key = String.t
  = Map.Make(String)

module Distribution:
  sig
    include module type of IntMap
    val combine: key t -> key t -> key t
    val to_string: key t -> string * string
    val to_tsv: key t -> string
  end
= struct
    include IntMap
    let combine = union (fun _ a b -> Some (a + b))
		let tab_sep a b = if a="" then b else a^"\t"^b

    let to_string dist =
      let res_quals = Buffer.create 1024 and res_counts = Buffer.create 1024 in
      iter
        (fun key value ->
          if Buffer.length res_quals <> 0 then begin
            Buffer.add_string res_quals ",";
            Buffer.add_string res_counts ","
          end;
          Buffer.add_string res_quals (string_of_int key);
          Buffer.add_string res_counts (string_of_int value))
        dist;
      Buffer.contents res_quals, Buffer.contents res_counts

    let to_tsv dist =
		let res = ref "" in
		for i = 0 to 80 do
			try 
				let qv = IntMap.find i dist in res := tab_sep !res  (string_of_int qv);
			with Not_found -> res := tab_sep !res "0"
		done;
		!res


  end

type distribution = int Distribution.t

module Pileup:
  sig
    type t = {
      seq: string;
      pos: int;
      refr: string;
      (* We rely upon the fact that genotypes on the reverse strand are lowercase.
         Data structure is:
          genotype -> (occurencies, sum of qualities, quality distribution) *)
      info: (int * int * distribution) StringMap.t
    }
    val from_mpileup_line: ?quality_offset:int -> string -> t
  end
= struct
    type t = {
      seq: string;
      pos: int;
      refr: string;
      info: (int * int * distribution) StringMap.t
    }
    let update_distribution d a = (* Stores distribution data *)
      try
        let t = Distribution.find a d in
        Distribution.add a (t + 1) d
      with Not_found->
        Distribution.add a 1 d
    let add_to stats what qual = (* map current_character current_quality *)
      try
        let num, qsum, distr = StringMap.find what stats in
        (* In this case the binding gets replaced *)
        StringMap.add what (num + 1, qsum + qual, update_distribution distr qual) stats
      with Not_found ->
        StringMap.add what (1, qual, update_distribution Distribution.empty qual) stats
    let parsed_lines = ref 1
    let parse_error s =
      Printf.eprintf "On line %d: %s\n%!" !parsed_lines s;
      exit 1
    (* Constructor from a line of mpileup *)
    let from_mpileup_line ?(quality_offset = 33) line =
      let line = Array.of_list (String.split_on_char '\t' line) in
      let len = Array.length line in
      if len < 6 then begin
        Printf.eprintf "Insufficient number of fields in input\n%!";
        exit 1
      end;
      let refr, pileup, quals = line.(2), line.(4), line.(5) in
      if String.length refr > 1 then (* reference base can only be one char *)
        parse_error ("Invalid reference '" ^ refr ^ "'");
      let refr_uc = String.uppercase_ascii refr and refr_lc = String.lowercase_ascii refr
      and len = String.length pileup and res = ref StringMap.empty  in
      if len > 0 then begin
        let quality_from_ascii c = Char.code c - quality_offset
        and i = ref 0 and qpos = ref 0 in
        while !i < len do
          let c = String.sub pileup !i 1 in
          begin match c with
          | "A" | "C" | "G" | "T" | "N" | "a" | "c" | "g" | "t" | "n" ->
            res := add_to !res c (quality_from_ascii quals.[!qpos]);
            incr qpos
          | "." -> (* match reference *)
            res := add_to !res refr_uc (quality_from_ascii quals.[!qpos]);
            incr qpos
          | "," -> (* match RC *)
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
            end do (* count indel size *)
              ()(*why is the body empty*)
            done;
            let how_many = int_of_string !how_many in
            (* In this case the qualities will become known only at some later point *)
            res := add_to !res (dir ^ String.sub pileup !i how_many) 0;
            i := !i + how_many - 1 (* Skip *)
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
      quals: int;
      distr: distribution
    }
    type t = {
      seq: string;
      pos: int;
      info: genobase_t array
    }
    val from_pileup: Pileup.t -> Strandedness.t -> t
  end
= struct
    type genobase_t = {
      symbol: string;
      counts: int;
      quals: int;
      distr: distribution
    }
    type t = {
      seq: string;
      pos: int;
      info: genobase_t array
    }
    let from_pileup pileup strandedness =
      let res = ref [] in
      (* We kill all unwanted pileup features according to cases *)
      begin match strandedness with
      | Strandedness.Forward ->
        (* Eliminate lowercase *)
        StringMap.iter
          (fun s (counts, quals, distr) ->
            if String.lowercase_ascii s <> s then
              res := { symbol = String.uppercase_ascii s; counts = counts; quals = quals; distr = distr } :: !res)
          pileup.Pileup.info
      | Strandedness.Reverse ->
        (* Eliminate uppercase, and turn lowercase to uppercase *)
        StringMap.iter
          (fun s (counts, quals, distr) ->
            if String.uppercase_ascii s <> s then
              res := { symbol = String.uppercase_ascii s; counts = counts; quals = quals; distr = distr } :: !res)
          pileup.Pileup.info             
      | Strandedness.Both ->
        (* Sum lowercase to uppercase, eliminate lowercase *)
        let new_info = ref StringMap.empty in
        StringMap.iter
          (fun s (counts, quals, distr) ->
            let upp = String.uppercase_ascii s in
            try
              let counts_upp, quals_upp, distr_upp = StringMap.find upp !new_info in
              new_info :=
                StringMap.add upp
                  (counts_upp + counts, quals_upp + quals, Distribution.combine distr_upp distr)
                  !new_info
            with Not_found ->
              new_info := StringMap.add upp (counts, quals, distr) !new_info)
          pileup.Pileup.info;
        StringMap.iter
          (fun s (counts, quals, distr) ->
            res := { symbol = String.uppercase_ascii s; counts = counts; quals = quals; distr = distr } :: !res)
          !new_info
      end;
      let res = Array.of_list !res in
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

  end

module Defaults =
  struct
    let input_file = ""
    let output_file = ""
    let theta = 0.001
    let strandedness = Strandedness.Both
  end

module Params =
  struct
    let input_file = ref Defaults.input_file
    let output_file = ref Defaults.output_file
    let theta = ref Defaults.theta
    let strandedness = ref Defaults.strandedness
  end

let version = "0.1"

let calculate_stats gtype_info = let i=ref 0 and tot = ref 0 and f=Array.make 8 0. in Array.iter
					(fun v ->
							tot:=!tot + v.Genotype.counts;
							match v.Genotype.symbol with  
							|"a"|"c"|"g"|"t"|"A"|"C"|"G"|"T" -> (* We only consider SNP variations*)
									f.(!i) <- float_of_int v.Genotype.counts;i := !i + 1;() 
							|_ -> ()
					)
					gtype_info;
(*

	We calculate the formulas m1 = f_2.f_3/f_1, m2 = (1-f_1)f_3/f_1 and m3 = f_2^2/f_4 

*)
(* DBG:Array.iteri (fun a b -> Printf.fprintf stderr "%i -- %.3g, " a b) f;Printf.fprintf stderr "\n";*)
					let f=Array.map (fun x -> x /. float_of_int !tot) f in
						let m1=ref (-1.) and  m2 = ref (-1.) and m3 = ref (-1.) in
							match (!i-1) with
							|0|1 -> (!m1,!m2,!m3)  
							|2 -> m1 := f.(1) *. f.(2) /. f.(0); m2 := (1. -. f.(0)) *. f.(2) /. f.(0); (!m1,!m2,!m3)  
							|_ -> m1 := f.(1) *. f.(2) /. f.(0); m2 := (1. -. f.(0)) *. f.(2) /. f.(0); m3 := f.(1) *. f.(1)/. f.(3); (!m1,!m2,!m3)    





let _ =
  Printf.eprintf "This is the SiNPle SNP calling program (version %s)\n%!" version;
  Printf.eprintf " (c) 2017 Luca Ferretti, <luca.ferretti@gmail.com>\n%!";
  Printf.eprintf " (c) 2017 Paolo Ribeca, <paolo.ribeca@gmail.com>\n%!";
  Argv.parse [
    [], None, [ "=== Algorithmic parameters ===" ], Argv.Optional, (fun _ -> ());
    [ "-t"; "-T"; "--theta" ],
      Some "<non_negative_float>",
      [ "prior estimate of nucleotide diversity" ],
      Argv.Default (fun () -> string_of_float !Params.theta),
      (fun _ -> Params.theta := Argv.get_non_neg_float_parameter ());
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
  let zero_if_div_by_zero a b =
    if b > 0 then
      float_of_int a /. float_of_int b
    else
      0.
  and lucas info theta =
    match info with
    | [||] | [|_|] -> 0.
    | x ->
      let fst = x.(0) and snd = x.(1) in
      if snd.Genotype.counts > 0 then begin
        let c_f_fst = float_of_int fst.Genotype.counts
        and c_f_snd = float_of_int snd.Genotype.counts in
        let q_fst = (float_of_int fst.Genotype.quals (*/. c_f_fst*)) /. 10.
        and q_snd = (float_of_int snd.Genotype.quals (*/. c_f_snd*)) /. 10. in
        let q_min = min q_fst q_snd
        and q_max = max q_fst q_snd
        and log10 = log 10. in
        exp begin
          -. log1p begin
            begin
              exp begin
                log_stirling (fst.Genotype.counts + snd.Genotype.counts)
                  -. log_stirling fst.Genotype.counts -. log_stirling snd.Genotype.counts
                  -. q_min *. log10 +. log1p (10. ** (q_min -. q_max))
              end
            end
              /. begin
                ((c_f_fst +. c_f_snd) /. (c_f_fst *. c_f_snd))
                *. theta
              end
          end
        end
      end else
        0. in
	let triplet = [|"_";"_";"_"|] and lastline = ref "" in
  try

		Printf.fprintf output "#Sequence_Name\tPosition\tBase_1\tBase_2\tBase_3\tBase_4\tCount_1\tCount_2\tCount_3\tCount4\tQual_1\tQual_2\tQual_3\tQual_4\tHist_1\tHist_2\tHist_3\tHist_4\tTriplet\n";
    while true do
			let bases = [|"";"";"";""|] and counts = [|0;0;0;0|] and quals = [|0.;0.;0.;0.|] and histo = [|"";"";"";""|]  and i = ref 0 in
      let pileup = Pileup.from_mpileup_line (input_line input) in
      let genotype = Genotype.from_pileup pileup !Params.strandedness in
			i:=0;
      Array.iter
        (fun g ->
					match g.Genotype.symbol with  
					|"a"|"c"|"g"|"t"|"A"|"C"|"G"|"T" -> 
							bases.(!i) <- g.Genotype.symbol;
							counts.(!i) <- g.Genotype.counts;
							quals.(!i) <- (zero_if_div_by_zero g.Genotype.quals g.Genotype.counts);
							histo.(!i) <- Distribution.to_tsv g.Genotype.distr;
							i := !i + 1
					|_ ->()
					)
        genotype.Genotype.info;

				if ( !i < 4 ) then for j = !i to 3 do histo.(j) <- Distribution.to_tsv Distribution.empty done;
				if !lastline = "" then (* find the triplet consisting of the current base and the adjacent  bases of the current base .. *)
						triplet.(1) <- bases.(0)
				else
					begin
						triplet.(2) <- if bases.(0) = "" then "_" else bases.(0);
						Printf.fprintf output "%s\t%s\n" !lastline  (triplet.(0)^triplet.(1)^triplet.(2));
						triplet.(0) <- triplet.(1); triplet.(1) <- triplet.(2)
					end;

				lastline := Printf.sprintf "%s\t%d\t%s\t%s\t%s\t%s\t%i\t%i\t%i\t%i\t%.3g\t%.3g\t%.3g\t%.3g\t%s\t%s\t%s\t%s" genotype.Genotype.seq genotype.Genotype.pos bases.(0) bases.(1) bases.(2) bases.(3) counts.(0) counts.(1) counts.(2) counts.(3) quals.(0) quals.(1) quals.(2) quals.(3) histo.(0) histo.(1) histo.(2) histo.(3);

    done

  with End_of_file -> ( if !lastline != "" then Printf.fprintf output "%s\t%s\n" !lastline  (triplet.(0)^triplet.(1)^triplet.(2)))

