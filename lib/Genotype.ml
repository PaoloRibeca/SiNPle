(*
    Genotype.ml -- (c) 2017-2026 Luca Ferretti, Chandana Tennakoon, Paolo Ribeca

    The model: the p-value of each genotype seen at a position against the
    others, from how many reads said it and with what qualities, and the line
    SiNPle writes for the position.

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

include (
  struct
    type genobase_t = {
      symbol: string;
      counts: int;
      quals: Mpileup.Qualities.t;
      p_value: float
    }
    type t = {
      seq: string;
      pos: int;
      refr: string;
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
            let mean_acc = Mpileup.Qualities.mean acc_quals
            and mean_gb = Mpileup.Qualities.mean_above_fraction gb.quals (1. -. tail_fraction)
            and var_acc = Mpileup.Qualities.variance acc_quals in
            (* Variance cannot be zero *)
            let var_acc = max 1. var_acc in
            let soq_gb = Mpileup.Qualities.sum gb.quals in
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
    let from_pileup pileup parameters =
      (* What arrives has had its strand resolved by the reader and is already
         uppercase, both strands summed where both were wanted, so there is
         nothing to filter here any more *)
      let res = ref [] in
      StringMap.iter
        (fun symbol (counts, quals) ->
          res := { symbol = symbol; counts = counts; quals = quals; p_value = 0. } :: !res)
        pileup.Pileup.info;
      (* In order to be able to compute the p-value we need the cumulative statistics *)
      let acc_acgtn_counts = ref 0 and acc_quals = Mpileup.Qualities.make () in
      List.iter
        (fun { symbol; counts; quals; _ } ->
          begin match symbol.[0] with
          | 'A' | 'C' | 'G' | 'T' | 'N' ->
            acc_acgtn_counts := !acc_acgtn_counts + counts
          | '+' | '-' ->
            ()
          | _ ->
            assert false
          end;
          Mpileup.Qualities.merge_into ~into:acc_quals quals)
        !res;
      let acc_acgtn_counts = !acc_acgtn_counts in
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
      { seq = pileup.Pileup.seq; pos = pileup.Pileup.pos; refr = pileup.Pileup.refr; info = res }
    let to_sinple { seq; pos; info } =
      let res = Buffer.create 512 in
      Buffer.add_string res (Printf.sprintf "%s\t%d" seq pos);
      Array.iter
        (fun { symbol; counts; quals; p_value } ->
          let soq = Mpileup.Qualities.sum quals in
          Buffer.add_string res begin
            Printf.sprintf "\t%s\t%d\t%.3g\t%.3g"
              symbol counts (zero_if_div_by_zero soq counts) p_value
          end)
        info;
      Buffer.contents res
    let [@warning "-27"] from_sinple file =
      { seq = "";
        pos = 0;
        refr = "";
        info = [||] }
    let [@warning "-27"] recalibrate_p_values obj x = obj
  end: sig
    type genobase_t = {
      symbol: string;
      counts: int;
      quals: Mpileup.Qualities.t;
      p_value: float
    }
    type t = {
      seq: string;
      pos: int;
      (* The reference base, as the input said it: one base, or N where the input did not know *)
      refr: string;
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
    val from_pileup: Pileup.t -> parameters_t -> t
    val to_sinple: t -> string
    val from_sinple: string -> t
    val recalibrate_p_values: t -> parameters_t -> t
  end
)

