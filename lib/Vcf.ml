(*
    Vcf.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    The model's calls as VCF: a header, and one record per site where some genotype other than
    the reference's is real enough.

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
    (* WHAT A RECORD SAYS, AND WHAT IT LEAVES OUT. A site is written when at least one genotype
        other than the reference base's has a posterior of being real at or above the threshold;
        those genotypes are its alternate alleles, and the record carries, for each, how many
        reads said it, what fraction of the reads voting for a base that is, their mean base
        quality and the posterior, and for the site the reads voting and those voting for the
        reference base. Genotypes below the threshold are not written: a VCF is a list of calls,
        and an error allele at every deep site would drown them; the same model run without a
        threshold is SiNPle's own format. An N called by the reads is not an allele.
       THE ALLELES ARE ANCHORED ON THE REFERENCE BASE, as the format wants: a substitution is REF
        the base, ALT the other; an insertion after the site is REF the base, ALT the base then the
        inserted sequence; a deletion is REF the base then the deleted sequence, ALT the base. A
        site with several alleles has one REF, the base and the longest deletion, and each ALT is
        completed with what it leaves of that deletion, every deletion at a site being a prefix of
        the longest, all of them starting at the same base of the reference *)
    let quality_of posterior =
      if posterior >= 1. then 999. else min 999. (-10. *. log10 (1. -. posterior))
    let record ?(min_posterior = 0.95) (g: Genotype.t) =
      let refr = if g.refr = "" then "N" else String.sub g.refr 0 1 in
      let is_base symbol = String.length symbol = 1 && symbol <> "N" in
      let alts =
        Array.to_list g.info
          |> List.filter
            (fun (gb: Genotype.genobase_t) ->
              gb.p_value >= min_posterior && gb.symbol <> refr
              && (is_base gb.symbol || gb.symbol.[0] = '+' || gb.symbol.[0] = '-')) in
      if alts = [] then
        None
      else begin
        let deleted =
          List.fold_left
            (fun acc (gb: Genotype.genobase_t) ->
              if gb.symbol.[0] = '-' then begin
                let d = String.sub gb.symbol 1 (String.length gb.symbol - 1) in
                if String.length d > String.length acc then d else acc
              end else
                acc)
            "" alts in
        let ref_allele = refr ^ deleted in
        let alt_allele (gb: Genotype.genobase_t) =
          let rest = String.sub gb.symbol 1 (String.length gb.symbol - 1) in
          match gb.symbol.[0] with
          | '+' -> refr ^ rest ^ deleted
          | '-' -> refr ^ String.sub deleted (String.length rest) (String.length deleted - String.length rest)
          | _ -> gb.symbol ^ deleted in
        (* The reads voting for a base, and for the reference base *)
        let voting = ref 0 and reference = ref 0 in
        Array.iter
          (fun (gb: Genotype.genobase_t) ->
            if String.length gb.symbol = 1 then begin
              voting := !voting + gb.counts;
              if gb.symbol = refr then
                reference := gb.counts
            end)
          g.info;
        let per_alt f = List.map f alts |> String.concat "," in
        let quality = List.fold_left (fun acc (gb: Genotype.genobase_t) -> max acc gb.p_value) 0. alts in
        Printf.sprintf "%s\t%d\t.\t%s\t%s\t%.4g\tPASS\tDP=%d;RD=%d;AC=%s;AF=%s;BQ=%s;PP=%s\tDP:AD\t%d:%d,%s"
          g.seq g.pos ref_allele (per_alt alt_allele) (quality_of quality) !voting !reference
          (per_alt (fun gb -> string_of_int gb.counts))
          (per_alt (fun gb -> Printf.sprintf "%.4g" (if !voting = 0 then 0. else float_of_int gb.counts /. float_of_int !voting)))
          (per_alt
            (fun gb ->
              Printf.sprintf "%.3g" (if gb.counts = 0 then 0. else float_of_int (QualitiesDistribution.get_sum gb.quals) /. float_of_int gb.counts)))
          (per_alt (fun gb -> Printf.sprintf "%.3g" gb.p_value))
          !voting !reference (per_alt (fun gb -> string_of_int gb.counts))
        |> Option.some
      end
    (* The header: the source and its parameters, the reference and its contigs when they are
        known -- they are with the mapper's own output, and not from a pileup, which names a
        contig only when its first line comes -- and what the records' fields mean *)
    let header ?reference ?(contigs = [||]) ~source ~(parameters: Genotype.parameters_t) ~sample () =
      let buf = Buffer.create 2048 in
      let t = Unix.localtime (Unix.time ()) in
      Printf.bprintf buf "##fileformat=VCFv4.3\n##fileDate=%04d%02d%02d\n##source=%s\n"
        (1900 + t.Unix.tm_year) (t.Unix.tm_mon + 1) t.Unix.tm_mday source;
      Option.iter (Printf.bprintf buf "##reference=file://%s\n") reference;
      Array.iter (fun (name, length) -> Printf.bprintf buf "##contig=<ID=%s,length=%d>\n" name length) contigs;
      Printf.bprintf buf
        "##SiNPle_parameters=<theta=%g,theta_indel=%g,quality_indel_short=%d,quality_indel_long=%d,\
         pcr_error_rate=%g,pcr_error_rate_indel=%g,error_rate=%g,error_rate_indel_short=%g,\
         error_rate_indel_long=%g>\n"
        parameters.theta parameters.theta_indel parameters.q_indel_short parameters.q_indel_long
        parameters.pcr_error_rate_substitution parameters.pcr_error_rate_indel
        parameters.error_rate_substitution parameters.error_rate_indel_short
        parameters.error_rate_indel_long;
      Buffer.add_string buf
        "##INFO=<ID=DP,Number=1,Type=Integer,Description=\"Reads voting for a base at the site\">\n\
         ##INFO=<ID=RD,Number=1,Type=Integer,Description=\"Reads voting for the reference base\">\n\
         ##INFO=<ID=AC,Number=A,Type=Integer,Description=\"Reads voting for each alternate allele\">\n\
         ##INFO=<ID=AF,Number=A,Type=Float,Description=\"Fraction of the reads voting for a base that voted for each alternate allele\">\n\
         ##INFO=<ID=BQ,Number=A,Type=Float,Description=\"Mean base quality of the reads voting for each alternate allele, 0 for an indel\">\n\
         ##INFO=<ID=PP,Number=A,Type=Float,Description=\"Posterior probability that each alternate allele is real, as SiNPle's model gives it\">\n\
         ##FORMAT=<ID=DP,Number=1,Type=Integer,Description=\"Reads voting for a base at the site\">\n\
         ##FORMAT=<ID=AD,Number=R,Type=Integer,Description=\"Reads voting for the reference base and for each alternate allele\">\n";
      Printf.bprintf buf "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\t%s\n" sample;
      Buffer.contents buf
  end: sig
    (* The record for a site, if some genotype other than the reference base's has a posterior
        at or above the threshold: CHROM POS ID REF ALT QUAL FILTER INFO FORMAT and one sample,
        QUAL being the Phred-scaled complement of the best alternate allele's posterior *)
    val record: ?min_posterior:float -> Genotype.t -> string option
    (* The header, ending with the column line naming the one sample. The reference's path and
        its contigs, as (name, length), are written when given *)
    val header:
      ?reference:string -> ?contigs:(string * int) array -> source:string ->
      parameters:Genotype.parameters_t -> sample:string -> unit -> string
  end
)

