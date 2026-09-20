(*
    Pileup.ml -- (c) 2017-2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    One position as the model wants it: what the reads said there, genotype by
    genotype, with how many said it and the qualities they said it with.  It is
    built from BiOCamLib's summary of a position, whoever made that -- a pileup
    line, or the mapper's own output walked against the reference.

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

type qualities_distribution_t = int QualitiesDistribution.t

include (
  struct
    type t = {
      seq: string;
      pos: int;
      refr: string;
      info: (int * qualities_distribution_t) StringMap.t
    }
    (* The reading is BiOCamLib's.  Mpileup.summarize counts a position into
       genotypes over a dense histogram of qualities; what is rebuilt below is
       the shape the model expects, rebuilt once per POSITION rather than once
       per read: the conversion walks the qualities that are present, of which
       there are a few dozen, where the counting walked the reads, of which
       there are thousands.
       The strand is resolved by the reader and can no longer be resolved after
       it: a summary merges the two strands and uppercases what it keeps, so
       which of them is wanted has to be said before the counting rather than
       filtered out of the result. *)
    let parsed_lines = ref 0
    let quals_of (g: Mpileup.Genotype.t) =
      match g.qualities with
      | Some qs ->
        let res = ref QualitiesDistribution.empty in
        Mpileup.Qualities.iter (fun q c -> res := QualitiesDistribution.add q c !res) qs;
        !res
      | None ->
        (* No quality is assigned by the machine to the presence of an indel,
           and this has always been recorded as a zero one.  It has to stay a
           zero rather than an absence: the cumulative distribution the p-value
           is taken against merges every genotype's qualities, an indel's
           included *)
        QualitiesDistribution.singleton 0 g.count
    let of_summary (summary: Mpileup.Summary.t) =
      { seq = summary.seq; pos = summary.pos; refr = String.make 1 summary.reference;
        info =
          List.fold_left
            (fun acc (g: Mpileup.Genotype.t) -> StringMap.add g.symbol (g.count, quals_of g) acc)
            StringMap.empty summary.genotypes }
    let from_mpileup_line ?(quality_offset = 33) ?strand line =
      incr parsed_lines;
      Mpileup.summarize ~quality_offset ?strand ~line_number:!parsed_lines line |> of_summary
  end: sig
    type t = {
      seq: string;
      pos: int;
      refr: string;
      (* Genotypes are ordered lexicographically here *)
      info: (int * qualities_distribution_t) StringMap.t
    }
    (* From the summary of a position, whoever made it *)
    val of_summary: Mpileup.Summary.t -> t
    (* From a line of a pileup, the lines being counted for the error messages *)
    val from_mpileup_line: ?quality_offset:int -> ?strand:Sequences.Types.strand_t -> string -> t
  end
)

